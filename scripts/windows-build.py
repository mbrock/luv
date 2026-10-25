#!/usr/bin/env python3
"""Build/package on native Windows x64 with Python 3.12+ and MSYS2 UCRT64.

First run windows-bootstrap.sh in the UCRT64 shell. Then, from PowerShell:
  python scripts/windows-build.py --sources lisp-sources.tar.gz

windows-build.lisp configures ASDF/the native toolchain; windows-image.lisp
uses Deploy to dump the game with DLL reopen and resource relocation hooks.
Pass --software-smoke-test after installing MSYS2 Mesa to render a frame with
lavapipe in a relocated copy. Mesa and the compiler are never shipped.
"""

import argparse
import hashlib
import json
import os
from pathlib import Path, PurePosixPath, PureWindowsPath
import re
import shlex
import shutil
import struct
import subprocess
import sys
import tarfile
import tempfile


def run(args, **kwargs):
    try:
        return subprocess.run([str(a) for a in args], check=True, **kwargs)
    except subprocess.CalledProcessError as error:
        # Preserve captured diagnostics before temporary smoke directories go
        # away; a failed native boot must be diagnosable from the Actions log.
        if error.stdout:
            print(error.stdout, file=sys.stderr)
        if error.stderr:
            print(error.stderr, file=sys.stderr)
        raise


def extract_sources(archive, destination, lock):
    """Reject unsafe/Windows-ambiguous names before extracting any files."""
    with tarfile.open(archive, "r:gz") as tar:
        seen = set()
        members = tar.getmembers()
        for member in members:
            path = PurePosixPath(member.name)
            if (not (member.isfile() or member.isdir()) or path.is_absolute()
                    or ".." in path.parts or "\\" in member.name
                    or any(re.search(r'[<>:"|?*]', part)
                           or part.endswith((".", " "))
                           or PureWindowsPath(part).is_reserved()
                           for part in path.parts)):
                raise ValueError(f"Not a portable source archive entry: {member.name}")
            key = path.as_posix().casefold()
            if key in seen:
                raise ValueError(f"Duplicate/case-colliding archive entry: {member.name}")
            seen.add(key)
        tar.extractall(destination, members=members, filter="data")
    exported_lock = (destination / "flake.lock").read_bytes()
    manifest = json.loads((destination / "lisp-sources.json").read_text())
    if (manifest["format"] != 1
            or manifest["lockSha256"] != hashlib.sha256(exported_lock).hexdigest()
            or json.loads(exported_lock) != json.loads(lock.read_bytes())):
        raise ValueError("Lisp source archive does not match this checkout's flake.lock")
    return manifest


def dll_imports(objdump, binary, env):
    output = run([objdump, "-p", binary], env=env, capture_output=True,
                 text=True).stdout
    if "pei-x86-64" not in output:
        raise ValueError(f"Not a native x86-64 PE: {binary}")
    return re.findall(r"DLL Name:\s*(\S+)", output, re.IGNORECASE)


def copy_dll_closure(stage, native_bin, system32, inspect, extra=()):
    """Resolve PE imports, including DLLs loaded by CFFI (not PE imports)."""
    available = {p.name.casefold(): p for p in native_bin.glob("*.dll")}
    queued = list(stage.rglob("*.dll")) + [stage / "luvcraft.exe"] + list(extra)
    # These libraries are opened by name, some lazily. All come from the same
    # MSYS2 transaction as the headers used by the groveller.
    for pattern in ("SDL3.dll", "SDL3_image.dll", "SDL3_ttf.dll",
                    "libharfbuzz-0.dll", "libffi-*.dll", "vulkan-1.dll",
                    "avutil-*.dll", "avcodec-*.dll", "avformat-*.dll",
                    "swscale-*.dll", "libssl-*.dll", "libcrypto-*.dll"):
        matches = list(native_bin.glob(pattern))
        if len(matches) != 1:
            raise ValueError(f"Expected one UCRT64 {pattern}, found {matches}")
        queued.extend(matches)
    bundled = {}
    while queued:
        source = queued.pop()
        key = source.name.casefold()
        if key in bundled:
            if source.read_bytes() != bundled[key].read_bytes():
                raise ValueError(f"Conflicting DLLs named {source.name}")
            continue
        target = stage / source.name
        if source != target:
            if target.exists() and source.read_bytes() != target.read_bytes():
                raise ValueError(f"Refusing to replace {target} with a different DLL")
            shutil.copy2(source, target)
        bundled[key] = target
        for name in inspect(target):
            lower = name.casefold()
            if lower in bundled:
                continue
            if lower in available:
                queued.append(available[lower])
            elif (stage / name).is_file():
                queued.append(stage / name)
            elif (lower.startswith(("api-ms-win-", "ext-ms-win-"))
                  or (system32 / name).is_file()):
                continue  # Host Windows, never redistribute System32.
            else:
                raise ValueError(f"Unresolved import {name} required by {target.name}")
    return sorted(bundled)


def set_gui_subsystem(executable):
    """Patch only PE Subsystem; objcopy/strip can discard SBCL's appended core."""
    with executable.open("r+b") as stream:
        header = stream.read(64)
        if len(header) != 64 or header[:2] != b"MZ":
            raise ValueError("Missing DOS/PE executable header")
        pe = struct.unpack_from("<I", header, 0x3C)[0]
        stream.seek(pe)
        header = stream.read(94)
        if (len(header) != 94 or header[:4] != b"PE\0\0"
                or struct.unpack_from("<H", header, 4)[0] != 0x8664
                or struct.unpack_from("<H", header, 24)[0] != 0x20B
                or struct.unpack_from("<H", header, 92)[0] not in (2, 3)):
            raise ValueError("Expected an x86-64 PE32+ GUI/console executable")
        stream.seek(pe + 92)
        stream.write(struct.pack("<H", 2))  # IMAGE_SUBSYSTEM_WINDOWS_GUI


def check_ffmpeg(prefix, work, env):
    source = work / "check-ffmpeg.c"
    source.write_text('''#include <libavutil/avutil.h>
#include <libavutil/hwcontext_vulkan.h>
#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>
#include <libswscale/swscale.h>
#include <stdio.h>
int main(void) {
  printf("FFmpeg runtime/header majors: %u/%d %u/%d %u/%d %u/%d\\n",
         avutil_version() >> 16, LIBAVUTIL_VERSION_MAJOR,
         avcodec_version() >> 16, LIBAVCODEC_VERSION_MAJOR,
         avformat_version() >> 16, LIBAVFORMAT_VERSION_MAJOR,
         swscale_version() >> 16, LIBSWSCALE_VERSION_MAJOR);
  return (avutil_version() >> 16) != LIBAVUTIL_VERSION_MAJOR ||
         (avcodec_version() >> 16) != LIBAVCODEC_VERSION_MAJOR ||
         (avformat_version() >> 16) != LIBAVFORMAT_VERSION_MAJOR ||
         (swscale_version() >> 16) != LIBSWSCALE_VERSION_MAJOR;
}
''', encoding="utf-8")
    flags = run([prefix / "bin/pkg-config.exe", "--cflags", "--libs",
                 "libavutil", "libavcodec", "libavformat", "libswscale"],
                capture_output=True, text=True, env=env).stdout
    helper = work / "check-ffmpeg.exe"
    run([env["CC"], source, "-o", helper, *shlex.split(flags)], env=env)
    run([helper], env=env)


def relocated_smoke(stage, system_root, prefix, build_env, software):
    # The Lisp dependency tree is already gone. The boot resource checks and
    # optional render use only the moved bundle; PATH contains no build tools.
    with tempfile.TemporaryDirectory(prefix="luv relocated ") as temporary:
        root = Path(temporary)
        moved = root / "Moved Game"
        shutil.copytree(stage, moved)
        home = root / "home"
        home.mkdir()
        env = {"SystemRoot": str(system_root), "WINDIR": str(system_root),
               "PATH": str(system_root / "System32"), "TEMP": str(root),
               "TMP": str(root), "USERPROFILE": str(home), "HOME": str(home),
               "APPDATA": str(home), "LOCALAPPDATA": str(home)}
        help_result = run([moved / "luvcraft.exe", "--help"], cwd=root,
                          env=env, capture_output=True, text=True, timeout=120)
        if "Usage: luvcraft" not in help_result.stdout + help_result.stderr:
            raise ValueError("Relocated EXE did not produce Luvcraft's help")
        invalid = subprocess.run(
            [str(moved / "luvcraft.exe"), "--not-a-luvcraft-option"],
            cwd=root, env=env, capture_output=True, text=True, timeout=120)
        if (invalid.returncode != 1
                or "Invalid luvcraft arguments" not in invalid.stdout + invalid.stderr):
            raise ValueError(f"Invalid arguments did not fail correctly: {invalid}")
        if software:
            icd_path = prefix / "share/vulkan/icd.d/lvp_icd.x86_64.json"
            icd = json.loads(icd_path.read_text())
            driver = icd_path.parent / icd["ICD"]["library_path"]
            if not driver.is_file():
                driver = prefix / "bin" / Path(icd["ICD"]["library_path"]).name
            driver = driver.resolve(strict=True)
            # Lavapipe's own DLL imports need the same isolated loader path.
            # Add them only to this disposable smoke copy, never the release.
            copy_dll_closure(
                moved, prefix / "bin", system_root / "System32",
                lambda binary: dll_imports(prefix / "bin/objdump.exe", binary, build_env),
                extra=[driver])
            icd["ICD"]["library_path"] = (moved / driver.name).as_posix()
            private_icd = root / "lavapipe.json"
            private_icd.write_text(json.dumps(icd))
            env["VK_DRIVER_FILES"] = str(private_icd)
            image = root / "frame.png"
            run([moved / "luvcraft.exe", "--vulkan-smoke-test", image],
                cwd=root, env=env, timeout=600)
            if not image.read_bytes().startswith(b"\x89PNG\r\n\x1a\n"):
                raise ValueError("Vulkan smoke did not produce a PNG")
            image.unlink()
            run([moved / "luvcraft.exe", "--game-smoke-test", image],
                cwd=root, env=env, timeout=600)
            if not image.read_bytes().startswith(b"\x89PNG\r\n\x1a\n"):
                raise ValueError("Game save/reload smoke did not produce a PNG")
            shutil.copy2(image, Path(str(stage) + "-smoke.png"))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--sources", type=Path, required=True)
    parser.add_argument("--msys-root", type=Path, default=Path("C:/msys64"))
    parser.add_argument("--output", type=Path, default=Path("build/luvcraft-windows"))
    parser.add_argument("--software-smoke-test", action="store_true")
    args = parser.parse_args()
    if sys.platform != "win32" or struct.calcsize("P") != 8:
        parser.error("Run with native Windows x64 Python, not MSYS Python or WSL")
    root = Path(__file__).resolve().parent.parent
    archive = args.sources.resolve(strict=True)
    stage = args.output.resolve()
    if stage.exists() or Path(str(stage) + ".zip").exists():
        parser.error("Output already exists; choose a fresh --output directory")
    prefix = args.msys_root.resolve() / "ucrt64"
    native_bin = prefix / "bin"
    system_root = Path(os.environ["SystemRoot"])
    env = {k: v for k, v in os.environ.items()
           if not k.upper().startswith(("LUV_", "LUVCRAFT_", "CL_", "ASDF_"))}
    # No /usr/bin: build helpers and SBCL must all be native, not MSYS DLLs.
    env.update(PATH=os.pathsep.join(map(str, [native_bin, system_root / "System32",
                                            system_root])),
               SBCL_HOME=(prefix / "lib/sbcl").as_posix(),
               CC=(native_bin / "gcc.exe").as_posix(),
               CPATH=(prefix / "include").as_posix(),
               PKG_CONFIG_PATH=(prefix / "lib/pkgconfig").as_posix(),
               PKG_CONFIG_LIBDIR=(prefix / "lib/pkgconfig").as_posix())
    for filename in ("sbcl.exe", "gcc.exe", "pkg-config.exe", "objdump.exe"):
        if not (native_bin / filename).is_file():
            parser.error(f"Missing {native_bin / filename}; run windows-bootstrap.sh")
    stage.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix="windows-build-") as temporary:
        work = Path(temporary)
        deps = work / "sources"
        deps.mkdir()
        manifest = extract_sources(archive, deps, root / "flake.lock")
        check_ffmpeg(prefix, work, env)
        # Compile the game from a disposable checkout too. Removing only its
        # dependencies would leave embedded project/font paths able to work.
        game = work / "game"
        shutil.copytree(
            root, game,
            ignore=lambda directory, names:
                (".git", "build", ".amp") if Path(directory) == root
                else ("equity",) if Path(directory) == root / "fonts"
                else ("__pycache__",))
        stage.mkdir()
        env.update({"LUV_WINDOWS_ROOT": game.as_posix() + "/",
                    "LUV_WINDOWS_DEPS": deps.as_posix() + "/",
                    "LUV_WINDOWS_CACHE": (work / "cache").as_posix() + "/",
                    "LUV_WINDOWS_OUTPUT": stage.as_posix() + "/",
                    "LUV_WINDOWS_DLLS": native_bin.as_posix() + "/",
                    "LUV_FFMPEG_LIBDIR": native_bin.as_posix() + "/"})
        run([native_bin / "sbcl.exe", "--dynamic-space-size", "4096",
             "--noinform", "--disable-debugger", "--script",
             game / "scripts/windows-build.lisp"], cwd=game, env=env)
        executable = stage / "luvcraft.exe"
        if not executable.is_file():
            raise ValueError("Image builder did not emit LUV_WINDOWS_OUTPUT/luvcraft.exe")
        set_gui_subsystem(executable)
        dlls = copy_dll_closure(
            stage, native_bin, system_root / "System32",
            lambda binary: dll_imports(native_bin / "objdump.exe", binary, env))
        shutil.copytree(prefix / "share/licenses", stage / "licenses/msys2",
                        dirs_exist_ok=True)
        (stage / "lisp-sources.json").write_text(json.dumps(manifest, indent=2) + "\n")
        packages = run([args.msys_root / "usr/bin/pacman.exe", "--query"],
                       env=env, text=True, capture_output=True).stdout
        (stage / "native-packages.txt").write_text(packages)
        (stage / "dll-closure.txt").write_text("\n".join(dlls) + "\n")
    # Temporary Lisp sources/cache have now been deleted, so the image cannot
    # accidentally reopen CFFI files or DejaVu resources from that directory.
    relocated_smoke(stage, system_root, prefix, env, args.software_smoke_test)
    zipped = shutil.make_archive(str(stage), "zip", stage.parent, stage.name)
    print(f"Built {stage}\nZIP: {zipped}\nRelocated --help passed.")
    if args.software_smoke_test:
        print(f"Relocated lavapipe frame: {stage}-smoke.png (not hardware GPU validation).")


if __name__ == "__main__":
    main()
