#!/usr/bin/env python3
"""Relocate the exact CFFI roots and their recursive Mach-O dependencies."""

from pathlib import Path
import plistlib
import shutil
import subprocess
import sys


def run(*args):
    return subprocess.check_output([str(arg) for arg in args], text=True).strip()


def system_library(name):
    return name.startswith(("/usr/lib/", "/System/Library/"))


def dependencies(path):
    return [line.strip().split(" (compatibility", 1)[0]
            for line in run("/usr/bin/otool", "-L", path).splitlines()[1:]]


def rpaths(path):
    lines = run("/usr/bin/otool", "-l", path).splitlines()
    return [lines[i + 2].strip().split("path ", 1)[1].rsplit(" (offset", 1)[0]
            for i, line in enumerate(lines) if line.strip() == "cmd LC_RPATH"]


def bundle(app):
    resources = app / "Contents/Resources"
    frameworks = app / "Contents/Frameworks"
    frameworks.mkdir()
    runtime = app / "Contents/MacOS/luvcraft-runtime"
    manifest = resources / "native-libraries.txt"
    copied = {}

    def resolve(name, source):
        if name.startswith("@loader_path/"):
            return source.parent / name.removeprefix("@loader_path/")
        if name.startswith("@rpath/"):
            for root in rpaths(source):
                root = root.replace("@loader_path", str(source.parent))
                candidate = Path(root) / name.removeprefix("@rpath/")
                if candidate.exists():
                    return candidate
            raise RuntimeError(f"Unresolved {name} in {source}")
        if not name.startswith("/"):
            raise RuntimeError(f"Unsupported dependency {name} in {source}")
        return Path(name)

    def copy_library(source):
        source = source.resolve(strict=True)
        if source in copied:
            return copied[source]
        # Different dependencies can use distinct libraries with the same
        # basename (Apple and GNU libiconv, for example). Preserve both.
        target = frameworks / f"{len(copied):03d}-{source.name}"
        copied[source] = target
        shutil.copyfile(source, target)
        target.chmod(0o755)
        relocate(source, target, library=True)
        return target

    def relocate(source, target, library=False):
        changes = []
        # otool -L includes LC_ID_DYLIB on dylibs but not executables.
        ids = run("/usr/bin/otool", "-D", source).splitlines()[1:]
        for dependency in dependencies(source):
            if dependency in ids or system_library(dependency):
                continue
            local = copy_library(resolve(dependency, source))
            prefix = "@loader_path/" if library else "@executable_path/../Frameworks/"
            changes += ["-change", dependency, prefix + local.name]
        # Even unused builder RPATHs should not survive in the bundle.
        for path in rpaths(source):
            changes += ["-delete_rpath", path]
        if library:
            changes += ["-id", "@rpath/" + target.name]
        if changes:
            run("/usr/bin/install_name_tool", *changes, target)

    for line in manifest.read_text().splitlines():
        source = Path(line)
        target = copy_library(source)
        # Deploy's CFFI spec uses the original filename, possibly a symlink.
        # Keep that alias beside the core without duplicating library bytes.
        alias = resources / source.name
        if alias.is_symlink():
            if alias.resolve() != target:
                raise RuntimeError(f"CFFI library basename collision: {source}")
            continue
        if alias.exists():
            alias.unlink()
        alias.symlink_to(Path("../Frameworks") / target.name)
    relocate(runtime, runtime)
    manifest.unlink()

    launcher = app / "Contents/MacOS/Luvcraft"
    launcher.write_text('''#!/bin/sh
set -eu
here=$(CDPATH='' cd -- "$(dirname -- "$0")" && pwd)
export LUV_BASH=/bin/bash
export LUVCRAFT_SLYNK_ENDPOINT="${LUVCRAFT_SLYNK_ENDPOINT:-${TMPDIR:-/tmp}/luvcraft-$(id -u).slynk}"
exec "$here/luvcraft-runtime" --core "$here/../Resources/luvcraft.core" \\
  --dynamic-space-size 3000 --noinform --end-runtime-options "$@"
''')
    launcher.chmod(0o755)
    runtime.chmod(0o755)
    info = {
        "CFBundleExecutable": "Luvcraft",
        "CFBundleIdentifier": "sh.swa.luvcraft",
        "CFBundleName": "Luvcraft",
        "CFBundleDisplayName": "Luvcraft",
        "CFBundlePackageType": "APPL",
        "CFBundleShortVersionString": "0.0.1",
        "CFBundleVersion": "1",
        "LSMinimumSystemVersion": "26.0",
        "NSHighResolutionCapable": True,
    }
    (app / "Contents/Info.plist").write_bytes(plistlib.dumps(info))
    for path in [runtime, *frameworks.iterdir()]:
        if "arm64" not in run("/usr/bin/lipo", "-archs", path).split():
            raise RuntimeError(f"Not an arm64 binary: {path}")
        for dependency in dependencies(path):
            if not system_library(dependency) and not dependency.startswith(
                    ("@loader_path/", "@executable_path/", "@rpath/")):
                raise RuntimeError(f"Unrelocated dependency: {path}: {dependency}")
    print(f"Relocated {len(copied)} libraries in {app}")


if __name__ == "__main__":
    bundle(Path(sys.argv[1]).resolve(strict=True))
