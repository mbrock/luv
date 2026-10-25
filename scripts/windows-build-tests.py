#!/usr/bin/env python3
"""Host-independent packaging regressions; not a native Windows game test."""
import hashlib
import importlib.util
import io
import json
from pathlib import Path
import struct
import tarfile
import tempfile
import unittest

spec = importlib.util.spec_from_file_location(
    "windows_build", Path(__file__).with_name("windows-build.py"))
build = importlib.util.module_from_spec(spec)
spec.loader.exec_module(build)


class PackagingTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)

    def archive(self, entries):
        path = self.root / "sources.tar.gz"
        with tarfile.open(path, "w:gz") as archive:
            for name, data in entries:
                info = tarfile.TarInfo(name)
                info.size = len(data)
                archive.addfile(info, io.BytesIO(data))
        return path

    def test_lock_identity_accepts_checkout_crlf(self):
        lock = b'{\n  "nodes": {}\n}\n'
        manifest = json.dumps({"format": 1, "lockSha256": hashlib.sha256(lock).hexdigest()})
        archive = self.archive([("flake.lock", lock),
                                ("lisp-sources.json", manifest.encode())])
        checkout_lock = self.root / "checkout.lock"
        checkout_lock.write_bytes(lock.replace(b"\n", b"\r\n"))
        build.extract_sources(archive, self.root / "deps", checkout_lock)
        checkout_lock.write_text('{"nodes": {"changed": true}}')
        with self.assertRaisesRegex(ValueError, "does not match"):
            build.extract_sources(archive, self.root / "other-deps", checkout_lock)

    def test_archive_rejects_windows_unsafe_names(self):
        for name in ("../escape", "/absolute", "C:/drive", "foo\\bar",
                     "CON", "NUL.txt", "file:stream", "trailing."):
            with self.subTest(name=name), self.assertRaises(ValueError):
                build.extract_sources(self.archive([(name, b"")]),
                                      self.root / "deps", self.root / "lock")

    def test_archive_rejects_case_collisions(self):
        with self.assertRaisesRegex(ValueError, "case-colliding"):
            build.extract_sources(self.archive([("A.lisp", b""), ("a.lisp", b"")]),
                                  self.root / "deps", self.root / "lock")

    def test_gui_patch_preserves_appended_core(self):
        data = bytearray(512)
        data[:2] = b"MZ"
        struct.pack_into("<I", data, 0x3C, 128)
        data[128:132] = b"PE\0\0"
        struct.pack_into("<H", data, 132, 0x8664)
        struct.pack_into("<H", data, 152, 0x20B)
        struct.pack_into("<H", data, 220, 3)
        data.extend(b"SBCL APPENDED CORE MUST SURVIVE")
        executable = self.root / "luvcraft.exe"
        executable.write_bytes(data)
        build.set_gui_subsystem(executable)
        data[220] = 2
        self.assertEqual(executable.read_bytes(), data)
        build.set_gui_subsystem(executable)  # Already GUI is harmless.
        self.assertEqual(executable.read_bytes(), data)

    def test_gui_patch_rejects_non_pe_without_writing(self):
        executable = self.root / "luvcraft.exe"
        executable.write_bytes(b"not an exe")
        with self.assertRaises(ValueError):
            build.set_gui_subsystem(executable)
        self.assertEqual(executable.read_bytes(), b"not an exe")

    def dll_fixture(self):
        stage, native, system = (self.root / name for name in ("stage", "native", "system"))
        for directory in (stage, native, system):
            directory.mkdir()
        (stage / "luvcraft.exe").write_bytes(b"image")
        for name in ("SDL3.dll", "SDL3_image.dll", "SDL3_ttf.dll", "libharfbuzz-0.dll",
                     "libffi-8.dll", "vulkan-1.dll", "avutil-60.dll", "avcodec-62.dll",
                     "avformat-62.dll", "swscale-9.dll", "libssl-3-x64.dll",
                     "libcrypto-3-x64.dll", "transitive.dll"):
            (native / name).write_bytes(name.encode())
        (native / "gcc.exe").write_bytes(b"do not ship")
        (system / "kernel32.dll").write_bytes(b"host OS")
        return stage, native, system

    def test_dll_closure_cycles_and_host_libraries(self):
        stage, native, system = self.dll_fixture()
        build.copy_dll_closure(stage, native, system, lambda path: {
            "SDL3.dll": ["transitive.dll", "kernel32.dll", "api-ms-win-core-test.dll"],
            "transitive.dll": ["sdl3.dll"],
        }.get(path.name, []))
        self.assertTrue((stage / "transitive.dll").exists())
        self.assertFalse((stage / "kernel32.dll").exists())
        self.assertFalse((stage / "gcc.exe").exists())

    def test_missing_dll_is_fatal(self):
        stage, native, system = self.dll_fixture()
        with self.assertRaisesRegex(ValueError, "Unresolved import"):
            build.copy_dll_closure(stage, native, system, lambda _: ["missing.dll"])

    def test_conflicting_staged_dll_is_fatal(self):
        stage, native, system = self.dll_fixture()
        (stage / "SDL3.dll").write_bytes(b"wrong SDL")
        with self.assertRaisesRegex(ValueError, "different DLL"):
            build.copy_dll_closure(stage, native, system, lambda _: [])


if __name__ == "__main__":
    unittest.main()
