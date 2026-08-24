#!/usr/bin/env python3

from __future__ import annotations

import os
from pathlib import Path
import subprocess
import sys
import tempfile
import time
import unittest


HELPER = Path(__file__).with_name("with-build-lock.py")


def helper_command(lock: Path, code: str) -> list[str]:
    return [
        sys.executable,
        str(HELPER),
        str(lock),
        "test artifact",
        "with-build-lock-tests.py",
        "--",
        sys.executable,
        "-c",
        code,
    ]


class BuildLockTests(unittest.TestCase):
    def test_stale_metadata_file_is_reused(self):
        with tempfile.TemporaryDirectory() as directory:
            lock = Path(directory) / "build.lock"
            marker = Path(directory) / "ran"
            lock.write_text("pid 99999999: vanished builder\n", encoding="utf-8")
            result = subprocess.run(
                helper_command(lock, f"from pathlib import Path; Path({str(marker)!r}).touch()"),
                check=False,
                capture_output=True,
                text=True,
            )
            self.assertEqual(0, result.returncode, result.stderr)
            self.assertTrue(marker.exists())

    def test_active_lock_excludes_a_second_builder(self):
        with tempfile.TemporaryDirectory() as directory:
            lock = Path(directory) / "build.lock"
            marker = Path(directory) / "second-ran"
            holder = subprocess.Popen(
                helper_command(lock, "print('ready', flush=True); input()"),
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
            )
            holder_error = ""
            try:
                self.assertEqual("ready\n", holder.stdout.readline())
                contender = subprocess.run(
                    helper_command(
                        lock,
                        f"from pathlib import Path; Path({str(marker)!r}).touch()",
                    ),
                    check=False,
                    capture_output=True,
                    text=True,
                )
                self.assertEqual(1, contender.returncode)
                self.assertIn("build is active", contender.stderr)
                self.assertFalse(marker.exists())
            finally:
                if holder.stdin:
                    holder.stdin.write("\n")
                    holder.stdin.flush()
                _, holder_error = holder.communicate(timeout=3)
            self.assertEqual(0, holder.returncode, holder_error)

    def test_stale_legacy_directory_is_reclaimed(self):
        with tempfile.TemporaryDirectory() as directory:
            lock = Path(directory) / "build.lock"
            marker = Path(directory) / "ran"
            lock.mkdir()
            (lock / "pid").write_text("99999999\n", encoding="ascii")
            old = time.time() - 60
            os.utime(lock / "pid", (old, old))
            os.utime(lock, (old, old))
            result = subprocess.run(
                helper_command(lock, f"from pathlib import Path; Path({str(marker)!r}).touch()"),
                check=False,
                capture_output=True,
                text=True,
            )
            self.assertEqual(0, result.returncode, result.stderr)
            self.assertTrue(marker.exists())
            self.assertTrue(lock.is_file())

    def test_fresh_pidless_legacy_directory_is_not_stolen(self):
        with tempfile.TemporaryDirectory() as directory:
            lock = Path(directory) / "build.lock"
            marker = Path(directory) / "ran"
            lock.mkdir()
            result = subprocess.run(
                helper_command(lock, f"from pathlib import Path; Path({str(marker)!r}).touch()"),
                check=False,
                capture_output=True,
                text=True,
            )
            self.assertEqual(1, result.returncode)
            self.assertIn("may be publishing", result.stderr)
            self.assertFalse(marker.exists())
            self.assertTrue(lock.is_dir())


if __name__ == "__main__":
    unittest.main(verbosity=2)
