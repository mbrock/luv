#!/usr/bin/env python3
"""Check release failure gates and archive ordering without Apple credentials."""

import importlib.util
import json
from pathlib import Path
import tempfile
import subprocess
import unittest
from unittest.mock import patch

spec = importlib.util.spec_from_file_location("sign_release", Path(__file__).with_name("macos-sign-release.py"))
release = importlib.util.module_from_spec(spec)
spec.loader.exec_module(release)


class SigningTests(unittest.TestCase):
    def exercise(self, status="Accepted", fail=None, identity="Developer ID Application: Test (TEAM)", profile="test", submit_error=False, nested=False):
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        output = Path(temporary.name)
        app = output / "Luvcraft.app"
        frameworks = app / "Contents/Frameworks"
        frameworks.mkdir(parents=True)
        (frameworks / "test.dylib").write_bytes(b"\xcf\xfa\xed\xfe")
        runtime = app / "Contents/MacOS/luvcraft-runtime"
        runtime.parent.mkdir()
        runtime.write_bytes(b"\xcf\xfa\xed\xfe")
        if nested:
            helper = frameworks / "Sparkle.framework/Versions/B/Updater.app/Contents/MacOS/Updater"
            helper.parent.mkdir(parents=True)
            helper.write_bytes(b"\xcf\xfa\xed\xfe")
        archive = output / "luvcraft-macos-arm64.zip"
        archive.write_text("stale")
        (output / "SHA256SUMS").write_text("stale")
        commands = []

        def run(*args):
            command = [str(arg) for arg in args]
            commands.append(command)
            if fail and fail in command:
                raise RuntimeError("injected command failure")
            if "submit" in command:
                response = json.dumps({"id": "submission-id", "status": status})
                if submit_error:
                    raise subprocess.CalledProcessError(65, command, output=response)
                return response
            if "log" in command:
                return '{}'
            if command[0] == "/usr/bin/ditto":
                Path(command[-1]).write_bytes(b"archive")
            return ""

        with patch.object(release, "run", side_effect=run):
            if status != "Accepted" or fail:
                with self.assertRaises(RuntimeError):
                    release.package(app, identity, profile)
                self.assertFalse(archive.exists())
                self.assertFalse((output / "SHA256SUMS").exists())
            else:
                release.package(app, identity, profile)
                self.assertTrue(archive.exists())
                self.assertIn(archive.name, (output / "SHA256SUMS").read_text())
        return commands

    def test_notarization_before_final_archive(self):
        commands = self.exercise()
        signing = [command for command in commands if "--sign" in command]
        self.assertEqual(len(signing), 3)
        self.assertTrue(all("--timestamp" in command and "runtime" in command for command in signing))
        operations = [command[2] for command in commands if command[0] == "/usr/bin/xcrun"]
        self.assertEqual(operations, ["submit", "log", "staple", "validate"])
        self.assertEqual(commands[-1][0], "/usr/bin/ditto")

    def test_nested_sparkle_helpers_signed_before_the_framework(self):
        commands = self.exercise(nested=True)
        paths = [command[-1] for command in commands if "--sign" in command]
        helper = next(i for i, p in enumerate(paths) if p.endswith("MacOS/Updater"))
        bundle = next(i for i, p in enumerate(paths) if p.endswith("Updater.app"))
        framework = next(i for i, p in enumerate(paths) if p.endswith("Sparkle.framework"))
        self.assertLess(helper, bundle)
        self.assertLess(bundle, framework)
        self.assertEqual(paths[-1].split("/")[-1], "Luvcraft.app")

    def test_rejected_submission_never_published(self):
        commands = self.exercise(status="Invalid")
        self.assertFalse(any("staple" in command for command in commands))

    def test_nonzero_rejection_still_fetches_diagnostics(self):
        commands = self.exercise(status="Invalid", submit_error=True)
        self.assertTrue(any("log" in command for command in commands))

    def test_staple_failure_never_published(self):
        self.exercise(fail="staple")

    def test_ad_hoc_does_not_contact_apple(self):
        commands = self.exercise(identity="-", profile=None)
        self.assertFalse(any(command[0] == "/usr/bin/xcrun" for command in commands))
        self.assertFalse(any("--timestamp" in command for command in commands))

    def test_invalid_credentials_fail_before_signing(self):
        for identity, profile in [("-", "profile"), ("Apple Development: Test", None)]:
            with patch.object(release, "run") as run:
                with self.assertRaises(ValueError):
                    release.package(Path("unused"), identity, profile)
                run.assert_not_called()


if __name__ == "__main__":
    unittest.main()
