#!/usr/bin/env python3

"""Exercise launcher handoff when a login shell changes the inherited PATH."""

from pathlib import Path
import shlex
import subprocess
import tempfile
import unittest


HELPER = Path(__file__).with_name("require-dev-environment").resolve()


class DevEnvironmentTests(unittest.TestCase):
    def run_launcher(self, mode="nix-develop", compiler="matching",
                     arguments=None, repository_launcher=False):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            tools = root / "bin"
            tools.mkdir()
            for name, body in (
                ("sbcl", "exit 99"),
                ("nix", "printf '%s\\n' \"$@\""),
            ):
                tool = tools / name
                tool.write_text(f"#!/bin/sh\n{body}\n")
                tool.chmod(0o755)
            launcher = root / "launcher"
            launcher.write_text(
                "#!/bin/sh\nset -eu\n"
                f"project_root={shlex.quote(str(root))}\n"
                f". {shlex.quote(str(HELPER))}\n"
                "printf 'ready\\n'\n"
            )
            launcher.chmod(0o755)
            if repository_launcher:
                root = HELPER.parent.parent
                launcher = root / "sly"
            env = {
                "PATH": str(tools),
                "LUV_DEV_ENVIRONMENT_MODE": mode,
                "LUV_NATIVE_LIBRARY_PATH": "/test/libraries",
                "LUV_NATIVE_ENVIRONMENT_ACTIVE": "1",
            }
            if compiler != "missing":
                env["LUV_SBCL"] = str(
                    tools / "sbcl" if compiler == "matching" else root / "nix-sbcl"
                )
            result = subprocess.run(
                [str(launcher), *(arguments or ["play", "--fullscreen"])],
                env=env, capture_output=True, text=True, timeout=5,
            )
            self.assertEqual(0, result.returncode, result.stderr)
            return result.stdout.splitlines(), str(root), str(launcher)

    def test_matching_compiler_stays_in_environment(self):
        output, _, _ = self.run_launcher()
        self.assertEqual(["ready"], output)

    def assert_handoff(self, **options):
        output, root, launcher = self.run_launcher(**options)
        self.assertEqual([
            "develop", "--accept-flake-config", "--no-write-lock-file",
            f"{root}#default", "-c", launcher, "play", "--fullscreen",
        ], output)

    def test_inherited_marker_with_replaced_compiler_reenters_nix(self):
        self.assert_handoff(compiler="different")

    def test_old_environment_without_compiler_identity_reenters_nix(self):
        self.assert_handoff(compiler="missing")

    def test_outside_environment_enters_nix(self):
        self.assert_handoff(mode="")

    def test_sly_reentry_preserves_session_and_standalone_options(self):
        for arguments in (
            ["--lisp", "OLD123", "status"],
            ["--luvcraft", "status"],
            ["--luvland", "status"],
        ):
            with self.subTest(arguments=arguments):
                output, root, launcher = self.run_launcher(
                    compiler="different", arguments=arguments,
                    repository_launcher=True,
                )
                self.assertEqual([
                    "develop", "--accept-flake-config", "--no-write-lock-file",
                    f"{root}#default", "-c", launcher, *arguments,
                ], output)


if __name__ == "__main__":
    unittest.main()
