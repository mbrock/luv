#!/usr/bin/env python3
"""Sign a relocated app, optionally notarize it, and produce the download ZIP."""

import argparse
import hashlib
import json
import os
from pathlib import Path
import plistlib
import subprocess
import tempfile


def run(*args):
    return subprocess.check_output([str(arg) for arg in args], text=True).strip()


def package(app, identity="-", profile=None, keychain=None):
    if identity != "-" and not identity.startswith("Developer ID Application:"):
        raise ValueError("Use a Developer ID Application identity, or '-' for ad-hoc signing")
    if profile and identity == "-":
        raise ValueError("Notarization requires a Developer ID Application identity")
    output = app.parent
    archive = output / "luvcraft-macos-arm64.zip"
    checksum = output / "SHA256SUMS"
    # A failed retry must not leave an older download looking successful.
    archive.unlink(missing_ok=True)
    checksum.unlink(missing_ok=True)
    signing = ["/usr/bin/codesign", "--force", "--sign", identity]
    if keychain:
        signing += ["--keychain", keychain]
    if identity != "-":
        signing += ["--options", "runtime", "--timestamp"]
    with tempfile.TemporaryDirectory(prefix="luv-sign-") as temporary:
        entitlements = Path(temporary) / "sbcl-entitlements.plist"
        # SBCL uses MAP_JIT for generated code, but also maps executable pages
        # from the separate Lisp core outside MAP_JIT. Hardened runtime kills
        # that mapping as an Invalid Page without unsigned-executable-memory.
        # All bundled dylibs share this identity; keep library validation on.
        entitlements.write_bytes(plistlib.dumps({
            "com.apple.security.cs.allow-jit": True,
            "com.apple.security.cs.allow-unsigned-executable-memory": True,
        }))
        for library in sorted((app / "Contents/Frameworks").iterdir()):
            if library.is_file() and not library.is_symlink():
                run(*signing, library)
        run(*signing, "--entitlements", entitlements,
            app / "Contents/MacOS/luvcraft-runtime")
        run(*signing, app)
    run("/usr/bin/codesign", "--verify", "--deep", "--strict", app)
    if profile:
        authentication = ["--keychain-profile", profile]
        if keychain:
            authentication += ["--keychain", keychain]
        # Submit a temporary ZIP; only publish an archive with a stapled ticket.
        with tempfile.TemporaryDirectory(prefix="luv-notary-") as temporary:
            submission = Path(temporary) / "Luvcraft.zip"
            run("/usr/bin/ditto", "-c", "-k", "--keepParent", app, submission)
            try:
                response = run("/usr/bin/xcrun", "notarytool", "submit", submission,
                               *authentication, "--wait", "--timeout", "30m", "--output-format", "json")
            except subprocess.CalledProcessError as error:
                # notarytool can exit nonzero for a rejected/timed-out request.
                # Retain its response and fetch the log when it includes an ID.
                response = error.output
                (output / "notarization-result.json").write_text(response + "\n")
                try:
                    json.loads(response)
                except ValueError:
                    raise error
            (output / "notarization-result.json").write_text(response + "\n")
            result = json.loads(response)
            if result.get("id"):
                log = run("/usr/bin/xcrun", "notarytool", "log", result["id"], *authentication)
                (output / "notarization-log.json").write_text(log + "\n")
            if result.get("status") != "Accepted":
                raise RuntimeError("Notarization was not accepted; see notarization-result.json and notarization-log.json")
        run("/usr/bin/xcrun", "stapler", "staple", app)
        run("/usr/bin/xcrun", "stapler", "validate", app)
        run("/usr/bin/codesign", "--verify", "--deep", "--strict", app)
        run("/usr/sbin/spctl", "--assess", "--type", "execute", "--verbose=2", app)
    run("/usr/bin/ditto", "-c", "-k", "--keepParent", app, archive)
    hasher = hashlib.sha256()
    with archive.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            hasher.update(chunk)
    digest = hasher.hexdigest()
    checksum.write_text(f"{digest}  {archive.name}\n")
    print(f"Built {archive} ({'notarized' if profile else 'signed only'})")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("app", type=Path)
    parser.add_argument("--identity", default=os.environ.get("LUV_MACOS_SIGN_IDENTITY", "-"))
    parser.add_argument("--notary-profile", default=os.environ.get("LUV_MACOS_NOTARY_PROFILE"))
    parser.add_argument("--keychain", default=os.environ.get("LUV_MACOS_KEYCHAIN"))
    args = parser.parse_args()
    package(args.app.resolve(strict=True), args.identity, args.notary_profile, args.keychain)
