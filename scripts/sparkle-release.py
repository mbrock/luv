#!/usr/bin/env python3
"""Pinned Sparkle tools, bundle metadata, and signed GitHub release appcasts."""
import argparse
import hashlib
import html
import json
import os
from pathlib import Path
import plistlib
import shutil
import subprocess
import tarfile
import urllib.request
import xml.etree.ElementTree as ET

VERSION = "2.10.1"
SHA256 = "1a856d2a9edb8e628de88121d7be04cf0e2167f8641650b78fca00b14244cf27"
ROOT = Path(__file__).resolve().parent.parent
CONFIG = ROOT / "deploy/macos/sparkle.json"


def fetch(destination):
    destination = destination.resolve()
    stamp = destination / ".sparkle-version"
    if stamp.exists() and stamp.read_text() == VERSION:
        return destination
    destination.mkdir(parents=True, exist_ok=True)
    archive = destination / "dist.tar.xz"
    urllib.request.urlretrieve(
        f"https://github.com/sparkle-project/Sparkle/releases/download/{VERSION}/Sparkle-{VERSION}.tar.xz", archive)
    if hashlib.sha256(archive.read_bytes()).hexdigest() != SHA256:
        raise RuntimeError("Sparkle distribution checksum mismatch")
    # The distribution is pinned and verified before extraction (Python 3.9
    # on Apple's system Python doesn't have tarfile's newer extraction filter).
    with tarfile.open(archive) as stream:
        stream.extractall(destination)
    stamp.write_text(VERSION)
    return destination


def metadata(info, config, version=None, build=None):
    if version:
        info["CFBundleShortVersionString"] = version.removeprefix("v")
    if build:
        if not all(part.isdigit() for part in build.split(".")):
            raise ValueError("Build number must be numeric and monotonic")
        info["CFBundleVersion"] = build
    info.update({
        "SUFeedURL": f"https://github.com/{config['repository']}/releases/latest/download/appcast.xml",
        "SUPublicEDKey": config["public_key"],
        "SUEnableAutomaticChecks": True,
        "SUAutomaticallyUpdate": False,
        "SUAllowsAutomaticUpdates": False,
        "SUEnableSystemProfiling": False,
        "SUVerifyUpdateBeforeExtraction": True,
        "SURequireSignedFeed": True,
    })
    return info


def prepare(app, tools, version=None, build=None):
    config = json.loads(CONFIG.read_text())
    plist = app / "Contents/Info.plist"
    info = metadata(plistlib.loads(plist.read_bytes()), config, version, build)
    plist.write_bytes(plistlib.dumps(info))
    destination = app / "Contents/Frameworks/Sparkle.framework"
    if destination.exists():
        shutil.rmtree(destination)
    shutil.copytree(tools / "Sparkle.framework", destination, symlinks=True)


def appcast(archive, tools, tag):
    config = json.loads(CONFIG.read_text())
    args = [str(tools / "bin/generate_appcast"), "--maximum-deltas", "0",
            "--maximum-versions", "1", "--download-url-prefix",
            f"https://github.com/{config['repository']}/releases/download/{tag}/",
            "--link", f"https://github.com/{config['repository']}/releases/tag/{tag}"]
    key = os.environ.get("SPARKLE_PRIVATE_KEY")
    if key:
        args += ["--ed-key-file", "-"]
    else:
        args += ["--account", config["account"]]
    release = f"https://github.com/{config['repository']}/releases/tag/{tag}"
    archive.with_suffix(".html").write_text(
        f"<p>{html.escape(archive.stem.split('-macos-')[0].capitalize())} {html.escape(tag)}</p>"
        f'<p><a href="{html.escape(release, quote=True)}">Read the release notes on GitHub</a>.</p>')
    args += ["--embed-release-notes"]
    # Only the final stapled archive belongs in this directory. generate_appcast
    # signs the ZIP and, because SURequireSignedFeed is set, the feed itself.
    subprocess.run([*args, str(archive.parent)], input=key, text=True, check=True)
    feed = archive.parent / "appcast.xml"
    authentication = ["--ed-key-file", "-"] if key else ["--account", config["account"]]
    verify = [str(tools / "bin/sign_update"), "--verify", *authentication]
    subprocess.run([*verify, str(feed)], input=key, text=True, check=True)
    item = ET.parse(feed).find("./channel/item/enclosure")
    if item is None or int(item.attrib["length"]) != archive.stat().st_size:
        raise RuntimeError("Appcast does not describe the final release archive")
    signature = item.attrib["{http://www.andymatuschak.org/xml-namespaces/sparkle}edSignature"]
    subprocess.run([*verify, str(archive), signature], input=key, text=True, check=True)
    return feed


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)
    p = sub.add_parser("fetch")
    p.add_argument("destination", type=Path)
    p = sub.add_parser("prepare")
    p.add_argument("app", type=Path)
    p.add_argument("tools", type=Path)
    p.add_argument("--version")
    p.add_argument("--build")
    p = sub.add_parser("appcast")
    p.add_argument("archive", type=Path)
    p.add_argument("tools", type=Path)
    p.add_argument("tag")
    args = parser.parse_args()
    if args.command == "fetch":
        fetch(args.destination)
    elif args.command == "prepare":
        prepare(args.app.resolve(), args.tools.resolve(), args.version, args.build)
    else:
        appcast(args.archive.resolve(), args.tools.resolve(), args.tag)
