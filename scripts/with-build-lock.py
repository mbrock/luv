#!/usr/bin/env python3
"""Run one build command while holding a cross-platform advisory file lock."""

from __future__ import annotations

import errno
import fcntl
import os
from pathlib import Path
import signal
import subprocess
import sys
import time
import uuid


def usage() -> int:
    print(
        "usage: with-build-lock.py LOCK LABEL LEGACY-COMMAND -- COMMAND ...",
        file=sys.stderr,
    )
    return 2


def process_alive(pid: int) -> bool:
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return False
    except PermissionError:
        return True
    return True


def process_command(pid: int) -> str:
    result = subprocess.run(
        ["ps", "-p", str(pid), "-o", "command="],
        check=False,
        stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL,
        text=True,
    )
    return result.stdout.strip() if result.returncode == 0 else ""


def legacy_owner(lock_path: Path, command_hint: str) -> int | None:
    try:
        value = (lock_path / "pid").read_text(encoding="ascii").strip()
        pid = int(value)
    except (FileNotFoundError, IsADirectoryError, ValueError, UnicodeError):
        return None
    if pid > 0 and process_alive(pid) and command_hint in process_command(pid):
        return pid
    return None


def migrate_legacy_directory(
    lock_path: Path, label: str, command_hint: str
) -> bool:
    """Replace the old mkdir/PID lock after proving it has no active owner."""
    for _ in range(8):
        try:
            status = lock_path.stat()
        except FileNotFoundError:
            return True
        if not lock_path.is_dir():
            return True

        owner = legacy_owner(lock_path, command_hint)
        if owner is not None:
            print(
                f"Another {label} build is active (legacy pid {owner}); "
                "retry this command.",
                file=sys.stderr,
            )
            return False

        # The old protocol created the directory before publishing its PID.
        # Treat a fresh PID-less directory as an in-progress publication; an
        # abandoned one becomes reclaimable after a short grace period.
        if not (lock_path / "pid").exists() and time.time() - status.st_mtime < 2.0:
            print(
                f"Another {label} build may be publishing its legacy lock; "
                "retry this command.",
                file=sys.stderr,
            )
            return False

        quarantine = lock_path.with_name(
            f"{lock_path.name}.reclaim.{os.getpid()}.{uuid.uuid4().hex}"
        )
        try:
            lock_path.rename(quarantine)
        except FileNotFoundError:
            continue
        except OSError as condition:
            print(
                f"Cannot reclaim stale {label} build lock {lock_path}: {condition}",
                file=sys.stderr,
            )
            return False

        try:
            pid_path = quarantine / "pid"
            if pid_path.exists() or pid_path.is_symlink():
                pid_path.unlink()
            quarantine.rmdir()
        except OSError as condition:
            if not lock_path.exists():
                try:
                    quarantine.rename(lock_path)
                except OSError:
                    pass
            print(
                f"Cannot clean reclaimed {label} build lock {quarantine}: "
                f"{condition}",
                file=sys.stderr,
            )
            return False
        return True

    print(
        f"Could not stabilize legacy {label} build lock {lock_path}; retry.",
        file=sys.stderr,
    )
    return False


def read_lock_description(stream) -> str:
    try:
        stream.seek(0)
        description = stream.read().strip()
    except OSError:
        return "owner unavailable"
    return description or "owner unavailable"


def run_locked(lock_path: Path, label: str, command: list[str]) -> int:
    lock_path.parent.mkdir(parents=True, exist_ok=True)
    with lock_path.open("a+", encoding="utf-8") as stream:
        try:
            fcntl.flock(stream.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
        except OSError as condition:
            if condition.errno not in (errno.EACCES, errno.EAGAIN):
                raise
            print(
                f"Another {label} build is active "
                f"({read_lock_description(stream)}); retry this command.",
                file=sys.stderr,
            )
            return 1

        stream.seek(0)
        stream.truncate()
        stream.write(f"pid {os.getpid()}: {' '.join(command)}\n")
        stream.flush()

        child = subprocess.Popen(
            command,
            pass_fds=(stream.fileno(),),
        )

        def forward(signum, _frame):
            if child.poll() is None:
                child.send_signal(signum)

        previous = {
            signum: signal.signal(signum, forward)
            for signum in (signal.SIGHUP, signal.SIGINT, signal.SIGTERM)
        }
        try:
            status = child.wait()
        finally:
            for signum, handler in previous.items():
                signal.signal(signum, handler)
        return status if status >= 0 else 128 - status


def main(arguments: list[str]) -> int:
    if len(arguments) < 6 or "--" not in arguments[4:]:
        return usage()
    separator = arguments.index("--", 4)
    if separator != 4 or separator + 1 == len(arguments):
        return usage()

    lock_path = Path(arguments[1])
    label = arguments[2]
    command_hint = arguments[3]
    command = arguments[separator + 1 :]

    if not migrate_legacy_directory(lock_path, label, command_hint):
        return 1
    try:
        return run_locked(lock_path, label, command)
    except OSError as condition:
        print(f"Cannot use {label} build lock {lock_path}: {condition}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
