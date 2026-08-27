#!/usr/bin/env python3
"""Compile the test systems once, then run their suites in parallel SBCLs."""

from __future__ import annotations

import argparse
import os
from pathlib import Path
import queue
import subprocess
import sys
import threading
import time


ROOT = Path(__file__).resolve().parent.parent
WORKER = ROOT / "scripts" / "test.lisp"
EVENT = "@@LUV-TEST@@"

# These are executable test systems rather than product umbrellas.  Keeping
# this list at the leaf avoids running a shared suite more than once when the
# suites live in separate ASDF processes.  Weights only influence scheduling.
SUITES = [
    ("luv/test", 4),
    ("luv/tracy-capture/test", 1),
    ("luv/application-agent/test", 3),
    ("luv/ghostty/test", 1),
    ("luv/libav/test", 1),
    ("luvcraft/core/test", 9),
    ("luv/mcclim/test", 1),
    ("luv/lobby/test", 1),
    ("luv/lobby/mcclim/test", 1),
    ("luvcraft/mcclim/test", 2),
    ("luvcraft/test", 1),
    ("luvcraft/agent/test", 1),
    ("mqtt/test", 1),
    ("openai/test", 1),
    ("chrome-cdp/test", 1),
    ("sly-client/test", 1),
    ("luv-wiki/test", 1),
    ("luft", 4),
    ("luft/render/test", 16),
    ("luft/z-fiber-benchmark/test", 1),
    ("shader-validate", 1),
]


def sbcl_command(*arguments: str) -> list[str]:
    return [str(ROOT / "scripts" / "dev"), "sbcl", "--script", str(WORKER), *arguments]


def assign_suites(suites: list[tuple[str, int]], jobs: int) -> list[list[str]]:
    """Greedily put slow suites in the currently lightest worker."""
    workers: list[tuple[int, list[str]]] = [(0, []) for _ in range(jobs)]
    for name, weight in sorted(suites, key=lambda suite: suite[1], reverse=True):
        index = min(range(jobs), key=lambda candidate: workers[candidate][0])
        total, names = workers[index]
        names.append(name)
        workers[index] = total + weight, names
    return [names for _, names in workers if names]


def read_worker(worker: int, process: subprocess.Popen[str], events: queue.Queue) -> None:
    current: str | None = None
    output: list[str] = []
    assert process.stdout is not None
    for line in process.stdout:
        if line.startswith(f"{EVENT} START "):
            current = line.removeprefix(f"{EVENT} START ").strip()
            output = []
        elif line.startswith(f"{EVENT} END "):
            _, _, name, status, seconds = line.strip().split()
            events.put(("suite", worker, name, status, float(seconds), output))
            current = None
            output = []
        else:
            output.append(line)
    events.put(("worker", worker, process.wait(), current, output))


def run_parallel(assignments: list[list[str]]) -> tuple[list[tuple[str, float]], list[str]]:
    events: queue.Queue = queue.Queue()
    processes: list[subprocess.Popen[str]] = []
    threads: list[threading.Thread] = []
    expected = {name for assignment in assignments for name in assignment}
    completed: list[tuple[str, float]] = []
    failed: list[str] = []
    start = time.monotonic()

    try:
        for worker, assignment in enumerate(assignments, 1):
            process = subprocess.Popen(
                sbcl_command("--worker", *assignment),
                cwd=ROOT,
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                text=True,
                bufsize=1,
            )
            processes.append(process)
            thread = threading.Thread(
                target=read_worker, args=(worker, process, events), daemon=True
            )
            thread.start()
            threads.append(thread)

        finished_workers = 0
        while finished_workers < len(processes):
            event = events.get()
            if event[0] == "suite":
                _, worker, name, status, seconds, output = event
                completed.append((name, seconds))
                if status != "passed":
                    failed.append(name)
                elapsed = time.monotonic() - start
                print(
                    f"{elapsed:05.1f}s {len(completed):2d}/{len(expected)}  "
                    f"{name} ({seconds:.1f}s, worker {worker})"
                )
                sys.stdout.writelines(output)
                sys.stdout.flush()
            else:
                _, worker, status, current, output = event
                finished_workers += 1
                # A worker returns 1 after reporting one or more failed suites.
                # Only unframed output or an unfinished suite is unexpected.
                if current is not None or output:
                    print(f"\nworker {worker} exited unexpectedly (status {status}).")
                    sys.stdout.writelines(output)
                    sys.stdout.flush()
    except BaseException:
        for process in processes:
            if process.poll() is None:
                process.terminate()
        for process in processes:
            process.wait()
        raise
    finally:
        for thread in threads:
            thread.join()

    missing = expected.difference(name for name, _ in completed)
    for name in sorted(missing):
        print(f"{name}: no result returned")
        failed.append(name)
    return completed, failed


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--jobs", type=int, default=4)
    arguments = parser.parse_args()
    if arguments.jobs < 1:
        parser.error("--jobs must be at least 1")

    suites = SUITES
    if not os.environ.get("LUV_GHOSTTY_LIBRARY"):
        suites = [suite for suite in suites if suite[0] != "luv/ghostty/test"]
        print("luv/ghostty/test (libghostty-vt unavailable; skipped)", flush=True)

    names = [name for name, _ in suites]
    start = time.monotonic()
    print(f"Preparing {len(names)} test suites for parallel execution...", flush=True)
    preparation = subprocess.run(
        sbcl_command("--prepare", *names), cwd=ROOT, check=False
    )
    if preparation.returncode != 0:
        return preparation.returncode

    assignments = assign_suites(suites, min(arguments.jobs, len(suites)))
    print(
        f"Running {len(names)} test suites in {len(assignments)} SBCL workers...",
        flush=True,
    )
    completed, failed = run_parallel(assignments)
    elapsed = time.monotonic() - start
    print(
        f"\n;; Tested {len(completed)} suite{'s' if len(completed) != 1 else ''} "
        f"in {elapsed:.1f}s with {len(assignments)} SBCL workers."
    )
    if completed:
        slowest = max(completed, key=lambda result: result[1])
        print(f";; Slowest was {slowest[0]} at {slowest[1]:.1f}s.")
    if failed:
        print(f";; Failed: {', '.join(failed)}")
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
