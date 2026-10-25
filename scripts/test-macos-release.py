#!/usr/bin/env python3
"""Exercise the shipped ZIP on a Metal 4 Mac, with builder paths denied."""

import argparse
from pathlib import Path
import shutil
import socket
import subprocess
import tempfile
import time


def lisp_string(value):
    return '"' + str(value).replace('\\', '\\\\').replace('"', '\\"') + '"'


def evaluate(port, code):
    request = (f'(:emacs-rex (slynk:eval-and-grab-output {lisp_string(code)}) '
               '"CL-USER" t 1)').encode()
    with socket.create_connection(("127.0.0.1", port), timeout=300) as connection:
        connection.sendall(f"{len(request):06x}".encode() + request)
        stream = connection.makefile("rb")
        while True:
            header = stream.read(6)
            if not header:
                raise RuntimeError("Slynk closed before returning a result")
            response = stream.read(int(header, 16)).decode()
            if response.startswith("(:debug") or ":abort" in response:
                raise RuntimeError(response)
            if response.startswith("(:return"):
                return response


def test(archive, evidence, cli_only=False, require_notarized=False):
    source = Path(__file__).resolve().parent.parent
    evidence.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix="luv-macos-") as temporary:
        root = Path(temporary).resolve()
        moved = root / "moved download with spaces"
        home = root / "fresh-home"
        home.mkdir()
        subprocess.run(["/usr/bin/ditto", "-x", "-k", str(archive), str(moved)], check=True)
        app = moved / "Luvcraft.app"
        subprocess.run(["/usr/bin/codesign", "--verify", "--deep", "--strict", str(app)], check=True)
        if require_notarized:
            subprocess.run(["/usr/bin/xcrun", "stapler", "validate", str(app)], check=True)
            subprocess.run(["/usr/sbin/spctl", "--assess", "--type", "execute", str(app)], check=True)
        profile = root / "without-builder.sb"
        denied = ["/nix", "/opt/homebrew", "/usr/local", str(source),
                  str(archive.parent), str(Path.home() / ".cache"),
                  str(Path.home() / "quicklisp")]
        profile.write_text('(version 1)\n(allow default)\n'
                           '(deny network-outbound (remote ip "*:*") '
                           '(with no-log))\n'
                           '(allow network-outbound (remote ip "localhost:*"))\n'
                           '(deny file-read*\n' + ''.join(
                               f'  (subpath {lisp_string(path)})\n' for path in denied)
                           + ')\n')
        endpoint = root / "game.slynk"
        environment = {"PATH": "/usr/bin:/bin:/usr/sbin:/sbin", "HOME": str(home),
                       "TMPDIR": str(root), "LUVCRAFT_SLYNK_ENDPOINT": str(endpoint)}
        sandbox = ["/usr/bin/sandbox-exec", "-f", str(profile)]
        command = sandbox + [str(app / "Contents/MacOS/Luvcraft")]

        def run(arguments, name, expected=0):
            with (evidence / f"{name}.log").open("w") as log:
                result = subprocess.run(arguments, env=environment, cwd=root,
                                        stdout=log, stderr=subprocess.STDOUT, timeout=180)
            if result.returncode != expected:
                raise RuntimeError(f"{name}: exit {result.returncode}, expected {expected}; see {log.name}")
            return Path(log.name).read_text()

        # A successful render is meaningful only if the deny profile works.
        run(sandbox + ["/usr/bin/stat", str(source / "luvcraft.asd")], "denied-source", 1)
        run(sandbox + ["/usr/bin/stat", "/nix/store"], "denied-nix", 1)
        assert "Usage: luvcraft" in run(command + ["--help"], "help")
        assert "Invalid luvcraft arguments" in run(command + ["--not-a-luvcraft-option"], "invalid", 1)
        if cli_only:
            print("PASS: moved ZIP, signatures, denied builder paths, help and errors.")
            print("Metal rendering was not tested (CLI-only hosted-runner check).")
            return
        smoke = root / "smoke.png"
        run(command + ["--metal-smoke-test", str(smoke)], "metal-smoke")
        assert smoke.read_bytes().startswith(b"\x89PNG\r\n\x1a\n")
        shutil.copyfile(smoke, evidence / "metal-smoke.png")

        # Exercise the actual default game too: --smoke-test uses the older
        # renderer, while no backend option here selects Luvcraft on Luft.
        world = root / "portable-world.sexp"
        for launch in range(2):
            with (evidence / f"game-{launch}.log").open("w") as log:
                process = subprocess.Popen(command + ["--world", str(world)], env=environment,
                                           cwd=root, stdout=log, stderr=subprocess.STDOUT)
                try:
                    deadline = time.monotonic() + 120
                    while not endpoint.exists():
                        if process.poll() is not None or time.monotonic() > deadline:
                            raise RuntimeError(f"Game did not start; see {log.name}")
                        time.sleep(0.2)
                    pid, port = map(int, endpoint.read_text().split())
                    assert pid == process.pid
                    response = evaluate(port, '''
                      (sb-ext:with-timeout 240
                        (loop until (and luft.render:*viewer*
                          (> (luv::sdl-canvas-frame-count
                               (luft.render:viewer-canvas luft.render:*viewer*)) 5)
                          (luv.application-agent:call-in-application-frame
                           luft.render:*viewer*
                           (lambda ()
                             (let ((scene (luft.render:viewer-source luft.render:*viewer*)))
                               (assert (null (luft.terrain::streaming-scene-production-errors scene)))
                               (and (plusp (hash-table-count (luft.terrain:streaming-scene-loaded scene)))
                                    (null (luft.terrain:streaming-scene-replacement scene))
                                    (luft.render::viewer-player-ground-resident-p luft.render:*viewer*))))))
                          do (sleep 0.2))
                        (assert (null (luv::sdl-canvas-startup-error
                          (luft.render:viewer-canvas luft.render:*viewer*))))
                        (assert (null (luv:canvas-failures
                          (luft.render:viewer-canvas luft.render:*viewer*))))
                        (dolist (kind '(:cat :gnome :fantus))
                          (luvcraft.agent::load-companion-mesh kind))
                        (format t "GAME-FRAMES-PASSED; terrain resident, companions loaded; ~S"
                          (luv:canvas-health (luft.render:viewer-canvas luft.render:*viewer*))))''')
                    assert "GAME-FRAMES-PASSED" in response
                    (evidence / f"game-{launch}-health.log").write_text(response)
                    # Let the first resident-ground physics/camera tick finish.
                    time.sleep(2)
                    screenshot = root / f"game-{launch}.png"
                    response = evaluate(port, f'(progn (luft.render:capture-viewer-frame {lisp_string(screenshot)}) :captured)')
                    shutil.copyfile(screenshot, evidence / screenshot.name)
                    (evidence / f"game-{launch}-capture.log").write_text(response)
                    if launch == 0:
                        evaluate(port, '''(luv.workbench:open-workbench-source-update
                          (luv.workbench:application-workbench luft.render:*viewer*))''')
                        panel = root / "source-update.png"
                        evaluate(port, f'(progn (luft.render:capture-viewer-frame {lisp_string(panel)}) :captured)')
                        shutil.copyfile(panel, evidence / panel.name)
                        evaluate(port, '''(luv.workbench:close-workbench-source-update
                          (luv.workbench:application-workbench luft.render:*viewer*))''')
                    evaluate(port, '(luft.render:stop-viewer)')
                    assert process.wait(timeout=60) == 0
                    assert not endpoint.exists(), "Stale Slynk endpoint after normal shutdown"
                    assert world.stat().st_size > 0, "World did not persist"
                finally:
                    if process.poll() is None:
                        process.terminate()
                        process.wait(timeout=30)
        print("PASS: moved ZIP, signatures, denied builder paths, help, errors, Metal frame,")
        print("      default game frames, screenshots, clean shutdown, saved world and relaunch.")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("archive", type=Path)
    parser.add_argument("evidence", type=Path)
    parser.add_argument("--cli-only", action="store_true",
                        help="Check relocation and CLI boot without requiring a GPU")
    parser.add_argument("--require-notarized", action="store_true",
                        help="Require a stapled ticket and Gatekeeper acceptance after extraction")
    args = parser.parse_args()
    test(args.archive.resolve(strict=True), args.evidence.resolve(), args.cli_only, args.require_notarized)
