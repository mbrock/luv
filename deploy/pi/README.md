# Luvland on a Raspberry Pi console

plank, a Raspberry Pi 5 on a TV, boots straight into Luvland on tty1 with no
other desktop, and is played from the sofa with a wired Xbox Series pad
(wiki: `compositor.org`, #JJJAO9).  These are the source copies of its
configuration.  Paths name the user `mbrock` and the checkout
`~/src/luv`; change them for another machine.

## The session

- `luvland.service` runs `luvland-session` as the user on tty1, with a
  logind session, and restarts it if it exits.
- `luvland-session` takes only Luvland's own variables from the Nix shell,
  so programs started from Luvland see the machine's ordinary environment,
  points Vulkan at the Pi's V3DV driver, and runs `build/luvland
  --fullscreen --no-terminal`.
- `init.lisp` is the init file (`~/.config/luvland/init.lisp`): the games
  the play menu offers, and `*client-scale*`.

Install:

```sh
make luvland
install -Dm755 deploy/pi/luvland-session ~/.local/bin/luvland-session
install -Dm644 deploy/pi/init.lisp ~/.config/luvland/init.lisp
sudo install -Dm644 deploy/pi/luvland.service /etc/systemd/system/luvland.service
sudo systemctl disable lightdm   # or whichever display manager is installed
sudo systemctl mask getty@tty1
sudo systemctl daemon-reload && sudo systemctl enable --now luvland
```

The screen runs at 1280x720: add `video=HDMI-A-1:1280x720@60` to
`/boot/firmware/cmdline.txt`.  At 1080p the Pi's GPU cannot hold a WebGL
game at 60 fps; at 720p, with Starfall drawn at 4/5 of its window and
scaled up (`(setf *client-scale* 5/4)`), it holds 58-60.

## The Xbox pad

A wired Xbox One or Series pad sends nothing, its logo blinking, until the
host sends it a start sequence.  The kernel's xpad driver sends it when it
binds and when the pad's input device is first opened.  A pad that powers
off and on again while plugged in announces itself again and waits for the
sequence once more, and stock xpad answers that announce only for a few
wireless dongles; with Luvland holding the device open, the pad then
blinks until xpad is rebound.

Two parts keep it started:

- `xpad/` builds xpad with `xpad-reinit-on-announce.patch`, which sends the
  start sequence again on every announce, as SDL's and xone's drivers for
  the protocol do.  `xpad/install` (as root, with `dkms` and the kernel's
  headers installed) fetches the running kernel's `xpad.c` from
  raspberrypi/linux, patches it, and installs it as the DKMS package
  `xpad-luv`, which takes precedence over the kernel's xpad and is rebuilt
  for each new kernel.
- `xpad-watchdog` is the safety net, a root service that reads usbmon.  It
  rebinds xpad to a pad that has done nothing but announce for 3 s, which
  starts it even with the stock driver, and binds xpad to any Xbox pad
  interface left without a driver, as happens if something claims the pad
  over raw USB.  Luvland's SDL is kept from doing that itself with
  `SDL_JOYSTICK_HIDAPI=0`.

Install:

```sh
sudo apt-get install dkms linux-headers-rpi-2712
sudo deploy/pi/xpad/install
sudo install -Dm755 deploy/pi/xpad-watchdog /usr/local/libexec/xpad-watchdog
sudo install -Dm644 deploy/pi/xpad-watchdog.service /etc/systemd/system/xpad-watchdog.service
sudo systemctl daemon-reload && sudo systemctl enable --now xpad-watchdog
journalctl -u xpad-watchdog   # says which pads it watches, and every rebind
```

Do not reset the pad's USB device through its `authorized` attribute: on
the Pi 5 that took down the whole xHCI controller until a reboot.
