# swa.sh deployment files

The files in this directory are the source copies of swa.sh's Caddy and
systemd configuration.  Install the Nix cache additions as root with:

```sh
install -Dm755 deploy/luv-nix-cache /usr/local/libexec/luv-nix-cache
install -Dm755 deploy/luv-deploy /usr/local/libexec/luv-deploy
install -Dm644 deploy/luv-nix-cache@.service \
  /etc/systemd/system/luv-nix-cache@.service
install -Dm644 deploy/luv-tmpfiles.conf /etc/tmpfiles.d/luv.conf
systemd-tmpfiles --create /etc/tmpfiles.d/luv.conf
systemctl daemon-reload
```

Merge `luv.swa.sh.Caddyfile` into `/etc/caddy/Caddyfile`, validate it with
`caddy validate --config /etc/caddy/Caddyfile`, and reload Caddy.  A successful
blue-green deployment then starts cache publication from the newly live slot.
The first run creates a persistent signing key under
`/var/lib/luv-nix-cache`; that directory must remain root-only and should be
backed up.

Once publication finishes, configure a client using the public key served by
the cache:

```sh
curl https://luv.swa.sh/nix-cache/luv-cache.pub
```

Add the returned line and cache URL to `/etc/nix/nix.conf`:

```ini
extra-substituters = https://luv.swa.sh/nix-cache?priority=30
extra-trusted-public-keys = luv.swa.sh-1:PpD45iCBkJ38ZkvlyZcLiGdIz6yVehXn3fm1JvG18Bw=
```

The flake contains the same public cache hint.  Nix installations that permit
flake-supplied configuration therefore use it without global configuration.

The cache contains the complete native closures of `.#dev` and `.#slim-dev`,
including the pinned McCLIM source, patched cl-sdl3 source, custom SBCL/Tracy/
Swash derivations where applicable, and their nixpkgs dependencies.  It is
append-only; old store objects remain available for older locked checkouts and
disk usage should be monitored.
