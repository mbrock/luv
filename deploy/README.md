# swa.sh deployment files

The files in this directory are the source copies of swa.sh's Caddy and
systemd configuration.  Install the Nix cache additions as root with:

```sh
install -Dm755 deploy/luv-nix-cache /usr/local/libexec/luv-nix-cache
install -Dm755 deploy/luv-deploy /usr/local/libexec/luv-deploy
install -Dm755 deploy/luv-private-gallery /usr/local/libexec/luv-private-gallery
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

The private Knuth gallery uses the existing authenticated owner account. Its
bcrypt verifier is supplied to Caddy as `LUV_PRIVATE_PASSWORD_HASH` by a
swa-only systemd environment file; neither the verifier nor the password
belongs in Git. The `swa-private` git-annex directory remote at
`/srv/luv-private-annex` is also swa-only and mode 0700. A deploy runs
`luv-private-gallery`, which obtains every referenced raster from that remote,
rejects missing content, and materializes a Caddy-readable release under
`/srv/luv-private-gallery/current`. The annex store itself is never below a
Caddy root.

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

The cache contains the complete native closures of `path:./nix#environment`
and `path:./nix#slim-environment`,
including the pinned McCLIM source, patched cl-sdl3 source, custom SBCL/Tracy/
Swash derivations where applicable, and their nixpkgs dependencies.  It is
append-only; old store objects remain available for older locked checkouts and
disk usage should be monitored.
