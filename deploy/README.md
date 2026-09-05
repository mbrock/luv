# swa.sh deployment files

## Live wiki and deploy button

On `swa`, `/etc/caddy/Caddyfile` terminates HTTPS for `luv.swa.sh` and
proxies the wiki to `/run/luv/live.sock`, a symlink to `1.sock` or `2.sock`.
Upstream keepalive is disabled so new requests follow a slot switch.
`luv-wiki@1.service` and `luv-wiki@2.service` run as `mbrock:www-data` in
`/srv/luv-slots/1` and `/srv/luv-slots/2`, using
`./env ./scripts/wiki serve --socket /run/luv/N.sock`. These are detached
worktrees of `/srv/luv`, separate from the development checkout.

The button posts to `/admin/deployments`, protected by Caddy Basic Auth.
The wiki writes `/run/luv/deploy.request`; `luv-deploy.path` notices the
change and starts `luv-deploy.service`, which runs
`/usr/local/libexec/luv-deploy`. It fetches `origin/main` in `/srv/luv`,
checks out that commit in the inactive slot, runs `make` and `make test`,
starts the slot, and checks `/healthz` and `/version`. Only then does it
atomically switch `live.sock`. It verifies the public version and retains
the previous slot for rollback. Failures before the switch leave traffic
on the existing slot. The browser streams terminal output from
`/run/luv/deployments/ID.log`; `ID.done` contains the exit status.

Caddy serves `/video/*` directly from `/var/www/luv` and `/nix-cache/*`
from `/var/www/luv/nix-cache`. Successful deployments queue the separate
`luv-nix-cache@ID.service` to publish the newly live Nix closure.

## Installing host configuration

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

The cache contains the complete native closures of `path:./nix#environment`
and `path:./nix#slim-environment`,
including the pinned McCLIM source, patched cl-sdl3 source, custom SBCL/Tracy/
Swash derivations where applicable, and their nixpkgs dependencies.  It is
append-only; old store objects remain available for older locked checkouts and
disk usage should be monitored.
