# luv-lobby

`luv-lobby` is a tailnet-only MQTT 5 broker. It embeds a tagged Tailscale node
with `tsnet` and gives that node's `svc:luv-lobby` TCP listener to Mochi MQTT.
There is no host-wide `tailscaled`, TUN interface, or open local TCP port.

Its durable state lives outside the checkout: the Tailscale node identity in
`~/.local/state/luv/lobby/tsnet` and the Pebble MQTT store in
`~/.local/state/luv/lobby/mqtt` (both under `$XDG_STATE_HOME` when set). Retained
messages have no broker-imposed expiry and survive service restarts. On the
first run, set `TS_AUTHKEY` to a tagged auth key carrying `tag:luv`; later runs
reuse the saved node state and do not need the key.

```sh
export TS_AUTHKEY=tskey-auth-...
scripts/lobby
```

Before the process can host the Service, define `luv-lobby` on port `1883` in
the Tailscale Services admin page. The tailnet policy must permit `tag:luv` to
auto-approve `svc:luv-lobby` and grant intended clients access to that Service.

tsnet delivers Service connections through a localhost proxy, so the listener
asks for a PROXY protocol v2 header and reads the peer's real tailnet address
out of it (`proxyproto.go`). Each MQTT connection is then resolved through
tsnet's LocalAPI and receives a Tailnet principal (node name/ID, user, and tags). An unresolved connection is
denied. The broker logs those principals at connect and disconnect, and only a
resolved principal can publish or subscribe. It currently grants every resolved
principal every topic; use the principal hook as the place to add per-user or
per-tag topic ACLs when the lobby needs them.

When run as a systemd `Type=notify` service, `luv-lobby` sends `READY=1` only
after the Tailscale Service listener is attached and the MQTT broker is serving.
