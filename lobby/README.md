# luv-lobby

`luv-lobby` is a tailnet-only MQTT 5 broker. It embeds a tagged Tailscale node
with `tsnet` and gives that node's `svc:luv-lobby` TCP listener to Mochi MQTT.
There is no host-wide `tailscaled`, TUN interface, or open local TCP port.

Its durable Tailscale node state belongs in `~/.local/state/luv/lobby/tsnet`
(or `$XDG_STATE_HOME/luv/lobby/tsnet`), not in this checkout. On its first run,
set `TS_AUTHKEY` to a tagged auth key carrying `tag:luv`; later runs reuse the
saved node state and do not need the key.

```sh
export TS_AUTHKEY=tskey-auth-...
scripts/lobby
```

Before the process can host the Service, define `luv-lobby` on port `1883` in
the Tailscale Services admin page. The tailnet policy must permit `tag:luv` to
auto-approve `svc:luv-lobby` and grant intended clients access to that Service.

Mochi currently uses its allow-all hook: Tailscale access policy is the network
admission boundary, but every admitted MQTT client may publish or subscribe to
any topic. Add a Mochi authentication/ACL hook before exposing this to mutually
untrusted tailnet users.
