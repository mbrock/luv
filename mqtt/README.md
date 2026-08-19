# mqtt

An MQTT 5 client in Common Lisp, written to talk to the tailnet lobby
(`lobby/`) from the luv image.  It follows the shape of `telegram/`:

- `mqtt` — sans-IO.  `wire.lisp` has the five wire data types and a
  reader/writer; `packets.lisp` makes each control packet a class that
  encodes and decodes its own body, plus a stream decoder; `session.lisp`
  is the client state machine.  You feed a session the octets that arrived
  (`session-receive`), and drain the octets it wants sent
  (`drain-session-outbox`) and what happened (`drain-session-events`).
  Nothing here touches a socket or a clock, which is why `mqtt/test` can
  check it against the specification's own byte examples.
- `mqtt.net` — the one socket-owning file.  `open-mqtt-connection`
  connects and waits for CONNACK; `publish`, `subscribe`, `unsubscribe`,
  `ping`, `next-message` block on the answer; `pump-connection` is the
  step they are built from and pings on the keep-alive schedule.
  `open-lobby-connection` finds the lobby on the local tailnet: the
  `luv-lobby` Service under the MagicDNS suffix that `tailscale status`
  reports (`LUV_LOBBY_HOST` or `*lobby-host*` override it).
- The lobby as a value store: retained messages under `luv/store/KEY`.
  `lobby-put`, `lobby-get`, `lobby-delete`, `lobby-keys`, and the
  error-swallowing `lobby-value` for optional fallbacks -- `openai:make-agent`
  asks it for `OPENAI_API_KEY` when the environment lacks one.  From the
  shell: `scripts/luv lobby put OPENAI_API_KEY` (value on stdin),
  `scripts/luv lobby get KEY`, `ls`, `rm`.

```lisp
(mqtt.net:with-mqtt-connection (c :host mqtt.net:*lobby-host* :client-id "me")
  (mqtt.net:subscribe c '("luv/#" :qos 1))
  (mqtt.net:publish c "luv/hello" "hi" :qos 1)
  (mqtt:publish-payload-string (mqtt.net:next-message c)))
```

Tests: `(asdf:test-system :mqtt)`, or `make test`, which now includes it.
