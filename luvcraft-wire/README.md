# luvcraft-wire

The wire protocol for two players in one Luvcraft world (wiki figure
A8LXLK, work mark H6A8P4). It depends on no game package.

- `luvcraft-wire` (`luvcraft.wire`) — sans-IO, like `mqtt/`. Messages and
  their parts are structures with typed fields (`codec.lisp`);
  `encode-message` and `decode-message` turn them into frames and back, and
  `feed-frame-decoder` cuts a stream into frames. Every frame starts with
  `LUVW`, the protocol version, the type, a 16-octet world tag (the MD5 that
  `luft.terrain:world-source-tag` returns in hex; `world-tag` takes either form), the
  session, and the body length. Decoding is strict: it refuses non-finite
  reals, non-unit quaternions, yaw outside [-π, π], pitch outside
  [-π/2, π/2], over-fast velocities, sites and positions outside the
  `wire-limits` domain, offsets outside the vocabulary (a welcome's own
  table counts for its own offsets), unknown enumerations, booleans other
  than 0 and 1, bad UTF-8, short input, and trailing octets.
- `luvcraft-wire/net` (`luvcraft.wire.net`) — endpoints. One worker
  thread owns the TCP connection(s) and the UDP socket, or a
  `loopback-network` for tests, and posts `arrival`s and `notice`s to a
  mailbox. The game never touches a socket.

```lisp
(defvar *host* (start-host :name "Mikael" :world-tag tag :announce "Little world"))
(defvar *guest* (start-guest "192.168.1.20:47475" :name "Ida"))  ; or a find-hosts target
(send-message *guest* (make-hello-message :identity "ida-uuid" :name "Ida"))
(drain-mailbox *host*)      ; => (#S(NOTICE :KIND :CONNECTED ...) #S(ARRIVAL ...))
(send-message *host* (make-welcome-message ...) peer)  ; peer from the arrival
(send-message *guest* (make-pose-message :player "ida-uuid" :pose pose))
(stop-endpoint *guest*)
```

Reliable messages take the stream. Unreliable ones (poses, entity
snapshots) take a datagram while both ends hear each other's half-second
probes, and the stream otherwise. Hosts broadcast a beacon every half
second to UDP 47474 while announcing; `start-discovery` or `find-hosts`
hears them.

Tests: `(asdf:test-system "luvcraft-wire")`, which runs two endpoints in one
image over loopback and over real sockets.
