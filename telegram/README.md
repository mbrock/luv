# telegram — an MTProto client in Common Lisp

A Telegram client, built from the wire up. It has no dependencies outside
SBCL: the digests, the block cipher, the big-integer work, the PEM and DER
parsing, and the base64 are all here, because MTProto needs AES-256 in IGE
mode and nothing ships that anyway.

Four ASDF systems:

| system            | what it is                                                    |
| ----------------- | ------------------------------------------------------------- |
| `telegram`        | the sans-IO core: bytes, TL, crypto, framing, auth, sessions   |
| `telegram/net`    | the one file that owns a socket                                |
| `telegram/test`   | the rove suite                                                 |
| `telegram/live`   | an opt-in check against a real data centre                     |

It is deliberately separate from `luv`; nothing here depends on it, and
`make test` does not run this suite.

## Running it

```sh
./scripts/dev sbcl --non-interactive \
  --eval '(asdf:test-system "telegram")'
```

And, against Telegram itself:

```lisp
(asdf:load-system "telegram/live")
(telegram.net:live-handshake-check :dc-id 2)
;; connected to #<MTPROTO-CONNECTION 149.154.167.51:443 dc2>
;; authorization key c0e30d70a24557eb
;; server salt 7059782079104119541, clock offset -1s
;; pong (:PONG 1)
```

That creates a real 2048-bit authorization key against Telegram's production
servers and then pings over it. Both transports and both networks work:

```lisp
(telegram.net:live-handshake-check
 :dc-id 2 :test t
 :transport (make-instance 'telegram:intermediate-transport))
```

## Shape

```
telegram.octets   dense byte vectors; entropy and clock as objects
telegram.tl       the TL codec, and constructors as classes
telegram.crypto   SHA-1/256, AES-256, IGE, expt-mod, RSA public keys
telegram          transports, envelopes, the handshake, sessions
telegram.net      sockets
```

The layering is one-directional and everything up to and including
`telegram` is a pure function of its inputs — no socket, no clock read, no
randomness that was not handed in. Entropy and time are objects
(`system-entropy` / `replaying-entropy`, `system-clock` / `frozen-clock`),
which is what lets a whole handshake be a value:

```lisp
(let ((exchange (mt:make-auth-exchange
                 :entropy (replaying +sample-random+ +sample-server-random+)
                 :clock (frozen 1693436740)
                 :public-keys (list sample-key))))
  (mt:begin-auth-exchange exchange :nonce +sample-nonce+)
  ;; => the exact req_pq_multi bytes, every time
  (mt:advance-auth-exchange exchange +sample-res-pq+))
  ;; => the exact req_DH_params bytes, RSA padding and all
```

The test suite replays one complete recorded exchange — resPQ through
dh_gen_ok, arriving at a named 2048-bit key — and it is the same recording
that an Elixir implementation (`~/exmt`) and a C++ one (`~/nxtui`) reproduce.
The rest of the vectors come from FIPS 180-4 and FIPS 197.

## Design notes

**Constructors are classes.** `define-tl-object` takes an id and a slot
layout and emits a `defclass` plus ordinary methods for
`tl-constructor-id`, `decode-tl-body`, and `encode-tl`. A constructor whose
layout the slot vocabulary cannot spell — `msg_container`, `future_salts` —
passes `:decode nil` and gets a hand-written method instead of a special
case in the macro. Slot types (`int128`, `bytes`, `(vector long)`) are
themselves EQL-dispatched generics, so a new type is a method rather than a
table entry.

**The handshake dispatches on the response.** `handle-auth-response` is
specialized on `res-pq`, `server-dh-params-ok`, `dh-gen-ok`, and their
siblings, and returns the next payload to send or `nil` when the key exists.
The phase slot rejects a response that arrives out of order; it never chooses
which code runs.

**Sessions dispatch on the message.** `handle-session-message` has a method
per service message and a default that logs. Acknowledgement is a `:before`
method, since whether a message needs acknowledging depends on its sequence
number and nothing else — which is also why a container's members get acked
and the container does not.

**Transports are a family.** `abridged-transport` and
`intermediate-transport` are two classes over three generic functions;
adding the obfuscated or padded variants is a class and three methods.

## Not yet

- `api.tl` — the 2346-constructor Telegram schema, which wants code
  generation on top of `define-tl-object`. Without it there is no
  `help.getConfig`, no login, no messages.
- `gzip_packed` is recognized but not inflated.
- Session persistence, DC migration on `USER_MIGRATE_*`, and the updates
  difference loop.
