# telegram — an MTProto client in Common Lisp

A Telegram client, built from the wire up. It has no dependencies outside
SBCL: the digests, the block cipher, the big-integer work, the PEM and DER
parsing, and the base64 are all here, because MTProto needs AES-256 in IGE
mode and nothing ships that anyway.

Six ASDF systems:

| system            | what it is                                                    |
| ----------------- | ------------------------------------------------------------- |
| `telegram`        | the sans-IO core: bytes, TL, crypto, framing, auth, sessions   |
| `telegram/api`    | Telegram's published schema, loaded as a table                 |
| `telegram/net`    | the one file that owns a socket                                |
| `telegram/client` | application identity, layer negotiation, `invoke`              |
| `telegram/test`   | the rove suite                                                 |
| `telegram/live`   | opt-in checks against a real data centre                       |

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
(telegram.live:live-handshake-check :dc-id 2)
;; connected to #<MTPROTO-CONNECTION 149.154.167.51:443 dc2>
;; authorization key c0e30d70a24557eb
;; server salt 7059782079104119541, clock offset -1s
;; pong (:PONG 1)

(telegram.live:live-api-check :dc-id 2)
;; CONFIG: this is dc2, 19 data centres known
;;     dc1 149.154.175.51:443
;;     dc2 149.154.167.41:443
;; NEAREST-DC: country "DE", nearest dc2
```

The first creates a real 2048-bit authorization key against Telegram's
production servers and pings over it. The second goes on to call the API.
Both transports and both networks work:

```lisp
(telegram.live:live-handshake-check
 :dc-id 2 :test t
 :transport (make-instance 'telegram:intermediate-transport))
```

## Shape

```
telegram.octets   dense byte vectors; DEFLATE; entropy and clock as objects
telegram.tl       the TL codec: constructors as classes, schema as records
telegram.crypto   SHA-1/256, AES-256, IGE, expt-mod, RSA public keys
telegram          transports, envelopes, the handshake, sessions, api.tl
telegram.net      sockets
telegram.client   who we say we are, and one INVOKE
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

## Calling it

```lisp
(let ((client:*application* (client:application-from-environment)))
  (client:with-telegram (connection :dc-id 2)
    (client:invoke connection :help.get-config)))
;; => #<config this-dc=2 dc-options=[19] …>
```

A request is a record, built by keyword:

```lisp
(client:invoke connection :messages.send-message
               :peer (tl:make-tl :input-peer-self)
               :message "hello" :random-id 1)
```

and the schema is browsable from the listener:

```lisp
(tl:find-tl-definitions "sendMessage" :functions t)
(tl:describe-tl :messages.send-message)
;; messages.sendMessage#545CD15A (function returning Updates)
;;   peer                     (OBJECT InputPeer)
;;   message                  STRING
;;   entities                 (VECTOR (OBJECT MessageEntity))  [optional: flags bit 3]
;;   …
```

## Design notes

**Two schemas, two representations, one decoder.** The thirty-odd MTProto
constructors are classes: `handle-session-message` has a method per service
message, so that set really is a protocol we extend. Telegram's own 2333 are
`tl-record`s — a definition plus a vector of values — because that set is
closed and someone else owns every name and number in it. Generating a class
per constructor would have cost 2364 classes and 10333 exported symbols to
buy nothing but names; it also took ten seconds to load, against a third of a
second now. `decode-tl-object` checks the class registry first and the schema
table second, so the two compose without either knowing about the other.

**The schema stays text.** `define-tl-schema` reads `schema/api.tl` at
macroexpansion and expands into one quoted table. The `.tl` file is the
single authority for a numbering Telegram owns, nothing is checked in twice,
and the fasl is half a megabyte.

**Constructors that are classes are made by `define-tl-object`**, which takes
an id and a slot layout and emits a `defclass` plus methods for
`tl-constructor-id`, `decode-tl-body`, and `encode-tl`. A constructor whose
layout the slot vocabulary cannot spell — `msg_container`, `future_salts` —
passes `:decode nil` and gets a hand-written method instead of a special
case in the macro. Slot types (`int128`, `bytes`, `(vector long)`) are
themselves EQL-dispatched generics, so a new type is a method rather than a
table entry, and both representations read and write through them.

**Flags are computed, not maintained.** TL's optional fields are guarded by
bits in a flags word. Absent is `nil` going both ways, and the word itself is
recomputed from what is actually set whenever a value is encoded. That is why
`Vector<T>` decodes to a Lisp vector rather than a list: an empty optional
vector has to stay distinguishable from an absent one.

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

- Login. `auth.sendCode` and `auth.signIn` are ordinary `invoke` calls now,
  but nothing drives them, and `auth.signIn` needs the SRP work for
  two-factor accounts.
- Session persistence, so a second run skips the handshake.
- DC migration on `USER_MIGRATE_*` / `PHONE_MIGRATE_*`.
- The updates loop: `updates.getDifference` and a `pts`/`qts`/`date` cursor.
- File upload and download, which need `upload.getFile` and its own
  chunking.
