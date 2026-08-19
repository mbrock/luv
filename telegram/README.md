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
telegram.client   who we say we are, logging in, and one INVOKE
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

## Logging in

Credentials come from `TELEGRAM_API_ID`/`TELEGRAM_API_HASH` — or the `TDLIB_`
names other clients use — in the environment, in `$TELEGRAM_ENV_FILE`, or in
one of `telegram.client:*credential-files*` (`./.env`, `~/.telegram.env`,
`~/.env`).

For a caller that has to go away and come back — a UI, a web form, anything
where the code arrives while the program is doing something else —
`begin-login` and `complete-login` split the wait in two:

```lisp
(defvar *login* (telegram.client:begin-login "+15551234567"))
;; connected to #<MTPROTO-CONNECTION 149.154.167.51:443 dc2>
;; PHONE_MIGRATE_4: moving to dc4
;; connected to #<MTPROTO-CONNECTION 149.154.167.91:443 dc4>
;; code sent by auth.sent-code-type-app to +15551234567

(telegram.client:complete-login *login* "29414")
;; logged in as Mikael (@…) id …
```

`begin-login` follows a `PHONE_MIGRATE` to the data centre that owns the
number and returns a `code-login`, which owns the live connection the code
was sent over. Nothing about a login in progress is written down: the code is
good for about a minute, so a half-finished login is a conversation rather
than a record, and one that outlived its process is a dead end rather than
something to resume. Give up on one with `abandon-login`, which closes the
connection; only the finished authorization key reaches `~/.telegram-session`.

If the account has two-factor auth, `complete-login` stops at
`SESSION_PASSWORD_NEEDED` and `complete-password` answers it by SRP over that
same connection:

```lisp
(telegram.client:complete-password *login* "…")
```

One-shot from a shell, `log-in` does the whole thing in one call and prompts
for what it needs — password reading turns terminal echo off:

```sh
scripts/telegram '(telegram.client:log-in :phone-number "+15551234567")'
```

A caller with no terminal passes `:password-reader nil` to `complete-login`
and gets a `password-required` condition instead of a prompt; that is how the
in-game panel does it.

### QR login

Telegram's mobile-app QR login is public MTProto, not a private desktop
feature.  `log-in-with-qr` exports a short-lived token, renders it in the
terminal, and keeps its unauthenticated connection alive for the
`updateLoginToken` that arrives when the phone accepts it.  If the account
has two-factor auth, the acceptance stops at the password —
`qr-login-password-hint` becomes the account's hint, and `complete-password`
answers by SRP over the same connection, exactly as for a code login.  The
completed session is saved exactly like a code login.

```sh
scripts/telegram '(telegram.client:log-in-with-qr)'
```

The small `qr-login` object is also available when a different surface wants
to render the URI itself: `begin-qr-login`, `qr-login-uri`, and
`wait-for-qr-login`.  The default terminal presenter uses `qrencode` from the
development shell and falls back to printing the `tg://login?...` URI.

In luvcraft the same flow runs on the phone (press `f`, then TAB): with no
credentials it asks for an api_id and hash and writes them to
`~/.telegram.env`; with no session it asks for a phone number, then the
code, then a password if the account has one.  A wall terminal offers the
panel as its third mode.

Afterwards the stored key is the credential, and `resume` is the ordinary way
in — no handshake, no code:

```lisp
(client:resume)
;; resumed as Mikael Brockman (@mbrockman) id 362441422
```

It makes the connection current, so nothing has to be threaded through
afterwards:

```lisp
client:*connection*   ; the connection INVOKE defaults to
client:*user*         ; who it is logged in as
(client:disconnect)   ; close it and forget it
```

## Calling it

```lisp
(client:resume)
(client:invoke :help.get-config)
;; => #<config this-dc=2 dc-options=[19] …>
```

A request is a record, built by keyword:

```lisp
(client:invoke :messages.send-message
               :peer (tl:make-tl :input-peer-channel
                                 :channel-id 3690254489
                                 :access-hash -1568329345395679826)
               :message "hello"
               :random-id (- (random (expt 2 62)) (expt 2 61)))
;; => #<updates updates=[3] users=[1] chats=[1] …>
```

`invoke` dispatches on what it is handed, so the connection can lead when
there is more than one, and a record can be built beforehand:

```lisp
(client:invoke connection :help.get-config)
(client:invoke (tl:make-tl :help.get-config))
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
message, so that set really is a protocol we extend. Telegram's own 2455 are
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

**A stored session keeps the key, not the session id.** An authorization key
is worth persisting; a session id is not, because the server also remembers
how far its sequence numbers have got, and resuming an id with the counters
back at zero earns a `bad_msg_notification` instead of an answer. A fresh
session over a kept key costs one round trip and nothing else.

**Round constants are computed.** FIPS 180-4 defines SHA's constants as the
leading fractional bits of the square and cube roots of the small primes, and
`fractional-root-bits` says exactly that, on integers, in eight lines — the
same reason the AES S-box is derived from its definition rather than copied.
A hundred and fifty transcribed hexadecimal words is a hundred and fifty
chances to be wrong.

**Transports are a family.** `abridged-transport` and
`intermediate-transport` are two classes over three generic functions;
adding the obfuscated or padded variants is a class and three methods.

## Not yet

- Peer resolution. Finding a group means paging `messages.getDialogs` and
  matching a title by hand; there should be a `find-peer` that caches
  `access_hash`es, since every send needs one.
- Sign-up, for a number with no account behind it.
- Keeping `schema/api.tl` current. It is a snapshot of layer 228, taken from
  tdlib; Telegram moves, and a constructor the snapshot lacks is a decode
  that stops rather than one that guesses. `+api-layer+` and the schema file
  have to move together.
- The updates loop: `updates.getDifference` and a `pts`/`qts`/`date` cursor.
- File upload and download, which need `upload.getFile` and its own
  chunking.
- Sending requests concurrently. `invoke` waits for its answer before the
  next request goes out, which is correct but leaves the container machinery
  unused.
- Migration for calls other than login: `USER_MIGRATE_*` and `FILE_MIGRATE_*`
  are recognized by `migration-data-center` but only `begin-login` acts on
  them.
