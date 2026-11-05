;;;; Packages for the Luvcraft wire protocol (#A8LXLK).
;;;;
;;;;   luvcraft.wire       the codec: typed records and messages over octets
;;;;   luvcraft.wire.net   endpoints: one worker that owns every socket
;;;;
;;;; The codec is sans-IO: every function is a function of its inputs.  It
;;;; knows nothing of the game; recipes, block identities, and entity state
;;;; are small typed values that the game fills in and reads back.

(defpackage #:luvcraft.wire
  (:use #:cl)
  (:documentation
   "The Luvcraft wire codec.  Messages are structures that encode to and
decode from octet vectors.  Every frame carries a magic, the protocol
version, a world tag, and a session; decoding refuses anything that is not
exactly what an encoder would have written.")
  (:export
   ;; octets and conditions
   #:octet #:octets #:make-octets
   #:wire-error #:malformed-message #:malformed-message-detail
   #:unsupported-version #:unsupported-version-version
   #:unencodable-value #:unencodable-value-detail
   ;; limits
   #:wire-limits #:make-wire-limits #:copy-wire-limits
   #:wire-limits-vocabulary-size #:wire-limits-domain-low
   #:wire-limits-domain-high #:wire-limits-max-speed
   #:wire-limits-max-frame-length #:*default-wire-limits*
   ;; constants
   #:+magic+ #:+protocol-version+ #:+world-tag-length+ #:+header-length+
   #:+max-datagram-length+ #:+default-port+ #:+default-announce-port+
   #:zero-world-tag #:world-tag
   ;; values and records
   #:wire-value #:wrap-yaw
   #:block-identity #:make-block-identity #:block-identity-p
   #:block-identity-offset #:block-identity-entity
   #:recipe #:make-recipe #:recipe-p #:recipe-generator #:recipe-version
   #:recipe-seed #:recipe-parameters
   #:principal #:make-principal #:principal-p #:principal-kind #:principal-id
   #:site #:make-site #:site-p #:site-x #:site-y #:site-z
   #:edit #:make-edit #:edit-p #:edit-site #:edit-before #:edit-after
   #:placement #:make-placement #:placement-p #:placement-site
   #:placement-block
   #:event #:make-event #:event-p #:event-sequence #:event-tick #:event-actor
   #:event-transaction #:event-site #:event-before #:event-after #:event-cause
   #:pose #:make-pose #:pose-p #:pose-tick #:pose-position #:pose-velocity
   #:pose-yaw #:pose-pitch #:pose-gait #:pose-flying #:pose-held
   #:pose-ride
   #:entity-state #:make-entity-state #:entity-state-p #:entity-state-id
   #:entity-state-kind #:entity-state-position #:entity-state-velocity
   #:entity-state-orientation #:entity-state-state
   #:entity-snapshot #:make-entity-snapshot #:entity-snapshot-p
   #:entity-snapshot-tick #:entity-snapshot-entities
   #:stack #:make-stack #:stack-p #:stack-block #:stack-count
   #:player-record #:make-player-record #:player-record-p
   #:player-record-identity #:player-record-name #:player-record-pose
   #:player-record-creative #:player-record-held #:player-record-simple
   #:player-record-kit #:player-record-inventory
   #:companion-record #:make-companion-record #:companion-record-p
   #:companion-record-id #:companion-record-kind #:companion-record-presence
   #:companion-record-asked #:companion-record-said #:companion-record-said-serial
   #:companion-record-attending #:companion-record-activity #:companion-record-tool
   #:companion-record-destination #:companion-record-pose #:companion-record-owner
   #:chunk-hash #:make-chunk-hash #:chunk-hash-p #:chunk-hash-chunk
   #:chunk-hash-digest
   ;; messages
   #:message #:message-p #:message-world-tag #:message-session
   #:message-type-code #:message-channel #:message-type-for-code
   #:hello-message #:make-hello-message #:hello-message-p
   #:hello-message-identity #:hello-message-name
   #:hello-message-resume #:hello-message-epoch
   #:welcome-message #:make-welcome-message #:welcome-message-p
   #:welcome-message-epoch #:welcome-message-recipe #:welcome-message-vocabulary
   #:welcome-message-watermark #:welcome-message-edits
   #:welcome-message-entities #:welcome-message-player
   #:events-message #:make-events-message #:events-message-p
   #:events-message-events
   #:edit-request-message #:make-edit-request-message #:edit-request-message-p
   #:edit-request-message-request #:edit-request-message-edits
   #:edit-request-message-cause
   #:edit-reply-message #:make-edit-reply-message #:edit-reply-message-p
   #:edit-reply-message-request #:edit-reply-message-outcome
   #:edit-reply-message-sequence #:edit-reply-message-reason
   #:player-update-message #:make-player-update-message
   #:player-update-message-p #:player-update-message-presence
   #:player-update-message-player
   #:chat-message #:make-chat-message #:chat-message-p
   #:chat-message-speaker #:chat-message-text
   #:chunk-hashes-message #:make-chunk-hashes-message #:chunk-hashes-message-p
   #:chunk-hashes-message-hashes #:chunk-hashes-message-watermark
   #:chunk-content-message #:make-chunk-content-message
   #:chunk-content-message-p #:chunk-content-message-chunk
   #:chunk-content-message-digest #:chunk-content-message-content
   #:bye-message #:make-bye-message #:bye-message-p #:bye-message-reason
   #:bye-message-text
   #:companion-message #:make-companion-message #:companion-message-p
   #:companion-message-companion
   #:companion-tell-message #:make-companion-tell-message #:companion-tell-message-p
   #:companion-tell-message-companion #:companion-tell-message-text
   #:companion-pose-message #:make-companion-pose-message #:companion-pose-message-p
   #:companion-pose-message-companion #:companion-pose-message-pose
   #:summon-message #:make-summon-message #:summon-message-p
   #:summon-message-kind #:summon-message-position
   #:sky-request-message #:make-sky-request-message #:sky-request-message-p
   #:sky-request-message-setting #:sky-request-message-value
   #:ride-message #:make-ride-message #:ride-message-p
   #:ride-message-rider #:ride-message-entity #:ride-message-action
   #:throw-message #:make-throw-message #:throw-message-p
   #:throw-message-kind #:throw-message-position #:throw-message-velocity
   #:pose-message #:make-pose-message #:pose-message-p
   #:pose-message-player #:pose-message-pose
   #:entity-snapshot-message #:make-entity-snapshot-message
   #:entity-snapshot-message-p #:entity-snapshot-message-snapshot
   #:probe-message #:make-probe-message #:probe-message-p
   #:probe-message-heard
   #:beacon-message #:make-beacon-message #:beacon-message-p
   #:beacon-message-name #:beacon-message-port #:beacon-message-world
   ;; encoding and decoding
   #:encode-message #:decode-message #:stamp-message
   #:frame-decoder #:make-frame-decoder #:feed-frame-decoder
   #:frame-decoder-buffered))

(defpackage #:luvcraft.wire.net
  (:use #:cl #:luvcraft.wire)
  (:documentation
   "Endpoints for the Luvcraft wire.  One worker thread per endpoint owns its
TCP connections and its UDP socket (or, for tests, an in-memory loopback
network) and posts decoded messages to a mailbox the game drains at a frame
boundary.  The game never touches a socket.")
  (:export
   ;; endpoints
   #:endpoint #:endpoint-role #:endpoint-name #:endpoint-state
   #:endpoint-port #:endpoint-world-tag #:endpoint-limits #:endpoint-peers
   #:endpoint-last-error
   #:start-host #:start-guest #:stop-endpoint #:endpoint-running-p
   #:send-message #:close-peer
   #:drain-mailbox #:next-mailbox-item
   ;; peers and mailbox items
   #:peer #:peer-id #:peer-session #:peer-address #:peer-port
   #:peer-datagram-p #:peer-open-p
   #:arrival #:arrival-p #:arrival-peer #:arrival-channel #:arrival-message
   #:notice #:notice-p #:notice-kind #:notice-peer #:notice-detail
   ;; discovery
   #:discovery #:start-discovery #:stop-discovery #:discovered-hosts
   #:find-hosts
   #:discovered-host #:discovered-host-address #:discovered-host-port
   #:discovered-host-name #:discovered-host-world #:discovered-host-world-tag
   #:discovered-host-heard #:discovered-host-target
   ;; loopback
   #:loopback-network #:make-loopback-network #:loopback-hosts
   #:loopback-network-datagrams))
