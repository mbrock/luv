;;; An agent in the little world.
;;;
;;; This package is where a language model meets luvcraft, and it meets it
;;; through CLIM: a tool is a command whose arguments are presentation types,
;;; a tool result is an object presented under a view the model reads, and
;;; the running transcript is a list of typed records that the HUD, a wall,
;;; and a plain ./sly session all draw from.  The design is the wiki page
;;; "An agent in the little world" (#4Y8Q8B); the provider behind it is the
;;; OpenAI websocket client, but nothing here is about OpenAI.

(defpackage #:luvcraft.agent
  (:use #:clim-lisp #:clim)
  (:local-nicknames (#:luv #:luv)
                    (#:climi #:clim-internals)
                    (#:mcluv #:mcluv)
                    (#:luvcraft #:luvcraft)
                    (#:luvcraft.clim #:luvcraft.clim)
                    (#:world #:luvcraft.world)
                    (#:openai #:openai))
  ;; Preserve Luvcraft's public vocabulary while making these names the exact
  ;; shared objects.  Remaining game files specialize and consume the common
  ;; protocols without wrappers or a duplicate compatibility layer.
  (:shadowing-import-from
   #:luv.application-agent
   #:model-view #:trace-view #:cassette-view
   #:+model-view+ #:+trace-view+ #:+cassette-view+
   #:model-text
   #:handle #:handle-table #:make-handle-table #:intern-handle
   #:handle-object #:*handles*
   #:presentation-type-json-schema
   #:command-tool #:make-command-tool #:command-tool-command
   #:command-tool-runs-on-canvas-p
   #:command-provider-output
   #:command-result-presentation-type #:command-output-line-limit
   #:settle-command-result
   #:tool-call #:tool-call-name #:tool-call-command #:tool-call-tool
   #:tool-call-arguments
   #:tool-call-status #:tool-call-result #:tool-call-error #:tool-call-output
   #:tool-call-elapsed-seconds #:tool-call-arguments-text
   #:turn #:turn-prompt #:turn-thought #:turn-text #:turn-calls #:turn-status
   #:turn-error #:turn-response #:turn-started #:turn-finished #:turn-thread
   #:turn-calls-in-order #:transcript-lines #:print-transcript
   #:add-agent-observer #:remove-agent-observer #:notify-agent-observers
   #:note-tool-call #:note-tool-call-finished
   #:release-application-agent #:*current-agent*)
  (:export
   ;; Views and handles
   #:model-view #:trace-view #:cassette-view
   #:+model-view+ #:+trace-view+ #:+cassette-view+
   #:model-text #:handle #:handle-table #:make-handle-table #:intern-handle
   #:handle-object #:*handles*
   #:presentation-type-json-schema
   ;; Tools
   #:command-tool #:make-command-tool #:command-tool-command
   #:command-tool-runs-on-canvas-p #:command-provider-output
   #:tool-approval #:tool-approval-state #:tool-approval-note
   #:approve-tool-approval #:deny-tool-approval #:steer-tool-approval
   ;; The transcript
   #:tool-call #:tool-call-command #:tool-call-tool #:tool-call-arguments
   #:tool-call-status #:tool-call-result #:tool-call-error #:tool-call-output
   #:tool-call-elapsed-seconds
   #:turn #:turn-prompt #:turn-thought #:turn-text #:turn-calls #:turn-status
   #:world-agent #:make-world-agent #:world-agent-session #:world-agent-turns
   #:world-agent-current-turn #:world-agent-handles
   #:ask #:ask-and-wait #:transcript-lines #:print-transcript
   #:release-application-agent
   #:*agent*
   ;; Commands
   #:luvcraft-agent #:com-where-am-i #:com-view-surroundings
   #:com-move-to #:com-place-block-at #:com-eval
   #:com-propose-block-box
   #:com-describe-handle #:com-block-at
   ;; The HUD
   #:open-agent-hud #:close-agent-hud #:toggle-agent-hud
   ;; The wall
   #:agent-terminal-display #:open-agent-wall #:com-open-agent-wall
   #:ansi-view #:+ansi-view+
   ;; Embodied agents
   #:embodied-agent #:gnome #:cat
   #:embodied-agent-pending-approval
   #:*agents* #:agents-in-session #:find-agent #:spawn-agent
   #:find-cat #:spawn-cat
   #:gnome-say #:gnome-agent #:com-say
   #:com-spawn-agent #:com-spawn-cat
   #:*current-agent* #:world-agent-presence))
