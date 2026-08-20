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
   ;; The transcript
   #:tool-call #:tool-call-command #:tool-call-tool #:tool-call-arguments
   #:tool-call-status #:tool-call-result #:tool-call-error #:tool-call-output
   #:tool-call-elapsed-seconds
   #:turn #:turn-prompt #:turn-thought #:turn-text #:turn-calls #:turn-status
   #:world-agent #:make-world-agent #:world-agent-session #:world-agent-turns
   #:world-agent-current-turn #:world-agent-handles
   #:ask #:ask-and-wait #:transcript-lines #:print-transcript
   #:*agent*
   ;; Commands
   #:luvcraft-agent #:com-where-am-i #:com-view-surroundings
   #:com-move-to #:com-place-block-at #:com-eval
   #:com-describe-handle #:com-block-at
   ;; The HUD
   #:open-agent-hud #:close-agent-hud #:toggle-agent-hud
   ;; The wall
   #:agent-terminal-display #:open-agent-wall #:com-open-agent-wall
   #:ansi-view #:+ansi-view+
   ;; Embodied agents
   #:embodied-agent #:gnome #:cat
   #:*agents* #:agents-in-session #:find-agent #:spawn-agent
   #:find-cat #:spawn-cat
   #:gnome-say #:gnome-agent #:com-say
   #:com-spawn-agent #:com-spawn-cat
   #:*current-agent* #:world-agent-presence))
