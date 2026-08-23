;;; Application-neutral language-agent machinery.
;;;
;;; The provider connection is OPENAI:AGENT, commands and presentations are
;;; McCLIM's, and the owning program supplies only its frame-boundary dispatch
;;; and command frame.  Nothing in this package knows about a Luvcraft world,
;;; a LUFT renderer, or an embodied character.

(defpackage #:luv.application-agent
  (:use #:clim-lisp #:clim)
  (:local-nicknames (#:climi #:clim-internals)
                    (#:openai #:openai))
  (:export
   ;; Views and durable semantic handles.
   #:model-view #:trace-view #:cassette-view
   #:+model-view+ #:+trace-view+ #:+cassette-view+
   #:model-text
   #:handle #:handle-table #:make-handle-table #:intern-handle
   #:handle-object #:release-handle-table #:handle-table-released
   #:*handles*
   ;; Presentation types as strict JSON arguments.
   #:presentation-type-json-schema
   #:presentation-type-specifier-json-schema
   #:command-argument-specification
   #:command-argument-name #:command-argument-json-name
   #:command-argument-type #:command-argument-options
   #:command-argument-required-p #:command-argument-keyword-p
   #:command-argument-specifications
   #:command-tool-parameters #:accept-command-argument
   #:tool-argument-error #:tool-argument-error-name
   #:tool-argument-error-detail
   ;; CLIM commands as provider tools.
   #:command-tool #:make-command-tool #:command-tool-command
   #:command-tool-argument-specifications #:command-tool-parse
   #:command-tool-runs-on-canvas-p
   #:command-result-presentation-type #:command-output-line-limit
   #:command-provider-output #:settle-command-result
   #:application-command-frame #:call-in-application-frame
   #:call-with-agent-command-context
   ;; Transcript records.
   #:tool-call #:tool-call-name #:tool-call-command #:tool-call-tool
   #:tool-call-arguments
   #:tool-call-status #:tool-call-result #:tool-call-error #:tool-call-output
   #:tool-call-elapsed-seconds #:tool-call-arguments-text
   #:turn #:turn-prompt #:turn-thought #:turn-text #:turn-calls #:turn-status
   #:turn-error #:turn-response #:turn-started #:turn-finished #:turn-thread
   #:turn-calls-in-order #:wait-for-turn #:transcript-lines #:print-transcript
   ;; The reusable asynchronous application harness.
   #:application-agent #:make-application-agent
   #:application-agent-application #:application-agent-turns
   #:application-agent-current-turn #:application-agent-handles
   #:application-agent-state #:application-agent-open-p
   #:application-agent-worker-thread
   #:application-agent-close-thread
   #:application-agent-close-finished-p #:application-agent-close-error
   #:application-agent-release-finished-p
   #:application-agent-observer-failures
   #:application-agent-released
   #:agent-observer-failure #:agent-observer-failure-observer
   #:agent-observer-failure-kind #:agent-observer-failure-object
   #:agent-observer-failure-cause
   #:add-agent-observer #:remove-agent-observer #:notify-agent-observers
   #:note-tool-call #:note-tool-call-finished
   #:run-turn #:ask #:ask-and-wait #:release-application-agent
   #:wait-for-application-agent-release
   #:*current-agent*))
