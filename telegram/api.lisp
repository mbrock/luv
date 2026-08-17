;;;; The Telegram API schema.
;;;;
;;;; One form, and it produces data rather than definitions: the whole of
;;;; api.tl as a table of TL-DEFINITIONs, keyed by constructor id and by
;;;; keyword.  Nothing here is a class, because nothing here is a vocabulary
;;;; we extend -- Telegram owns every name and number in it.
;;;;
;;;; Names follow one rule: camelCase becomes kebab-case, underscores become
;;;; hyphens, and the namespace keeps its dot.  So messages.sendMessage is
;;;; :MESSAGES.SEND-MESSAGE and its no_webpage field is :NO-WEBPAGE.
;;;;
;;;;   (tl:make-tl :messages.send-message
;;;;               :peer (tl:make-tl :input-peer-self)
;;;;               :message "hello" :random-id 1)
;;;;
;;;;   (tl:describe-tl :messages.send-message)
;;;;   (tl:find-tl-definitions "sendMessage" :functions t)

(in-package #:telegram)

(defparameter +api-schema-size+
  (tl:define-tl-schema "schema/api.tl")
  "How many constructors and functions the loaded schema holds.")

(defconstant +api-layer+ 214
  "The schema layer this snapshot is.  Every session announces it through
invokeWithLayer, and the server answers in the dialect of the layer it was
told, so this constant and schema/api.tl move together.")
