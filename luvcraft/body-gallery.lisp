;;;; The browser edge for luvcraft's analytic agent bodies.
;;;;
;;;; A WEB-BODY-TYPE is the small piece of application knowledge that the
;;;; browser needs around the shader graph: which shader role names the body,
;;;; which knob group belongs to it, and the conservative sphere that its
;;;; billboard rasterizes.  Everything else -- WGSL and knob metadata -- is
;;;; derived from the same Lisp objects that the native game uses.

(in-package #:luvcraft.body-gallery)

(defclass web-body-type ()
  ((id :initarg :id :reader web-body-id)
   (label :initarg :label :reader web-body-label)
   (role :initarg :role :reader web-body-role)
   (knob-group :initarg :knob-group :reader web-body-knob-group)
   (center-height :initarg :center-height :reader web-body-center-height)
   (radius :initarg :radius :reader web-body-radius))
  (:documentation
   "One native analytic body as a browser-facing shader and framing preset."))

(defparameter *web-body-types*
  (list (make-instance 'web-body-type
                       :id "gnome" :label "Gnome" :role :gnome-sdf
                       ;; Covers GNOME-FIGURE-RADIUS throughout every
                       ;; published knob range, including the tallest leaned hat.
                       :knob-group :gnome :center-height 0.85 :radius 1.90)
        (make-instance 'web-body-type
                       :id "cat" :label "Cat" :role :cat-sdf
                       ;; Covers CAT-FIGURE-RADIUS throughout its knob ranges.
                       :knob-group :cat :center-height 0.64 :radius 1.16)))

(defun web-body-types ()
  "The ordered body catalog shown by the web gallery."
  (copy-list *web-body-types*))

(defclass body-gallery-bundle ()
  ((body :initarg :body :reader body-gallery-bundle-body)
   (vertex :initarg :vertex :reader body-gallery-bundle-vertex)
   (fragment :initarg :fragment :reader body-gallery-bundle-fragment)))

(defun compile-body-gallery ()
  "Compile every catalog body from the shared shader graph to WGSL."
  (loop for body in (web-body-types)
        for knobs = (luvcraft:knobs-in-group (web-body-knob-group body))
        for overrides = (mapcar #'luvcraft:knob-name knobs)
        collect
        (make-instance
         'body-gallery-bundle
         :body body
         :vertex (wgsl:compile-wgsl
                  (shader:shader-specification-for (web-body-role body) :vertex)
                  :overrides overrides)
         :fragment (wgsl:compile-wgsl
                    (shader:shader-specification-for (web-body-role body) :fragment)
                    :overrides overrides))))

(defun web-name (name)
  (string-downcase (string name)))

(defun override-for-knob (knob bundle)
  (let ((name (luvcraft:knob-name knob)))
    (or (find name (wgsl:wgsl-document-overrides
                    (body-gallery-bundle-vertex bundle))
              :key #'wgsl:wgsl-override-name)
        (find name (wgsl:wgsl-document-overrides
                    (body-gallery-bundle-fragment bundle))
              :key #'wgsl:wgsl-override-name))))

(defun knob-stages (knob bundle)
  (let ((name (luvcraft:knob-name knob)))
    (loop for (stage document)
            in `(("vertex" ,(body-gallery-bundle-vertex bundle))
                 ("fragment" ,(body-gallery-bundle-fragment bundle)))
          when (find name (wgsl:wgsl-document-overrides document)
                     :key #'wgsl:wgsl-override-name)
            collect stage)))

(defun knob-json-object (knob bundle)
  (let ((override (override-for-knob knob bundle)))
    (unless override
      (error "Body knob ~S did not occur in either compiled shader."
             (luvcraft:knob-name knob)))
    `(("name" . ,(web-name (luvcraft:knob-name knob)))
      ("identifier" . ,(wgsl:wgsl-override-identifier override))
      ("label" . ,(luvcraft:knob-label knob))
      ("documentation" . ,(or (luvcraft:knob-documentation knob) ""))
      ("default" . ,(luvcraft:knob-value knob nil))
      ("minimum" . ,(luvcraft:knob-minimum knob))
      ("maximum" . ,(luvcraft:knob-maximum knob))
      ("step" . ,(luvcraft:knob-step knob))
      ("unit" . ,(luvcraft:knob-unit-label knob))
      ("stages" . ,(knob-stages knob bundle)))))

(defun bundle-json-object (bundle)
  (let* ((body (body-gallery-bundle-body bundle))
         (id (web-body-id body))
         (knobs (luvcraft:knobs-in-group (web-body-knob-group body))))
    `(("id" . ,id)
      ("label" . ,(web-body-label body))
      ("centerHeight" . ,(web-body-center-height body))
      ("radius" . ,(web-body-radius body))
      ("statureKnob" . ,(format nil "~A-stature" id))
      ("vertexUrl" . ,(format nil "/body/~A/vertex.wgsl" id))
      ("fragmentUrl" . ,(format nil "/body/~A/fragment.wgsl" id))
      ("knobs" . ,(mapcar
                    (lambda (knob) (knob-json-object knob bundle))
                    knobs)))))

(defun body-gallery-json (&optional (bundles (compile-body-gallery)))
  "Encode the catalog and actual native knob metadata for JavaScript."
  (cl-json:encode-json-alist-to-string
   `(("bodies" . ,(mapcar #'bundle-json-object bundles)))))

(defun gallery-asset (name)
  (uiop:read-file-string
   (asdf:system-relative-pathname
    "luvcraft/body-gallery" (format nil "luvcraft/web/~A" name))))

(defun response-octet-length (string)
  (length (sb-ext:string-to-octets string :external-format :utf-8)))

(defun write-http-response (stream status content-type body)
  (format stream "HTTP/1.1 ~A~C~C" status #\Return #\Linefeed)
  (format stream "Content-Type: ~A~C~C" content-type #\Return #\Linefeed)
  (format stream "Content-Length: ~D~C~C"
          (response-octet-length body) #\Return #\Linefeed)
  (format stream "Cache-Control: no-store~C~C" #\Return #\Linefeed)
  (format stream "Connection: close~C~C~C~C" #\Return #\Linefeed
          #\Return #\Linefeed)
  (write-string body stream)
  (finish-output stream))

(defun bounded-read-line (stream limit)
  (let ((line (read-line stream nil nil)))
    (when (and line (> (length line) limit))
      (error "HTTP line exceeds ~D characters." limit))
    line))

(defun discard-http-headers (stream)
  (loop repeat 64
        for line = (bounded-read-line stream 8192)
        until (or (null line) (string= line "") (string= line (string #\Return)))
        finally (unless (or (null line) (string= line "")
                            (string= line (string #\Return)))
                  (error "Too many HTTP headers."))))

(defun request-path (request-line)
  (let ((first-space (and request-line (position #\Space request-line)))
        (second-space nil))
    (when first-space
      (setf second-space (position #\Space request-line :start (1+ first-space))))
    (unless (and first-space second-space
                 (string= "GET" request-line :end2 first-space))
      (error "Only a well-formed GET request is supported."))
    (subseq request-line (1+ first-space) second-space)))

(defun find-bundle (id bundles)
  (find id bundles :test #'string=
        :key (lambda (bundle)
               (web-body-id (body-gallery-bundle-body bundle)))))

(defun body-shader-response (path bundles)
  (let* ((parts (uiop:split-string path :separator '(#\/)))
         (id (third parts))
         (file (fourth parts))
         (bundle (and (= 4 (length parts)) (find-bundle id bundles))))
    (when bundle
      (cond ((string= file "vertex.wgsl")
             (wgsl:wgsl-document-source
              (body-gallery-bundle-vertex bundle)))
            ((string= file "fragment.wgsl")
             (wgsl:wgsl-document-source
              (body-gallery-bundle-fragment bundle)))))))

(defun serve-gallery-request (client bundles catalog-json)
  (let ((stream (sb-bsd-sockets:socket-make-stream
                 client :input t :output t :element-type 'character
                 :buffering :full :external-format :utf-8)))
    (unwind-protect
         (handler-case
             (let* ((line (bounded-read-line stream 8192))
                    (path (request-path line)))
               (discard-http-headers stream)
               (format t "GET ~A~%" path)
               (cond ((or (string= path "/") (string= path "/index.html"))
                      (write-http-response stream "200 OK" "text/html; charset=utf-8"
                                           (gallery-asset "body-gallery.html")))
                     ((string= path "/gallery.js")
                      (write-http-response stream "200 OK" "text/javascript; charset=utf-8"
                                           (gallery-asset "body-gallery.js")))
                     ((string= path "/gallery.css")
                      (write-http-response stream "200 OK" "text/css; charset=utf-8"
                                           (gallery-asset "body-gallery.css")))
                     ((string= path "/bodies.json")
                      (write-http-response stream "200 OK" "application/json; charset=utf-8"
                                           catalog-json))
                     ((string= path "/healthz")
                      (write-http-response stream "200 OK" "text/plain; charset=utf-8"
                                           (format nil "ok~%")))
                     (t
                      (let ((source (body-shader-response path bundles)))
                        (if source
                            (write-http-response stream "200 OK"
                                                 "text/wgsl; charset=utf-8" source)
                            (write-http-response stream "404 Not Found"
                                                 "text/plain; charset=utf-8"
                                                 (format nil "not found~%")))))))
           (error (condition)
             (format *error-output* "body gallery request failed: ~A~%" condition)
             (ignore-errors
              (write-http-response stream "400 Bad Request"
                                   "text/plain; charset=utf-8"
                                   (format nil "bad request~%")))))
      (ignore-errors (close stream))
      (ignore-errors (sb-bsd-sockets:socket-close client)))))

(defun ipv4-address (host)
  (sb-bsd-sockets:host-ent-address
   (sb-bsd-sockets:get-host-by-name host)))

(defun serve-body-gallery (&key (host "127.0.0.1") (port 8765))
  "Compile the body catalog once, then serve its WebGPU gallery until stopped."
  (let* ((bundles (compile-body-gallery))
         (catalog-json (body-gallery-json bundles))
         (socket (make-instance 'sb-bsd-sockets:inet-socket
                                :type :stream :protocol :tcp)))
    (unwind-protect
         (progn
           (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
           (sb-bsd-sockets:socket-bind socket (ipv4-address host) port)
           (sb-bsd-sockets:socket-listen socket 16)
           (format t "luvcraft body gallery: http://~A:~D/~%" host port)
           (finish-output)
           (handler-case
               (loop
                 for client = (sb-bsd-sockets:socket-accept socket)
                 do (serve-gallery-request client bundles catalog-json))
             (sb-sys:interactive-interrupt ()
               (format t "Stopping luvcraft body gallery.~%"))))
      (ignore-errors (sb-bsd-sockets:socket-close socket)))))
