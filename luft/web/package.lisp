(defpackage #:luft.web
  (:use #:cl #:parenscript)
  (:local-nicknames (#:ps #:parenscript)
                    (#:browser #:luv.wiki.browser))
  (:export #:demo-javascript #:demo-resources #:publish-demo #:serve-demo))
