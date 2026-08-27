(defpackage #:luvcraft.agent.tests
  (:use #:cl)
  (:import-from #:parachute #:define-test #:true #:false #:fail #:group #:skip)
  (:local-nicknames (#:agent #:luvcraft.agent)
                    (#:luvcraft #:luvcraft)
                    (#:world #:luvcraft.world)))

(in-package #:luvcraft.agent.tests)
