(in-package #:luft.web)

(defun demo-style ()
  "html,body{margin:0;width:100%;height:100%;overflow:hidden;background:#b9d5db;color:#fff8e7;font:14px system-ui,sans-serif;overscroll-behavior:none}
*{box-sizing:border-box}#world{display:block;width:100%;height:100%;touch-action:none}
button{font:inherit;color:inherit;cursor:pointer;border:1px solid transparent;border-radius:8px;background:transparent;touch-action:none;user-select:none;-webkit-user-select:none}
button:focus-visible{outline:2px solid #fff8df;outline-offset:3px}
.panel,#metrics{background:#20332cba;border:1px solid #ffffff30;box-shadow:0 3px 16px #0002;backdrop-filter:blur(10px);border-radius:12px}
#metrics{position:absolute;top:max(16px,env(safe-area-inset-top));left:max(16px,env(safe-area-inset-left));padding:7px 10px;font:12px ui-monospace,monospace;font-variant-numeric:tabular-nums;pointer-events:none}
#status{position:absolute;top:58px;left:16px;right:16px;margin:0;text-align:center;text-shadow:0 1px 4px #000;background:#20332cba;border-radius:8px;padding:10px}#status:empty{display:none}
footer{position:absolute;bottom:max(18px,env(safe-area-inset-bottom));left:50%;transform:translateX(-50%)}
#materials{display:flex;gap:4px;padding:5px}#materials button{min-width:44px;min-height:44px;padding:8px 10px;display:flex;align-items:center;justify-content:center;gap:8px;font-size:12px}
button[aria-pressed=true]{background:#fff1cb24;border-color:#fff1cbcc;box-shadow:inset 0 0 0 1px #fff1cb40}
@media(hover:hover){button:hover{background:#ffffff20}}
i{display:inline-block;width:22px;height:22px;border-radius:5px;border:1px solid #fff5;box-shadow:inset 0 -4px 0 #0002}.earth{background:#708447}.stone{background:#bbb295}.wood{background:#81522f}.crystal{background:#69cae6}.leaves{background:#44723c}
#crosshair{position:absolute;top:50%;left:50%;transform:translate(-50%,-50%);font:22px monospace;color:#fff9;text-shadow:0 1px 3px #0008;pointer-events:none}
#touch-controls{display:none}#touch-controls button{width:48px;height:48px;border-radius:50%;background:#18281e30;border:1px solid #ffffff80;color:#ffffffe0;font-size:23px;text-shadow:0 1px 3px #0009;box-shadow:0 1px 3px #0002}#touch-controls button.held{background:#ffffff55;border-color:white;color:white;box-shadow:0 0 0 3px #ffffff30}
#touch-controls #movement{position:absolute;left:max(24px,env(safe-area-inset-left));bottom:calc(92px + env(safe-area-inset-bottom));width:96px;height:96px;border:1px solid #ffffff65;background:#18281e18;touch-action:none}
#stick{display:block;position:absolute;left:27px;top:27px;width:40px;height:40px;border-radius:50%;background:#ffffff22;border:1px solid #ffffff65;pointer-events:none;transform:translate(var(--stick-x,0px),var(--stick-y,0px))}
#actions{position:absolute;right:max(24px,env(safe-area-inset-right));bottom:calc(92px + env(safe-area-inset-bottom));display:grid;grid-template-columns:repeat(2,48px);gap:12px}#actions button:first-child{grid-column:2}#actions button:nth-child(2){grid-column:1;grid-row:2}
#crosshair.editing{color:white;transform:translate(-50%,-50%) scale(1.25)}#crosshair.editing:after{content:'';position:absolute;inset:-8px;border:1px solid #fff8;border-radius:50%}
@media(max-width:760px),(any-pointer:coarse){#materials .name{display:none}}
@media(any-pointer:coarse){#touch-controls{display:block}#materials{background:#20332c66;box-shadow:none;backdrop-filter:none;padding:3px;gap:1px}#materials i{width:18px;height:18px}#metrics{background:#20332c66;box-shadow:none;backdrop-filter:none}}")

(defun demo-html ()
  (spinneret:with-html-string
    (:doctype)
    (:html :lang "en"
      (:head
        (:meta :charset "utf-8")
        (:meta :name "viewport" :content "width=device-width,initial-scale=1,viewport-fit=cover")
        (:meta :name "description" :content "Explore and reshape a little Luft highland. The native game's beveled star atlas, rendered in your browser.")
        (:title "Luft — a little world")
        (:link :rel "icon" :href "data:,")
        (:link :rel "stylesheet" :href "luft-demo.css")
        (:script :type "importmap"
          (:raw "{\"imports\":{\"three\":\"https://cdn.jsdelivr.net/npm/three@0.180.0/build/three.module.js\",\"three/addons/\":\"https://cdn.jsdelivr.net/npm/three@0.180.0/examples/jsm/\"}}")))
      (:body
        (:canvas#world :aria-label "Walk with WASD or arrows, Space to jump. Click to capture the mouse, then click to remove or right-click to place. Escape releases the mouse. On touchscreens, use the movement pad and drag the world to look." :tabindex "0")
        (:div#crosshair :aria-hidden "true" "+")
        (:p#status :role "status" :aria-live "polite" "Opening the highland…")
        (:div#metrics :aria-label "Frames per second" "— fps")
        (:div#touch-controls :role "group" :aria-label "Touch game controls"
          (:button#movement :type "button" :aria-label "Movement thumbstick: drag to walk, push fully to run"
            (:span#stick :aria-hidden "true"))
          (:div#actions
            (:button :type "button" :data-key "Space" :aria-label "Jump" "⇧")
            (:button#remove :type "button" :aria-label "Smash block; hold to repeat" :data-place "false" "⛏︎")
            (:button#place :type "button" :aria-label "Build block; hold to repeat" :data-place "true" "◇")))
        (:footer
          (:div#materials.panel :role "group" :aria-label "Block material"
            (loop for (kind name class) in '((1 "Earth" "earth") (2 "Stone" "stone")
                                             (3 "Wood" "wood") (4 "Crystal" "crystal")
                                             (5 "Leaves" "leaves"))
                  do (spinneret:with-html
                       (:button :type "button" :data-kind kind
                                :aria-label (format nil "~D: ~A" kind name)
                                :aria-pressed (if (= kind 2) "true" "false")
                         (:i :class class :aria-hidden "true")
                         (:span.name (format nil "~D ~A" kind name)))))))
        (:script :type "module" :src "luft-demo.js")))))

(defun demo-resources (site)
  (declare (ignore site))
  (list
   (luv.wiki:make-generated-resource
    "/luft-demo.html" "luft-demo.html" "text/html; charset=utf-8" 'demo-html
    :label "Play Luft" :description "An editable highland in the browser" :kind "demo")
   (luv.wiki:make-generated-resource
    "/luft-demo.js" "luft-demo.js" "text/javascript; charset=utf-8" 'demo-javascript)
   (luv.wiki:make-generated-resource
    "/luft-demo.css" "luft-demo.css" "text/css; charset=utf-8" 'demo-style)))

(luv.wiki:register-resource-provider 'luft-demo #'demo-resources)

(defun publish-demo (directory)
  "Write the standalone demo through the same resources as the live wiki."
  (dolist (resource (demo-resources nil))
    (luv.wiki::publish-resource resource (uiop:ensure-directory-pathname directory)))
  directory)

(defvar *demo-server* nil)

(defun serve-demo (&key (port 8777) (host "127.0.0.1"))
  "Start a background Clack/Woo server for the demo in the durable image."
  (when *demo-server* (clack:stop *demo-server*))
  (let ((site (luv.wiki:make-site nil)))
    (setf *demo-server*
          (clack:clackup (luv.wiki::wiki-clack-application site)
                         :server :woo :address host :port port :debug nil
                         :use-default-middlewares nil)))
  (format nil "http://~A:~D/luft-demo.html" host port))
