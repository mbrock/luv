(in-package #:luft.web)

(defun demo-style ()
  "html,body{margin:0;width:100%;height:100%;overflow:hidden;background:#b9d5db;color:#243934;font:14px system-ui,sans-serif}*{box-sizing:border-box}#world{display:block;width:100%;height:100%;touch-action:none}header{position:absolute;top:28px;left:32px;pointer-events:none}h1{font:500 52px Georgia,serif;letter-spacing:-3px;margin:0}header p{margin:5px 0;color:#465a53;font-size:12px;letter-spacing:.12em;text-transform:uppercase}.playing aside,.playing header p,.playing nav{display:none}.panel{background:#fbf9f0e8;border:1px solid #fff9;box-shadow:0 8px 32px #24393412;backdrop-filter:blur(12px);border-radius:12px}nav{position:absolute;right:28px;top:28px;padding:6px;display:flex;gap:4px}button{font:inherit;color:inherit;background:transparent;border:1px solid transparent;border-radius:7px;padding:10px 14px;cursor:pointer}button:hover{background:#e7ecdf}button:focus-visible,a:focus-visible,input:focus-visible{outline:3px solid #427e72;outline-offset:3px}#walk,button[aria-pressed=true]{background:#294e43;color:#fff8df}aside{position:absolute;bottom:95px;left:28px;max-width:295px;padding:18px 20px}aside h2{font:500 21px Georgia,serif;margin:0 0 8px}aside p{margin:0;line-height:1.55;color:#536158;font-size:13px}aside details{margin-top:12px;font-size:12px}aside summary{cursor:pointer}aside label{display:block;margin-top:12px}aside a{color:#294e43}footer{position:absolute;left:28px;right:28px;bottom:20px;display:flex;align-items:center;justify-content:space-between;gap:12px}#status{font-size:12px;margin:0;max-width:460px;line-height:1.5}#metrics{font:11px ui-monospace,monospace;color:#536158;margin-top:4px}#materials{display:flex;gap:3px;padding:5px}#materials button{padding:8px 10px;display:flex;align-items:center;gap:7px;font-size:12px}i{display:inline-block;width:16px;height:16px;border-radius:4px;border:1px solid #0002}.earth{background:#708447}.stone{background:#bbb295}.wood{background:#81522f}.crystal{background:#69cae6}.leaves{background:#44723c}#crosshair{position:absolute;top:50%;left:50%;transform:translate(-50%,-50%);font:24px monospace;color:white;text-shadow:0 1px 4px #0008;pointer-events:none}#crosshair[hidden]{display:none}@media(max-width:760px){header{top:20px;left:20px}h1{font-size:40px}header p{font-size:9px}nav{right:14px;top:20px}nav button{padding:9px;font-size:12px}aside{left:14px;bottom:126px;max-width:235px;padding:13px}aside h2{font-size:18px}aside p{font-size:12px}footer{left:14px;right:14px;bottom:14px;flex-direction:column;align-items:flex-start;gap:8px}#materials button{padding:8px}#materials .name{display:none}#status{max-width:94vw}#metrics{font-size:10px}}@media(pointer:coarse){#walk{display:none}}")

(defun demo-html ()
  (spinneret:with-html-string
    (:doctype)
    (:html :lang "en"
      (:head
        (:meta :charset "utf-8")
        (:meta :name "viewport" :content "width=device-width,initial-scale=1")
        (:meta :name "description" :content "Explore and reshape a little Luft highland. The native game's beveled star atlas, rendered in your browser.")
        (:title "Luft — a little world")
        (:link :rel "icon" :href "data:,")
        (:link :rel "stylesheet" :href "luft-demo.css")
        (:script :type "importmap"
          (:raw "{\"imports\":{\"three\":\"https://cdn.jsdelivr.net/npm/three@0.180.0/build/three.module.js\",\"three/addons/\":\"https://cdn.jsdelivr.net/npm/three@0.180.0/examples/jsm/\"}}")))
      (:body
        (:canvas#world :aria-label "Interactive beveled voxel highland. Drag to orbit and scroll to zoom. Walk here enables keyboard movement and block editing." :tabindex "0")
        (:header (:h1 "luft") (:p "A little world, shaped by stars"))
        (:nav.panel :aria-label "View controls"
          (:button#walk :type "button" "Walk here")
          (:button#home :type "button" "Overview")
          (:button#wire :type "button" :aria-pressed "false" "Wireframe"))
        (:aside.panel
          (:h2 "A highland in miniature.")
          (:p "Wander through the arches. Add a block, carve a notch, and watch the edges meet. These are the same beveled stars as native Luft.")
          (:details
            (:summary "Controls & rendering")
            (:p "Orbit: drag, scroll, or pinch. Walk: WASD or arrows, Space to jump, Shift to run. Click to remove; right-click to place. Keys 1–5 select a material. Escape releases the mouse. Edits last until you reset or reload.")
            (:p "Width-one native atlas: 256 occupancy patterns, instanced in Three.js. Standard lighting, shadows, bloom, and tone mapping.")
            (:label (:input#bloom :type "checkbox" :checked t) " Soft bloom")
            (:button#reset :type "button" "Reset the world")
            (:p (:a :href "https://mbrock.github.io/luv/luft-star-atlas.html" "Explore the atlas"))))
        (:div#crosshair :hidden t :aria-hidden "true" "+")
        (:footer
          (:div (:p#status :role "status" :aria-live "polite" "Opening the highland…")
                (:div#metrics))
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
