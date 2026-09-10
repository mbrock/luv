(in-package #:luft.web)

(defun client-form ()
  `(progn
    (defvar renderer) (defvar scene) (defvar camera)
    (defvar composer) (defvar bloom) (defvar occlusion) (defvar material) (defvar world)
    (defvar outline) (defvar sky) (defvar sun) (defvar meshes (array))
    (defvar keys (new (|Set|)))
    (defvar yaw 0) (defvar pitch 0)
    (defvar velocity 0) (defvar grounded false) (defvar selected 2)
    (defvar last-time 0) (defvar target null) (defvar frame-count 0)
    (defvar coarse-pointer (@ ((@ window match-media) "(pointer: coarse)") matches))
    (defvar stats-time 0)
    (defvar touch-keys (new (|Map|)))
    (defvar stick-pointer null) (defvar stick-x 0) (defvar stick-y 0)
    (defvar edit-pointer null) (defvar edit-place false) (defvar edit-next 0)
    (defvar look-pointer null) (defvar look-x 0) (defvar look-y 0)
    (defvar |THREE|) (defvar status (chain document (get-element-by-id "status")))
    (defvar canvas (chain document (get-element-by-id "world")))
    (defun element (id) (chain document (get-element-by-id id)))
    (defun set-status (message) (setf (@ status text-content) message))
    (defun vector (x y z) (new ((@ |THREE| |Vector3|) x y z)))
    (defun remove-mesh (mesh)
      (when mesh
        ((@ world remove) mesh)
        ((@ mesh geometry dispose))
        (setf meshes ((@ meshes filter) (lambda (other) (not (= other mesh))))
              (@ sun shadow needs-update) true)))
    (defun add-mesh (data)
      (let ((geometry (new ((@ |THREE| |BufferGeometry|)))))
        ((@ geometry set-attribute) "position"
         (new ((@ |THREE| |BufferAttribute|) (@ data positions) 3)))
        ((@ geometry set-attribute) "normal"
         (new ((@ |THREE| |BufferAttribute|) (@ data normals) 3 true)))
        ((@ geometry set-attribute) "color"
         (new ((@ |THREE| |BufferAttribute|) (@ data colors) 3 true)))
        ((@ geometry compute-bounding-sphere))
        (let ((mesh (new ((@ |THREE| |Mesh|) geometry material))))
          (setf (@ mesh cast-shadow) true (@ mesh receive-shadow) true)
          ((@ world add) mesh)
          ((@ meshes push) mesh)
          (setf (@ sun shadow needs-update) true)
          mesh)))
    (defun realize-chunk (chunk)
      (let ((mesh (add-mesh (@ chunk product))))
        (remove-mesh (@ chunk mesh))
        (setf (@ chunk mesh) mesh)
        (delete (@ chunk product))))
    (defun rebuild ()
      ;; Explicit finite-fixture mode used by GPU regressions and inspection.
      ;; Production edits only remesh the touched resident chunks.
      (stop-streaming)
      ((@ ((@ meshes slice)) for-each) remove-mesh)
      (add-mesh (mesh-data ((@ |Array| from) ((@ (surface-sites) values))) cell-at)))
    (defun look ()
      ((@ camera look-at)
       (+ (@ camera position x) (* (sin yaw) (cos pitch)))
       (+ (@ camera position y) (* (cos yaw) (cos pitch)))
       (+ (@ camera position z) (sin pitch))))
    (defun spawn-player ()
      ;; Start at the clear stone terrace without requiring mouse capture.
      (let ((height 10))
        (while (collides 23 20 height) (incf height))
        ((@ camera position set) 23 20 (+ height 1.62)))
      (setf yaw 0 pitch 0 velocity 0 grounded false)
      (look))
    (defun pressed (code)
      (or ((@ keys has) code)
          ((@ ((@ |Array| from) ((@ touch-keys values))) includes) code)))
    (defun turn-player (dx dy)
      (incf yaw (* dx 0.002))
      (setf pitch (max -1.5 (min 1.5 (- pitch (* dy 0.002)))))
      (look))
    (defun clear-input ()
      ((@ keys clear))
      ((@ touch-keys clear))
      (setf look-pointer null)
      (reset-stick)
      (stop-edit)
      ((@ (chain document (query-selector-all "button.held")) for-each)
       (lambda (button) ((@ button class-list remove) "held"))))
    (defun reset-stick ()
      (setf stick-pointer null stick-x 0 stick-y 0)
      ((@ (element "movement") style set-property) "--stick-x" "0px")
      ((@ (element "movement") style set-property) "--stick-y" "0px"))
    (defun update-stick (event)
      (let* ((pad (element "movement")) (rect ((@ pad get-bounding-client-rect)))
             (x (/ (- (@ event client-x) (@ rect left) (/ (@ rect width) 2)) 40))
             (y (/ (- (@ event client-y) (@ rect top) (/ (@ rect height) 2)) 40))
             (length ((@ |Math| hypot) x y))
             (scale (/ (max 0 (- (min 1 length) 0.15)) 0.85 (max length 0.001))))
        (setf stick-x (* x scale) stick-y (* y scale))
        ((@ pad style set-property) "--stick-x" (+ (* stick-x 34) "px"))
        ((@ pad style set-property) "--stick-y" (+ (* stick-y 34) "px"))))
    (defun stop-edit ()
      (setf edit-pointer null)
      ((@ (element "crosshair") class-list remove) "editing")
      (dolist (id (array "remove" "place"))
        ((@ (element id) class-list remove) "held")))
    (defun repeat-edit (time)
      (when (and (not (= edit-pointer null)) (>= time edit-next))
        (aim)
        ((@ (element "crosshair") class-list toggle) "editing" (edit-cell edit-place))
        (setf edit-next (+ time 280))))
    (defun begin-edit (event place)
      (when (= edit-pointer null)
        (setf edit-pointer (@ event pointer-id) edit-place place edit-next 0)
        (repeat-edit ((@ performance now)))))
    (defun capture-mouse ()
      (try
       (let ((request ((@ canvas request-pointer-lock))))
         (when (and request (@ request catch))
           ((@ request catch)
            (lambda () (set-status "Mouse capture unavailable. Drag to look around instead.")))))
       (:catch (error)
         (set-status "Mouse capture unavailable. Drag to look around instead."))))
    (defun fullscreen-supported ()
      (and (@ document fullscreen-enabled) (@ document document-element request-fullscreen)))
    (defun update-fullscreen-affordance ()
      (let ((button (element "fullscreen")))
        (when button
          (if (fullscreen-supported)
              (progn
                ((@ button remove-attribute) "disabled")
                (setf (@ button aria-label)
                      (if (@ document fullscreen-element) "Leave full screen" "Full screen")
                      (@ button title) "Full screen"))
              (progn
                (setf (@ button aria-label)
                      "Full screen is unavailable in this browser"
                      (@ button title) "Safari: use Share → Add to Home Screen"))))))
    (defun toggle-fullscreen ()
      (if (fullscreen-supported)
          (if (@ document fullscreen-element)
              ((@ document exit-fullscreen))
              (let ((request ((@ document document-element request-fullscreen))))
                (when (and request (@ request catch))
                  ((@ request catch)
                   (lambda () (set-status "Full screen was blocked by this browser."))))))
          ;; Capability detection, not an iOS version assumption. Installed
          ;; web apps can fill the display without the Fullscreen API.
          (set-status "Full screen is unavailable here. In Safari, Share → Add to Home Screen opens Luft without browser chrome."))
      (update-fullscreen-affordance))
    (defun suppress-gesture-zoom ()
      ;; Safari's proprietary gesture events cover pinch zoom; the touchmove
      ;; guard handles multi-touch on browsers that do not expose them.
      (dolist (name (array "gesturestart" "gesturechange" "gestureend"))
        ((@ document add-event-listener) name (lambda (event) ((@ event prevent-default)))
         (create :passive false)))
      ((@ document add-event-listener) "touchmove"
       (lambda (event)
         (when (> (@ event touches length) 1) ((@ event prevent-default))))
       (create :passive false))
      ((@ canvas add-event-listener) "dblclick" (lambda (event) ((@ event prevent-default)))))
    (defun bind-pointer-controls ()
      (let ((pad (element "movement")))
        ((@ pad add-event-listener) "pointerdown"
         (lambda (event)
           ((@ event prevent-default))
           (when (= stick-pointer null)
             (setf stick-pointer (@ event pointer-id))
             ((@ pad set-pointer-capture) stick-pointer)
             (update-stick event))))
        ((@ pad add-event-listener) "pointermove"
         (lambda (event)
           (when (= stick-pointer (@ event pointer-id)) (update-stick event))))
        (dolist (name (array "pointerup" "pointercancel" "lostpointercapture"))
          ((@ pad add-event-listener) name
           (lambda (event)
             (when (= stick-pointer (@ event pointer-id)) (reset-stick))))))
      (let ((drag-distance 0))
        ((@ canvas add-event-listener) "pointerdown"
         (lambda (event)
           ((@ canvas focus))
           (if (= (@ document pointer-lock-element) canvas)
               (when (or (= (@ event button) 0) (= (@ event button) 2))
                 (begin-edit event (= (@ event button) 2)))
               (when (and (= look-pointer null) (= (@ event button) 0))
                 (setf look-pointer (@ event pointer-id)
                       look-x (@ event client-x) look-y (@ event client-y)
                       drag-distance 0)
                 ((@ canvas set-pointer-capture) (@ event pointer-id))))))
        ((@ canvas add-event-listener) "pointermove"
         (lambda (event)
           (when (= look-pointer (@ event pointer-id))
             (let ((dx (- (@ event client-x) look-x))
                   (dy (- (@ event client-y) look-y)))
               (incf drag-distance (+ (abs dx) (abs dy)))
               (turn-player dx dy)
               (setf look-x (@ event client-x) look-y (@ event client-y))))))
        ((@ canvas add-event-listener) "pointerup"
         (lambda (event)
           (when (= look-pointer (@ event pointer-id))
             (setf look-pointer null)
             (when (and (= (@ event pointer-type) "mouse") (< drag-distance 5))
               (capture-mouse)))))
        (dolist (name (array "pointercancel" "lostpointercapture"))
          ((@ canvas add-event-listener) name
           (lambda (event)
             (when (= look-pointer (@ event pointer-id)) (setf look-pointer null))))))
      ((@ (chain document (query-selector-all "button[data-key]")) for-each)
       (lambda (button)
         ((@ button add-event-listener) "pointerdown"
          (lambda (event)
            ((@ event prevent-default))
            ((@ button set-pointer-capture) (@ event pointer-id))
            ((@ touch-keys set) (@ event pointer-id) (@ button dataset key))
            ((@ button class-list add) "held")))
         (dolist (name (array "pointerup" "pointercancel" "lostpointercapture"))
           ((@ button add-event-listener) name
            (lambda (event)
              ((@ touch-keys delete) (@ event pointer-id))
              ((@ button class-list toggle) "held" (pressed (@ button dataset key))))))))
      ((@ (chain document (query-selector-all "button[data-place]")) for-each)
       (lambda (button)
         ((@ button add-event-listener) "pointerdown"
          (lambda (event)
            ((@ event prevent-default))
            ((@ button set-pointer-capture) (@ event pointer-id))
            (begin-edit event (= (@ button dataset place) "true"))
            (when (= edit-pointer (@ event pointer-id))
              ((@ button class-list add) "held"))))
         ((@ button add-event-listener) "click"
          (lambda (event)
            ;; Keyboard/assistive activation only; pointerdown already edited.
            (when (= (@ event detail) 0)
              (aim) (edit-cell (= (@ button dataset place) "true")))))
         ((@ button add-event-listener) "lostpointercapture"
          (lambda (event)
            (when (= edit-pointer (@ event pointer-id)) (stop-edit))))))
      (dolist (name (array "pointerup" "pointercancel"))
        ((@ document add-event-listener) name
         (lambda (event)
           (when (= edit-pointer (@ event pointer-id)) (stop-edit)))))
      (let ((fullscreen (element "fullscreen")))
        (when fullscreen
          ((@ fullscreen add-event-listener) "click"
           (lambda (event) ((@ event prevent-default)) (toggle-fullscreen))))))
    (defun autojump-clear-p (x y feet)
      ;; A one-cell rise needs a blocker to land on, headroom at the landing,
      ;; and a clear arc above the present body.  This avoids hopping at walls
      ;; and into low ceilings while retaining the light touch of autojump.
      (and grounded
           (collides x y feet)
           (not (collides x y (+ feet 1)))
           (not (collides (@ camera position x) (@ camera position y) (+ feet 1)))))
    (defun move-player (dt)
      (let* ((forward (- (if (or (pressed "KeyW") (pressed "ArrowUp")) 1 0)
                         (if (or (pressed "KeyS") (pressed "ArrowDown")) 1 0) stick-y))
             (side (+ (- (if (or (pressed "KeyD") (pressed "ArrowRight")) 1 0)
                         (if (or (pressed "KeyA") (pressed "ArrowLeft")) 1 0)) stick-x))
             (speed (* dt (if (or (pressed "ShiftLeft") (< stick-y -0.95)) 7 4.5)
                       (/ 1 (max 1 ((@ |Math| hypot) forward side)))))
             (dx (* speed (+ (* forward (sin yaw)) (* side (cos yaw)))))
             (dy (* speed (- (* forward (cos yaw)) (* side (sin yaw)))))
             (feet (- (@ camera position z) 1.62)))
        (when (and (> (+ (abs dx) (abs dy)) 0.0001)
                   (autojump-clear-p (+ (@ camera position x) dx)
                                     (+ (@ camera position y) dy) feet))
          (setf velocity 7.5 grounded false))
        (unless (collides (+ (@ camera position x) dx) (@ camera position y) feet)
          (incf (@ camera position x) dx))
        (unless (collides (@ camera position x) (+ (@ camera position y) dy) feet)
          (incf (@ camera position y) dy))
        (when (and grounded (pressed "Space")) (setf velocity 7.5 grounded false))
        (decf velocity (* 22 dt))
        (let ((dz (* velocity dt)))
          (if (collides (@ camera position x) (@ camera position y) (+ feet dz))
              (progn (setf grounded (< velocity 0)) (setf velocity 0))
              (progn (incf (@ camera position z) dz) (setf grounded false))))
        (when (< (@ camera position z) -12)
          ((@ camera position set) 23 20 20)
          (setf velocity 0))
        (look)))
    (defun aim ()
      (let ((direction (vector 0 0 0)))
        ((@ camera get-world-direction) direction)
        (setf target (trace-cells ((@ camera position to-array))
                                  ((@ direction to-array)) 7)
              (@ outline visible) (not (null (and target (@ target previous)))))
        (when (@ outline visible)
          (let* ((cell (@ target cell)) (previous (@ target previous))
                 (normal (vector (- (aref previous 0) (aref cell 0))
                                 (- (aref previous 1) (aref cell 1))
                                 (- (aref previous 2) (aref cell 2)))))
            ((@ outline position set) (+ 0.5 (aref cell 0))
                                      (+ 0.5 (aref cell 1))
                                      (+ 0.5 (aref cell 2)))
            ((@ outline position add-scaled-vector) normal .502)
            ((@ outline quaternion set-from-unit-vectors) (vector 0 0 1) normal)))))
    (defun edit-cell (place)
      (when target
        (let ((point (if place (@ target previous) (@ target cell))))
          (when (and point (>= (aref point 2) 0) (< (aref point 2) world-height))
            (let ((x (aref point 0)) (y (aref point 1)) (z (aref point 2))
                  (feet (- (@ camera position z) 1.62)))
              (when (and place
                         (<= x (floor (+ (@ camera position x) .28)))
                         (>= x (floor (- (@ camera position x) .28)))
                         (<= y (floor (+ (@ camera position y) .28)))
                         (>= y (floor (- (@ camera position y) .28)))
                         (<= z (floor (+ feet 1.7))) (>= z (floor (+ feet .001))))
                (return-from edit-cell false))
              (if streaming-enabled
                  (edit-world-cell x y z (if place selected 0))
                  (progn
                    (if place ((@ cells set) (cell-key x y z) selected)
                        ((@ cells delete) (cell-key x y z)))
                    (rebuild)))
              (aim)
              (return-from edit-cell true)))))
      false)
    (defun update-environment ()
      ((@ sky position copy) (@ camera position))
      (when streaming-enabled
        ;; Fog ends before the nearest not-yet-resident ring, including during
        ;; startup. The ready radius grows as time-sliced chunks arrive.
        (let ((radius (stream-ready-radius (@ camera position x) (@ camera position y))))
          (setf (@ scene fog far) (max 12 (min 76 (- (* radius chunk-size) 2)))
                (@ scene fog near) (* .55 (@ scene fog far))))
        ;; Snap in the light's own plane, not world XY. Whole shadow-texel
        ;; shifts preserve the sampling grid and the fixed concave-contact fix.
        (let* ((direction ((@ (vector -0.72 0.43 0.22) normalize)))
               (right ((@ ((@ (vector 0 0 1) cross) direction) normalize)))
               (up ((@ ((@ direction clone)) cross) right))
               (center ((@ camera position clone)))
               (step (/ 128 2048)))
          (dolist (axis (array right up direction))
            (let* ((grid (if (= axis direction) 4 (* step 8)))
                   (distance ((@ center dot) axis)))
              ((@ center add-scaled-vector) axis (- (* (round (/ distance grid)) grid) distance))))
          (when (> ((@ center distance-to-squared) (@ sun target position)) 0.00001)
            ((@ sun target position copy) center)
            ((@ sun position copy) center)
            ((@ sun position add-scaled-vector) direction 114)
            (setf (@ sun shadow needs-update) true)))))
    (defun resize ()
      (let ((width (@ canvas client-width)) (height (@ canvas client-height)))
        (setf (@ camera aspect) (/ width height))
        ((@ camera update-projection-matrix))
        ((@ renderer set-size) width height false)
        ((@ composer set-size) width height)))
    (defun animate (time)
      (let ((dt (min 0.04 (/ (- time last-time) 1000))))
        (setf last-time time)
        (unless (@ document hidden)
          ;; Small fixed upper bound prevents tunnelling on slow frames.
          (dotimes (step 4) (move-player (/ dt 4)))
          (update-streaming (@ camera position x) (@ camera position y))
          (update-environment)
          (aim)
          (repeat-edit time)
          ((@ composer render))
          (incf frame-count)
          (when (> (- time stats-time) 1000)
            (setf (@ (element "metrics") text-content)
                  (+ (round (/ (* frame-count 1000) (- time stats-time))) " fps")
                  frame-count 0 stats-time time))))
      ((@ window request-animation-frame) animate))
    (browser:async-defun start ()
      (try
       (progn
         (setf |THREE| (browser:await (import "three")))
         (let* ((effects (browser:await (import "three/addons/postprocessing/EffectComposer.js")))
                (renders (browser:await (import "three/addons/postprocessing/RenderPass.js")))
                (occlusions (browser:await (import "three/addons/postprocessing/SSAOPass.js")))
                (blooms (browser:await (import "three/addons/postprocessing/UnrealBloomPass.js")))
                (outputs (browser:await (import "three/addons/postprocessing/OutputPass.js"))))
           (setf renderer (new ((@ |THREE| |WebGLRenderer|)
                                (create :canvas canvas :antialias true)))
                 scene (new ((@ |THREE| |Scene|)))
                 camera (new ((@ |THREE| |PerspectiveCamera|) 58 1 0.08 220))
                 world (new ((@ |THREE| |Group|))))
           ((@ camera up set) 0 0 1)
           ((@ renderer set-pixel-ratio) (min (if coarse-pointer 1.5 2) (or (@ window device-pixel-ratio) 1)))
           (setf (@ renderer tone-mapping) (@ |THREE| |ACESFilmicToneMapping|)
                 (@ renderer tone-mapping-exposure) 1.1
                 (@ renderer shadow-map enabled) true
                 (@ renderer shadow-map type) (@ |THREE| |PCFSoftShadowMap|)
                 (@ scene fog) (new ((@ |THREE| |Fog|)
                                     (new ((@ |THREE| |Color|) .48 .59 .68)) 42 76)))
           ((@ scene add) world)
           ;; A small shader sky gives the far fog a luminous horizon instead
           ;; of exposing a flat clear color between the highland's ridges.
           (let ((atmosphere (new ((@ |THREE| |ShaderMaterial|)))))
             (setf (@ atmosphere side) (@ |THREE| |BackSide|)
                   (@ atmosphere allow-override) false
                   (@ atmosphere depth-write) false
                   (@ atmosphere vertex-shader)
                   "varying vec3 direction; void main() { direction = normalize(position); gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0); }"
                   (@ atmosphere fragment-shader)
                   "varying vec3 direction;
void main() {
  vec3 d = normalize(direction);
  float h = smoothstep(0.0, .9, d.z);
  vec3 color = mix(vec3(.48, .59, .68), vec3(.08, .20, .40), h);
  float sun = pow(max(dot(d, normalize(vec3(-.72, .43, .22))), 0.0), 22.0);
  float wisps = smoothstep(.58, .92, sin(d.x * 17.0 + d.y * 9.0 + sin(d.y * 19.0) * .5));
  wisps *= smoothstep(.08, .3, d.z) * (1.0 - smoothstep(.35, .7, d.z));
  color += vec3(.19, .14, .08) * sun + vec3(.035, .03, .025) * wisps;
  gl_FragColor = vec4(color, 1.0);
}")
             (setf sky (new ((@ |THREE| |Mesh|)
                             (new ((@ |THREE| |SphereGeometry|) 180 24 16)) atmosphere))
                   (@ sky frustum-culled) false)
             ((@ scene add) sky))
           ;; Open, friendly daylight: brighter diffuse fill lifts shaded
           ;; faces without blowing out the sunlit stone or erasing contacts.
           ;; Three's diffuse BRDF divides irradiance by pi; compensate here.
           (let ((sky (new ((@ |THREE| |HemisphereLight|)
                            (new ((@ |THREE| |Color|) 0.30 0.36 0.46))
                            (new ((@ |THREE| |Color|) 0.34 0.29 0.25))
                            3.14159265))))
             ((@ sky position set) 0 0 1)
             ((@ scene add) sky))
           (setf sun (new ((@ |THREE| |DirectionalLight|)
                           (new ((@ |THREE| |Color|) 1.05 0.98 0.85))
                           3.14159265)))
           ((@ sun position set) -48 67 22)
           ((@ sun target position set) 24 24 0)
           (setf (@ sun cast-shadow) true
                 (@ sun shadow map-size width) 2048
                 (@ sun shadow map-size height) 2048
                 (@ sun shadow camera left) -64 (@ sun shadow camera right) 64
                 (@ sun shadow camera top) 64 (@ sun shadow camera bottom) -64
                 (@ sun shadow camera near) 1 (@ sun shadow camera far) 230
                 ;; Front-face depths need enough bias for the PCF footprint
                 ;; at this low sun angle. Depth bias is normalized by the
                 ;; 229-cell shadow range; -0.0015 is about 0.34 cells.
                 (@ sun shadow normal-bias) 0.04 (@ sun shadow bias) -0.0015
                 (@ sun shadow auto-update) false (@ sun shadow needs-update) true)
           ((@ scene add) sun (@ sun target))
           ((@ sun shadow camera up set) 0 0 1)
           ;; Three defaults to BACK faces for shadow casting. Those are exit
           ;; depths, not the nearest blockers: at a concave bevel they expose
           ;; a false strip of sunlight which PCF spreads into a bright fringe.
           ;; Use the same outward-facing surface for visibility and shadows.
           (setf material (new ((@ |THREE| |MeshStandardMaterial|)
                                (create :roughness 0.82 :metalness 0.0)))
                 (@ material shadow-side) (@ |THREE| |FrontSide|)
                 (@ material vertex-colors) true)
           ;; Inset on the pointed-at face, leaving bevels uncovered. The
           ;; derivative-smoothed rim is stable at oblique angles and retina
           ;; resolutions, unlike one-device-pixel WebGL lines.
           (let ((geometry (new ((@ |THREE| |PlaneGeometry|) .78 .78)))
                 (ink (new ((@ |THREE| |ShaderMaterial|)))))
             (setf (@ ink transparent) true (@ ink depth-write) false
                   (@ ink allow-override) false
                   (@ ink uniforms) (create "normalPass" (create :value false))
                   (@ ink vertex-shader)
                   "varying vec2 point; void main() { point = uv - .5; gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0); }"
                   (@ ink fragment-shader)
                   "varying vec2 point;
uniform bool normalPass;
void main() {
  if (normalPass) discard;
  vec2 q = abs(point) - vec2(.42);
  float d = length(max(q, 0.0)) + min(max(q.x, q.y), 0.0) - .05;
  float aa = max(fwidth(d), .001);
  float fill = 1.0 - smoothstep(-aa, aa, d);
  float rim = 1.0 - smoothstep(.006, .006 + aa, abs(d));
  gl_FragColor = vec4(.85, .72, .46, fill * .055 + rim * .42);
}")
             (setf outline (new ((@ |THREE| |Mesh|) geometry ink))
                   (@ outline visible) false (@ outline render-order) 2
                   (@ outline on-before-render)
                   (lambda ()
                     (setf (@ ink uniforms normal-pass value)
                           (not (null (@ scene override-material))))))
             ((@ scene add) outline))
           (let ((target (new ((@ |THREE| |WebGLRenderTarget|) 1 1
                               (create :type (@ |THREE| |HalfFloatType|) :samples 4)))))
             (setf composer (new ((@ effects |EffectComposer|) renderer target))))
           ((@ composer add-pass) (new ((@ renders |RenderPass|) scene camera)))
           ;; World-scale contact shading, before bloom and the single output
           ;; transform. SSAOPass multiplies the existing linear HDR buffer.
           (setf occlusion (new ((@ occlusions |SSAOPass|) scene camera 1 1 16))
                 (@ occlusion kernel-radius) 0.65
                 (@ occlusion min-distance) 0.0002
                 (@ occlusion max-distance) 0.012)
           ;; SSAO normally multiplies AFTER fog and makes hidden distant
           ;; bevels reappear as dark ghost terrain. Fade the final multiplier
           ;; to white using the same view-depth fog law as MeshStandardMaterial.
           (let* ((copy (@ occlusion copy-material)) (uniforms (@ copy uniforms)))
             (setf (@ uniforms t-depth) (create :value (@ occlusion normal-render-target depth-texture))
                   (@ uniforms camera-near) (create :value (@ camera near))
                   (@ uniforms camera-far) (create :value (@ camera far))
                   (@ uniforms fog-near) (create :value 42)
                   (@ uniforms fog-far) (create :value 76)
                   (@ copy on-before-render)
                   (lambda ()
                     (setf (@ uniforms fog-near value) (if (@ scene fog) (@ scene fog near) 100000)
                           (@ uniforms fog-far value) (if (@ scene fog) (@ scene fog far) 100001)))
                   (@ copy fragment-shader)
                   "#include <packing>
varying vec2 vUv;
uniform sampler2D tDiffuse, tDepth;
uniform float cameraNear, cameraFar, fogNear, fogFar;
void main() {
  float depth = texture2D(tDepth, vUv).x;
  float distance = -perspectiveDepthToViewZ(depth, cameraNear, cameraFar);
  float fog = smoothstep(fogNear, fogFar, distance);
  gl_FragColor = vec4(mix(texture2D(tDiffuse, vUv).rgb, vec3(1.0), fog), 1.0);
}"))
           ((@ composer add-pass) occlusion)
           (setf bloom (new ((@ blooms |UnrealBloomPass|)
                             (new ((@ |THREE| |Vector2|) 1 1)) 0.12 0.5 1.15)))
           (setf (@ bloom enabled) (not coarse-pointer))
           ((@ composer add-pass) bloom)
           ((@ composer add-pass) (new ((@ outputs |OutputPass|))))
           (setf (@ window luft-demo)
                 (create :ready false :cells cells :atlas atlas :trace trace-cells
                         :selection outline :aim aim
                         :surface-sites surface-sites :rebuild rebuild
                         :composer composer :occlusion occlusion :sun sun
                         :chunks chunks :edits world-edits :source source-cell-at
                         "workerState" worker-state
                         "startStreaming" (lambda (x y)
                                            (stop-streaming)
                                            ((@ ((@ meshes slice)) for-each) remove-mesh)
                                            (start-streaming x y))
                         "stopStreaming" stop-streaming
                         "updateStreaming" update-streaming "editWorldCell" edit-world-cell
                         "updateEnvironment" update-environment
                         :camera camera :renderer renderer :meshes (lambda () meshes)))
           (setf chunk-added realize-chunk
                 chunk-removed (lambda (chunk) (remove-mesh (@ chunk mesh))))
           (install-worker-generation)
           (reset-cells) (start-streaming 23 20) (resize)
           (browser:await (await-startup-chunks 23 20))
           (spawn-player)
           ((@ window add-event-listener) "resize" resize)
           ;; Dynamic viewport units follow Safari's moving toolbar. Observe
           ;; actual canvas size, including changes without a window resize.
           ((@ (new (|ResizeObserver| resize)) observe) canvas)
           ((@ window add-event-listener) "blur" clear-input)
           ((@ document add-event-listener) "visibilitychange"
            (lambda ()
              (clear-input)
              (setf last-time ((@ performance now)) stats-time last-time frame-count 0)))
           ((@ (element "materials") add-event-listener) "click"
            (lambda (event)
              (let ((button (chain event target (closest "button[data-kind]"))))
                (when button
                  (setf selected (|Number| (@ button dataset kind)))
                  (select-material)))))
           ((@ document add-event-listener) "pointerlockchange"
            (lambda ()
              (clear-input)
              (set-status "")))
           ((@ document add-event-listener) "fullscreenchange" update-fullscreen-affordance)
           ((@ document add-event-listener) "pointerlockerror"
            (lambda () (set-status "Mouse capture unavailable. Drag to look around instead.")))
           ((@ document add-event-listener) "mousemove"
            (lambda (event)
              (when (= (@ document pointer-lock-element) canvas)
                (turn-player (@ event movement-x) (@ event movement-y)))))
           ((@ canvas add-event-listener) "contextmenu" (lambda (event) ((@ event prevent-default))))
           (bind-pointer-controls)
           (suppress-gesture-zoom)
           (update-fullscreen-affordance)
           ((@ document add-event-listener) "keydown"
            (lambda (event)
              (unless (and (= (@ event code) "Space")
                           (= (@ document active-element tag-name) "BUTTON"))
                ((@ keys add) (@ event code))
                (when ((@ (array "Space" "ArrowUp" "ArrowDown" "ArrowLeft" "ArrowRight") includes) (@ event code))
                  ((@ event prevent-default)))
                (let ((number (|Number| (@ event key))))
                  (when (and (>= number 1) (<= number 5))
                    (setf selected number) (select-material))))))
           ((@ document add-event-listener) "keyup"
            (lambda (event) ((@ keys delete) (@ event code))))
           (set-status "")
           (setf (@ window luft-demo ready) true)
           (animate 0)))
       (:catch (error)
         (set-status (+ "The demo could not start: " (@ error message)
                        ". It needs WebGL 2 and access to the Three.js CDN."))
         ((@ console error) error))))
    (defun select-material ()
      ((@ (chain document (query-selector-all "button[data-kind]")) for-each)
       (lambda (button)
         ((@ button set-attribute) "aria-pressed"
          (if (= (|Number| (@ button dataset kind)) selected) "true" "false")))))
    (start)))

(defun demo-javascript ()
  (ps:ps* `(progn
             (defvar atlas ,(array-form (atlas-data)))
             (defvar initial-cells ,(array-form (demo-cells)))
             ,(core-form)
             ,(streaming-form)
             ,(meshing-form)
             ,(worker-transport-form)
             ,(client-form))))
