(in-package #:luft.web)

(defparameter *material-vertex-header*
  "attribute float materialMask;
attribute vec4 kindsA;
attribute vec4 kindsB;
varying vec3 luftTone;
vec3 cellTone(float kind, float up) {
  if (kind < 1.5) return mix(vec3(.42,.32,.21), vec3(.18,.31,.105), up);
  if (kind < 2.5) return vec3(.53,.49,.39);
  if (kind < 3.5) return vec3(.23,.13,.065);
  if (kind < 4.5) return vec3(.16,.68,.94);
  return mix(vec3(.085,.19,.045),vec3(.22,.37,.085),up);
}
")

(defparameter *material-vertex-color*
  "#include <color_vertex>
luftTone = vec3(0.0);
float count = 0.0;
for (int i=0; i<8; i++) {
  if (mod(floor(materialMask / exp2(float(i))), 2.0) > .5) {
    float kind = i < 4 ? kindsA[i] : kindsB[i-4];
    luftTone += cellTone(kind, max(normal.z, 0.0));
    count += 1.0;
  }
}
luftTone /= max(count, 1.0);
")

(defun client-form ()
  `(progn
    (defvar renderer) (defvar scene) (defvar camera) (defvar orbit)
    (defvar composer) (defvar bloom) (defvar material) (defvar world)
    (defvar outline) (defvar sun) (defvar meshes (array))
    (defvar keys (new (|Set|)))
    (defvar walking false) (defvar yaw 0) (defvar pitch 0)
    (defvar velocity 0) (defvar grounded false) (defvar selected 2)
    (defvar last-time 0) (defvar target null) (defvar frame-count 0)
    (defvar coarse-pointer (@ ((@ window match-media) "(pointer: coarse)") matches))
    (defvar wireframe false) (defvar stats-time 0)
    (defvar |THREE|) (defvar status (chain document (get-element-by-id "status")))
    (defvar canvas (chain document (get-element-by-id "world")))
    (defun element (id) (chain document (get-element-by-id id)))
    (defun set-status (message) (setf (@ status text-content) message))
    (defun vector (x y z) (new ((@ |THREE| |Vector3|) x y z)))
    (defun rebuild ()
      (let ((groups (new (|Map|))) (count 0))
        ((@ (surface-sites) for-each)
         (lambda (site)
           (let* ((star (aref site 3)) (entry (aref atlas star)))
             (when (> (@ (aref entry 0) length) 0)
               (unless ((@ groups has) star) ((@ groups set) star (array)))
               ((@ ((@ groups get) star) push) site)
               (incf count)))))
        ((@ meshes for-each)
         (lambda (mesh)
           ((@ world remove) mesh)
           ((@ mesh geometry dispose))
           ((@ mesh dispose))))
        (setf meshes (array))
        ((@ groups for-each)
         (lambda (sites star)
           (let* ((entry (aref atlas star)) (positions (array)) (masks (array))
                  (kinds-a (array)) (kinds-b (array))
                  (geometry (new ((@ |THREE| |BufferGeometry|)))))
             ((@ (aref entry 0) for-each)
              (lambda (triangle index)
                ((@ triangle for-each)
                 (lambda (point)
                   ((@ positions push) (/ (aref point 0) 8)
                                       (/ (aref point 1) 8)
                                       (/ (aref point 2) 8))
                   ((@ masks push) (aref (aref (aref entry 1) index) 0))))))
             ((@ geometry set-attribute) "position"
              (new ((@ |THREE| |Float32BufferAttribute|) positions 3)))
             ((@ geometry set-attribute) "materialMask"
              (new ((@ |THREE| |Float32BufferAttribute|) masks 1)))
             ((@ geometry compute-vertex-normals))
             ((@ sites for-each)
              (lambda (site)
                (dotimes (sample 8)
                  ((@ (if (< sample 4) kinds-a kinds-b) push)
                   (sample-cell (aref site 0) (aref site 1) (aref site 2) sample)))))
             ((@ geometry set-attribute) "kindsA"
              (new ((@ |THREE| |InstancedBufferAttribute|)
                    (new (|Float32Array| kinds-a)) 4)))
             ((@ geometry set-attribute) "kindsB"
              (new ((@ |THREE| |InstancedBufferAttribute|)
                    (new (|Float32Array| kinds-b)) 4)))
             (let ((mesh (new ((@ |THREE| |InstancedMesh|)
                               geometry material (@ sites length))))
                   (matrix (new ((@ |THREE| |Matrix4|)))))
               ((@ sites for-each)
                (lambda (site index)
                  ((@ matrix make-translation) (aref site 0) (aref site 1) (aref site 2))
                  ((@ mesh set-matrix-at) index matrix)))
               (setf (@ mesh cast-shadow) true (@ mesh receive-shadow) true)
               ((@ mesh compute-bounding-sphere))
               ((@ world add) mesh)
               ((@ meshes push) mesh)))))
        (setf (@ window luft-demo sites) count
              (@ window luft-demo batches) (@ meshes length)
              (@ sun shadow needs-update) true)))
    (defun home ()
      (setf walking false (@ orbit enabled) true (@ outline visible) false)
      ((@ keys clear))
      ((@ camera position set) 57 -17 34)
      (when (< (@ camera aspect) 0.8)
        ((@ camera position set) 70 -33 44))
      ((@ orbit target set) 24 23 8)
      ((@ orbit update))
      (setf (@ (element "walk") text-content) "Walk here"
            (@ (element "crosshair") hidden) true)
      (set-status "Drag to look around · Scroll to approach"))
    (defun look ()
      ((@ camera look-at)
       (+ (@ camera position x) (* (sin yaw) (cos pitch)))
       (+ (@ camera position y) (* (cos yaw) (cos pitch)))
       (+ (@ camera position z) (sin pitch))))
    (defun enter-walk ()
      ;; Always enter at the clear stone terrace, including after edits.
      (let ((height 10))
        (while (collides 23 20 height) (incf height))
        ((@ camera position set) 23 20 (+ height 1.62)))
      (setf yaw 0 pitch 0 velocity 0 grounded false)
      (look)
      (let ((request ((@ canvas request-pointer-lock))))
        (when (and request (@ request catch))
          ((@ request catch)
           (lambda (error) (set-status "Mouse capture unavailable. Drag to explore the orbit view."))))))
    (defun move-player (dt)
      (let* ((forward (- (if (or ((@ keys has) "KeyW") ((@ keys has) "ArrowUp")) 1 0)
                         (if (or ((@ keys has) "KeyS") ((@ keys has) "ArrowDown")) 1 0)))
             (side (- (if (or ((@ keys has) "KeyD") ((@ keys has) "ArrowRight")) 1 0)
                      (if (or ((@ keys has) "KeyA") ((@ keys has) "ArrowLeft")) 1 0)))
             (speed (* dt (if ((@ keys has) "ShiftLeft") 7 4.5)
                       (/ 1 (max 1 ((@ |Math| hypot) forward side)))))
             (dx (* speed (+ (* forward (sin yaw)) (* side (cos yaw)))))
             (dy (* speed (- (* forward (cos yaw)) (* side (sin yaw)))))
             (feet (- (@ camera position z) 1.62)))
        (unless (collides (+ (@ camera position x) dx) (@ camera position y) feet)
          (incf (@ camera position x) dx))
        (unless (collides (@ camera position x) (+ (@ camera position y) dy) feet)
          (incf (@ camera position y) dy))
        (when (and grounded ((@ keys has) "Space")) (setf velocity 7.5 grounded false))
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
              (@ outline visible) (and walking (not (= target null))))
        (when target
          ((@ outline position set) (+ 0.5 (aref (@ target cell) 0))
                                    (+ 0.5 (aref (@ target cell) 1))
                                    (+ 0.5 (aref (@ target cell) 2))))))
    (defun edit-cell (place)
      (when (and walking target)
        (let ((point (if place (@ target previous) (@ target cell))))
          (when (and point (>= (aref point 2) 0) (< (aref point 2) 40)
                     (>= (aref point 0) 0) (< (aref point 0) 48)
                     (>= (aref point 1) 0) (< (aref point 1) 48))
            (let* ((key (cell-key (aref point 0) (aref point 1) (aref point 2)))
                   (old ((@ cells get) key)))
              (if place ((@ cells set) key selected) ((@ cells delete) key))
              (when (and place (collides (@ camera position x) (@ camera position y)
                                         (- (@ camera position z) 1.62)))
                (if old ((@ cells set) key old) ((@ cells delete) key)))
              (rebuild)
              (aim))))))
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
          (if walking
              (progn
                ;; Small fixed upper bound prevents tunnelling on slow frames.
                (dotimes (step 4) (move-player (/ dt 4)))
                (aim))
              ((@ orbit update)))
          ((@ composer render))
          (incf frame-count)
          (when (> (- time stats-time) 1000)
            (setf (@ (element "metrics") text-content)
                  (+ (round (/ (* frame-count 1000) (- time stats-time))) " fps · "
                     (@ window luft-demo sites) " stars · " (@ meshes length) " batches")
                  frame-count 0 stats-time time))))
      ((@ window request-animation-frame) animate))
    (browser:async-defun start ()
      (try
       (progn
         (setf |THREE| (browser:await (import "three")))
         (let* ((controls (browser:await (import "three/addons/controls/OrbitControls.js")))
                (effects (browser:await (import "three/addons/postprocessing/EffectComposer.js")))
                (renders (browser:await (import "three/addons/postprocessing/RenderPass.js")))
                (blooms (browser:await (import "three/addons/postprocessing/UnrealBloomPass.js")))
                (outputs (browser:await (import "three/addons/postprocessing/OutputPass.js"))))
           (setf renderer (new ((@ |THREE| |WebGLRenderer|)
                                (create :canvas canvas :antialias true)))
                 scene (new ((@ |THREE| |Scene|)))
                 camera (new ((@ |THREE| |PerspectiveCamera|) 58 1 0.08 220))
                 world (new ((@ |THREE| |Group|))))
           ((@ camera up set) 0 0 1)
           ((@ renderer set-pixel-ratio) (min (if coarse-pointer 1 2) (or (@ window device-pixel-ratio) 1)))
           (setf (@ renderer tone-mapping) (@ |THREE| |ACESFilmicToneMapping|)
                 (@ renderer tone-mapping-exposure) 1.2
                 (@ renderer shadow-map enabled) true
                 (@ renderer shadow-map type) (@ |THREE| |PCFSoftShadowMap|)
                 (@ scene background) (new ((@ |THREE| |Color|) "#b9d5db"))
                 (@ scene fog) (new ((@ |THREE| |Fog|) "#b9d5db" 65 155))
                 orbit (new ((@ controls |OrbitControls|) camera canvas))
                 (@ orbit enable-damping) true
                 (@ orbit min-distance) 3 (@ orbit max-distance) 110
                 (@ orbit max-polar-angle) 1.52)
           ((@ scene add) world)
           (let ((sky (new ((@ |THREE| |HemisphereLight|) "#d3efff" "#645039" 2.0))))
             ((@ sky position set) 0 0 1)
             ((@ scene add) sky))
           (setf sun (new ((@ |THREE| |DirectionalLight|) "#fff0d5" 3.1)))
           ((@ sun position set) -15 -5 65)
           ((@ sun target position set) 24 24 0)
           (setf (@ sun cast-shadow) true
                 (@ sun shadow map-size width) (if coarse-pointer 1024 2048)
                 (@ sun shadow map-size height) (if coarse-pointer 1024 2048)
                 (@ sun shadow camera left) -44 (@ sun shadow camera right) 44
                 (@ sun shadow camera top) 44 (@ sun shadow camera bottom) -44
                 (@ sun shadow camera near) 1 (@ sun shadow camera far) 130
                 (@ sun shadow normal-bias) 0.025 (@ sun shadow bias) -0.0001
                 (@ sun shadow auto-update) false (@ sun shadow needs-update) true)
           ((@ scene add) sun (@ sun target))
           (setf material (new ((@ |THREE| |MeshStandardMaterial|)
                                (create :roughness 0.82 :metalness 0.0))))
           (setf (@ material on-before-compile)
                 (lambda (shader)
                   (setf (@ shader vertex-shader)
                         (+ ,*material-vertex-header* (@ shader vertex-shader))
                         (@ shader vertex-shader)
                         ((@ shader vertex-shader replace) "#include <color_vertex>"
                          ,*material-vertex-color*)
                         (@ shader fragment-shader)
                         (+ "varying vec3 luftTone;
" (@ shader fragment-shader))
                         (@ shader fragment-shader)
                         ((@ shader fragment-shader replace) "#include <color_fragment>"
                          "#include <color_fragment>
 diffuseColor.rgb *= luftTone;"))))
           (let ((geometry (new ((@ |THREE| |EdgesGeometry|)
                                 (new ((@ |THREE| |BoxGeometry|) 1.006 1.006 1.006)))))
                 (ink (new ((@ |THREE| |LineBasicMaterial|)
                            (create :color "#fff5c9")))))
             (setf outline (new ((@ |THREE| |LineSegments|) geometry ink))
                   (@ outline visible) false)
             ((@ scene add) outline))
           (let ((target (new ((@ |THREE| |WebGLRenderTarget|) 1 1
                               (create :type (@ |THREE| |HalfFloatType|) :samples 4)))))
             (setf composer (new ((@ effects |EffectComposer|) renderer target))))
           ((@ composer add-pass) (new ((@ renders |RenderPass|) scene camera)))
           (setf bloom (new ((@ blooms |UnrealBloomPass|)
                             (new ((@ |THREE| |Vector2|) 1 1)) 0.12 0.5 1.15)))
           (setf (@ bloom enabled) (not coarse-pointer)
                 (@ (element "bloom") checked) (not coarse-pointer))
           ((@ composer add-pass) bloom)
           ((@ composer add-pass) (new ((@ outputs |OutputPass|))))
           (setf (@ window luft-demo)
                 (create :ready false :cells cells :atlas atlas :trace trace-cells
                         :surface-sites surface-sites :rebuild rebuild
                         :camera camera :renderer renderer :meshes (lambda () meshes)))
           (reset-cells) (rebuild) (resize) (home)
           ((@ window add-event-listener) "resize" resize)
           ((@ window add-event-listener) "blur" (lambda () ((@ keys clear))))
           ((@ document add-event-listener) "visibilitychange"
            (lambda () ((@ keys clear))))
           ((@ (element "walk") add-event-listener) "click" enter-walk)
           ((@ (element "home") add-event-listener) "click"
            (lambda () (when (@ document pointer-lock-element) ((@ document exit-pointer-lock))) (home)))
           ((@ (element "reset") add-event-listener) "click"
            (lambda () (reset-cells) (rebuild) (home)))
           ((@ (element "wire") add-event-listener) "click"
            (lambda ()
              (setf wireframe (not wireframe) (@ material wireframe) wireframe)
              ((@ (element "wire") set-attribute) "aria-pressed" (if wireframe "true" "false"))))
           ((@ (element "bloom") add-event-listener) "change"
            (lambda (event) (setf (@ bloom enabled) (@ event target checked))))
           ((@ (element "materials") add-event-listener) "click"
            (lambda (event)
              (let ((button (chain event target (closest "button[data-kind]"))))
                (when button
                  (setf selected (|Number| (@ button dataset kind)))
                  (select-material)))))
           ((@ document add-event-listener) "pointerlockchange"
            (lambda ()
              ((@ document body class-list toggle) "playing" (= (@ document pointer-lock-element) canvas))
              (setf walking (= (@ document pointer-lock-element) canvas)
                    (@ orbit enabled) (not walking)
                    (@ (element "crosshair") hidden) (not walking))
              ((@ keys clear))
              (unless walking
                ((@ orbit target copy) (@ camera position))
                (incf (@ orbit target x) (* 6 (sin yaw)))
                (incf (@ orbit target y) (* 6 (cos yaw)))
                (setf (@ outline visible) false))
              (set-status (if walking "WASD move · Space jump · Click remove · Right-click place · Esc release"
                                      "Drag to look around · Scroll to approach"))))
           ((@ document add-event-listener) "pointerlockerror"
            (lambda () (set-status "Mouse capture unavailable. Drag to look around instead.")))
           ((@ document add-event-listener) "mousemove"
            (lambda (event)
              (when walking
                (incf yaw (* (@ event movement-x) 0.002))
                (setf pitch (max -1.5 (min 1.5 (- pitch (* (@ event movement-y) 0.002))))))))
           ((@ canvas add-event-listener) "contextmenu" (lambda (event) ((@ event prevent-default))))
           ((@ canvas add-event-listener) "mousedown"
            (lambda (event)
              (when (and walking (or (= (@ event button) 0) (= (@ event button) 2)))
                (edit-cell (= (@ event button) 2)))))
           ((@ document add-event-listener) "keydown"
            (lambda (event)
              (when walking
                ((@ keys add) (@ event code))
                (when ((@ (array "Space" "ArrowUp" "ArrowDown" "ArrowLeft" "ArrowRight") includes) (@ event code))
                  ((@ event prevent-default)))
                (let ((number (|Number| (@ event key))))
                  (when (and (>= number 1) (<= number 5))
                    (setf selected number) (select-material))))))
           ((@ document add-event-listener) "keyup"
            (lambda (event) ((@ keys delete) (@ event code))))
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
             ,(client-form))))
