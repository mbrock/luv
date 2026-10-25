;;;; Appearance-only experiment. Load after LUFT/RENDERER, in a disposable image.
;;;; No vertex, normal, material descriptor, resource, or production mutation.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package '#:terrain-lod)
    (defpackage #:terrain-lod
      (:use #:cl)
      (:local-nicknames (#:r #:luft.render)
                        (#:s #:luft.render.shaders)
                        (#:q #:luv.shader))))
  (export (intern "APPEARANCE-FRAGMENT-SPECIFICATION" '#:terrain-lod)
          '#:terrain-lod))

(in-package #:terrain-lod)

(defun appearance-live-bindings (bindings result)
  "Keep the original lexical order, dropping unused material work entirely."
  (let ((needed (make-hash-table :test #'eq)))
    (labels ((visit (form)
               (cond ((consp form) (mapc #'visit form))
                     ((symbolp form)
                      (let ((binding (assoc form bindings)))
                        (when (and binding (not (gethash form needed)))
                          (setf (gethash form needed) t)
                          (visit (second binding))))))))
      (visit result))
    (remove-if-not (lambda (binding) (gethash (first binding) needed)) bindings)))

(defun appearance-far-bindings (bindings painterly)
  "Production's unresolved-material approximation, NOT an energy integral.

HEIGHT=.5 cancels the relief brightness term, not signed wear. FADE=0 leaves
the existing mortar-width colour mixture for laid masonry and material tone
for face-aware patterns. Grass fringe has mean .14 over a vertical cell.
Keep the production lighting, roughness, metal, character, and atmosphere
expressions; only their material inputs change. No normals are averaged."
  (let ((far (copy-tree bindings)))
    (flet ((replace-binding (name expression)
             (let ((binding (assoc name far)))
               (assert binding () "STAR-FRAGMENT no longer binds ~S." name)
               (setf (second binding) expression))))
      (replace-binding 's::height 0.5)
      (replace-binding 's::fade 0.0)
      (replace-binding 's::nap-fade 0.0)
      (replace-binding 's::bent 's::normal)
      (replace-binding
       's::base-tone
       '(if (< (q:swizzle (q:representation s::material-pattern) :x) 0.5)
            s::material-tone
            (if (> (q:swizzle (q:representation s::material-pattern) :x) 4.5)
                s::material-tone
                (q:mix s::material-tone s::mortar-tone
                       (q:swizzle (q:representation s::material-pattern) :w)))))
      (replace-binding
       's::fringe
       '(if (< (abs (- s::relief 1.0)) 0.5)
            (* 0.14 (- 1.0 (abs (q:swizzle s::normal :z))))
            0.0))
      (replace-binding
       's::surface-tone
       (if painterly
           ;; Additional broad, world-anchored pigment; unit mean only under
           ;; the uniform-hash model, not over any particular visible image.
           '(* s::base-tone
               (+ (q:vec3 1.0 1.0 1.0)
                  (* (q:vec3 0.12 0.08 -0.03)
                     (- (s::paper-noise (* s::world 0.04)) 0.5))))
           's::base-tone))
      ;; FACE-PATTERN-GLOW at fade=0 is accent.rgb * accent.w * .5 * lit.
      ;; A window's side lit=1 is exact. For motifs use mean masks, not a
      ;; representative point: mean(1-frame)=4*(a²+a*d+.3*d²)=.54832,
      ;; a=.34,d=.06. Screen duty .65*.7 gives .2494856 (ignores correlation
      ;; with its frame). A basin uses production's abs(normal.z) top test
      ;; (including undersides). Mineral veins use the
      ;; uniform-phase integral of max(0,1-2.2*abs(sin phase)); inclusions
      ;; use their .15 hash duty. These are documented approximations.
      (let* ((a (/ 1.0 2.2))
             (vein-mean (float (* (/ 2 pi)
                                  (- (asin a)
                                     (* 2.2 (- 1.0 (sqrt (- 1.0 (* a a)))))))
                               1.0))
             (glow
               `(let* ((shape (q:swizzle (q:representation s::material-pattern) :x))
                       (top (> (abs (q:swizzle s::normal :z)) 0.7))
                       (lit (if (> shape 8.5)
                                (if (< shape 9.5)
                                    (if top 0.0 0.2494856)
                                    (if (> shape 12.5)
                                        (if (< shape 13.5)
                                            (if top 0.54832 0.0)
                                            (if (< shape 14.5) ,vein-mean 0.15))
                                        0.0))
                                (if (> shape 5.5)
                                    (if (< shape 6.5)
                                        (if (< (abs (q:swizzle s::normal :z)) 0.7)
                                            1.0 0.0)
                                        0.0)
                                    0.0))))
                  (* (q:swizzle (q:representation s::material-accent) :xyz)
                     (* 0.5 lit
                        (q:swizzle (q:representation s::material-accent) :w)))))
             (radiance (assoc 's::radiance far))
             (original-glow
               (find 's::face-pattern-glow (rest (second radiance))
                     :key (lambda (form) (and (consp form) (first form))))))
        (assert original-glow () "STAR-FRAGMENT glow expression changed.")
        (setf (second radiance) (subst glow original-glow (second radiance) :test #'eq))))
    (appearance-live-bindings far 's::final)))

(defun appearance-fragment-specification (limit &optional painterly)
  "Retire material detail below LIMIT render-target pixels; zero is original.

The canonical bevel's sqrt(2)/8 cell enclosing radius is projected using the
actual inspection reciprocal extent. Perspective includes off-axis stretch
and nearest radius depth; orthographic has no depth divisor. This changes
neither geometry nor facet normals. Below LIMIT only the far expression is
run; above 2*LIMIT only production runs; the intervening smoothstep blends
their scene-radiance outputs. Motion is always the original expression.

Call under the SAME host hook bindings as STAR-FRAGMENT-SPECIFICATION (game
character, clouds, blocklight, atmosphere, edge shaping, screen filtering).
Re-embedding its source body must retain those bindings too. PAINTERLY adds
an optional low-frequency pigment, not an energy-preserving correction.
The near shader retains production's derivatives inside the selected arm:
quads straddling a retirement boundary require GPU validation (derivatives
in divergent control flow are not portably defined)."
  (check-type limit (real 0 *))
  (let ((original (s:star-fragment-specification)))
    (when (zerop limit)
      (return-from appearance-fragment-specification original))
    (assert (< limit (/ most-positive-single-float 2)))
    (let* ((limit (coerce limit 'single-float))
           (source (q:shader-object-source-form original))
           (body (copy-tree (cdddr source)))
           (block (first body)))
      (assert (plusp limit) () "LIMIT underflowed the shader's float precision.")
      (assert (and (= (length body) 1) (eq (first block) 'let*)))
      (let* ((bindings (second block))
             (color (find 's::color-output (cddr block) :key #'second))
             (motion (find 's::motion-output (cddr block) :key #'second))
             (near `(let* ,bindings s::final))
             (far `(let* ,(appearance-far-bindings bindings painterly) s::final)))
        (assert (equal color '(q:set-output s::color-output (q:vec4 s::final 1.0))))
        (assert motion)
        (q:parse-shader-specification
         (second source) (third source)
         `((let* ((point (q:representation s::world-position))
                  (anchor (floor (+ point (q:vec3 0.5 0.5 0.5))))
                  (relative (- anchor (q:swizzle (q:representation s::camera-position) :xyz)))
                  (z (q:dot relative (q:swizzle (q:representation s::camera-forward) :xyz)))
                  (safe-z (max 0.001 z))
                  (x (q:dot relative (q:swizzle (q:representation s::camera-right) :xyz)))
                  (y (q:dot relative (q:swizzle (q:representation s::camera-up) :xyz)))
                  (focal (/ (abs (q:swizzle s::camera-projection :xy))
                            (* (q:swizzle (q:representation s::inspection-parameters) :zw) 2.0)))
                  (pixels (max (q:swizzle focal :x) (q:swizzle focal :y)))
                  (radius ,(sqrt (/ 2.0 64.0)))
                  (radial (/ (q:vec2 x y) safe-z))
                  (footprint
                    (if (< (q:swizzle (q:representation s::render-parameters) :z) 0.5)
                        (* radius pixels)
                        (/ (* radius pixels (sqrt (+ 1.0 (q:dot radial radial))))
                           (max 0.001 (- z radius)))))
                  (appearance
                    (if (< footprint ,limit)
                        ,far
                        (let* ((near-result ,near))
                          (if (>= footprint ,(* 2.0 limit))
                              near-result
                              (q:mix ,far near-result
                                     (q:smoothstep ,limit ,(* 2.0 limit) footprint)))))))
             (q:set-output s::color-output (q:vec4 appearance 1.0))
             ,motion)))))))
