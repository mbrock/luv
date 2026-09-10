(defpackage #:luft.web.streaming-tests
  (:use #:cl)
  (:import-from #:parachute #:define-test #:true)
  (:local-nicknames (#:web #:luft.web)))

(in-package #:luft.web.streaming-tests)

(defun streaming-claims ()
  (parenscript:ps*
   `(progn
      (defvar initial-cells ,(web::array-form (web::demo-cells)))
      ;; STREAMING is tested with the actual browser core and generated code.
      ,(web::core-form)
      ,(web::streaming-form)
      ,(let ((*package* (find-package '#:luft.web)))
         (read-from-string
         "(progn
            (defun claim (value message) (unless value (throw (new (|Error| message)))))
            (reset-cells)
            (let ((added 0) (removed 0) (changed 0))
              (setf chunk-added (lambda (chunk) (declare (ignore chunk)) (incf added)))
              (setf chunk-removed (lambda (chunk) (declare (ignore chunk)) (incf removed)))
              (setf chunk-changed (lambda (chunk) (declare (ignore chunk)) (incf changed)))
              (start-streaming -1 -1)
              (claim (= (@ chunks size) 9) \"synchronous negative startup\")
              (let ((a ((@ chunks get) \"-1,-1\")) (b ((@ chunks get) \"0,0\")))
                (claim (and a b) \"negative chunk keys\")
                (claim (= (chunk-cell-at a -1 -1 0) (source-cell-at -1 -1 0)) \"negative halo\")
                (claim (= (chunk-cell-at b 0 0 64) 0) \"height air\")
                (let ((seen (new (|Set|))))
                  ((@ (array a b ((@ chunks get) \"-1,0\") ((@ chunks get) \"0,-1\")) for-each)
                   (lambda (chunk)
                     ((@ (chunk-sites chunk) for-each)
                      (lambda (site)
                        (let ((key (cell-key (aref site 0) (aref site 1) (aref site 2))))
                          (claim (not ((@ seen has) key)) \"duplicate owned seam site\")
                          ((@ seen add) key)
                          (let ((star 0))
                            (dotimes (sample 8)
                              (when (source-cell-at (+ (aref site 0) (if (logand sample 1) 0 -1))
                                                    (+ (aref site 1) (if (logand sample 2) 0 -1))
                                                    (+ (aref site 2) (if (logand sample 4) 0 -1)))
                                (setf star (logior star (ash 1 sample)))))
                            (claim (= star (aref site 3)) \"sample-cell convention parity\"))))))))
                ;; Use core's real surface-sites over the same haloed source,
                ;; then retain only these chunks' owned site coordinates.
                (let ((authored cells) (occupancy (new (|Map|))))
                  (loop for x from -17 to 16 do
                    (loop for y from -17 to 16 do
                      (loop for z from 0 to 63 do
                        (let ((kind (source-cell-at x y z)))
                          (when kind ((@ occupancy set) (cell-key x y z) kind))))))
                  (setf cells occupancy)
                  (let ((native (surface-sites)))
                    ((@ (array a b ((@ chunks get) \"-1,0\") ((@ chunks get) \"0,-1\")) for-each)
                     (lambda (chunk)
                       (let ((expected 0))
                         ((@ native for-each)
                          (lambda (site)
                            (when (and (/= (aref site 3) 255)
                                       (>= (aref site 0) (@ chunk x0)) (< (aref site 0) (+ (@ chunk x0) 16))
                                       (>= (aref site 1) (@ chunk y0)) (< (aref site 1) (+ (@ chunk y0) 16)))
                              (incf expected))))
                         (claim (= expected (@ (chunk-sites chunk) length)) \"no missing owned sites\"))
                       ((@ (chunk-sites chunk) for-each)
                        (lambda (site)
                          (let ((native-site ((@ native get) (cell-key (aref site 0) (aref site 1) (aref site 2)))))
                            (claim (and native-site (= (aref native-site 3) (aref site 3)))
                                   \"native core surface-sites parity\")))))))
                  (setf cells authored)))
              (claim (= (stream-ready-radius -1 -1) 1) \"ready startup ring\")
              (claim (= (stream-cell-at 10000 10000 2) 1) \"unloaded collision wall\")
              (claim (collides 10000 10000 2) \"physics uses resident source\")
              (start-streaming 16 16)
              (edit-world-cell 16 16 3 0)
              (claim (>= changed 4) \"boundary edit updates halos\")
              (loop for step from 0 to 219 do
                (update-streaming (* step 16) (* step -16))
                (claim (<= (@ chunks size) 169) \"bounded travel residency\"))
              (update-streaming 16000 16000)
              (claim (not ((@ chunks has) \"0,0\")) \"teleport discards old demand\")
              (start-streaming 256 256)
              (loop for n from 0 to 79 do (update-streaming 256 256))
              (claim (= (source-cell-at 16 16 3) 0) \"zero edit persists after reload\")
              (start-streaming 16 16)
              (claim (= (chunk-cell-at ((@ chunks get) \"1,1\") 16 16 3) 0) \"zero replayed into resident data\")
              (claim (> removed 0) \"eviction callback\")
              ;; Fixtures from the authored domain include both structure and air.
              (claim (= (source-cell-at 24 24 16) (or ((@ cells get) \"24,24,16\") 0)) \"authored material\")
              (claim (= (source-cell-at 0 0 63) 0) \"authored air\")
              (stop-streaming)
              (claim (and (not streaming-enabled) (null cell-source) (= (@ chunks size) 0)) \"stop teardown\")
              ((@ console log) \"streaming claims passed\"))))")))))

(define-test browser-streaming-contract
  (uiop:with-temporary-file (:pathname path :stream stream :type "mjs")
    (write-string (streaming-claims) stream)
    (finish-output stream)
    (multiple-value-bind (output errors code)
        (uiop:run-program (list "node" (namestring path))
                          :output :string :error-output :string :ignore-error-status t)
      (unless (zerop code) (error "Streaming browser claims failed:~%~A" errors))
      (true (search "streaming claims passed" output)))))

(define-test packed-chunk-mesh-preserves-atlas
  (uiop:with-temporary-file (:pathname path :stream stream :type "mjs")
    (write-string "import assert from 'node:assert/strict';" stream)
    (terpri stream)
    (write-string
     (parenscript:ps* `(progn
                        (defvar web::atlas ,(web::array-form (web::atlas-data)))
                        ,(web::core-form) ,(web::meshing-form))) stream)
    (write-string
     "
for (let star=0;star<256;star++) {
  const kinds=Array.from({length:8},(_,i)=>(star & (1<<i)) ? 1+i%5 : 0);
  const sampler=(x,y,z)=>kinds[(x===-32?1:0)+(y===48?2:0)+(z===-2?4:0)];
  const mesh=meshData([[-32,48,-2,star]],sampler);
  assert.equal(mesh.positions.length,atlas[star][0].length*9);
  assert.equal(mesh.normals.length,mesh.positions.length);
  assert.equal(mesh.colors.length,mesh.positions.length);
  assert.ok(mesh.normals instanceof Int8Array && mesh.colors instanceof Uint8Array);
  atlas[star][0].forEach((triangle,i)=>{
    const [a,b,c]=triangle;
    const u=b.map((v,j)=>v-a[j]),v=c.map((v,j)=>v-a[j]);
    const n=[u[1]*v[2]-u[2]*v[1],u[2]*v[0]-u[0]*v[2],u[0]*v[1]-u[1]*v[0]];
    const length=Math.hypot(...n), up=Math.max(0,n[2]/length);
    const mask=atlas[star][1][i][0], selected=kinds.filter((_,j)=>mask&(1<<j));
    const color=[0,1,2].map(axis=>selected.reduce((sum,kind)=>sum+cellTone(kind,up)[axis],0)/Math.max(1,selected.length));
    triangle.forEach((p,j)=>p.forEach((value,axis)=>{
      const offset=i*9+j*3+axis;
      assert.equal(mesh.positions[offset],[-32,48,-2][axis]+value/8,'exact owned geometry');
      assert.ok(Math.abs(mesh.normals[offset]/127-n[axis]/length)<.004,'winding and packed normal');
      assert.ok(Math.abs(mesh.colors[offset]/255-color[axis])<.002,'native appearance-mask mean');
    }));
  });
}
console.log('256 packed mesh fixtures passed');
" stream)
    (finish-output stream)
    (multiple-value-bind (output errors code)
        (uiop:run-program (list "node" (namestring path))
                          :output :string :error-output :string :ignore-error-status t)
      (unless (zerop code) (error "Packed mesh claims failed:~%~A" errors))
      (true (search "256 packed mesh fixtures passed" output)))))
