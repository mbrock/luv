(defpackage #:luft.web.tests
  (:use #:cl)
  (:import-from #:parachute #:define-test #:true)
  (:local-nicknames (#:web #:luft.web)))

(in-package #:luft.web.tests)

(defun occupancy-fixtures ()
  "Every 2x2x2 occupancy, translated off the origin into negative coordinates."
  (loop for mask below 256
        for cells = (make-hash-table :test #'equal)
        do (dotimes (sample 8)
             (when (logbitp sample mask)
               (setf (gethash (list (- (ldb (byte 1 0) sample) 3)
                                   (+ (ldb (byte 1 1) sample) 5)
                                   (- (ldb (byte 1 2) sample) 2))
                              cells) 1)))
        collect
        (list (loop for cell being the hash-keys of cells
                    collect (append cell '(1)))
              (loop for site being the hash-keys of (luft:star-surface-sites cells)
                    using (hash-value star)
                    collect (append site (list star))))))

(defun browser-claims ()
  (parenscript:ps*
   `(progn
      (defvar luft.web::initial-cells (parenscript:array))
      ,(web::core-form)
      (defvar fixtures ,(web::array-form (occupancy-fixtures)))
      ,(let ((*package* (find-package '#:luft.web)))
         (read-from-string
          "(progn
            (defun claim (value message)
              (unless value (throw (new (|Error| message)))))
            ((@ fixtures for-each)
             (lambda (fixture)
               (setf initial-cells (aref fixture 0))
               (reset-cells)
               (let ((sites (surface-sites)) (expected (aref fixture 1)))
                 (claim (= (@ sites size) (@ expected length)) \"site count\")
                 ((@ expected for-each)
                  (lambda (site)
                    (let ((actual ((@ sites get) (cell-key (aref site 0) (aref site 1) (aref site 2)))))
                      (claim (and actual (= (aref actual 3) (aref site 3))) \"native star parity\")))))))
            ((@ cells clear))
            ((@ cells set) (cell-key 2 0 0) 2)
            (let ((hit (trace-cells (array 0.5 0.5 0.5) (array 1 0 0) 4)))
              (claim (= (aref (@ hit cell) 0) 2) \"axis-aligned hit\")
              (claim (= (aref (@ hit previous) 0) 1) \"placement neighbor\"))
            (claim (= (trace-cells (array 0.5 0.5 0.5) (array -1 0 0) 4) null) \"miss\")
            (claim (= (trace-cells (array 0.5 0.5 0.5) (array 1 0 0) 1) null) \"reach\")
            (claim (collides 2.5 0.5 0) \"body intersects solid\")
            (claim (not (collides 2.5 0.5 1)) \"standing above solid\")
            (claim (not (collides 0.5 0.5 0)) \"empty body\")
            ((@ console log) \"256 native occupancy fixtures and browser picking/collision passed\"))")))))

(define-test browser-star-selection-matches-native
  ;; Execute the actual compiled browser code, not a second Lisp port of it.
  (uiop:with-temporary-file (:pathname path :stream stream :type "mjs")
    (write-string (browser-claims) stream)
    (finish-output stream)
    (multiple-value-bind (output errors code)
        (uiop:run-program (list "node" (namestring path))
                          :output :string :error-output :string
                          :ignore-error-status t)
      (unless (zerop code) (error "Browser claims failed:~%~A" errors))
      (true (search "passed" output)))))

(define-test demo-publishes-without-a-native-gpu
  (let ((resources (web:demo-resources nil)))
    (true (= 3 (length resources)))
    (true (search "importmap" (web::demo-html)))
    (true (search "luft-demo.js" (web::demo-html)))
    (true (> (length (web::demo-cells)) 10000))))

(define-test minimal-walking-interface
  (let ((html (web::demo-html)) (javascript (web:demo-javascript)))
    (dolist (removed '("<header" "<aside" "<nav" "Wireframe" "Overview" "Walk here"))
      (true (not (search removed html))))
    (dolist (removed '("OrbitControls" "wireframe" "enterWalk"))
      (true (not (search removed javascript))))
    (dolist (control '("id=metrics" "id=materials" "id=movement" "id=actions"))
      (true (search control html)))))

(define-test pointer-controls-and-walking
  ;; Run the actual generated client with a small DOM stand-in, omitting only
  ;; START's GPU/CDN initialization. Exercise handlers, not a second input model.
  (uiop:with-temporary-file (:pathname path :stream stream :type "mjs")
    (write-string
     "import assert from 'node:assert/strict';
const buttons = [];
function node(key) {
  const classes = new Set();
  return {dataset: {key}, handlers: {}, textContent: '',
    addEventListener(name, fn) {this.handlers[name] = fn;},
    setPointerCapture() {}, focus() {},
    style: {setProperty() {}},
    getBoundingClientRect() {return {left:0,top:0,width:96,height:96};},
    classList: {add: x => classes.add(x), remove: x => classes.delete(x),
      toggle(x, on) {on ? classes.add(x) : classes.delete(x);}},
    classes};
}
const nodes = new Map(['world','status','remove','place','movement','crosshair'].map(id => [id,node()]));
nodes.get('remove').dataset.place = 'false';
nodes.get('place').dataset.place = 'true';
for (const key of ['KeyW','KeyA','KeyS','KeyD','Space']) buttons.push(node(key));
globalThis.window = {matchMedia: () => ({matches: true})};
globalThis.document = {pointerLockElement: null, handlers: {},
  addEventListener(name, fn) {this.handlers[name] = fn;},
  getElementById: id => nodes.get(id),
  querySelectorAll: selector => selector === 'button[data-key]' ? buttons : selector === 'button[data-place]' ? [nodes.get('remove'),nodes.get('place')] : buttons.filter(b => b.classes.has('held'))};
function fire(node, type, id, extra = {}) {
  node.handlers[type]({pointerId: id, pointerType: 'touch', button: 0,
    clientX: 0, clientY: 0, preventDefault() {}, ...extra});
}
" stream)
    (write-string (parenscript:ps* `(progn ,(web::core-form)
                                         ,@(butlast (cdr (web::client-form))))) stream)
    (write-string
     "
let edits = [], captures = 0;
const realEditCell = editCell;
camera = {position: {x: 23, y: 20, z: 11.62,
  set(x,y,z) {Object.assign(this,{x,y,z});}}, lookAt() {}};
aim = () => {};
editCell = place => edits.push(place);
canvas.requestPointerLock = () => {captures++;};
bindPointerControls();
spawnPlayer();
const pad = nodes.get('movement');
fire(pad, 'pointerdown', 10, {clientX:48,clientY:48});
assert.equal(stickX,0);
assert.equal(stickY,0);
fire(pad, 'pointermove', 10, {clientX:64,clientY:32});
assert.ok(stickX>0 && stickX<1 && stickY<0 && stickY>-1, 'analog diagonal');
fire(pad, 'pointermove', 11, {clientX:48,clientY:48});
assert.ok(stickX>0, 'other finger cannot steal stick');
fire(pad, 'pointermove', 10, {clientX:48,clientY:-100});
assert.equal(stickY,-1, 'clamp full deflection');
fire(pad, 'lostpointercapture', 10);
assert.equal(stickY,0);
assert.equal(camera.position.x, 23);
fire(buttons[0], 'pointerdown', 1);
fire(canvas, 'pointerdown', 2);
fire(canvas, 'pointermove', 2, {clientX: 80, clientY: 30});
assert.ok(yaw > 0 && pitch < 0, 'look while holding movement');
const before = camera.position.y;
movePlayer(.04);
assert.ok(camera.position.y > before, 'touch movement drives player physics');
fire(buttons[4], 'pointerdown', 3);
grounded = true;
movePlayer(.01);
assert.ok(velocity > 0, 'jump while moving and looking');
fire(buttons[0], 'pointercancel', 1);
assert.equal(pressed('KeyW'), false);
fire(buttons[4], 'lostpointercapture', 3);
assert.equal(pressed('Space'), false);
fire(canvas, 'pointercancel', 2);
const cancelledYaw = yaw;
fire(canvas, 'pointermove', 2, {clientX: 200});
assert.equal(yaw, cancelledYaw, 'cancelled look stops');
fire(buttons[0], 'pointerdown', 4);
fire(buttons[0], 'pointerdown', 5);
fire(buttons[0], 'pointerup', 4);
assert.equal(pressed('KeyW'), true, 'second finger still holds key');
keys.add('KeyD');
clearInput();
assert.equal(pressed('KeyW'), false);
assert.equal(pressed('KeyD'), false);
assert.ok(buttons.every(b => !b.classes.has('held')), 'blur clears feedback');
fire(canvas, 'pointerdown', 6);
fire(canvas, 'pointerup', 6);
assert.equal(captures, 0, 'touch never requests mouse capture');
fire(canvas, 'pointerdown', 7, {pointerType: 'mouse'});
fire(canvas, 'pointerup', 7, {pointerType: 'mouse'});
assert.equal(captures, 1);
assert.deepEqual(edits, [], 'capture click does not edit');
document.pointerLockElement = canvas;
fire(canvas, 'pointerdown', 8, {pointerType: 'mouse', button: 0});
fire(document, 'pointerup', 8);
fire(canvas, 'pointerdown', 8, {pointerType: 'mouse', button: 2});
fire(document, 'pointerup', 8);
nodes.get('remove').handlers.click({detail:0});
nodes.get('place').handlers.click({detail:0});
assert.deepEqual(edits, [false,true,false,true]);
fire(nodes.get('remove'), 'pointerdown', 12);
const deadline = editNext;
assert.equal(edits.length,5,'immediate smash');
repeatEdit(deadline-1);
assert.equal(edits.length,5,'repeat throttled');
repeatEdit(deadline);
assert.equal(edits.length,6,'held smash repeats');
nodes.get('remove').handlers.click({detail:1});
assert.equal(edits.length,6,'no duplicate synthetic click');
fire(document,'pointercancel',12);
repeatEdit(deadline+1000);
assert.equal(edits.length,6,'cancel stops repeats');
fire(nodes.get('place'),'pointerdown',13);
clearInput();
repeatEdit(editNext+1000);
assert.equal(edits.length,7,'blur stops building');
document.pointerLockElement = null;
const releasedPosition = {...camera.position};
keys.add('KeyW');
movePlayer(.04);
assert.ok(camera.position.y > releasedPosition.y, 'walking survives mouse release');
editCell = realEditCell;
let rebuilds = 0;
rebuild = () => rebuilds++;
camera.position.set(.5,.5,2.62);
target = {cell:[0,1,1],previous:[0,0,1]};
assert.equal(editCell(true),false,'cannot build inside player');
assert.equal(cells.size,0);
assert.equal(rebuilds,0,'blocked build avoids remeshing');
target.previous = [2,0,1];
assert.equal(editCell(true),true);
assert.equal(cells.get('2,0,1'),selected);
target.cell = [2,0,1];
assert.equal(editCell(false),true);
assert.equal(cells.size,0);
assert.equal(rebuilds,2);
console.log('pointer and walking claims passed');
" stream)
    (finish-output stream)
    (multiple-value-bind (output errors code)
        (uiop:run-program (list "node" (namestring path))
                          :output :string :error-output :string
                          :ignore-error-status t)
      (unless (zerop code) (error "Input claims failed:~%~A" errors))
      (true (search "claims passed" output)))))

(define-test complete-demo-is-valid-javascript-module
  (uiop:with-temporary-file (:pathname path :stream stream :type "mjs")
    (write-string (web:demo-javascript) stream)
    (finish-output stream)
    (multiple-value-bind (output errors code)
        (uiop:run-program (list "node" "--check" (namestring path))
                          :output :string :error-output :string
                          :ignore-error-status t)
      (declare (ignore output))
      (unless (zerop code) (error "Demo compilation failed:~%~A" errors))
      (true (zerop code)))))
