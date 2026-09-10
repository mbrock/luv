(defpackage #:luft.web.worker-tests
  (:use #:cl)
  (:import-from #:parachute #:define-test #:true)
  (:local-nicknames (#:ps #:parenscript)
                    (#:web #:luft.web)))

(in-package #:luft.web.worker-tests)

(defun javascript-string (string)
  "A JavaScript string literal; CL's ~S does not escape literal newlines."
  (with-output-to-string (output)
    (write-char #\" output)
    (loop for character across string do
      (write-string (case character
                      (#\\ "\\\\")
                      (#\" "\\\"")
                      (#\Newline "\\n")
                      (#\Return "\\r")
                      (#\Tab "\\t")
                      (otherwise (string character)))
                    output))
    (write-char #\" output)))

(defun worker-claims ()
  "Node claims for the generated CPU worker and its main-thread transport."
  (let ((worker (web::worker-javascript))
        (runtime
          (ps:ps*
           `(progn
              (defvar web::atlas ,(web::array-form (web::atlas-data)))
              (defvar initial-cells ,(web::array-form (web::demo-cells)))
              ,(web::core-form)
              ,(web::generation-form)
              ,(web::meshing-form))))
        (transport
          (ps:ps*
           `(progn
              (defvar initial-cells ,(web::array-form (web::demo-cells)))
              ,(web::core-form)
              ,(web::streaming-form)
              ,(web::worker-transport-form)))))
    (with-output-to-string (source)
      (format source "import assert from 'node:assert/strict';~%")
      (format source "import { Worker as NodeWorker } from 'node:worker_threads';~%")
      (format source "const workerSource = ~A;~%" (javascript-string worker))
      (format source "const runtimeSource = ~A;~%" (javascript-string runtime))
      (format source "const transportSource = ~A;~%" (javascript-string transport))
      (write-string
       "
assert.doesNotMatch(workerSource, /\\b(?:THREE|document|window)\\b/i,
                    'CPU worker has no Three or DOM dependency');
const synchronousRuntime = new Function(runtimeSource + `
  return {
    reset() { resetCells(); },
    cells() { return Array.from(cells); },
    key: cellKey,
    generate(cx, cy, edits) {
      worldEdits = new Map(edits);
      const chunk = makeChunk(cx, cy);
      return { chunk, product: meshData(chunkSites(chunk),
                                        (x, y, z) => chunkCellAt(chunk, x, y, z)) };
    }
  };`)();
synchronousRuntime.reset();

const bootstrap = `
  const { parentPort } = require('node:worker_threads');
  globalThis.self = {
    postMessage(value, transfer) { parentPort.postMessage(value, transfer); }
  };
  parentPort.on('message', data => globalThis.self.onmessage({ data }));
  ${workerSource}`;

function equalArray(actual, expected, label) {
  assert.deepEqual(Array.from(actual), Array.from(expected), label);
}
function workerChunk(cx, cy, edits) {
  return new Promise((resolve, reject) => {
    const thread = new NodeWorker(bootstrap, { eval: true });
    thread.once('error', reject);
    thread.once('message', reply => {
      thread.terminate().then(() => resolve(reply), reject);
    });
    thread.postMessage({ type: 'init', cells: synchronousRuntime.cells() });
    thread.postMessage({ type: 'chunk', cx, cy, epoch: 7, edits });
  });
}
for (const fixture of [
  ['authored', 0, 0, []],
  ['negative', -1, -1, []],
  ['edited left halo', 0, 0, [[synchronousRuntime.key(16, 8, 3), 5]]],
  ['edited right owner', 1, 0, [[synchronousRuntime.key(16, 8, 3), 5]]],
]) {
  const [name, cx, cy, edits] = fixture;
  const expected = synchronousRuntime.generate(cx, cy, edits);
  const reply = await workerChunk(cx, cy, edits);
  assert.equal(reply.epoch, 7, name + ' epoch');
  assert.ok(reply.chunk.data instanceof Uint8Array, name + ' transferred cells');
  assert.ok(reply.product.positions instanceof Float32Array, name + ' transferred positions');
  assert.ok(reply.product.normals instanceof Int8Array, name + ' transferred normals');
  assert.ok(reply.product.colors instanceof Uint8Array, name + ' transferred colors');
  for (const value of [reply.chunk.data, reply.product.positions,
                       reply.product.normals, reply.product.colors])
    assert.ok(value.buffer instanceof ArrayBuffer && value.buffer.byteLength > 0,
              name + ' transferred buffer');
  equalArray(reply.chunk.data, expected.chunk.data, name + ' cells');
  equalArray(reply.product.positions, expected.product.positions, name + ' positions');
  equalArray(reply.product.normals, expected.product.normals, name + ' normals');
  equalArray(reply.product.colors, expected.product.colors, name + ' colors');
}

class MockWorker {
  static instances = [];
  constructor() { this.posts = []; this.terminated = false; MockWorker.instances.push(this); }
  postMessage(message) { this.posts.push(message); }
  terminate() { this.terminated = true; }
  message(reply) { this.onmessage({ data: reply }); }
  error(message) { this.onerror({ message }); }
}
const transport = new Function('Worker', 'setStatus',
  transportSource + `
  installWorkerGeneration();
  return {
    state: workerState,
    request: requestWorkerChunk,
    edit: editWorldCell,
    update: updateStreaming,
    stop: stopStreaming,
    enable(x, y) { streamingEnabled = true; demandX = x; demandY = y; },
    demand(x, y) { demandX = x; demandY = y; },
    resident(chunk) { chunks.set(chunk.key, chunk); },
    size() { return chunks.size; }
  };`)(MockWorker, () => {});
transport.enable(0, 0);
transport.request(0, 0);
const mocked = MockWorker.instances[0];
assert.deepEqual(mocked.posts.map(job => job.type), ['init', 'chunk'], 'worker initializes then receives one job');
transport.request(1, 0);
assert.equal(mocked.posts.length, 2, 'one outstanding job is bounded');
const oldEpoch = transport.state.epoch;
assert.equal(transport.edit(0, 0, 1, 2), true, 'edit accepted');
mocked.message({ epoch: oldEpoch, chunk: { cx: 0, cy: 0, key: '0,0', data: new Uint8Array(1) },
                 product: { positions: new Float32Array(), normals: new Int8Array(), colors: new Uint8Array() } });
assert.equal(transport.state.discarded, 1, 'edit epoch invalidates in-flight reply');
transport.request(0, 0);
transport.demand(99, 99);
mocked.message({ epoch: transport.state.epoch, chunk: { cx: 0, cy: 0, key: '0,0', data: new Uint8Array(1) },
                 product: { positions: new Float32Array(), normals: new Int8Array(), colors: new Uint8Array() } });
assert.equal(transport.state.discarded, 2, 'teleport rejects old-demand reply');
transport.demand(0, 0);
transport.resident({ cx: 0, cy: 0, key: '0,0', x0: 0, y0: 0, data: new Uint8Array(1), dirty: true });
transport.update(0, 0);
const remesh = mocked.posts.at(-1);
assert.deepEqual([remesh.cx, remesh.cy], [0, 0], 'dirty resident remesh precedes expansion');
transport.stop();
assert.ok(mocked.terminated, 'stop terminates worker');
transport.enable(0, 0);
transport.request(0, 0);
const failed = MockWorker.instances.at(-1);
failed.error('mock load failure');
assert.equal(transport.state.error, 'mock load failure', 'worker error becomes transport state');
const workersBeforeRetry = MockWorker.instances.length;
transport.request(1, 0);
assert.equal(MockWorker.instances.length, workersBeforeRetry, 'error state blocks further work');
console.log('worker thread and transport claims passed');
" source)
      source)))

(define-test browser-worker-contract
  (uiop:with-temporary-file (:pathname path :stream stream :type "mjs")
    (write-string (worker-claims) stream)
    (finish-output stream)
    (multiple-value-bind (output errors code)
        (uiop:run-program (list "node" (namestring path))
                          :output :string :error-output :string :ignore-error-status t)
      (unless (zerop code) (error "Worker browser claims failed:~%~A" errors))
      (true (search "worker thread and transport claims passed" output)))))
