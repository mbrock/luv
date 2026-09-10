// Run on a fresh, ready demo with agent-browser eval "$(cat luft/web/streaming-check.js)".
// Exercises actual GPU product replacement/disposal; reload afterward.
(async () => {
  const d = window.luftDemo;
  if (!d?.ready) throw Error('Wait for luftDemo.ready');
  const frame = window.requestAnimationFrame.bind(window);
  window.requestAnimationFrame = () => 0;
  await new Promise(resolve => frame(() => frame(resolve)));
  const assert = (condition, message) => { if (!condition) throw Error(message); };
  async function drain(x, y) {
    const until = performance.now() + 90000;
    while (performance.now() < until) {
      d.updateStreaming(x, y);
      if (d.workerState.error) throw Error(d.workerState.error);
      if (!d.workerState.busy && d.chunks.size >= 121 &&
          [...d.chunks.values()].every(c => !c.dirty)) return;
      await new Promise(resolve => setTimeout(resolve, 10));
    }
    throw Error('Worker stream did not settle');
  }
  async function settle(x, y) {
    let height = 64;
    while (height > 0 && !d.source(x, y, height - 1)) height--;
    d.camera.position.set(x, y, height + 1.62);
    await drain(x, y);
    d.updateEnvironment();
    d.composer.render();
    assert(d.chunks.size <= 169, 'resident CPU bound');
    assert(d.meshes().length === d.chunks.size, 'exactly one GPU product per resident chunk');
    assert(d.renderer.info.programs.every(p => !p.diagnostics || p.diagnostics.runnable), 'shader compile');
    const attributes = d.meshes().flatMap(m => Object.values(m.geometry.attributes));
    assert(attributes.every(a => a.array.every(Number.isFinite)), 'finite geometry');
    return {chunks: d.chunks.size, geometries: d.renderer.info.memory.geometries,
      meshBytes: attributes.reduce((n, a) => n + a.array.byteLength, 0)};
  }
  const initial = await settle(23, 20);
  const oldMeshes = new Map([...d.chunks].map(([key, chunk]) => [key, chunk.mesh]));
  d.editWorldCell(31, 31, 30, 4);
  await drain(23, 20);
  const changed = [...d.chunks].filter(([key, chunk]) => chunk.mesh !== oldMeshes.get(key)).length;
  assert(changed === 4, 'boundary edit remeshes four owners/halos');
  d.editWorldCell(23, 20, 8, 0);
  const samples = [initial];
  for (const [x, y] of [[256, 256], [-256, -256], [512, -128], [23, 20]])
    samples.push(await settle(x, y));
  const chunk = d.chunks.get('1,1');
  const read = (x, y, z) => chunk.data[z + 64 * (x - chunk.x0 + 1 + 18 * (y - chunk.y0 + 1))];
  assert(read(31, 31, 30) === 4 && read(23, 20, 8) === 0, 'edits replay into reloaded chunk data');
  assert(d.source(31, 31, 30) === 4 && d.source(23, 20, 8) === 0, 'source overlay persists');
  d.stopStreaming();
  d.composer.render();
  assert(d.chunks.size === 0 && d.meshes().length === 0, 'complete residency teardown');
  assert(d.renderer.info.memory.geometries <= 4, 'terrain GPU geometries disposed');
  assert(!d.renderer.getContext().isContextLost(), 'WebGL context intact');
  return {passed: true, changed, samples, geometriesAfterTeardown: d.renderer.info.memory.geometries};
})()
