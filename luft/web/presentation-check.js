// Run on a fresh ready demo. Checks real GPU fog/AO composition and selection
// orientation; reload afterward to restore gameplay.
(async () => {
  const d = window.luftDemo;
  const {DataUtils} = await import('three');
  const assert = (ok, message) => { if (!ok) throw Error(message); };
  assert(d?.ready, 'Wait for the demo');
  const frame = requestAnimationFrame.bind(window);
  window.requestAnimationFrame = () => 0;
  await new Promise(r => frame(() => frame(r)));
  d.cells.clear();
  d.cells.set('24,24,2', 2);
  d.rebuild();
  const faces = [[1,0,0], [-1,0,0], [0,1,0], [0,-1,0], [0,0,1], [0,0,-1]];
  const center = [24.5,24.5,2.5];
  for (const normal of faces) {
    d.camera.position.set(...center.map((c, i) => c + normal[i] * 3));
    d.camera.lookAt(...center);
    d.aim();
    assert(d.selection.visible === true, 'Selected face visible');
    const position = d.selection.position.toArray();
    assert(position.every((p, i) => Math.abs(p - center[i] - normal[i] * .502) < 1e-6), 'Inset on entry face');
    const actualNormal = d.selection.position.clone().set(0,0,1).applyQuaternion(d.selection.quaternion);
    assert(actualNormal.toArray().every((v,i) => Math.abs(v-normal[i]) < 1e-6), 'Face orientation');
  }
  d.camera.position.set(24.5,18,2.5);
  d.camera.lookAt(24.5,0,2.5);
  d.aim();
  assert(d.selection.visible === false, 'No stale selection when aiming into air');

  // A fully fogged wall/floor should not reappear when SSAO is enabled.
  for(let x=18;x<32;x++) for(let y=18;y<32;y++) {
    d.cells.set(`${x},${y},0`,2);
    if(y>=24) for(let z=1;z<5;z++) d.cells.set(`${x},${y},${z}`,2);
  }
  d.rebuild();
  d.camera.position.set(26,20,2.6);
  d.camera.lookAt(26,25,1.2);
  d.camera.aspect = 1;
  d.camera.updateProjectionMatrix();
  d.renderer.setSize(256,256,false);
  d.composer.setSize(256,256);
  d.camera.updateMatrixWorld();
  const scene = d.composer.passes[0].scene;
  scene.fog.near = 1; scene.fog.far = 2;
  d.composer.passes[2].enabled = false;
  function pixels(ao) {
    d.occlusion.enabled = ao;
    d.composer.render();
    const target = d.composer.writeBuffer;
    const array = new Uint16Array(target.width*target.height*4);
    d.renderer.readRenderTargetPixels(target, 0,0,target.width,target.height,array);
    const values = Array.from(array, DataUtils.fromHalfFloat);
    assert(values[3] > .9 && values[0] > .01, 'Read actual nonblack HDR pixels');
    return values;
  }
  const without = pixels(false), withAO = pixels(true);
  let maxDifference = 0;
  for(let i=0;i<withAO.length;i++) {
    assert(Number.isFinite(withAO[i]), 'Finite HDR output');
    maxDifference = Math.max(maxDifference, Math.abs(withAO[i]-without[i]));
  }
  assert(maxDifference < .002, 'AO must not reveal fully fogged terrain');
  scene.fog.near = 100; scene.fog.far = 120;
  const clearWithout = pixels(false), clearAO = pixels(true);
  let clearDifference = 0;
  for(let i=0;i<clearAO.length;i++) clearDifference=Math.max(clearDifference,Math.abs(clearAO[i]-clearWithout[i]));
  assert(clearDifference > .005, 'Fixture exercises visible near-field AO');
  assert(d.renderer.info.programs.every(p=>!p.diagnostics || p.diagnostics.runnable), 'Shader compilation');
  return {passed:true, selectedFaces:faces.length, foggedAODifference:maxDifference, clearAODifference:clearDifference};
})()
