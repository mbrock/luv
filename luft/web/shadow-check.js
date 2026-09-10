// Browser GPU regression, not application code. Run with the demo open:
// agent-browser eval "$(cat luft/web/shadow-check.js)"
// Replaces the world with a wall/floor fixture and stops animation. Reload after.
(async () => {
  const d = window.luftDemo;
  if (!d?.ready) throw new Error('Wait for luftDemo.ready first');
  const frame = window.requestAnimationFrame.bind(window);
  window.requestAnimationFrame = () => 0;
  await new Promise(resolve => frame(() => frame(resolve)));
  d.occlusion.enabled = false;
  d.composer.passes[2].enabled = false;
  // Startup fog tracks residency and can otherwise mask the whole fixture.
  d.composer.passes[0].scene.fog = null;
  d.cells.clear();
  for (let x = 18; x < 32; x++) {
    for (let y = 18; y < 32; y++) {
      d.cells.set(`${x},${y},0`, 2);
      if (x < 24) for (let z = 1; z < 4; z++) d.cells.set(`${x},${y},${z}`, 2);
    }
  }
  d.rebuild();
  const material = d.meshes()[0].material;
  const fixed = {side: material.shadowSide, bias: d.sun.shadow.bias,
    normalBias: d.sun.shadow.normalBias};
  if (fixed.side !== 0) throw new Error('Terrain must cast front-face shadows');
  const intensity = d.sun.intensity;
  const pixels = new Uint16Array(4);
  const half = h => (h & 32768 ? -1 : 1) * ((h >> 10 & 31) === 0
    ? (h & 1023) * 2 ** -24 : 2 ** ((h >> 10 & 31) - 15) * (1 + (h & 1023) / 1024));
  function red(x, y, z) {
    // OutputPass writes to screen, then EffectComposer swaps its buffers.
    // The HDR input just presented is therefore now named writeBuffer.
    const target = d.composer.writeBuffer;
    const p = d.camera.position.clone().set(x, y, z).project(d.camera);
    const px = Math.floor((p.x * .5 + .5) * target.width);
    const py = Math.floor((p.y * .5 + .5) * target.height);
    if (px < 0 || px >= target.width || py < 0 || py >= target.height)
      throw new Error('Probe outside render target');
    d.renderer.readRenderTargetPixels(target, px, py, 1, 1, pixels);
    return half(pixels[0]);
  }
  const contacts = [[24.16, 24, 1], [24.20, 24, 1], [24.16, 26, 1]];
  const tops = [[20, 22, 4], [20.03, 22, 4], [20.1, 22, 4], [20.3, 22, 4]];
  const results = {};
  try {
    for (const [name, settings] of Object.entries({
      old: {side: null, bias: -.0001, normalBias: .025}, fixed
    })) {
      material.shadowSide = settings.side;
      d.sun.shadow.bias = settings.bias;
      d.sun.shadow.normalBias = settings.normalBias;
      d.sun.shadow.needsUpdate = true;
      const contactRatios = [], topRatios = [];
      for (const shift of [0, .03, .1, .3]) {
        d.camera.position.set(31 + shift, 14, 10);
        d.camera.lookAt(24, 24, 1.5);
        d.sun.intensity = 0;
        d.composer.render();
        const ambient = contacts.map(p => red(...p));
        d.sun.intensity = intensity;
        d.composer.render();
        contacts.forEach((p, i) => contactRatios.push(red(...p) / ambient[i]));
        const shadowedTops = tops.map(p => red(...p));
        d.meshes().forEach(m => { m.receiveShadow = false; });
        d.composer.render();
        tops.forEach((p, i) => topRatios.push(shadowedTops[i] / red(...p)));
        d.meshes().forEach(m => { m.receiveShadow = true; });
      }
      results[name] = {maxContactLightRatio: Math.max(...contactRatios),
        minLitSurfaceRatio: Math.min(...topRatios)};
    }
    // Brighter diffuse fill reduces the old leak's relative contrast. Still
    // require a clear positive control, four times the fixed 5% tolerance.
    if (!(results.old.maxContactLightRatio > 1.2))
      throw new Error(`Fixture failed to reproduce the old light leak: ${JSON.stringify(results)}`);
    if (!(results.fixed.maxContactLightRatio < 1.05))
      throw new Error(`Contact light leak: ${JSON.stringify(results)}`);
    if (!(results.fixed.minLitSurfaceRatio > .98))
      throw new Error(`Shadow acne: ${JSON.stringify(results)}`);
    return {passed: true, ...results};
  } finally {
    material.shadowSide = fixed.side;
    d.sun.shadow.bias = fixed.bias;
    d.sun.shadow.normalBias = fixed.normalBias;
    d.sun.intensity = intensity;
    d.sun.shadow.needsUpdate = true;
    d.meshes().forEach(m => { m.receiveShadow = true; });
    d.camera.position.set(31, 14, 10);
    d.camera.lookAt(24, 24, 1.5);
    d.composer.render();
  }
})()
