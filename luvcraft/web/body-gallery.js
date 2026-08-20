const canvas = document.querySelector("#body-canvas");
const status = document.querySelector("#status");
const title = document.querySelector("#body-title");
const picker = document.querySelector("#body-picker");
const knobsElement = document.querySelector("#knobs");
const resetButton = document.querySelector("#reset");

const FRAME_FLOATS = 76;
const FRAME_INTERVAL = 1000 / 60;
const RENDER_SCALE = 1;
const QUAD = new Float32Array([
  0, 0, 0,  1, 0, 0,  1, 1, 0,
  0, 0, 0,  1, 1, 0,  0, 1, 0,
]);
const frameData = new Float32Array(FRAME_FLOATS);
const instanceData = new Float32Array(4);
const sunLength = Math.hypot(0.55, 0.82, 0.28);
const sunX = 0.55 / sunLength;
const sunY = 0.82 / sunLength;
const sunZ = 0.28 / sunLength;

let device;
let context;
let format;
let frameBuffer;
let quadBuffer;
let instanceBuffer;
let facingBuffer;
let depthTexture;
let depthView;
let pipeline;
let bindGroup;
let catalog;
let selectedBody;
let currentValues = new Map();
const moduleCache = new Map();
let yaw = 0.32;
let elevation = 0.12;
let distance = 4.1;
let dragging = false;
let previousPointer = null;
let pipelineGeneration = 0;
let lastFrameTime = -Infinity;
let canvasSizeDirty = true;
let cameraBufferDirty = true;
let instanceBufferDirty = true;

function setStatus(message, failed = false) {
  status.textContent = message;
  status.style.color = failed ? "#ef927f" : "";
}

function writeVec4(array, index, x, y, z, w) {
  const offset = index * 4;
  array[offset] = x;
  array[offset + 1] = y;
  array[offset + 2] = z;
  array[offset + 3] = w;
}

function resizeCanvas() {
  if (!canvasSizeDirty) return false;
  canvasSizeDirty = false;
  const width = Math.max(1, Math.floor(canvas.clientWidth * RENDER_SCALE));
  const height = Math.max(1, Math.floor(canvas.clientHeight * RENDER_SCALE));
  if (canvas.width === width && canvas.height === height) return false;
  canvas.width = width;
  canvas.height = height;
  context.configure({ device, format, alphaMode: "premultiplied" });
  depthTexture?.destroy();
  depthTexture = device.createTexture({
    size: [width, height],
    format: "depth32float",
    usage: GPUTextureUsage.RENDER_ATTACHMENT,
  });
  depthView = depthTexture.createView();
  cameraBufferDirty = true;
  return true;
}

function stature() {
  return currentValues.get(selectedBody.statureKnob) ?? 1;
}

function updateBuffers() {
  const scale = stature();
  const center = selectedBody.centerHeight * scale;
  const radius = selectedBody.radius * scale;
  if (instanceBufferDirty) {
    instanceData[0] = 0;
    instanceData[1] = center;
    instanceData[2] = 0;
    instanceData[3] = radius;
    device.queue.writeBuffer(instanceBuffer, 0, instanceData);
    instanceBufferDirty = false;
  }
  if (!cameraBufferDirty) return;

  const cosElevation = Math.cos(elevation);
  const cameraX = Math.sin(yaw) * cosElevation * distance;
  const cameraY = center + Math.sin(elevation) * distance;
  const cameraZ = Math.cos(yaw) * cosElevation * distance;
  let forwardX = -cameraX;
  let forwardY = center - cameraY;
  let forwardZ = -cameraZ;
  const forwardLength = Math.hypot(forwardX, forwardY, forwardZ) || 1;
  forwardX /= forwardLength;
  forwardY /= forwardLength;
  forwardZ /= forwardLength;
  let rightX = forwardZ;
  let rightZ = -forwardX;
  const rightLength = Math.hypot(rightX, rightZ) || 1;
  rightX /= rightLength;
  rightZ /= rightLength;
  const upX = forwardY * rightZ;
  const upY = forwardZ * rightX - forwardX * rightZ;
  const upZ = -forwardY * rightX;
  const near = 0.05;
  const far = 40;
  const focal = 1 / Math.tan((42 * Math.PI / 180) / 2);
  writeVec4(frameData, 0, cameraX, cameraY, cameraZ, 0);
  writeVec4(frameData, 1, rightX, 0, rightZ, 0);
  writeVec4(frameData, 2, upX, upY, upZ, 0);
  writeVec4(frameData, 3, forwardX, forwardY, forwardZ, 0);
  writeVec4(frameData, 4, focal / (canvas.width / canvas.height), focal,
            far / (far - near), (-far * near) / (far - near));
  writeVec4(frameData, 6, sunX, sunY, sunZ, 1);
  writeVec4(frameData, 7, 2.0, 1.55, 1.15, 0.02);
  writeVec4(frameData, 8, 0.20, 0.27, 0.34, canvas.height);
  writeVec4(frameData, 9, 0.42, 0.33, 0.25, canvas.width);
  writeVec4(frameData, 10, 0.13, 0.16, 0.19, 1);
  device.queue.writeBuffer(frameBuffer, 0, frameData);
  cameraBufferDirty = false;
}

function compilationMessages(info) {
  return info.messages.map(message =>
    `${message.type}: ${message.message} (${message.lineNum}:${message.linePos})`
  ).join("\n");
}

async function checkedShaderModule(label, code) {
  const module = device.createShaderModule({ label, code });
  const info = await module.getCompilationInfo();
  const errors = info.messages.filter(message => message.type === "error");
  if (errors.length) throw new Error(compilationMessages(info));
  return module;
}

function stageConstants(body, values, stage) {
  return Object.fromEntries(
    body.knobs
      .filter(knob => knob.stages.includes(stage))
      .map(knob => [knob.identifier, values.get(knob.name)])
  );
}

function modulesForBody(body) {
  if (!moduleCache.has(body.id)) {
    moduleCache.set(body.id, (async () => {
      const [vertexCode, fragmentCode] = await Promise.all([
        fetch(body.vertexUrl).then(response => response.text()),
        fetch(body.fragmentUrl).then(response => response.text()),
      ]);
      const [vertex, fragment] = await Promise.all([
        checkedShaderModule(`${body.id} vertex`, vertexCode),
        checkedShaderModule(`${body.id} fragment`, fragmentCode),
      ]);
      return { vertex, fragment };
    })());
  }
  return moduleCache.get(body.id);
}

async function rebuildPipeline() {
  const generation = ++pipelineGeneration;
  const body = selectedBody;
  const values = new Map(currentValues);
  setStatus(moduleCache.has(body.id) ? "Retuning this creature…" : "Compiling this creature…");
  const { vertex: vertexModule, fragment: fragmentModule } = await modulesForBody(body);
  const nextPipeline = await device.createRenderPipelineAsync({
    label: `${body.label} body`,
    layout: "auto",
    vertex: {
      module: vertexModule,
      entryPoint: `${body.id}_sdf_vertex_specification`,
      constants: stageConstants(body, values, "vertex"),
      buffers: [
        { arrayStride: 12, attributes: [{ shaderLocation: 0, offset: 0, format: "float32x3" }] },
        { arrayStride: 16, stepMode: "instance", attributes: [{ shaderLocation: 1, offset: 0, format: "float32x4" }] },
        { arrayStride: 16, stepMode: "instance", attributes: [{ shaderLocation: 2, offset: 0, format: "float32x4" }] },
      ],
    },
    fragment: {
      module: fragmentModule,
      entryPoint: `${body.id}_sdf_fragment_specification`,
      constants: stageConstants(body, values, "fragment"),
      targets: [{
        format,
        blend: {
          color: { srcFactor: "src-alpha", dstFactor: "one-minus-src-alpha" },
          alpha: { srcFactor: "one", dstFactor: "one-minus-src-alpha" },
        },
      }],
    },
    primitive: { topology: "triangle-list", cullMode: "none" },
    depthStencil: { format: "depth32float", depthWriteEnabled: true, depthCompare: "less" },
  });
  if (generation !== pipelineGeneration) return;
  pipeline = nextPipeline;
  bindGroup = device.createBindGroup({
    layout: pipeline.getBindGroupLayout(0),
    entries: [{ binding: 2, resource: { buffer: frameBuffer } }],
  });
  setStatus(`${body.knobs.length} live shader knobs`);
}

function render(timestamp) {
  requestAnimationFrame(render);
  if (document.hidden || timestamp - lastFrameTime + 0.25 < FRAME_INTERVAL) return;
  lastFrameTime = timestamp;
  resizeCanvas();
  if (pipeline && selectedBody) {
    updateBuffers();
    const encoder = device.createCommandEncoder();
    const pass = encoder.beginRenderPass({
      colorAttachments: [{
        view: context.getCurrentTexture().createView(),
        clearValue: { r: 0.055, g: 0.065, b: 0.058, a: 1 },
        loadOp: "clear",
        storeOp: "store",
      }],
      depthStencilAttachment: {
        view: depthView,
        depthClearValue: 1,
        depthLoadOp: "clear",
        depthStoreOp: "discard",
      },
    });
    pass.setPipeline(pipeline);
    pass.setBindGroup(0, bindGroup);
    pass.setVertexBuffer(0, quadBuffer);
    pass.setVertexBuffer(1, instanceBuffer);
    pass.setVertexBuffer(2, facingBuffer);
    pass.draw(6, 1);
    pass.end();
    device.queue.submit([encoder.finish()]);
  }
}

function formatValue(knob, value) {
  const decimals = Math.max(0, Math.min(4, (`${knob.step}`.split(".")[1] || "").length));
  return `${value.toFixed(decimals)}${knob.unit}`;
}

function buildKnobs() {
  knobsElement.replaceChildren();
  for (const knob of selectedBody.knobs) {
    const wrapper = document.createElement("section");
    wrapper.className = "knob";
    const line = document.createElement("div");
    line.className = "knob-line";
    const label = document.createElement("label");
    const output = document.createElement("output");
    const input = document.createElement("input");
    const id = `knob-${knob.name}`;
    label.htmlFor = id;
    label.textContent = knob.label;
    input.id = id;
    input.type = "range";
    input.min = knob.minimum;
    input.max = knob.maximum;
    // HTML range steps are based at MIN, but a native knob default is not
    // required to be an integral number of steps above its minimum. Keep the
    // exact native default and quantize around that same origin ourselves.
    input.step = "any";
    input.value = currentValues.get(knob.name);
    output.value = formatValue(knob, Number(input.value));
    input.addEventListener("input", () => {
      const raw = Number(input.value);
      const snapped = knob.default + Math.round((raw - knob.default) / knob.step) * knob.step;
      const value = Math.max(knob.minimum, Math.min(knob.maximum, snapped));
      input.value = value;
      currentValues.set(knob.name, value);
      if (knob.name === selectedBody.statureKnob) {
        instanceBufferDirty = true;
        cameraBufferDirty = true;
      }
      output.value = formatValue(knob, value);
      rebuildPipeline().catch(fail);
    });
    line.append(label, output);
    wrapper.append(line);
    if (knob.documentation) {
      const doc = document.createElement("p");
      doc.textContent = knob.documentation.split("\n")[0];
      wrapper.append(doc);
    }
    wrapper.append(input);
    knobsElement.append(wrapper);
  }
}

async function selectBody(body) {
  selectedBody = body;
  currentValues = new Map(body.knobs.map(knob => [knob.name, knob.default]));
  instanceBufferDirty = true;
  cameraBufferDirty = true;
  title.textContent = body.label;
  for (const button of picker.children) {
    button.setAttribute("aria-current", button.dataset.body === body.id ? "true" : "false");
  }
  buildKnobs();
  await rebuildPipeline();
}

function buildPicker() {
  for (const body of catalog.bodies) {
    const button = document.createElement("button");
    button.type = "button";
    button.dataset.body = body.id;
    button.textContent = body.label;
    button.addEventListener("click", () => selectBody(body).catch(fail));
    picker.append(button);
  }
}

function fail(error) {
  console.error(error);
  setStatus(error.message || String(error), true);
}

async function start() {
  if (!navigator.gpu) throw new Error("WebGPU is unavailable in this browser.");
  const adapter = await navigator.gpu.requestAdapter();
  if (!adapter) throw new Error("No WebGPU adapter is available.");
  device = await adapter.requestDevice();
  device.lost.then(info => fail(new Error(`WebGPU device lost: ${info.message}`)));
  context = canvas.getContext("webgpu");
  format = navigator.gpu.getPreferredCanvasFormat();
  frameBuffer = device.createBuffer({
    size: FRAME_FLOATS * 4,
    usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST,
  });
  quadBuffer = device.createBuffer({
    size: QUAD.byteLength,
    usage: GPUBufferUsage.VERTEX | GPUBufferUsage.COPY_DST,
  });
  instanceBuffer = device.createBuffer({
    size: 16,
    usage: GPUBufferUsage.VERTEX | GPUBufferUsage.COPY_DST,
  });
  facingBuffer = device.createBuffer({
    size: 16,
    usage: GPUBufferUsage.VERTEX | GPUBufferUsage.COPY_DST,
  });
  device.queue.writeBuffer(quadBuffer, 0, QUAD);
  // The body retains a world heading while the camera orbits it, matching the
  // native third-person glance instead of turning to face the viewer.
  device.queue.writeBuffer(facingBuffer, 0, new Float32Array([0, 0, 1, 0]));
  catalog = await fetch("/bodies/bodies.json").then(response => response.json());
  buildPicker();
  resizeCanvas();
  await selectBody(catalog.bodies[0]);
  requestAnimationFrame(render);
}

resetButton.addEventListener("click", () => selectBody(selectedBody).catch(fail));
canvas.addEventListener("pointerdown", event => {
  dragging = true;
  previousPointer = [event.clientX, event.clientY];
  canvas.setPointerCapture(event.pointerId);
});
canvas.addEventListener("pointermove", event => {
  if (!dragging) return;
  yaw -= (event.clientX - previousPointer[0]) * 0.008;
  elevation = Math.max(-0.5, Math.min(0.7,
    elevation + (event.clientY - previousPointer[1]) * 0.006));
  previousPointer = [event.clientX, event.clientY];
  cameraBufferDirty = true;
});
canvas.addEventListener("pointerup", () => { dragging = false; });
canvas.addEventListener("pointercancel", () => { dragging = false; });
canvas.addEventListener("wheel", event => {
  event.preventDefault();
  distance = Math.max(2.0, Math.min(8.0, distance * Math.exp(event.deltaY * 0.001)));
  cameraBufferDirty = true;
}, { passive: false });

new ResizeObserver(() => {
  canvasSizeDirty = true;
}).observe(canvas);

document.addEventListener("visibilitychange", () => {
  lastFrameTime = -Infinity;
});

start().catch(fail);
