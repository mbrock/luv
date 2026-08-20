const canvas = document.querySelector("#body-canvas");
const status = document.querySelector("#status");
const title = document.querySelector("#body-title");
const picker = document.querySelector("#body-picker");
const knobsElement = document.querySelector("#knobs");
const resetButton = document.querySelector("#reset");

const FRAME_FLOATS = 76;
const QUAD = new Float32Array([
  0, 0, 0,  1, 0, 0,  1, 1, 0,
  0, 0, 0,  1, 1, 0,  0, 1, 0,
]);

let device;
let context;
let format;
let frameBuffer;
let quadBuffer;
let instanceBuffer;
let facingBuffer;
let depthTexture;
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

function setStatus(message, failed = false) {
  status.textContent = message;
  status.style.color = failed ? "#ef927f" : "";
}

function normalize([x, y, z]) {
  const length = Math.hypot(x, y, z) || 1;
  return [x / length, y / length, z / length];
}

function cross([ax, ay, az], [bx, by, bz]) {
  return [ay * bz - az * by, az * bx - ax * bz, ax * by - ay * bx];
}

function writeVec4(array, index, values) {
  array.set(values, index * 4);
}

function resizeCanvas() {
  const scale = Math.min(devicePixelRatio, 2);
  const width = Math.max(1, Math.floor(canvas.clientWidth * scale));
  const height = Math.max(1, Math.floor(canvas.clientHeight * scale));
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
  return true;
}

function stature() {
  return currentValues.get(selectedBody.statureKnob) ?? 1;
}

function updateBuffers() {
  const scale = stature();
  const center = selectedBody.centerHeight * scale;
  const radius = selectedBody.radius * scale;
  device.queue.writeBuffer(instanceBuffer, 0, new Float32Array([0, center, 0, radius]));

  const target = [0, center, 0];
  const camera = [
    target[0] + Math.sin(yaw) * Math.cos(elevation) * distance,
    target[1] + Math.sin(elevation) * distance,
    target[2] + Math.cos(yaw) * Math.cos(elevation) * distance,
  ];
  const forward = normalize(target.map((value, index) => value - camera[index]));
  const right = normalize(cross([0, 1, 0], forward));
  const up = normalize(cross(forward, right));
  const near = 0.05;
  const far = 40;
  const focal = 1 / Math.tan((42 * Math.PI / 180) / 2);
  const frame = new Float32Array(FRAME_FLOATS);
  writeVec4(frame, 0, [...camera, 0]);
  writeVec4(frame, 1, [...right, 0]);
  writeVec4(frame, 2, [...up, 0]);
  writeVec4(frame, 3, [...forward, 0]);
  writeVec4(frame, 4, [focal / (canvas.width / canvas.height), focal,
                       far / (far - near), (-far * near) / (far - near)]);
  writeVec4(frame, 6, [...normalize([0.55, 0.82, 0.28]), 1]);
  writeVec4(frame, 7, [2.0, 1.55, 1.15, 0.02]);
  writeVec4(frame, 8, [0.20, 0.27, 0.34, canvas.height]);
  writeVec4(frame, 9, [0.42, 0.33, 0.25, canvas.width]);
  writeVec4(frame, 10, [0.13, 0.16, 0.19, 1]);
  device.queue.writeBuffer(frameBuffer, 0, frame);
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

function render() {
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
        view: depthTexture.createView(),
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
  requestAnimationFrame(render);
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
});
canvas.addEventListener("pointerup", () => { dragging = false; });
canvas.addEventListener("pointercancel", () => { dragging = false; });
canvas.addEventListener("wheel", event => {
  event.preventDefault();
  distance = Math.max(2.0, Math.min(8.0, distance * Math.exp(event.deltaY * 0.001)));
}, { passive: false });

start().catch(fail);
