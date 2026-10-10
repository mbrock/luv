// Validate WGSL with Dawn, the WebGPU implementation Chrome ships.
//
//   node scripts/wgsl-validate.mjs FILE.wgsl...
//     compiles each module and prints its diagnostics.
//
//   node scripts/wgsl-validate.mjs --programs DIR...
//     reads every NAME.json that luv-shaderc wrote with its wgsl target in
//     each DIR, compiles each stage, and creates the program's render or
//     compute pipeline against bind group layouts built from the manifest
//     alone, as a renderer would: buffers in group 0, textures in group 1,
//     samplers in group 2.  That checks what a module by itself cannot: the
//     vertex and fragment interface, each binding's type against its use,
//     and which sampler meets which texture.
//
//   node scripts/wgsl-validate.mjs --probe
//     only reports whether Dawn can be loaded.
//
// Dawn comes from the `webgpu` npm package, looked for from the directory
// LUV_WEBGPU names, the working directory, and this script's directory:
//
//   mkdir /tmp/webgpu && (cd /tmp/webgpu && npm install webgpu)
//   LUV_WEBGPU=/tmp/webgpu node scripts/wgsl-validate.mjs --programs DIR
//
// The exit status is 0 when everything is valid, 1 when something is not,
// and 77 when Dawn or a GPU adapter is missing.

import { readFileSync, readdirSync } from 'node:fs';
import { createRequire } from 'node:module';
import path from 'node:path';
import { pathToFileURL } from 'node:url';

const unavailable = 77;

async function loadDawn() {
  const bases = [process.env.LUV_WEBGPU, process.cwd(), import.meta.dirname];
  for (const base of bases.filter(Boolean)) {
    try {
      const require = createRequire(path.join(path.resolve(base), 'index.js'));
      return await import(pathToFileURL(require.resolve('webgpu')));
    } catch {
      // Look from the next directory.
    }
  }
  return null;
}

// The limits a WebGPU device has without asking for more.  A program past
// one of them needs a device created with that limit raised.
const coreLimits = {
  maxUniformBuffersPerShaderStage: 12,
  maxStorageBuffersPerShaderStage: 8,
  maxSampledTexturesPerShaderStage: 16,
  maxStorageTexturesPerShaderStage: 4,
  maxSamplersPerShaderStage: 16,
};

const limitOfFamily = {
  uniform_block: 'maxUniformBuffersPerShaderStage',
  storage_buffer: 'maxStorageBuffersPerShaderStage',
  read_write_storage_buffer: 'maxStorageBuffersPerShaderStage',
  read_write_texture_2d: 'maxStorageTexturesPerShaderStage',
  sampler: 'maxSamplersPerShaderStage',
  comparison_sampler: 'maxSamplersPerShaderStage',
};

const textureKinds = {
  texture_2d: { sampleType: 'float', viewDimension: '2d' },
  depth_texture_2d: { sampleType: 'depth', viewDimension: '2d' },
  uint_texture_2d: { sampleType: 'uint', viewDimension: '2d' },
  texture_2d_array: { sampleType: 'float', viewDimension: '2d-array' },
  depth_texture_2d_array: { sampleType: 'depth', viewDimension: '2d-array' },
  texture_cube: { sampleType: 'float', viewDimension: 'cube' },
  texture_3d: { sampleType: 'float', viewDimension: '3d' },
};

const storageFormats = {
  rgba32f: 'rgba32float',
  rgba16f: 'rgba16float',
  rgba8: 'rgba8unorm',
  r32f: 'r32float',
  r32ui: 'r32uint',
};

const storageAccesses = {
  read: 'read-only',
  write: 'write-only',
  read_write: 'read-write',
};

// The standard sampler set: 0 and 1 filter, 2 is nearest, 3 compares.
function samplerType(resource) {
  if (resource.kind === 'comparison_sampler') return 'comparison';
  return resource.binding === 2 ? 'non-filtering' : 'filtering';
}

function limitOf(resource) {
  return resource.kind in textureKinds
    ? 'maxSampledTexturesPerShaderStage'
    : limitOfFamily[resource.kind];
}

function layoutEntry(resource, binding, stageBits) {
  const visibility = resource.stages.reduce(
    (bits, stage) => bits | stageBits[stage], 0);
  const entry = { binding, visibility };
  if (resource.kind === 'uniform_block') {
    entry.buffer = { type: 'uniform' };
  } else if (resource.kind === 'storage_buffer') {
    entry.buffer = { type: 'read-only-storage' };
  } else if (resource.kind === 'read_write_storage_buffer') {
    entry.buffer = { type: 'storage' };
  } else if (resource.kind in textureKinds) {
    entry.texture = textureKinds[resource.kind];
  } else if (resource.kind === 'read_write_texture_2d') {
    entry.storageTexture = {
      access: storageAccesses[resource.wgsl_access],
      format: storageFormats[resource.format],
      viewDimension: '2d',
    };
  } else {
    entry.sampler = { type: samplerType(resource) };
  }
  return entry;
}

// A colour target a fragment output of this type can be written to: the
// output needs the format's scalar kind and at least its components.
function targetFormat(type) {
  if (type.startsWith('uvec') || type === 'uint') return 'rgba32uint';
  if (type.startsWith('ivec') || type === 'int') return 'rgba32sint';
  return { float: 'r32float', vec2: 'rg16float', vec3: 'rg16float' }[type]
    ?? 'rgba16float';
}

function describe(message, file) {
  return `${file}:${message.lineNum}:${message.linePos}: ` +
    `${message.type}: ${message.message}`;
}

class Validator {
  constructor(device, globals) {
    this.device = device;
    this.globals = globals;
    this.stages = 0;
    this.pipelines = 0;
    this.errors = [];
    this.errorCount = 0;
    this.warnings = [];
    this.warningCount = 0;
    this.notes = [];
  }

  fail(line) {
    this.errors.push(line);
    this.errorCount += 1;
  }

  async module(file) {
    const code = readFileSync(file, 'utf8');
    this.device.pushErrorScope('validation');
    const module = this.device.createShaderModule({ code });
    const information = await module.getCompilationInfo();
    const scope = await this.device.popErrorScope();
    let valid = true;
    // An info message elaborates the error or warning before it.
    let messages = this.warnings;
    for (const message of information.messages) {
      if (message.type === 'error') {
        valid = false;
        messages = this.errors;
        this.errorCount += 1;
      } else if (message.type === 'warning') {
        messages = this.warnings;
        this.warningCount += 1;
      }
      messages.push(describe(message, file));
    }
    if (scope && valid) {
      valid = false;
      this.fail(`${file}: ${scope.message}`);
    }
    if (valid) this.stages += 1;
    return valid ? module : null;
  }

  async guarded(label, make) {
    this.device.pushErrorScope('validation');
    let value = null;
    try {
      value = await make();
    } catch (error) {
      this.fail(`${label}: ${error.message}`);
    }
    const scope = await this.device.popErrorScope();
    if (scope) {
      this.fail(`${label}: ${scope.message}`);
      return null;
    }
    return value;
  }

  // Validate one manifest's program; false when it was not lowered to WGSL.
  async program(directory, manifestFile, automatic) {
    const { GPUShaderStage } = this.globals;
    const manifest = JSON.parse(
      readFileSync(path.join(directory, manifestFile), 'utf8'));
    const stages = (manifest.stages ?? []).filter((stage) => stage.wgsl);
    if (stages.length === 0) return false;
    const name = manifest.name;
    const stageBits = {
      vertex: GPUShaderStage.VERTEX,
      fragment: GPUShaderStage.FRAGMENT,
      compute: GPUShaderStage.COMPUTE,
    };
    const modules = {};
    let valid = true;
    for (const stage of stages) {
      modules[stage.stage] = await this.module(
        path.join(directory, stage.wgsl));
      valid &&= modules[stage.stage] !== null;
    }

    const groups = [];
    const used = {};
    for (const resource of manifest.resources) {
      const place = /^group (\d+), binding (\d+)$/.exec(resource.wgsl);
      (groups[Number(place[1])] ??= []).push(
        layoutEntry(resource, Number(place[2]), stageBits));
      for (const stage of resource.stages) {
        const key = `${stage} ${limitOf(resource)}`;
        used[key] = (used[key] ?? 0) + 1;
      }
    }
    for (const [key, count] of Object.entries(used)) {
      const [stage, limit] = key.split(' ');
      if (count > coreLimits[limit]) {
        this.notes.push(`${name}: the ${stage} stage binds ${count}, past ` +
          `${limit}'s default of ${coreLimits[limit]}`);
      }
    }
    if (!valid) return true;

    const layout = automatic ? 'auto' : await this.guarded(
      `${name}: layout`,
      () => this.device.createPipelineLayout({
        bindGroupLayouts: Array.from(groups, (entries) =>
          this.device.createBindGroupLayout({ entries: entries ?? [] })),
      }));
    if (!layout) return true;

    const stage = (kind) => {
      const described = stages.find((each) => each.stage === kind);
      return described &&
        { module: modules[kind], entryPoint: described.entry };
    };
    const pipeline = await this.guarded(`${name}: pipeline`, () => {
      if (stage('compute')) {
        return this.device.createComputePipelineAsync(
          { layout, compute: stage('compute') });
      }
      const descriptor = {
        layout,
        vertex: stage('vertex'),
        primitive: { topology: 'triangle-list' },
        depthStencil: {
          format: 'depth32float',
          depthWriteEnabled: true,
          depthCompare: 'less',
        },
      };
      if (stage('fragment')) {
        descriptor.fragment = {
          ...stage('fragment'),
          targets: manifest.fragment_outputs.map(
            (output) => ({ format: targetFormat(output.type) })),
        };
      }
      return this.device.createRenderPipelineAsync(descriptor);
    });
    if (pipeline) this.pipelines += 1;
    return true;
  }

  report(programs) {
    for (const line of this.errors) console.log(line);
    for (const line of this.warnings) console.log(line);
    for (const line of this.notes) console.log(`note: ${line}`);
    console.log(
      `wgsl-validate: ${this.stages} module${this.stages === 1 ? '' : 's'}` +
      (programs ? `, ${this.pipelines} of ${programs} pipelines` : '') +
      `, ${this.errorCount} error${this.errorCount === 1 ? '' : 's'}` +
      `, ${this.warningCount} warning${this.warningCount === 1 ? '' : 's'}.`);
    return this.errorCount === 0 ? 0 : 1;
  }
}

async function main(parameters) {
  const dawn = await loadDawn();
  if (!dawn) {
    console.error('wgsl-validate: no webgpu package; see this script\'s head.');
    return unavailable;
  }
  Object.assign(globalThis, dawn.globals);
  // The instance must outlive everything made from it: Dawn's event loop
  // keeps using it after the garbage collector would let it go.
  globalThis.gpu = dawn.create([]);
  const adapter = await globalThis.gpu.requestAdapter();
  if (!adapter) {
    console.error('wgsl-validate: no GPU adapter.');
    return unavailable;
  }
  if (parameters[0] === '--probe') return 0;

  // Ask for what the adapter can give, so that a program past a default
  // limit is still validated; the notes say which programs those are.
  const requiredLimits = {};
  for (const limit of Object.keys(coreLimits)) {
    requiredLimits[limit] = adapter.limits[limit];
  }
  const device = await adapter.requestDevice({ requiredLimits });
  const validator = new Validator(device, dawn.globals);

  const automatic = parameters.includes('--auto-layout');
  parameters = parameters.filter((each) => each !== '--auto-layout');
  let programs = 0;
  if (parameters[0] === '--programs') {
    for (const directory of parameters.slice(1)) {
      const manifests = readdirSync(directory)
        .filter((file) => file.endsWith('.json')).sort();
      for (const manifest of manifests) {
        const before = validator.errorCount;
        if (await validator.program(directory, manifest, automatic)) {
          programs += 1;
        }
        if (validator.errorCount > before) {
          console.log(`failed: ${path.join(directory, manifest)}`);
        }
      }
    }
  } else {
    for (const file of parameters) await validator.module(file);
  }
  const status = validator.report(programs);
  device.destroy();
  return status;
}

process.exit(await main(process.argv.slice(2)));
