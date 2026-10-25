"""Run and inspect the isolated HAL/DSL Metal probe; no production changes.

The indexed source, triangulated convex partition, and clipped planar quads
share lighting, cameras, targets and indexed instancing. Timings omit builds,
uploads and readbacks; they are not game frame times. CPU oracle is independent.
"""
import argparse
import csv
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys

import numpy as np

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / 'yawl-far-field-c0c7a8c'))
from common import Camera, load_meshes, triangle_frame, unit
from run import camera_for, display, panel


def case_for(mesh, scene, pose, angle=.2, elevation=.45, scale=1, grid=1,
             perspective=False, width=256, height=192, light=(-.4, -.7, 1)):
    camera = camera_for(mesh, angle, elevation, scale)
    extent = float(max(mesh.bounds[1] - mesh.bounds[0]))
    spacing = extent * 1.8
    if grid > 1:
        camera.span = spacing * grid
    right, up, forward = camera.basis()
    near, far = .1, 200.
    distance = np.linalg.norm(camera.eye - camera.target)
    sx = 2 / camera.span * (distance if perspective else 1)
    a, b = ((far / (far - near), -near * far / (far - near)) if perspective
            else (1 / (far - near), -near / (far - near)))
    rows = [*camera.eye, int(perspective), *right, sx, *up, sx * width / height,
            *forward, a, *unit(light), b, grid, spacing, width, height]
    return dict(name=f'{scene}-{pose}', scene=scene, width=width, height=height,
                camera=np.float32(rows).astype(float).tolist(), grid=grid,
                eye=camera.eye.tolist(), target=camera.target.tolist(), span=camera.span,
                perspective=perspective, light=list(light), near=near, far=far)


def lisp(value):
    if isinstance(value, str):
        return json.dumps(value)
    if isinstance(value, (list, tuple)):
        return '(' + ' '.join(map(lisp, value)) + ')'
    if isinstance(value, float):
        return format(value, '.10e').replace('e', 'f')
    return str(value)


def execute(cases, input_dir, output):
    output.mkdir(parents=True, exist_ok=True)
    config = output / 'cases.sexp'
    keys = ['name', 'scene', 'width', 'height', 'camera', 'grid', 'modes', 'warmup', 'frames']
    config.write_text('(\n' + '\n'.join(
        '(' + ' '.join(':' + key + ' ' + lisp(case[key]) for key in keys if key in case) + ')'
        for case in cases) + '\n)\n')
    (output / 'cases.json').write_text(json.dumps(cases, indent=2) + '\n')
    env = {**os.environ, 'PROBE_CONFIG': str(config), 'PROBE_INPUT': str(input_dir),
           'PROBE_OUTPUT': str(output)}
    with (output / 'run.log').open('w') as log:
        subprocess.run(['./env', 'sbcl', '--script', str(HERE / 'run.lisp')],
                       env=env, stdout=log, stderr=subprocess.STDOUT, check=True)


def read_frame(root, case, variant):
    shape = case['height'], case['width']
    prefix = root / (case['name'] + '-' + variant)
    color = np.fromfile(str(prefix) + '.rgba16', '<f2').reshape(*shape, 4).astype(float)
    normal = np.fromfile(str(prefix) + '.normal16', '<f2').reshape(*shape, 4).astype(float)
    depth = np.fromfile(str(prefix) + '.depth32', '<f4').reshape(shape).astype(float)
    a, b = case['camera'][15], case['camera'][19]
    depth = b / (depth - a) if case['perspective'] else (depth - b) / a
    return dict(color=color[..., :3], hit=color[..., 3] > .5,
                normal=normal[..., :3], material=normal[..., 3], depth=depth)


def metrics(a, b):
    union, both = a['hit'] | b['hit'], a['hit'] & b['hit']
    difference = np.abs(a['depth'][both] - b['depth'][both])
    return dict(coverage_disagreements=int(np.count_nonzero(a['hit'] != b['hit'])),
                union_samples=int(union.sum()),
                material_disagreements=int(np.count_nonzero(a['material'][both] != b['material'][both])),
                normal_disagreements=int(np.count_nonzero(np.max(np.abs(a['normal'] - b['normal']), axis=2)[both] > .002)),
                depth_p95_cells=float(np.quantile(difference, .95)) if both.any() else None,
                depth_max_cells=float(difference.max()) if both.any() else None,
                depth_over_001_cells=int(np.count_nonzero(difference > .01)),
                rgb_mae_union=float(np.abs(a['color'] - b['color'])[union].mean()),
                rgb_max=float(np.abs(a['color'] - b['color']).max()))


def inspect(cases, meshes, output):
    result = {}
    for case in cases:
        frames = {mode: read_frame(output, case, mode)
                  for mode in ['reference', 'merged', 'analytic', 'proxy']}
        ref = frames['reference']
        record = {mode: metrics(frame, ref) for mode, frame in frames.items() if mode != 'reference'}
        if not case['perspective'] and case['grid'] == 1:
            camera = Camera(np.array(case['eye']), np.array(case['target']), case['span'])
            cpu = triangle_frame(meshes[case['scene']], camera, case['width'], case['height'], case['light'])
            oracle = dict(color=cpu.color, hit=cpu.coverage > .5, depth=cpu.depth,
                          normal=cpu.normal, material=cpu.material + 1)
            record['indexed_vs_cpu'] = metrics(ref, oracle)
            record['analytic_vs_cpu'] = metrics(frames['analytic'], oracle)
        result[case['name']] = record
        if case['name'] in ['cave-overview', 'terrain-perspective', 'sheets-grazing', 'ridge-far', 'cave-grid8']:
            panel([display(frames[m]['color']) for m in ['reference', 'merged', 'analytic', 'proxy']],
                  ['Indexed source', 'Merged / indexed', 'Analytic / 8 edges', 'No clipping (wrong)'],
                  output / (case['name'] + '.png'),
                  case['name'] + ': real Metal render, fixed bevels, matched point samples; no temporal AA')
    (output / 'accuracy.json').write_text(json.dumps(result, indent=2) + '\n')
    return result


def timing_summary(output):
    groups = {}
    with (output / 'timings.tsv').open() as stream:
        for row in csv.DictReader(stream, delimiter='\t'):
            groups.setdefault(row['case'], {}).setdefault(row['variant'], []).append(row)
    report = {}
    for case, variants in groups.items():
        report[case] = {}
        for variant, rows in variants.items():
            stats = {}
            for field in ['gpu_ms', 'cpu_encode_ms', 'completion_ms']:
                values = np.array([float(row[field]) for row in rows])
                stats[field] = dict(mean=float(values.mean()), median=float(np.median(values)),
                                    p95=float(np.quantile(values, .95)), count=len(values))
            report[case][variant] = stats
        print(case, ' '.join(f"{v}={x['gpu_ms']['median']:.3f}ms" for v, x in report[case].items()))
    (output / 'timing-summary.json').write_text(json.dumps(report, indent=2) + '\n')


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('mode', choices=['accuracy', 'bench', 'inspect'])
    parser.add_argument('--meshes', default='.amp/in/artifacts/far-field-comparison/meshes.json')
    parser.add_argument('--input', default='.amp/in/artifacts/analytic-gpu/input')
    parser.add_argument('--output', default='.amp/in/artifacts/analytic-gpu/accuracy')
    args = parser.parse_args()
    meshes, output = load_meshes(args.meshes), Path(args.output).resolve()
    if args.mode == 'inspect':
        inspect(json.loads((output / 'cases.json').read_text()), meshes, output)
        return
    cases = []
    if args.mode == 'accuracy':
        for scene, mesh in meshes.items():
            for pose, angle, elevation, scale, perspective in [
                    ('overview', .2, .45, 1, False), ('grazing', -.35, .06, 1, False),
                    ('far', .37, .3, 4, False), ('rear', 2.83, .38, 1, False),
                    ('perspective', .37, .33, 1, True)]:
                cases.append(case_for(mesh, scene, pose, angle, elevation, scale, perspective=perspective))
        for i, angle in enumerate(np.linspace(-.55, .55, 13)):
            cases.append(case_for(meshes['cave'], 'cave', f'motion{i:02}', angle, .22,
                                  light=(np.sin(angle), -.5, .8)))
        cases.append(case_for(meshes['cave'], 'cave', 'grid8', grid=8, width=512, height=512))
        cases.append(case_for(meshes['cave'], 'cave', 'far-supersample', .37, .3, 4, width=1024, height=768))
    else:
        for order in range(2):
            for scene in ['ridge', 'cave', 'terrain', 'sheets']:
                for grid in [1, 8, 32]:
                    case = case_for(meshes[scene], scene, f'grid{grid}-order{order}',
                                    grid=grid, width=1024, height=1024)
                    modes = ['reference', 'merged', 'analytic', 'reference-copy']
                    case.update(modes=modes if order == 0 else list(reversed(modes)), warmup=64, frames=256)
                    cases.append(case)
    execute(cases, Path(args.input).resolve(), output)
    provenance = dict(revision=subprocess.check_output(['git', 'rev-parse', 'HEAD'], text=True).strip(),
                      sources={p.name: hashlib.sha256(p.read_bytes()).hexdigest()
                               for p in HERE.iterdir() if p.suffix in ['.py', '.lisp']},
                      meshes_sha256=hashlib.sha256(Path(args.meshes).read_bytes()).hexdigest(),
                      system=subprocess.check_output(['sw_vers'], text=True),
                      geometry='fixed physical bevels; no distance simplification',
                      timing_scope='one offscreen pass, serial completion, no readback/build/upload; not production FPS')
    (output / 'provenance.json').write_text(json.dumps(provenance, indent=2) + '\n')
    if args.mode == 'accuracy':
        results = inspect(cases, meshes, output)
        print('cases:', len(results), 'analytic coverage disagreements:',
              sum(v['analytic']['coverage_disagreements'] for v in results.values()))
    else:
        timing_summary(output)


if __name__ == '__main__':
    main()
