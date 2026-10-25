"""Compare three drawing representations on identical atlas geometry.

Run from the repository root; see README.org for reproduction and limits.
This intentionally does not report Python runtime as rendering performance.
"""
import argparse
import hashlib
import importlib
import json
from pathlib import Path
import subprocess
import textwrap
import time

import numpy as np
from PIL import Image, ImageDraw, ImageFont

from common import (BACKGROUND, Camera, check_closed, load_meshes,
                    resolve, shade, triangle_frame, unit)


def display(rgb):
    rgb = np.maximum(rgb, 0)
    rgb = rgb / (1 + rgb)  # identical simple shoulder for every representation
    rgb = np.where(rgb <= 0.0031308, 12.92 * rgb, 1.055 * rgb ** (1 / 2.4) - 0.055)
    return Image.fromarray(np.rint(np.clip(rgb, 0, 1) * 255).astype(np.uint8))


def camera_for(mesh, angle=0.2, elevation=0.45, distance_scale=1):
    lo, hi = mesh.bounds
    target = (lo + hi) / 2
    extent = max(hi - lo)
    offset = unit([np.sin(angle), -np.cos(angle), elevation]) * (extent * 4 + 10)
    return Camera(target + offset, target, extent * 1.6 * distance_scale)


def averaged_normal_control(frame, camera, light, samples):
    """Wrong control: shade each material's mean normal, same exact visibility."""
    h, w = frame.depth.shape
    height, width = h // samples, w // samples
    def reduce(a):
        return a.reshape(height, samples, width, samples, *a.shape[2:]).mean(axis=(1, 3))
    out = (1 - reduce(frame.coverage))[..., None] * BACKGROUND
    for material in range(4):
        mask = frame.material == material
        weight = reduce(mask)
        normals = unit(reduce(frame.normal * mask[..., None]))
        value = shade(normals, np.full((height, width), material),
                      np.zeros((height, width, 3)), light, -camera.basis()[2])
        out += weight[..., None] * value
    return out


def image_metrics(rgb, coverage, reference, reference_coverage):
    mask = (coverage > 0) | (reference_coverage > 0)
    difference = np.abs(rgb - reference)
    return {
        'linear_rgb_mae_on_union': float(difference[mask].mean()) if mask.any() else 0,
        'coverage_mae_on_union': float(np.abs(coverage - reference_coverage)[mask].mean()) if mask.any() else 0,
        'covered_area_relative_error': float((coverage.sum() - reference_coverage.sum()) / max(reference_coverage.sum(), 1)),
        'mean_linear_rgb': rgb[mask].mean(axis=0).tolist() if mask.any() else [0, 0, 0],
    }


def hit_metrics(frame, reference):
    both = np.isfinite(frame.depth) & np.isfinite(reference.depth)
    union = np.isfinite(frame.depth) | np.isfinite(reference.depth)
    difference = np.abs(frame.depth[both] - reference.depth[both])
    return {
        'hit_disagreement_fraction_on_union': float((np.isfinite(frame.depth) != np.isfinite(reference.depth)).sum() / max(union.sum(), 1)),
        'material_disagreement_fraction_on_common': float((frame.material[both] != reference.material[both]).mean()) if both.any() else 0,
        'depth_absolute_p95_cells': float(np.quantile(difference, .95)) if both.any() else None,
        'depth_absolute_max_cells': float(difference.max()) if both.any() else None,
    }


def panel(images, labels, path, subtitle):
    width, height = images[0].size
    scale = max(1, 256 // width)
    width, height = width * scale, height * scale
    font = ImageFont.load_default(size=15)
    canvas = Image.new('RGB', (width * len(images), height + 80), '#121b25')
    draw = ImageDraw.Draw(canvas)
    for i, (image, label) in enumerate(zip(images, labels)):
        canvas.paste(image.resize((width, height), Image.Resampling.NEAREST), (i * width, 32))
        draw.text((i * width + 8, 8), label, font=font, fill='white')
    caption = '\n'.join(textwrap.wrap(subtitle, width=max(20, (canvas.width - 16) // 8)))
    draw.multiline_text((8, height + 40), caption, font=font, fill='#c0ccd8')
    canvas.save(path)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--meshes', default='.amp/in/artifacts/far-field-comparison/meshes.json')
    parser.add_argument('--output', default='.amp/in/artifacts/far-field-comparison/run')
    parser.add_argument('--variants', default='analytic,bricks,aggregate')
    parser.add_argument('--scenes', default='ridge,junction,sheets,cave,terrain,edited-cave')
    parser.add_argument('--width', type=int, default=128)
    parser.add_argument('--samples', type=int, default=2)
    parser.add_argument('--reference', type=int, default=8)
    parser.add_argument('--frames', type=int, default=12)
    args = parser.parse_args()
    root = Path(args.output)
    root.mkdir(parents=True, exist_ok=True)
    modules = {name: importlib.import_module(name) for name in args.variants.split(',') if name}
    meshes = load_meshes(args.meshes)
    width, height = args.width, args.width * 3 // 4
    s, ref_s = args.samples, args.reference
    source = Path(__file__).parent
    report = {
        'kind': 'CPU information probe, not GPU or frame-time evidence',
        'source_revision': subprocess.check_output(['git', 'rev-parse', 'HEAD'], text=True).strip(),
        'source_sha256': {p.name: hashlib.sha256(p.read_bytes()).hexdigest() for p in sorted(source.glob('*.py'))},
        'meshes_sha256': hashlib.sha256(Path(args.meshes).read_bytes()).hexdigest(),
        'output_extent': [width, height], 'candidate_grid_samples': s * s,
        'reference_grid_samples': ref_s * ref_s, 'temporal': False,
        'build': {}, 'cases': {}, 'motion': {},
    }
    scenes = args.scenes.split(',')
    for scene in scenes:
        mesh = meshes[scene]
        check_closed(mesh)
        reps = {}
        report['build'][scene] = {'source_triangles': len(mesh.triangles)}
        for name, module in modules.items():
            start = time.perf_counter()
            reps[name] = module.build(mesh)
            report['build'][scene][name + '_python_seconds'] = time.perf_counter() - start
        cases = [('overview', .2, .45, 1, [-.4, -.7, 1]),
                 ('grazing', -.35, .06, 1, [.7, -.3, .3]),
                 ('far', .37, .3, 3, [.4, -.6, .8])]
        if scene in ('ridge', 'cave'):
            cases += [('light-band', .2, .45, 1, [-.2, -.5, .85]),
                      ('light-back', .2, .45, 1, [.1, 1, .2])]
        for pose, angle, elevation, scale, light in cases:
            key = scene + '-' + pose
            camera = camera_for(mesh, angle, elevation, scale)
            reference = triangle_frame(mesh, camera, width * ref_s, height * ref_s, light)
            ref_rgb, ref_coverage = resolve(reference, ref_s)
            native = triangle_frame(mesh, camera, width * s, height * s, light)
            rgb, coverage = resolve(native, s)
            images = [display(ref_rgb), display(rgb)]
            labels = [f'Exact / {ref_s * ref_s} samples', f'Triangles / {s * s} samples']
            record = {'triangles': image_metrics(rgb, coverage, ref_rgb, ref_coverage)}
            for name, module in modules.items():
                frame, stats = module.render(reps[name], camera, width * s, height * s, light)
                rgb, coverage = resolve(frame, s)
                assert np.isfinite(rgb).all() and np.isfinite(coverage).all(), (key, name)
                assert np.all((coverage >= 0) & (coverage <= 1)), (key, name)
                record[name] = {**image_metrics(rgb, coverage, ref_rgb, ref_coverage),
                                **hit_metrics(frame, native), 'work': stats}
                images.append(display(rgb))
                labels.append(name.capitalize())
            if 'aggregate' in modules:
                frame, stats = modules['aggregate'].render(reps['aggregate'], camera,
                                                         width * s, height * s, light, painterly=True)
                rgb, coverage = resolve(frame, s)
                record['paint'] = {**image_metrics(rgb, coverage, ref_rgb, ref_coverage), 'work': stats}
                images.append(display(rgb))
                labels.append('Cache + world marks')
            wrong = averaged_normal_control(reference, camera, light, ref_s)
            record['averaged_normal_control'] = image_metrics(wrong, ref_coverage, ref_rgb, ref_coverage)
            if pose == 'far':
                panel([display(ref_rgb), display(wrong)], ['Visible response mixture', 'Mean-normal control'],
                      root / (key + '-normal-control.png'), scene + ': same exact visibility, different lighting integration')
            panel(images, labels, root / (key + '.png'), key + ': fixed world surface; linear box resolve; no shadows or TAA')
            report['cases'][key] = record
            print(key, {n: round(v['linear_rgb_mae_on_union'], 5) for n, v in record.items()}, flush=True)
        # Convergence is independently checked, rather than naming 64 samples ground truth.
        camera = camera_for(mesh)
        a = triangle_frame(mesh, camera, width * ref_s, height * ref_s, [-.4, -.7, 1])
        b = triangle_frame(mesh, camera, width * ref_s * 2, height * ref_s * 2, [-.4, -.7, 1])
        report['build'][scene]['reference_convergence'] = image_metrics(*resolve(a, ref_s), *resolve(b, ref_s * 2))
        if scene in ('ridge', 'cave') and args.frames:
            motion = []
            previous = {}
            for i in range(args.frames):
                fraction = i / max(args.frames - 1, 1)
                camera = camera_for(mesh, -.42 + .84 * fraction, .3, 1 + .5 * fraction)
                # Separate camera sweep and light sweep: the latter must not rebake.
                light = [-.4, -.7, 1]
                ref = triangle_frame(mesh, camera, width * ref_s, height * ref_s, light)
                ref_rgb, ref_coverage = resolve(ref, ref_s)
                rgb_by_name = {'reference': ref_rgb}
                labels, images = ['Exact'], [display(ref_rgb)]
                row = {}
                for name, module in modules.items():
                    frame, stats = module.render(reps[name], camera, width * s, height * s, light)
                    rgb, coverage = resolve(frame, s)
                    row[name] = image_metrics(rgb, coverage, ref_rgb, ref_coverage)
                    # This is change-error, NOT reprojected temporal stability.
                    if i:
                        delta = (rgb - previous[name]) - (ref_rgb - previous['reference'])
                        row[name]['frame_change_error_mae'] = float(np.abs(delta).mean())
                    rgb_by_name[name] = rgb
                    labels.append(name.capitalize())
                    images.append(display(rgb))
                panel(images, labels, root / f'{scene}-motion-{i:02d}.png',
                      f'{scene} orbit / zoom out {i + 1}/{args.frames}; slowed review, not runtime footage')
                motion.append(row)
                previous = rgb_by_name
            report['motion'][scene] = motion
        # Save progressively so a later failed candidate cannot erase earlier evidence.
        (root / 'results.json').write_text(json.dumps(report, indent=2, allow_nan=False) + '\n')
    print(root / 'results.json', flush=True)


if __name__ == '__main__':
    main()
