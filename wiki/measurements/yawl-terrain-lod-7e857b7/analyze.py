"""Compare actual GPU scene-linear radiance/depth and adjacent-frame timings.

No image registration, edge removal, rescaling, or exposure fitting. Native
raw HDR buffers exclude the HUD; displayed PNGs are the actual presentation.
"""
import argparse
import csv
import hashlib
import json
from pathlib import Path

import numpy as np


def distribution(values):
    a = np.asarray(values, dtype=float)
    return dict(n=a.size, mean=float(a.mean()), p50=float(np.median(a)),
                p95=float(np.quantile(a, .95)), maximum=float(a.max()))


def compare(folder):
    width, height = map(int, folder.name.rsplit('-', 1)[1].split('x'))
    result = dict(width=width, height=height, comparisons={}, timing={}, hashes={})
    control = folder / 'control.rgba16'
    if control.exists():
        ref = np.fromfile(control, '<f2').astype(np.float32).reshape(height, width, 4)[..., :3]
        depth = np.fromfile(folder / 'control.depth32', '<f4').reshape(height, width)
        for file in sorted(folder.glob('*.rgba16')):
            name = file.stem
            image = np.fromfile(file, '<f2').astype(np.float32).reshape(height, width, 4)[..., :3]
            d = np.fromfile(folder / f'{name}.depth32', '<f4').reshape(height, width)
            assert np.isfinite(image).all() and np.isfinite(d).all(), name
            error = np.abs(image - ref)
            common = (depth < 1) & (d < 1)
            row = dict(linear_rgb=distribution(error),
                       terrain_linear_rgb=distribution(error[common]),
                       coverage_changes=int(np.count_nonzero((depth < 1) != (d < 1))),
                       reference_covered=int(np.count_nonzero(depth < 1)),
                       depth_unequal=int(np.count_nonzero(depth != d)),
                       depth_maximum=float(np.max(np.abs(depth - d))),
                       depth_over_1e_6=int(np.count_nonzero(np.abs(depth - d) > 1e-6)))
            camera = folder / 'control.camera.txt'
            if camera.exists():
                u = np.loadtxt(camera)
                def view_depth(v):
                    return u[19] / (v - u[18]) if u[22] > .5 else (v - u[19]) / u[18]
                delta = np.abs(view_depth(depth[common]) - view_depth(d[common]))
                row['common_hit_view_depth_error'] = distribution(delta)
                row['common_hit_errors_over_one_cell'] = int(np.count_nonzero(delta > 1))
            result['comparisons'][name] = row
            for source in [file, folder / f'{name}.depth32']:
                result['hashes'][source.name] = hashlib.sha256(source.read_bytes()).hexdigest()
    timing = folder / 'timings.tsv'
    if timing.exists():
        rows = list(csv.DictReader(timing.open(), delimiter='\t'))
        names = list(dict.fromkeys(row['variant'] for row in rows))
        # AB...BA cohorts; block ratios compare each complete symmetric cycle.
        cycle = 2 * len(names)
        complete = len(rows) // cycle
        for name in names:
            own = [row for row in rows if row['variant'] == name]
            ratios = []
            for index in range(complete):
                block = rows[index * cycle:(index + 1) * cycle]
                a = [float(r['gpu_ms']) for r in block if r['variant'] == name]
                b = [float(r['gpu_ms']) for r in block if r['variant'] == 'control']
                if a and b:
                    ratios.append(np.mean(a) / np.mean(b))
            result['timing'][name] = {
                **{key: distribution([float(row[key]) for row in own])
                   for key in ['gpu_ms', 'cpu_encode_ms', 'completion_ms']},
                'symmetric_cycle_gpu_ratio_to_control': distribution(ratios) if ratios else None,
            }
    motion = folder / 'motion'
    if motion.exists():
        result['motion'] = {}
        for candidate in sorted(motion.iterdir()):
            if not candidate.is_dir():
                continue
            rows, previous = [], None
            for file in sorted(candidate.glob('*.rgba16')):
                refpath = motion / 'control' / file.name
                a = np.fromfile(refpath, '<f2').astype(np.float32).reshape(height, width, 4)[..., :3]
                b = np.fromfile(file, '<f2').astype(np.float32).reshape(height, width, 4)[..., :3]
                ad = np.fromfile(refpath.with_suffix('.depth32'), '<f4')
                bd = np.fromfile(file.with_suffix('.depth32'), '<f4')
                assert all(np.isfinite(v).all() for v in (a, b, ad, bd))
                u = np.loadtxt(refpath.with_suffix('.camera.txt'))
                assert np.array_equal(u, np.loadtxt(file.with_suffix('.camera.txt'))), file
                common = (ad < 1) & (bd < 1)
                if u[22] > .5:
                    delta = np.abs(u[19] / (ad[common] - u[18]) - u[19] / (bd[common] - u[18]))
                else:
                    delta = np.abs((ad[common] - bd[common]) / u[18])
                error = b - a
                rows.append(dict(frame=file.stem, mae=float(np.abs(error).mean()),
                                 maximum=float(np.abs(error).max()),
                                 coverage_changes=int(np.count_nonzero((ad < 1) != (bd < 1))),
                                 reference_covered=int(np.count_nonzero(ad < 1)),
                                 depth_unequal=int(np.count_nonzero(ad != bd)),
                                 depth_maximum=float(np.max(np.abs(ad - bd))),
                                 common_hit_view_depth_error=distribution(delta) if delta.size else None,
                                 common_hit_errors_over_one_cell=int(np.count_nonzero(delta > 1)),
                                 temporal_error_change_mae=None if previous is None else
                                 float(np.abs(error - previous).mean())))
                previous = error
            result['motion'][candidate.name] = rows
    return result


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('root', type=Path)
    args = parser.parse_args()
    results = {}
    for timing in sorted(args.root.rglob('timings.tsv')):
        folder = timing.parent
        results[str(folder.relative_to(args.root))] = compare(folder)
    path = args.root / 'analysis.json'
    path.write_text(json.dumps(results, indent=2, allow_nan=False) + '\n')
    for name, result in results.items():
        print(name)
        for variant, row in result['timing'].items():
            c = result['comparisons'].get(variant, {})
            print(f"  {variant:24} {row['gpu_ms']['mean']:.3f} ms, "
                  f"RGB MAE {c.get('linear_rgb', {}).get('mean', float('nan')):.7f}, "
                  f"coverage changes {c.get('coverage_changes', '?')}")
    print(path)


if __name__ == '__main__':
    main()
