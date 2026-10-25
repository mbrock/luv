"""Compare retained production frames, including original forward controls."""
import csv
import json
import sys
from pathlib import Path

import numpy as np


def analyze(folder):
    result = {}
    timing = folder / 'timings.tsv'
    if timing.exists():
        rows = list(csv.DictReader(timing.open(), delimiter='\t'))
        medians = {}
        for name in ['control', 'visibility']:
            selected = [r for r in rows if r['variant'] == name]
            medians[name] = {field: float(np.median([float(r[field]) for r in selected]))
                             for field in ['gpu_ms', 'cpu_encode_ms', 'completion_ms']}
        ratios = []
        for i in range(0, len(rows), 4):
            block = rows[i:i+4]
            mean = {name: np.mean([float(r['gpu_ms']) for r in block if r['variant'] == name])
                    for name in medians}
            ratios.append(mean['visibility'] / mean['control'])
        result['timing'] = {'medians': medians, 'paired_gpu_ratio': float(np.median(ratios)),
                            'pairs': len(ratios)}
    pairs = [(folder/'control', folder/'visibility', 'static')]
    pairs += [(p.with_suffix(''), p.parent.parent/'visibility'/p.stem, 'motion')
              for p in sorted((folder/'motion/control').glob('*.depth32'))]
    for category in ['edit', 'resize']:
        pairs += [(p/'control', p/'visibility', category) for p in (folder/category).glob('*')]
    for category in ['static', 'motion', 'edit', 'resize']:
        chosen = [(a, b) for a, b, c in pairs if c == category and a.with_suffix('.depth32').exists()]
        if not chosen:
            continue
        report = {'frames': len(chosen)}
        for extension, dtype, lanes in [('depth32', '<f4', 1), ('rgba16', '<f2', 4),
                                        ('rg16', '<f2', 2), ('bgra8', 'u1', 4)]:
            count = different = nonfinite = coverage = large_depth = 0
            total = maximum = max_world = 0.0
            worst = None
            for a, b in chosen:
                if not a.with_suffix('.'+extension).exists():
                    continue
                x = np.fromfile(a.with_suffix('.'+extension), dtype).astype('f8').reshape(-1, lanes)
                y = np.fromfile(b.with_suffix('.'+extension), dtype).astype('f8').reshape(-1, lanes)
                assert x.shape == y.shape
                if lanes == 4:
                    x, y = x[:, :3], y[:, :3]
                delta = np.abs(x-y)
                count += delta.size
                different += int(np.count_nonzero(delta))
                nonfinite += int(np.count_nonzero(~np.isfinite(y)))
                total += float(delta.sum())
                if delta.max(initial=0) > maximum:
                    maximum = float(delta.max())
                    worst = str(a.relative_to(folder))
                if extension == 'depth32':
                    coverage += int(np.count_nonzero((x < 1) != (y < 1)))
                    camera = np.loadtxt(a.with_suffix('.camera.txt'))
                    assert np.array_equal(camera, np.loadtxt(b.with_suffix('.camera.txt')))
                    covered = (x < 1) & (y < 1)
                    # Perspective distance = B / (hardware-depth - A).
                    world_delta = np.abs(camera[19]/(x[covered]-camera[18]) -
                                         camera[19]/(y[covered]-camera[18]))
                    max_world = max(max_world, float(world_delta.max(initial=0)))
                    large_depth += int(np.count_nonzero(world_delta > 1))
            if count:
                report[extension] = {'samples': count, 'different': different, 'mae': total/count,
                                     'maximum': maximum, 'worst': worst, 'nonfinite': nonfinite}
                if extension == 'depth32':
                    report[extension].update(coverage_disagreements=coverage,
                                              max_distance_cells=max_world,
                                              distance_errors_over_one_cell=large_depth)
        result[category] = report
    return result


root = Path(sys.argv[1])
results = {str(p.relative_to(root)): analyze(p) for p in sorted(root.glob('*/*-*x*'))
           if p.is_dir()}
output = root/'analysis.json'
output.write_text(json.dumps(results, indent=2)+'\n')
for name, result in results.items():
    print(name, 'GPU ratio', result.get('timing', {}).get('paired_gpu_ratio'))
    for category in ['static', 'motion', 'edit', 'resize']:
        if category in result:
            data = result[category]
            print(' ', category, 'frames', data['frames'],
                  'coverage', data['depth32']['coverage_disagreements'],
                  'max cell delta', data['depth32']['max_distance_cells'],
                  'RGB MAE', data['rgba16']['mae'],
                  'display max', data['bgra8']['maximum'])
print(output)
