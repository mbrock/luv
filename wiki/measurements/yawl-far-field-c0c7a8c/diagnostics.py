"""Expose visibility errors which can masquerade as pleasant shade or softness."""
import argparse
import json
from pathlib import Path

import numpy as np

import aggregate
from common import load_meshes, triangle_frame
from run import camera_for, display, panel


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--meshes', default='.amp/in/artifacts/far-field-comparison/meshes.json')
    parser.add_argument('--output', default='.amp/in/artifacts/far-field-comparison/diagnostics')
    args = parser.parse_args()
    root = Path(args.output)
    root.mkdir(parents=True, exist_ok=True)
    mesh = load_meshes(args.meshes)['cave']
    rep = aggregate.build(mesh)
    light = [-.4, -.7, 1]
    camera = camera_for(mesh, np.radians(-12), .22)
    exact = triangle_frame(mesh, camera, 192, 144, light)
    cached, stats = aggregate.render(rep, camera, 192, 144, light)
    missing = np.isfinite(exact.depth) & ~np.isfinite(cached.depth)
    extra = ~np.isfinite(exact.depth) & np.isfinite(cached.depth)
    both = np.isfinite(exact.depth) & np.isfinite(cached.depth)
    wrong_layer = np.zeros(both.shape, dtype=bool)
    wrong_layer[both] = np.abs(exact.depth[both] - cached.depth[both]) > 1
    errors = exact.color * .2
    errors[missing] = [1, 0, 1]
    errors[extra] = [1, .3, 0]
    errors[wrong_layer] = [0, 1, 1]
    panel([display(exact.color), display(cached.color), display(errors)],
          ['Exact point samples', 'Directional cache', 'Visibility errors'], root / 'cave-errors.png',
          'Magenta: missing hit. Cyan: depth error >1 cell. Orange: false hit. Not shadows.')
    report = {'view': {'angle_degrees': -12, 'elevation': .22, 'extent': [192, 144]},
              'reference_covered_samples': int(exact.coverage.sum()),
              'missing_samples': int(missing.sum()), 'extra_samples': int(extra.sum()),
              'wrong_layer_samples_over_one_cell': int(wrong_layer.sum()),
              'cache_bytes': stats['bytes'], 'switch': {}}
    exact_pair, cache_pair, views = [], [], []
    for angle in (12.499, 12.501):
        camera = camera_for(mesh, np.radians(angle), .22)
        exact_pair.append(triangle_frame(mesh, camera, 192, 144, light))
        frame, stats = aggregate.render(rep, camera, 192, 144, light)
        cache_pair.append(frame)
        views.append(stats['selected_view'])
    report['switch'] = {'angles_degrees': [12.499, 12.501], 'cache_views': views,
                        'exact_coverage_changes': int(np.count_nonzero(exact_pair[0].coverage != exact_pair[1].coverage)),
                        'cache_coverage_changes': int(np.count_nonzero(cache_pair[0].coverage != cache_pair[1].coverage))}
    panel([display(f.color) for f in (exact_pair + cache_pair)],
          ['Exact 12.499 deg', 'Exact 12.501 deg', 'Cache 12.499 deg', 'Cache 12.501 deg'],
          root / 'direction-switch.png', 'A 0.002 degree turn across the nearest-view boundary; no geometry or light change')
    (root / 'results.json').write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps(report, indent=2))


if __name__ == '__main__':
    main()
