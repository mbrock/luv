"""Capacity/quality counterfactuals; neither wall-clock nor GPU benchmarks."""
import argparse
import json
from pathlib import Path

import numpy as np

import aggregate
import bricks
from common import load_meshes, resolve, triangle_frame
from run import camera_for, display, hit_metrics, image_metrics, panel


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--meshes', default='.amp/in/artifacts/far-field-comparison/meshes.json')
    parser.add_argument('--output', default='.amp/in/artifacts/far-field-comparison/sweep')
    args = parser.parse_args()
    root = Path(args.output)
    root.mkdir(parents=True, exist_ok=True)
    records = {}
    meshes = load_meshes(args.meshes)
    for scene in ('ridge', 'cave'):
        mesh = meshes[scene]
        camera = camera_for(mesh, -.21, .22)
        light = [-.4, -.7, 1]
        exact = triangle_frame(mesh, camera, 96 * 8, 72 * 8, light)
        native = triangle_frame(mesh, camera, 96 * 2, 72 * 2, light)
        ref_rgb, ref_coverage = resolve(exact, 8)
        for family, settings in (
            ('bricks', [(1.0, 4), (1.0, 8), (.5, 4), (.5, 8)]),
            ('aggregate', [16, 32, 64]),
        ):
            images, labels = [display(ref_rgb)], ['Exact / 64 samples']
            for setting in settings:
                if family == 'bricks':
                    rep = bricks.build(mesh, cell_size=setting[0], mask_size=setting[1])
                    frame, stats = bricks.render(rep, camera, 192, 144, light)
                    label = f'Cell {setting[0]} / mask {setting[1]}'
                else:
                    rep = aggregate.build(mesh, resolution=setting)
                    frame, stats = aggregate.render(rep, camera, 192, 144, light)
                    label = f'Cache {setting} x {setting}'
                rgb, coverage = resolve(frame, 2)
                assert np.isfinite(rgb).all()
                records[f'{scene}/{family}/{setting}'] = {
                    **image_metrics(rgb, coverage, ref_rgb, ref_coverage),
                    **hit_metrics(frame, native), 'work': stats,
                    'visible_blue_samples_relative_to_reference':
                        float((frame.material == 2).sum() / max((native.material == 2).sum(), 1)),
                }
                images.append(display(rgb))
                labels.append(label)
                print(scene, label, records[f'{scene}/{family}/{setting}']['linear_rgb_mae_on_union'], flush=True)
            panel(images, labels, root / f'{scene}-{family}.png',
                  f'{scene}: fixed view, light, surface; resolution/memory tradeoff, not GPU performance')
        (root / 'results.json').write_text(json.dumps(records, indent=2, allow_nan=False) + '\n')


if __name__ == '__main__':
    main()
