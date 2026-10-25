"""Checks for the probe's evidence contract, independent of image preference."""
import argparse
import importlib
import json
from pathlib import Path

import numpy as np

from common import Camera, Mesh, check_closed, load_meshes, shade, triangle_frame, unit
from run import camera_for, hit_metrics


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--meshes', default='.amp/in/artifacts/far-field-comparison/meshes.json')
    parser.add_argument('--output', default='.amp/in/artifacts/far-field-comparison/check.json')
    parser.add_argument('--variants', default='analytic,bricks,aggregate')
    args = parser.parse_args()
    modules = {name: importlib.import_module(name) for name in args.variants.split(',') if name}
    # The central four rays hit a square five cells away. A different-colored
    # back square must not leak through it, regardless of triangle order.
    square = np.array([[[-1, 0, -1], [1, 0, -1], [1, 0, 1]],
                       [[-1, 0, -1], [1, 0, 1], [-1, 0, 1]]], dtype=float)
    triangles = np.concatenate([square, square + [0, 2, 0]])
    mesh = Mesh(triangles, np.array([0, 0, 2, 2]))
    camera = Camera(np.array([0., -5., 0.]), np.zeros(3), 4)
    a = triangle_frame(mesh, camera, 4, 4, [0, -1, 1])
    assert a.coverage.sum() == 4
    assert np.all(a.depth[1:3, 1:3] == 5)
    assert np.all(a.material[1:3, 1:3] == 0)
    assert np.all(a.normal[1:3, 1:3] == [0, -1, 0])
    reverse = Mesh(triangles[::-1], mesh.material[::-1])
    b = triangle_frame(reverse, camera, 4, 4, [0, -1, 1])
    assert np.array_equal(a.depth, b.depth)
    assert np.array_equal(a.color, b.color)
    # Independent nonlinear witness: averaging normals invents a strong lobe.
    normals = np.array([[0., -1., 0.], [0., 0., 1.]])
    light = unit([0., -1., 1.])
    mixed = shade(normals, np.array([0, 0]), np.zeros((2, 3)), light, light).mean(axis=0)
    averaged = shade(unit(normals.mean(axis=0)), np.array(0), np.zeros(3), light, light)
    assert np.max(np.abs(averaged - mixed)) > .5
    report = {'reference_occlusion': 'passed', 'nonlinear_normal_witness': {
        'mixture': mixed.tolist(), 'mean_normal': averaged.tolist()}, 'fixtures': {}}
    for name, mesh in load_meshes(args.meshes).items():
        edges = check_closed(mesh)
        camera = camera_for(mesh, -.27, .19)
        reference = triangle_frame(mesh, camera, 97, 73, [-.3, -.6, 1])
        fixture = {'oriented_edge_segments': edges, 'source_triangles': len(mesh.triangles)}
        for variant, module in modules.items():
            rep = module.build(mesh)
            frame, stats = module.render(rep, camera, 97, 73, [-.3, -.6, 1])
            assert frame.depth.shape == reference.depth.shape
            assert np.isfinite(frame.color).all()
            assert np.all((frame.coverage >= 0) & (frame.coverage <= 1))
            assert np.all(np.isfinite(frame.depth[frame.coverage > 0]))
            metrics = hit_metrics(frame, reference)
            if variant == 'analytic':
                assert metrics['hit_disagreement_fraction_on_union'] == 0, (name, metrics)
                assert metrics['material_disagreement_fraction_on_common'] == 0, (name, metrics)
                assert metrics['depth_absolute_max_cells'] < 1e-9, (name, metrics)
                assert np.max(np.abs(frame.color - reference.color)) < 1e-9, name
            # Approximate candidates are measured, not asserted to equal a reference
            # that their representation intentionally cannot reproduce everywhere.
            fixture[variant] = {**metrics, 'work': stats}
        report['fixtures'][name] = fixture
    Path(args.output).write_text(json.dumps(report, indent=2, allow_nan=False) + '\n')
    print('Reference occlusion, nonlinear witness, six closed fixtures, and candidate contracts passed.')


if __name__ == '__main__':
    main()
