"""Shared CPU information probe, not a production renderer or GPU benchmark.

All coordinates are world cells, Z up. Cameras are orthographic, appropriate
to a small distant patch; depth is distance along the camera forward axis.
Colors are scene-linear. Source triangles keep their outward orientation.
"""
from dataclasses import dataclass
import json
from pathlib import Path

import numpy as np


BACKGROUND = np.array([0.055, 0.075, 0.11])
PALETTE = np.array([[0.44, 0.23, 0.095], [0.18, 0.32, 0.11],
                    [0.12, 0.19, 0.43], [0.54, 0.10, 0.12]])


def unit(v):
    v = np.asarray(v, dtype=float)
    return v / np.maximum(np.linalg.norm(v, axis=-1, keepdims=True), 1e-30)


@dataclass
class Mesh:
    triangles: np.ndarray  # T,3,3
    material: np.ndarray  # T, integer palette index

    @property
    def normals(self):
        return unit(np.cross(self.triangles[:, 1] - self.triangles[:, 0],
                             self.triangles[:, 2] - self.triangles[:, 0]))

    @property
    def bounds(self):
        return self.triangles.min(axis=(0, 1)), self.triangles.max(axis=(0, 1))


@dataclass
class Camera:
    eye: np.ndarray
    target: np.ndarray
    span: float  # horizontal world extent

    def basis(self):
        forward = unit(np.asarray(self.target) - self.eye)
        right = unit(np.cross(forward, [0, 0, 1]))
        up = np.cross(right, forward)
        return right, up, forward

    def project(self, points, width, height):
        right, up, forward = self.basis()
        p = np.asarray(points) - self.eye
        return np.stack([(p @ right / self.span + 0.5) * width,
                         height / 2 - (p @ up / self.span) * width,
                         p @ forward], axis=-1)

    def rays(self, width, height):
        right, up, forward = self.basis()
        y, x = np.mgrid[:height, :width]
        origins = (np.asarray(self.eye)
                   + ((x + 0.5 - width / 2) * self.span / width)[..., None] * right
                   - ((y + 0.5 - height / 2) * self.span / width)[..., None] * up)
        return origins, forward


@dataclass
class Frame:
    color: np.ndarray  # H,W,3 linear radiance including background
    depth: np.ndarray  # H,W, +inf for misses, no averaged-depth compositing
    coverage: np.ndarray  # H,W, in [0,1]
    material: np.ndarray  # H,W, -1 for misses; dominant ID for mixtures
    normal: np.ndarray  # H,W,3; diagnostic only for mixture representations


def shade(normals, materials, positions, light, view):
    """Matched cheap relightable response; no shadows/procedural terrain shader.

    A narrow white highlight deliberately distinguishes E[f(n)] from f(E[n]).
    Position is accepted so independent candidates may add world-fixed marks.
    """
    normals = np.asarray(normals)
    light, view = unit(light), unit(view)
    diffuse = np.maximum(normals @ light, 0)
    halfway = unit(light + view)
    specular = np.maximum(normals @ halfway, 0) ** 48
    ambient = 0.16 + 0.10 * (normals[..., 2] * 0.5 + 0.5)
    return (PALETTE[np.maximum(materials, 0)] * (ambient + 0.85 * diffuse)[..., None]
            + (0.65 * specular * (diffuse > 0))[..., None])


def frame_from_hits(camera, depth, normals, materials, light):
    height, width = depth.shape
    origins, direction = camera.rays(width, height)
    valid = np.isfinite(depth)
    points = origins + np.where(valid, depth, 0)[..., None] * direction
    color = shade(normals, materials, points, light, -direction)
    color[~valid] = BACKGROUND
    return Frame(color, depth, valid.astype(float), materials, normals)


def triangle_hits(mesh, camera, width, height):
    """Independent barycentric z-buffer reference; no candidate helpers used."""
    depth = np.full((height, width), np.inf)
    ids = np.full((height, width), -1, dtype=np.int32)
    projected = camera.project(mesh.triangles, width, height)
    normals = mesh.normals
    forward = camera.basis()[2]
    for i, p in enumerate(projected):
        if normals[i] @ forward >= -1e-12:
            continue
        lo = np.maximum(np.floor(p[:, :2].min(axis=0)).astype(int), 0)
        hi = np.minimum(np.ceil(p[:, :2].max(axis=0)).astype(int), [width, height])
        if np.any(hi <= lo):
            continue
        x0, y0 = lo
        x1, y1 = hi
        y, x = np.mgrid[y0:y1, x0:x1]
        x, y = x + 0.5, y + 0.5
        a, b, c = p
        denominator = (b[1] - c[1]) * (a[0] - c[0]) + (c[0] - b[0]) * (a[1] - c[1])
        if abs(denominator) < 1e-14:
            continue
        u = ((b[1] - c[1]) * (x - c[0]) + (c[0] - b[0]) * (y - c[1])) / denominator
        v = ((c[1] - a[1]) * (x - c[0]) + (a[0] - c[0]) * (y - c[1])) / denominator
        w = 1 - u - v
        z = u * a[2] + v * b[2] + w * c[2]
        region = depth[y0:y1, x0:x1]
        take = ((u >= -1e-10) & (v >= -1e-10) & (w >= -1e-10)
                & (z > 0) & (z < region - 1e-10))
        region[take] = z[take]
        ids[y0:y1, x0:x1][take] = i
    valid = ids >= 0
    n = np.zeros((height, width, 3))
    m = np.full((height, width), -1, dtype=np.int32)
    n[valid] = normals[ids[valid]]
    m[valid] = mesh.material[ids[valid]]
    return depth, n, m


def triangle_frame(mesh, camera, width, height, light):
    return frame_from_hits(camera, *triangle_hits(mesh, camera, width, height), light)


def resolve(frame, samples):
    """Box integration of linear radiance and coverage, never average normals."""
    h, w = frame.depth.shape
    def mean(a):
        return a.reshape(h // samples, samples, w // samples, samples,
                         *a.shape[2:]).mean(axis=(1, 3))
    return mean(frame.color), mean(frame.coverage)


def load_meshes(path):
    data = json.loads(Path(path).read_text())
    return {name: Mesh(np.asarray(scene['triangles'], dtype=float) / 8,
                       np.asarray(scene['material'], dtype=np.int32))
            for name, scene in data.items()}


def check_closed(mesh):
    """Oriented chain closure on integer ticks, with long edges subdivided.

    This is not a proof of manifoldness or absence of self-intersection.
    """
    from collections import Counter
    from functools import reduce
    from math import gcd
    edges = Counter()
    ticks = np.rint(mesh.triangles * 8).astype(int)
    assert np.allclose(ticks / 8, mesh.triangles)
    for triangle in ticks:
        for a, b in zip(triangle, np.roll(triangle, -1, axis=0)):
            d = b - a
            count = reduce(gcd, map(abs, d))
            for j in range(count):
                x = tuple(a + d * j // count)
                y = tuple(a + d * (j + 1) // count)
                edges[(x, y)] += 1
    assert all(count == edges[(b, a)] for (a, b), count in edges.items())
    assert np.all(np.linalg.norm(np.cross(mesh.triangles[:, 1] - mesh.triangles[:, 0],
                                         mesh.triangles[:, 2] - mesh.triangles[:, 0]), axis=1) > 0)
    return len(edges)
