"""Bounded CPU directional surface cache; deliberately not a solved terrain LOD.

Bake first hits from nine fixed directions. Each coarse texel keeps at most two
depth sheets and four normal/material/plane lobes per sheet. A 4x4 occupancy mask
per lobe prevents averaging a cave wall and its background into one depth.
Render intersects these cached planes, never the original triangles. Their
support is only a quantized first-hit mask: new views can reveal holes, overlap,
and wrong visibility. Nearest-view switching is intentionally discontinuous.

The mixture is shaded, not its mean normal. render_averaged is a separate control
which keeps the same geometry and material weights. Neither path handles shadows
or disoccluded surfaces absent from the bake. Caller owns supersampling/resolve.
"""
from dataclasses import dataclass
from time import perf_counter

import numpy as np

from common import BACKGROUND, Camera, Frame, shade, triangle_hits, unit


SAMPLES = 4
LAYERS = 2
LOBES = 4


@dataclass
class View:
    camera: Camera
    cells: np.ndarray
    points: np.ndarray
    normals: np.ndarray
    materials: np.ndarray
    weights: np.ndarray
    masks: np.ndarray
    depth_ranges: np.ndarray

    def nbytes(self):
        arrays = (self.cells, self.points, self.normals, self.materials,
                  self.weights, self.masks, self.depth_ranges,
                  self.camera.eye, self.camera.target)
        return sum(a.nbytes for a in arrays) + 8  # camera span


@dataclass
class SurfaceCache:
    resolution: int
    views: list
    stats: dict


def _bake_view(mesh, camera, resolution, stats):
    size = resolution * SAMPLES
    depth, normal, material = triangle_hits(mesh, camera, size, size)
    origins, forward = camera.rays(size, size)
    points = origins + np.where(np.isfinite(depth), depth, 0)[..., None] * forward
    records = []
    # A heuristic sheet split, NOT topology inference. Large within-texel depth
    # gaps must not be merged; a slanted continuous surface may also be split.
    gap = 1.5 * camera.span / resolution
    for y in range(resolution):
        for x in range(resolution):
            s = np.s_[y*SAMPLES:(y+1)*SAMPLES, x*SAMPLES:(x+1)*SAMPLES]
            d, n, m = depth[s].ravel(), normal[s].reshape(-1, 3), material[s].ravel()
            p = points[s].reshape(-1, 3)
            live = np.flatnonzero(np.isfinite(d))
            stats['first_hit_samples'] += int(len(live))
            if not len(live):
                continue
            ordered = live[np.argsort(d[live], kind='stable')]
            sheets = np.split(ordered, np.flatnonzero(np.diff(d[ordered]) > gap) + 1)
            stats['layer_overflow_texels'] += int(len(sheets) > LAYERS)
            stats['layer_truncated_samples'] += sum(len(a) for a in sheets[LAYERS:])
            for sheet in sheets[:LAYERS]:
                # Distinct parallel planes remain distinct lobes even when
                # normal/material coincide. A lobe anchor is on ONE plane.
                groups = {}
                for i in sheet:
                    key = (*np.rint(n[i] * 1e6).astype(int), int(m[i]),
                           int(round(float(p[i] @ n[i]) * 1e5)))
                    groups.setdefault(key, []).append(int(i))
                groups = sorted(groups.values(), key=lambda a: (-len(a), a[0]))
                stats['lobe_overflow_layers'] += int(len(groups) > LOBES)
                stats['lobe_truncated_samples'] += sum(len(a) for a in groups[LOBES:])
                kept = groups[:LOBES]
                count = sum(map(len, kept))
                anchors = np.zeros((LOBES, 3), dtype=np.float32)
                normals = np.zeros_like(anchors)
                materials = np.full(LOBES, -1, dtype=np.int16)
                weights = np.zeros(LOBES, dtype=np.float32)
                masks = np.zeros(LOBES, dtype=np.uint16)
                for j, indices in enumerate(kept):
                    anchors[j] = p[indices].mean(axis=0)
                    normals[j], materials[j] = n[indices[0]], m[indices[0]]
                    # Explicitly condition on retained samples. Dropped samples
                    # have no geometry, and their count is reported above.
                    weights[j] = len(indices) / count
                    masks[j] = sum(1 << i for i in indices)
                records.append(((x, y), anchors, normals, materials, weights,
                                masks, (float(d[sheet].min()), float(d[sheet].max()))))
    shapes = ((2,), (LOBES, 3), (LOBES, 3), (LOBES,), (LOBES,), (LOBES,), (2,))
    types = (np.int32, np.float32, np.float32, np.int16,
             np.float32, np.uint16, np.float32)
    arrays = [np.asarray([r[i] for r in records], dtype=t).reshape((-1, *shape))
              for i, (shape, t) in enumerate(zip(shapes, types))]
    return View(camera, *arrays)


def build(mesh, resolution=32):
    """Bake an immutable-by-convention cache. The result retains no mesh."""
    if not isinstance(resolution, (int, np.integer)) or resolution < 4:
        raise ValueError('resolution must be an integer >= 4')
    start = perf_counter()
    lo, hi = mesh.bounds
    center = (lo + hi) / 2
    extent = float(np.max(hi - lo))
    if extent <= 0:
        raise ValueError('mesh must have nonzero extent')
    stats = dict(first_hit_samples=0, layer_overflow_texels=0,
                 layer_truncated_samples=0, lobe_overflow_layers=0,
                 lobe_truncated_samples=0)
    views = []
    for elevation in (0.10, 0.375, 0.65):
        for angle in (-25.0, 0.0, 25.0):
            a = np.radians(angle)
            offset = unit([np.sin(a), -np.cos(a), elevation])
            camera = Camera(center + offset * extent * 3, center.copy(), extent * 1.5)
            views.append(_bake_view(mesh, camera, resolution, stats))
    stats.update(resolution=int(resolution), directions=len(views),
                 subsamples_per_texel=SAMPLES**2, layer_capacity=LAYERS,
                 lobe_capacity=LOBES, bytes=sum(v.nbytes() for v in views),
                 layers=sum(len(v.cells) for v in views),
                 lobes=sum(int(np.count_nonzero(v.weights)) for v in views),
                 bake_rays=len(views) * (resolution * SAMPLES)**2,
                 bake_seconds=perf_counter() - start,
                 bytes_scope='array payload plus camera scalars; excludes Python object overhead',
                 source_mesh_retained=False,
                 hidden_layers_retained=False,
                 sheet_gap_world_cells=1.5 * extent * 1.5 / resolution)
    return SurfaceCache(int(resolution), views, stats)


def _mark_gain(points):
    """Broad continuous world-space strokes; no view, screen, or cache index.

    This is an artistic modulation, not an exactly mean-preserving filter.
    """
    p = np.asarray(points)
    broad = np.sin(p @ np.array([1.7, 0.23, 0.61]))
    warp = np.sin(p @ np.array([0.19, -0.77, 1.13]))
    return 1 + 0.12 * broad * (0.7 + 0.3 * warp)


def _rectangle(view, cell, lobe, mask, resolution, camera, width, height):
    """Projected bound of the occupied bake subcells on this cached plane."""
    bits = np.flatnonzero((int(mask) >> np.arange(SAMPLES**2)) & 1)
    x, y = view.cells[cell]
    left = x + (bits % SAMPLES).min() / SAMPLES
    right = x + ((bits % SAMPLES).max() + 1) / SAMPLES
    top = y + (bits // SAMPLES).min() / SAMPLES
    bottom = y + ((bits // SAMPLES).max() + 1) / SAMPLES
    uv = np.array([[left, top], [right, top], [right, bottom], [left, bottom]])
    br, bu, bf = view.camera.basis()
    origins = (view.camera.eye + (uv[:, :1] / resolution - 0.5) * view.camera.span * br
               - (uv[:, 1:] / resolution - 0.5) * view.camera.span * bu)
    n, anchor = view.normals[cell, lobe], view.points[cell, lobe]
    t = ((anchor - origins) @ n) / (bf @ n)
    screen = camera.project(origins + t[:, None] * bf, width, height)
    lo = np.maximum(np.floor(screen[:, :2].min(axis=0)).astype(int), 0)
    hi = np.minimum(np.ceil(screen[:, :2].max(axis=0)).astype(int), [width, height])
    return (*lo, *hi)


def _render(rep, camera, width, height, light, painterly, averaged):
    if width <= 0 or height <= 0:
        raise ValueError('width and height must be positive')
    start = perf_counter()
    origins, forward = camera.rays(width, height)
    toward_eye = -forward
    directions = np.array([-v.camera.basis()[2] for v in rep.views])
    scores = directions @ toward_eye
    index = int(np.argmax(scores))
    view = rep.views[index]
    br, bu, bf = view.camera.basis()
    stats = dict(rep.stats)
    azimuth = float(np.degrees(np.arctan2(toward_eye[0], -toward_eye[1])))
    elevation = float(toward_eye[2] / max(np.linalg.norm(toward_eye[:2]), 1e-30))
    stats.update(selected_view=index, view_policy='nearest; no interpolation or fallback',
                 view_error_degrees=float(np.degrees(np.arccos(np.clip(scores[index], -1, 1)))),
                 outside_bake_cone=not (-25.00001 <= azimuth <= 25.00001
                                       and 0.099999 <= elevation <= 0.650001),
                 plane_tests=0, raster_candidates=0, depth_updates=0,
                 backfacing_lobes=0, shaded_lobes=int(np.count_nonzero(view.weights)),
                 shade_slots_including_padding=int(view.weights.size),
                 averaged_normal_control=bool(averaged), painterly=bool(painterly),
                 coverage_mode='binary subcell masks; caller supersamples',
                 original_triangle_tests_at_render=0)
    depth = np.full((height, width), np.inf)
    color = np.broadcast_to(BACKGROUND, (height, width, 3)).copy()
    material = np.full((height, width), -1, dtype=np.int32)
    normal = np.zeros((height, width, 3))
    mean_n = unit(np.sum(view.normals * view.weights[..., None], axis=1))
    dominant = np.argmax(view.weights, axis=1)
    dominant_material = view.materials[np.arange(len(dominant)), dominant]
    shaded_normals = (np.broadcast_to(mean_n[:, None], view.normals.shape)
                      if averaged else view.normals)
    # common.shade is position-independent. Evaluate all retained lobes once
    # per render, not the expensive source material nor a pre-baked sun colour.
    response = shade(shaded_normals, view.materials, view.points, light, toward_eye)
    mixture = np.sum(response * view.weights[..., None], axis=1)
    for cell in range(len(view.cells)):
        cx, cy = view.cells[cell]
        for lobe in range(LOBES):
            mask = int(view.masks[cell, lobe])
            if not mask:
                continue
            n, anchor = view.normals[cell, lobe], view.points[cell, lobe]
            den = float(forward @ n)
            if den >= -1e-8:
                stats['backfacing_lobes'] += 1
                continue
            x0, y0, x1, y1 = _rectangle(view, cell, lobe, mask, rep.resolution,
                                      camera, width, height)
            if x1 <= x0 or y1 <= y0:
                continue
            region = np.s_[y0:y1, x0:x1]
            ray = origins[region]
            t = ((anchor - ray) @ n) / den
            stats['plane_tests'] += int(t.size)
            stats['raster_candidates'] += int(t.size)
            p = ray + t[..., None] * forward
            q = p - view.camera.eye
            bx = ((q @ br) / view.camera.span + 0.5) * rep.resolution - cx
            by = (0.5 - (q @ bu) / view.camera.span) * rep.resolution - cy
            inside = (bx >= 0) & (bx < 1) & (by >= 0) & (by < 1)
            sx = np.clip(np.floor(bx * SAMPLES).astype(int), 0, SAMPLES-1)
            sy = np.clip(np.floor(by * SAMPLES).astype(int), 0, SAMPLES-1)
            occupied = ((mask >> (sy * SAMPLES + sx)) & 1) != 0
            take = inside & occupied & (t > 0) & (t < depth[region] - 1e-8)
            if not np.any(take):
                continue
            stats['depth_updates'] += int(np.count_nonzero(take))
            depth[region][take] = t[take]  # a plane hit, NEVER a sheet-depth mean
            gain = _mark_gain(p[take]) if painterly else np.ones(np.count_nonzero(take))
            color[region][take] = mixture[cell] * gain[:, None]
            material[region][take] = dominant_material[cell]
            normal[region][take] = mean_n[cell]
    coverage = np.isfinite(depth).astype(float)
    stats.update(render_seconds=perf_counter() - start,
                 covered_pixels=int(coverage.sum()),
                 uncovered_pixels_including_background=int(coverage.size - coverage.sum()))
    return Frame(color, depth, coverage, material, normal), stats


def render(rep, camera, width, height, light, painterly=False):
    """Reconstruct cached planes and shade the retained normal/material mixture."""
    return _render(rep, camera, width, height, light, painterly, False)


def render_averaged(rep, camera, width, height, light, painterly=False):
    """Same visibility/material weights, but shade one mean normal (control)."""
    return _render(rep, camera, width, height, light, painterly, True)
