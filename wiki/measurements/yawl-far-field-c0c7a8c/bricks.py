"""CPU surface bricks: baked planes and finite tangent masks, never solid cubes.

Each sparse cell belongs to a spatial 4^3 brick. Distinct oriented planes and
materials remain separate records, with center-sampled binary footprint masks.
Rendering projects cell bounds and ray-tests only baked records: no source mesh,
triangle fallback, alpha blending, or averaged depth. Caller owns supersampling.
Packed-array byte counts exclude Python objects and transient render arrays.
"""
from dataclasses import dataclass
from itertools import product
from time import perf_counter

import numpy as np

from common import frame_from_hits


RECORD = np.dtype([("normal", "<u2"), ("material", "<i4"), ("plane", "<f8")])
CORNERS = np.array(list(product((0, 1), repeat=3)))


@dataclass
class Bricks:
    cell_size: float
    mask_size: int
    cells: np.ndarray
    cell_starts: np.ndarray
    bricks: np.ndarray
    brick_starts: np.ndarray
    normals: np.ndarray
    records: np.ndarray
    masks: np.ndarray
    stats: dict


def _clip(polygon, axis, bound, lower):
    """Sutherland-Hodgman clipping, used only while baking."""
    if len(polygon) == 0:
        return polygon
    out = []
    a = polygon[-1]
    da = (a[axis] - bound) * (1 if lower else -1)
    for b in polygon:
        db = (b[axis] - bound) * (1 if lower else -1)
        if (da >= 0) != (db >= 0):
            out.append(a + (b - a) * (da / (da - db)))
        if db >= 0:
            out.append(b)
        a, da = b, db
    return np.asarray(out).reshape(-1, 3)


def _area(polygon):
    if len(polygon) < 3:
        return 0.0
    return float(np.linalg.norm(np.cross(polygon[1:-1] - polygon[0],
                                        polygon[2:] - polygon[0]), axis=1).sum() / 2)


def _mask(polygon, normal, base, size, count):
    axes = [a for a in range(3) if a != np.argmax(np.abs(normal))]
    p = (polygon[:, axes] - base[axes]) / size * count
    lo = np.maximum(np.floor(p.min(axis=0)).astype(int), 0)
    hi = np.minimum(np.ceil(p.max(axis=0)).astype(int), count)
    mask = np.zeros((count, count), dtype=bool)
    if np.any(hi <= lo):
        return mask
    y, x = np.mgrid[lo[1]:hi[1], lo[0]:hi[0]]
    q = np.stack((x + 0.5, y + 0.5), axis=-1)
    positive = np.ones(x.shape, dtype=bool)
    negative = positive.copy()
    for a, b in zip(p, np.roll(p, -1, axis=0)):
        side = (b[0] - a[0]) * (q[..., 1] - a[1]) - (b[1] - a[1]) * (q[..., 0] - a[0])
        positive &= side >= -1e-10
        negative &= side <= 1e-10
    mask[lo[1]:hi[1], lo[0]:hi[0]] = positive | negative
    return mask


def build(mesh, cell_size=0.5, mask_size=8):
    """Bake directly from input triangles, retaining no source geometry.

    There is no record-count cap or exact fallback. Thin fragments can vanish;
    losses and quantized area proxies are reported rather than silently repaired.
    """
    start = perf_counter()
    if not np.isfinite(cell_size) or cell_size <= 0:
        raise ValueError("cell_size must be finite and positive")
    if not isinstance(mask_size, (int, np.integer)) or not 1 <= mask_size <= 256:
        raise ValueError("mask_size must be an integer in [1, 256]")
    cell_size, mask_size = float(cell_size), int(mask_size)
    groups, normal_ids, normals = {}, {}, []
    source_area = clipped_area = missed_fragment_area = 0.0
    fragments = missed_fragments = 0
    for triangle, normal, material in zip(mesh.triangles, mesh.normals, mesh.material):
        source_area += _area(triangle)
        nk = tuple(np.round(normal, 12))
        if nk not in normal_ids:
            normal_ids[nk] = len(normals)
            normals.append(normal.copy())
        ni = normal_ids[nk]
        normal = normals[ni]
        # Source coordinates are ticks; rounding only identifies coincident planes.
        plane = round(float(triangle[0] @ normal), 12)
        lo = np.floor(triangle.min(axis=0) / cell_size).astype(int)
        hi = np.floor(triangle.max(axis=0) / cell_size).astype(int)
        for key in product(*(range(a, b + 1) for a, b in zip(lo, hi))):
            base = np.asarray(key) * cell_size
            polygon = triangle.copy()
            for axis in range(3):
                polygon = _clip(polygon, axis, base[axis], True)
                polygon = _clip(polygon, axis, base[axis] + cell_size, False)
            area = _area(polygon)
            if area < 1e-14:
                continue
            # A coplanar grid-boundary surface belongs to the positive-side cell.
            if any(np.all(np.abs(polygon[:, a] - (base[a] + cell_size)) < 1e-12)
                   for a in range(3)):
                continue
            fragments += 1
            clipped_area += area
            mask = _mask(polygon, normal, base, cell_size, mask_size)
            if not mask.any():
                missed_fragments += 1
                missed_fragment_area += area
            group = groups.setdefault(key, {})
            rk = (ni, int(material), plane)
            if rk not in group:
                group[rk] = [mask, area]
            else:
                group[rk][0] |= mask
                group[rk][1] += area
    keys = sorted(groups, key=lambda k: (tuple(v // 4 for v in k), k))
    cells, cell_starts, bricks, brick_starts, records, masks = [], [0], [], [], [], []
    lost_records = 0
    lost_area = quantized_area = 0.0
    layers, unique_planes = [], set()
    for key in keys:
        entries = [(r, m) for r, m in sorted(groups[key].items()) if m[0].any()]
        for r, (mask, area) in groups[key].items():
            if not mask.any():
                lost_records += 1
                lost_area += area
        if not entries:
            continue
        bk = tuple(v // 4 for v in key)
        if not bricks or bk != bricks[-1]:
            bricks.append(bk)
            brick_starts.append(len(cells))
        cells.append(key)
        planes = set()
        for (ni, material, plane), (mask, area) in entries:
            records.append((ni, material, plane))
            masks.append(np.packbits(mask.ravel(), bitorder="little"))
            planes.add((ni, plane))
            unique_planes.add((ni, plane))
            quantized_area += (int(mask.sum()) * (cell_size / mask_size) ** 2
                               / max(abs(normals[ni])))
        layers.append(len(planes))
        cell_starts.append(len(records))
    brick_starts.append(len(cells))
    rep = Bricks(cell_size, mask_size,
                 np.asarray(cells, dtype=np.int32).reshape(-1, 3),
                 np.asarray(cell_starts, dtype=np.uint32),
                 np.asarray(bricks, dtype=np.int32).reshape(-1, 3),
                 np.asarray(brick_starts, dtype=np.uint32),
                 np.asarray(normals, dtype=np.float64).reshape(-1, 3),
                 np.asarray(records, dtype=RECORD),
                 np.asarray(masks, dtype=np.uint8).reshape(-1, (mask_size ** 2 + 7) // 8), {})
    counts = {name: int(getattr(rep, name).nbytes) for name in
              ("cells", "cell_starts", "bricks", "brick_starts", "normals", "records", "masks")}
    rep.stats = dict(cell_size=cell_size, mask_size=mask_size,
                     source_triangles=len(mesh.triangles), source_area=source_area,
                     clipped_area=clipped_area, clipped_fragments=fragments,
                     center_missed_fragments=missed_fragments,
                     center_missed_fragment_area=missed_fragment_area,
                     dropped_empty_records=lost_records, dropped_record_area=lost_area,
                     quantized_area_proxy=quantized_area, cells=len(cells), bricks=len(bricks),
                     records=len(records), normals=len(normals), planes=len(unique_planes),
                     cell_plane_instances=sum(layers), max_planes_per_cell=max(layers, default=0),
                     plane_histogram={str(n): layers.count(n) for n in sorted(set(layers))},
                     array_bytes=counts, record_bytes=counts["records"], mask_bytes=counts["masks"],
                     total_array_bytes=sum(counts.values()), original_mesh_fallbacks=0,
                     build_seconds=perf_counter() - start)
    return rep


def render(rep, camera, width, height, light):
    """First-hit opaque ray/plane queries, scheduled via projected cell bounds.

    The same depth wins both material and normal. Mask expansion may incorrectly
    occlude; mask erosion may expose a rear wall. Those are quantization errors,
    not hidden source-mesh repairs. frame_from_hits calls the shared shade.
    """
    start = perf_counter()
    origins, direction = camera.rays(width, height)
    depth = np.full((height, width), np.inf)
    normal_out = np.zeros((height, width, 3))
    material_out = np.full((height, width), -1, dtype=np.int32)
    work = dict(bricks_tested=0, bricks_visible=0, cells_tested=0, cells_visible=0,
                records_considered=0, backface_records=0, ray_plane_tests=0,
                mask_tests=0, mask_hits=0, depth_updates=0)
    normals = rep.normals
    denominators = normals @ direction
    axes = [[a for a in range(3) if a != np.argmax(np.abs(n))] for n in normals]

    def bounds(base, size):
        projected = camera.project(base + CORNERS * size, width, height)
        if projected[:, 2].max() <= 0:
            return None
        lo = np.maximum(np.floor(projected[:, :2].min(axis=0)).astype(int), 0)
        hi = np.minimum(np.ceil(projected[:, :2].max(axis=0)).astype(int), [width, height])
        return None if np.any(hi <= lo) else (*lo, *hi)

    for bi, brick in enumerate(rep.bricks):
        work["bricks_tested"] += 1
        if bounds(brick * (4 * rep.cell_size), 4 * rep.cell_size) is None:
            continue
        work["bricks_visible"] += 1
        for ci in range(int(rep.brick_starts[bi]), int(rep.brick_starts[bi + 1])):
            work["cells_tested"] += 1
            base = rep.cells[ci] * rep.cell_size
            box = bounds(base, rep.cell_size)
            if box is None:
                continue
            work["cells_visible"] += 1
            x0, y0, x1, y1 = box
            ray = origins[y0:y1, x0:x1]
            region = depth[y0:y1, x0:x1]
            for ri in range(int(rep.cell_starts[ci]), int(rep.cell_starts[ci + 1])):
                work["records_considered"] += 1
                record = rep.records[ri]
                ni = int(record["normal"])
                denominator = denominators[ni]
                if denominator >= -1e-12:
                    work["backface_records"] += 1
                    continue
                work["ray_plane_tests"] += region.size
                z = (float(record["plane"]) - ray @ normals[ni]) / denominator
                local = (ray + z[..., None] * direction - base) / rep.cell_size
                valid = ((z > 0) & (z < region - 1e-10)
                         & np.all((local >= -1e-10) & (local <= 1 + 1e-10), axis=-1))
                work["mask_tests"] += int(valid.sum())
                if not valid.any():
                    continue
                uv = np.floor(local[valid][:, axes[ni]] * rep.mask_size).astype(int)
                uv = np.clip(uv, 0, rep.mask_size - 1)
                index = uv[:, 1] * rep.mask_size + uv[:, 0]
                hit = ((rep.masks[ri, index // 8] >> (index % 8)) & 1).astype(bool)
                work["mask_hits"] += int(hit.sum())
                valid[valid] = hit
                region[valid] = z[valid]
                normal_out[y0:y1, x0:x1][valid] = normals[ni]
                material_out[y0:y1, x0:x1][valid] = record["material"]
                work["depth_updates"] += int(hit.sum())
    frame = frame_from_hits(camera, depth, normal_out, material_out, light)
    return frame, dict(rep.stats, work=work, render_seconds=perf_counter() - start)
