#!/usr/bin/env python3
"""Bake exact bounded convex Luft domains; no renderer or GPU measurements.

    uv run --with numpy python wiki/measurements/yawl-analytic-gpu-006b266/prepare.py

Public CPU test interface: build(mesh), buffers(rep), contains(patch, points),
nearest_hits(rep, origins, direction), validate(mesh, rep). Mesh is common.Mesh
(world coordinates, integer material IDs). Patches own immutable integer-tick
polygons, not source triangles. Only reference_buffers reads source geometry.

Greedy merges cancel oppositely oriented atomic lattice edges, trace ONE simple
boundary, and accept only convex, <=8-edge, <=8-cell unions. There is no convex
hull operation. Subdividing edges to primitive lattice segments matches long
edges with T-junction neighbours. Point-only contacts do not count as adjacency.
Input is an oriented, non-overlapping atlas triangulation, not an arbitrary
intersecting triangle soup. Degenerate/non-lattice/nonmanifold-edge input fails.
"""

import argparse
from collections import Counter, defaultdict
from dataclasses import dataclass
from functools import reduce
import hashlib
import heapq
import json
from math import gcd
from pathlib import Path
import sys
from time import perf_counter

import numpy as np


ROOT = Path(__file__).resolve().parents[3]
COMMON = ROOT / "wiki/measurements/yawl-far-field-c0c7a8c/common.py"
TICKS = 8
MAX_EDGES = 8
MAX_SPAN = 8 * TICKS


def sub(a, b):
    return tuple(x - y for x, y in zip(a, b))


def cross(a, b):
    return (a[1] * b[2] - a[2] * b[1],
            a[2] * b[0] - a[0] * b[2],
            a[0] * b[1] - a[1] * b[0])


def dot(a, b):
    return sum(x * y for x, y in zip(a, b))


def pairs(vertices):
    return zip(vertices, vertices[1:] + vertices[:1])


def canonical(vertices):
    """Rotate without reversing outward winding."""
    i = vertices.index(min(vertices))
    return vertices[i:] + vertices[:i]


def plane(triangle, material):
    n = cross(sub(triangle[1], triangle[0]), sub(triangle[2], triangle[0]))
    divisor = reduce(gcd, map(abs, n))
    if not divisor:
        raise ValueError("degenerate source triangle")
    n = tuple(c // divisor for c in n)
    return (*n, dot(n, triangle[0]), material)


def projection(key):
    drop = max(range(3), key=lambda axis: abs(key[axis]))
    axes = tuple(axis for axis in range(3) if axis != drop)
    sign = (1 if key[drop] > 0 else -1) * (-1 if drop == 1 else 1)
    return drop, axes, sign


def area2(vertices, key):
    """Exact oriented doubled projected area in tick squared units."""
    _, (u, v), _ = projection(key)
    return sum(a[u] * b[v] - a[v] * b[u] for a, b in pairs(vertices))


def atomic_edges(vertices):
    result = set()
    for a, b in pairs(vertices):
        delta = sub(b, a)
        steps = reduce(gcd, map(abs, delta))
        step = tuple(c // steps for c in delta)
        p = a
        for _ in range(steps):
            q = tuple(c + d for c, d in zip(p, step))
            result.add((p, q))
            p = q
    return frozenset(result)


def within_span(vertices):
    return all(max(p[i] for p in vertices) - min(p[i] for p in vertices)
               <= MAX_SPAN for i in range(3))


def convex(vertices, key):
    _, (u, v), sign = projection(key)
    for i, b in enumerate(vertices):
        a, c = vertices[i - 1], vertices[(i + 1) % len(vertices)]
        if sign * ((b[u] - a[u]) * (c[v] - b[v])
                   - (b[v] - a[v]) * (c[u] - b[u])) <= 0:
            return False
    return True


@dataclass(frozen=True)
class Patch:
    vertices: tuple  # immutable outward polygon; integer eighth-cell ticks
    key: tuple       # primitive oriented normal xyz, tick plane offset, material

    @property
    def normal(self):
        n = np.asarray(self.key[:3], dtype=float)
        return n / np.linalg.norm(n)

    @property
    def material(self):
        return self.key[4]


@dataclass(frozen=True)
class Representation:
    patches: tuple
    stats: dict


@dataclass(frozen=True)
class _Piece:
    vertices: tuple
    edges: frozenset
    area: int


def _merge(a, b, key):
    if not within_span(a.vertices + b.vertices):
        return None, "span"
    shared = a.edges & frozenset((q, p) for p, q in b.edges)
    if not shared:
        return None, "not_adjacent"
    edges = (a.edges - shared) | (b.edges - {(q, p) for p, q in shared})
    following = {}
    for p, q in edges:
        if p in following:
            return None, "multiple_boundaries"
        following[p] = q
    if not following or len(set(following.values())) != len(following):
        return None, "multiple_boundaries"
    start = min(following)
    path, visited, p = [], set(), start
    while p not in visited:
        visited.add(p)
        path.append(p)
        if p not in following:
            return None, "open_boundary"
        p = following[p]
    if p != start or len(path) != len(edges):
        return None, "multiple_boundaries"  # includes holes, never fills them
    # Remove only collinear boundary subdivisions, not concave corners.
    vertices = tuple(p for i, p in enumerate(path)
                     if cross(sub(p, path[i - 1]),
                              sub(path[(i + 1) % len(path)], p)) != (0, 0, 0))
    if not 3 <= len(vertices) <= MAX_EDGES:
        return None, "edge_count"
    if not convex(vertices, key):
        return None, "nonconvex"
    if area2(vertices, key) != a.area + b.area:
        raise AssertionError("merge did not conserve exact oriented area")
    return _Piece(canonical(vertices), edges, a.area + b.area), None


def _source(mesh):
    triangles = np.asarray(mesh.triangles)
    materials = np.asarray(mesh.material)
    if triangles.ndim != 3 or triangles.shape[1:] != (3, 3) or not len(triangles):
        raise ValueError("expected nonempty T,3,3 triangles")
    if not np.all(np.isfinite(triangles)):
        raise ValueError("nonfinite source geometry")
    if materials.shape != (len(triangles),) or not np.all(np.isfinite(materials)):
        raise ValueError("one finite material ID required per triangle")
    if np.any(materials < 0) or np.any(materials > 2**24) or np.any(materials != np.floor(materials)):
        raise ValueError("material IDs must be nonnegative integers exactly representable in float32")
    # Python ints do all geometric arithmetic; avoid int64 cross-product overflow.
    result = []
    for triangle, material in zip(triangles, materials):
        vertices = tuple(tuple(int(round(float(c) * TICKS)) for c in p) for p in triangle)
        if any(c / TICKS != original for p, q in zip(vertices, triangle)
               for c, original in zip(p, q)):
            raise ValueError("source must be on the exact eighth-cell lattice")
        if not within_span(vertices):
            raise ValueError("source triangle exceeds eight-cell bound; subdivision required")
        result.append((canonical(vertices), plane(vertices, int(material))))
    return result


def build(mesh):
    """Return a source-independent exact convex partition, in deterministic order."""
    start = perf_counter()
    groups = defaultdict(list)
    source = _source(mesh)
    for vertices, key in source:
        groups[key].append(vertices)
    patches, rejected = [], Counter()
    merges = attempts = visits = 0
    for key, triangles in sorted(groups.items()):
        active, owners = {}, defaultdict(list)
        for i, vertices in enumerate(sorted(triangles)):
            edges = atomic_edges(vertices)
            visits += len(edges)
            active[i] = _Piece(vertices, edges, area2(vertices, key))
            for p, q in edges:
                owners[tuple(sorted((p, q)))].append((i, p, q))
        neighbours = {i: Counter() for i in active}
        for entries in owners.values():
            if len(entries) > 2:
                raise ValueError("nonmanifold coplanar same-material atomic edge")
            if len(entries) == 2:
                (a, p, q), (b, r, s) = entries
                if (p, q) != (s, r):
                    raise ValueError("overlapping or inconsistently wound source edges")
                neighbours[a][b] += 1
                neighbours[b][a] += 1
        heap = []

        def enqueue(a, b, shared):
            a, b = sorted((a, b))
            # Long shared boundaries first, then larger unions, then stable IDs.
            heapq.heappush(heap, (-shared, -abs(active[a].area + active[b].area), a, b))

        for a in active:
            for b, shared in neighbours[a].items():
                if a < b:
                    enqueue(a, b, shared)
        next_id = len(active)
        while heap:
            _, _, a, b = heapq.heappop(heap)
            if a not in active or b not in active:
                continue
            attempts += 1
            merged, reason = _merge(active[a], active[b], key)
            if merged is None:
                rejected[reason] += 1
                continue
            neighbours[next_id] = neighbours[a] + neighbours[b]
            neighbours[next_id].pop(a, None)
            neighbours[next_id].pop(b, None)
            active[next_id] = merged
            for other, shared in neighbours[next_id].items():
                neighbours[other].pop(a, None)
                neighbours[other].pop(b, None)
                neighbours[other][next_id] = shared
                enqueue(next_id, other, shared)
            del active[a], active[b], neighbours[a], neighbours[b]
            next_id += 1
            merges += 1
        patches.extend(Patch(piece.vertices, key) for piece in active.values())
    patches.sort(key=lambda patch: (patch.key, patch.vertices))
    if len(patches) >= 2**24:
        raise ValueError("patch IDs exceed exact float32 integer range")
    rep = Representation(tuple(patches), {
        "source_triangle_count": len(source), "patch_count": len(patches),
        "plane_material_groups": len(groups), "accepted_merges": merges,
        "merge_attempts": attempts, "rejected_merges": dict(sorted(rejected.items())),
        "atomic_edge_visits": visits,
        "polygon_edge_distribution": dict(sorted(Counter(len(p.vertices) for p in patches).items())),
        "normal_directions": [list(n) for n in sorted({p.key[:3] for p in patches})],
        "canonical_26_directions_only": all(max(map(abs, p.key[:3])) == 1 for p in patches),
    })
    rep.stats["geometric_checks"] = _exact_checks(source, rep)
    rep.stats["build_seconds"] = perf_counter() - start
    return rep


def patch_halfspaces(patch):
    """Eight vec4s: e.xyz dot world-position + e.w >= 0, including boundary.

    Primitive integer projected edge normals and eighth-integer offsets avoid
    needless normalized-plane rounding. The dropped-axis coefficient is zero.
    These inequalities constrain the patch plane, not all of 3D space.
    """
    _, (u, v), sign = projection(patch.key)
    result = np.tile([0., 0., 0., 1.], (MAX_EDGES, 1))
    for i, (a, b) in enumerate(pairs(patch.vertices)):
        dx, dy = b[u] - a[u], b[v] - a[v]
        divisor = gcd(abs(dx), abs(dy))
        dx, dy = dx // divisor, dy // divisor
        result[i, u], result[i, v] = -sign * dy, sign * dx
        result[i, 3] = sign * (dy * a[u] - dx * a[v]) / TICKS
    return result


def quad(patch):
    """Padded tangent rectangle lifted onto the exact plane.

    A 1/64-cell broad-phase guard keeps raster-snapped tight proxy boundaries
    from dropping geometrically interior rays in this probe's size range.
    It changes no halfspace/domain; it is not universal conservative raster.
    """
    drop, (u, v), sign = projection(patch.key)
    lo = [min(p[i] for p in patch.vertices) - TICKS / 64 for i in (u, v)]
    hi = [max(p[i] for p in patch.vertices) + TICKS / 64 for i in (u, v)]
    corners = [(lo[0], lo[1]), (hi[0], lo[1]), (hi[0], hi[1]), (lo[0], hi[1])]
    if sign < 0:
        corners.reverse()
    result = np.zeros((4, 3), dtype=float)
    for p, (a, b) in zip(result, corners):
        p[u], p[v] = a / TICKS, b / TICKS
        p[drop] = (patch.key[3] - patch.key[u] * a - patch.key[v] * b) / (TICKS * patch.key[drop])
    return result


def _vertices(points, patch, patch_id):
    return [(*p, patch.material, *patch.normal, patch_id) for p in points]


def buffers(rep):
    """Only domains feed the analytic proxy, clip stream and merged control."""
    analytic, ai, merged, mi, edges = [], [], [], [], []
    for patch_id, patch in enumerate(rep.patches):
        base = len(analytic)
        analytic.extend(_vertices(quad(patch), patch, patch_id))
        ai.extend(base + i for i in (0, 1, 2, 0, 2, 3))
        base = len(merged)
        merged.extend(_vertices(np.asarray(patch.vertices, dtype=float) / TICKS, patch, patch_id))
        for i in range(1, len(patch.vertices) - 1):
            mi.extend((base, base + i, base + i + 1))
        edges.extend(patch_halfspaces(patch))
    return {"analytic-vertices.f32": np.asarray(analytic, dtype="<f4").reshape(-1, 8),
            "analytic-indices.u32": np.asarray(ai, dtype="<u4"),
            "merged-vertices.f32": np.asarray(merged, dtype="<f4").reshape(-1, 8),
            "merged-indices.u32": np.asarray(mi, dtype="<u4"),
            "edges.f32": np.asarray(edges, dtype="<f4").reshape(-1, 4)}


def reference_buffers(mesh):
    vertices, indices, seen = [], [], {}
    for triangle, key in _source(mesh):
        patch = Patch(triangle, key)
        for p in triangle:
            identity = (p, key[:3], key[4])
            if identity not in seen:
                seen[identity] = len(vertices)
                vertices.extend(_vertices([tuple(c / TICKS for c in p)], patch, 0))
            indices.append(seen[identity])
    return {"reference-vertices.f32": np.asarray(vertices, dtype="<f4").reshape(-1, 8),
            "reference-indices.u32": np.asarray(indices, dtype="<u4")}


def contains(patch, points, tolerance=1e-10):
    """Closed-set plane/polygon containment; not a raster edge ownership rule."""
    points = np.asarray(points, dtype=float)
    n = np.asarray(patch.key[:3], dtype=float)
    on_plane = np.abs(points @ n - patch.key[3] / TICKS) <= tolerance * np.linalg.norm(n)
    e = patch_halfspaces(patch)
    return on_plane & np.all(points @ e[:, :3].T + e[:, 3] >= -tolerance, axis=-1)


def nearest_hits(rep, origins, direction, front_only=True):
    """CPU validation query; returns depth and patch ID, with no source access.

    Direction is one shared vector (need not be unit length); t > 0 only.
    Lowest patch ID wins equal-depth CPU ties. GPU raster ties remain untested.
    """
    origins, direction = np.asarray(origins, dtype=float), np.asarray(direction, dtype=float)
    depth = np.full(origins.shape[:-1], np.inf)
    ids = np.full(depth.shape, -1, dtype=np.int64)
    for i, patch in enumerate(rep.patches):
        n = np.asarray(patch.key[:3], dtype=float)
        facing = float(n @ direction)
        if abs(facing) < 1e-12 or (front_only and facing >= 0):
            continue
        t = (patch.key[3] / TICKS - origins @ n) / facing
        points = origins + t[..., None] * direction
        take = (t > 0) & (t < depth - 1e-10) & contains(patch, points)
        depth[take], ids[take] = t[take], i
    return depth, ids


def _exact_checks(source, rep):
    source_area, patch_area = Counter(), Counter()
    source_boundary, patch_boundary = Counter(), Counter()

    def accumulate(vertices, key, areas, boundary):
        areas[key] += area2(vertices, key)
        for a, b in atomic_edges(vertices):
            if a < b:
                boundary[key, a, b] += 1
            else:
                boundary[key, b, a] -= 1

    for vertices, key in source:
        accumulate(vertices, key, source_area, source_boundary)
    for patch in rep.patches:
        vertices, key = patch.vertices, patch.key
        if not (3 <= len(vertices) <= MAX_EDGES and convex(vertices, key) and within_span(vertices)):
            raise AssertionError("invalid convex patch")
        if any(dot(key[:3], p) != key[3] for p in vertices):
            raise AssertionError("patch vertex left its exact plane")
        _, (u, v), sign = projection(key)
        for a, b in pairs(vertices):
            if any(sign * ((b[u] - a[u]) * (p[v] - a[v])
                           - (b[v] - a[v]) * (p[u] - a[u])) < 0 for p in vertices):
                raise AssertionError("vertex outside exact edge halfspace")
        accumulate(vertices, key, patch_area, patch_boundary)
    if dict(source_area) != dict(patch_area):
        raise AssertionError("plane/material exact oriented area changed")
    if {k: v for k, v in source_boundary.items() if v} != {k: v for k, v in patch_boundary.items() if v}:
        raise AssertionError("plane/material oriented boundary changed")
    return {"exact_oriented_projected_area_per_plane_material": True,
            "exact_atomic_boundary_per_plane_material": True,
            "convexity_plane_membership_halfspaces_and_span": True,
            "area_units": "signed doubled projected tick-squared area; divide by 128 for cell-squared",
            "areas": [{"plane_material": list(key), "source": area, "patches": patch_area[key]}
                      for key, area in sorted(source_area.items())]}


def _barycentric(points, triangle):
    """Independent 3D Gram-matrix barycentric oracle; no domain edge tests."""
    a, b, c = triangle
    v0, v1, v2 = b - a, c - a, points - a
    d00, d01, d11 = v0 @ v0, v0 @ v1, v1 @ v1
    denominator = d00 * d11 - d01 * d01
    u = (d11 * (v2 @ v0) - d01 * (v2 @ v1)) / denominator
    v = (d00 * (v2 @ v1) - d01 * (v2 @ v0)) / denominator
    n = np.cross(v0, v1)
    return ((u >= -1e-10) & (v >= -1e-10) & (u + v <= 1 + 1e-10)
            & (abs(v2 @ n) <= 1e-10 * np.linalg.norm(n)))


def validate(mesh, rep):
    """Independent source barycentrics vs domains, plus actual float32 streams."""
    start = perf_counter()
    grouped_source, grouped_patches = defaultdict(list), defaultdict(list)
    for i, (_, key) in enumerate(_source(mesh)):
        grouped_source[key].append(i)
    for patch in rep.patches:
        grouped_patches[patch.key].append(patch)
    sample_count = outside_count = boundary_count = 0
    rng = np.random.default_rng(601266)
    for key, indices in grouped_source.items():
        triangles = np.asarray(mesh.triangles)[indices]
        vertices = triangles.reshape(-1, 3)
        mids = (triangles + np.roll(triangles, -1, axis=1)).reshape(-1, 3) / 2
        asymmetric = np.einsum("i,tij->tj", [0.173, 0.291, 0.536], triangles)
        drop, (u, v), _ = projection(key)
        lo, hi = vertices.min(axis=0), vertices.max(axis=0)
        x, y = np.meshgrid(np.linspace(lo[u] - .137, hi[u] + .193, 17),
                           np.linspace(lo[v] - .211, hi[v] + .157, 13))
        grid = np.zeros((x.size + 97, 3))
        grid[:x.size, u], grid[:x.size, v] = x.ravel(), y.ravel()
        grid[x.size:, u] = rng.uniform(lo[u] - .25, hi[u] + .25, 97)
        grid[x.size:, v] = rng.uniform(lo[v] - .25, hi[v] + .25, 97)
        grid[:, drop] = (key[3] / TICKS - key[u] * grid[:, u] - key[v] * grid[:, v]) / key[drop]
        points = np.concatenate((vertices, mids, asymmetric, grid))
        expected = np.zeros(len(points), dtype=bool)
        actual = expected.copy()
        for triangle in triangles:
            expected |= _barycentric(points, triangle)
        for patch in grouped_patches[key]:
            actual |= contains(patch, points)
        if not np.array_equal(actual, expected):
            raise AssertionError(f"plane/material containment mismatch: {key}")
        sample_count += len(points)
        outside_count += int(np.sum(~expected))
        boundary_count += len(vertices) + len(mids)

    # Independent ray/triangle plane intersection and barycentrics, including
    # multiple material layers and backs of fixtures. Equal-depth attributes
    # are set-valued: a valid candidate must match ANY source at nearest t.
    triangles = np.asarray(mesh.triangles)
    source_normals = np.cross(triangles[:, 1] - triangles[:, 0], triangles[:, 2] - triangles[:, 0])
    source_normals /= np.linalg.norm(source_normals, axis=1)[:, None]
    lo, hi = triangles.min(axis=(0, 1)), triangles.max(axis=(0, 1))
    centre = (lo + hi) / 2
    radius = np.linalg.norm(hi - lo) + 2
    targets = rng.uniform(lo - .27, hi + .31, (233, 3))
    selected = triangles[np.linspace(0, len(triangles) - 1, min(113, len(triangles)), dtype=int)]
    targets = np.concatenate((targets, selected[:, 0],
                              np.einsum("i,tij->tj", [.197, .319, .484], selected)))
    ray_count = hit_count = tie_count = 0
    max_error = 0.
    for vector in [(1, .371, .219), (-.293, 1, -.417), (.193, -.311, 1),
                   (-1, -.371, -.219), (.293, -1, .417), (-.193, .311, -1)]:
        direction = np.asarray(vector) / np.linalg.norm(vector)
        origins = targets - (radius + (targets - centre) @ direction)[:, None] * direction
        expected = np.full(len(origins), np.inf)
        hits = []
        for triangle, normal in zip(triangles, source_normals):
            facing = normal @ direction
            t = np.full(len(origins), np.inf)
            if facing < -1e-12:
                distance = (triangle[0] - origins) @ normal / facing
                inside = _barycentric(origins + distance[:, None] * direction, triangle) & (distance > 0)
                t[inside] = distance[inside]
            expected = np.minimum(expected, t)
            hits.append(t)
        actual, ids = nearest_hits(rep, origins, direction)
        valid = np.isfinite(expected)
        if not np.array_equal(np.isfinite(actual), valid):
            raise AssertionError("nearest-hit coverage mismatch")
        error = float(np.max(abs(actual[valid] - expected[valid]), initial=0))
        if error > 1e-8:
            raise AssertionError("nearest-hit depth mismatch")
        max_error = max(max_error, error)
        hits = np.asarray(hits)
        for j in np.flatnonzero(valid):
            tied = np.flatnonzero(abs(hits[:, j] - expected[j]) <= 1e-8)
            patch = rep.patches[ids[j]]
            if not any(int(mesh.material[k]) == patch.material
                       and np.allclose(source_normals[k], patch.normal, atol=1e-10, rtol=0) for k in tied):
                raise AssertionError("nearest-hit material/normal mismatch")
            tie_count += int(len(tied) > 1)
        ray_count += len(origins)
        hit_count += int(np.sum(valid))

    packed = buffers(rep)
    for patch_id, patch in enumerate(rep.patches):
        q = packed["analytic-vertices.f32"][4 * patch_id:4 * patch_id + 4, :3].astype(float)
        if np.max(abs(q @ np.asarray(patch.key[:3]) - patch.key[3] / TICKS)) > 1e-6:
            raise AssertionError("float32 bounding quad left plane")
        for i, j, k in [(0, 1, 2), (0, 2, 3)]:
            if np.cross(q[j] - q[i], q[k] - q[i]) @ patch.normal <= 0:
                raise AssertionError("bounding quad winding reversed")
        e = packed["edges.f32"][8 * patch_id:8 * patch_id + 8].astype(float)
        if not np.array_equal(e, patch_halfspaces(patch)):
            raise AssertionError("edge coefficients lost exactness in float32")
        p = np.asarray(patch.vertices, dtype=float) / TICKS
        if not np.all(p @ e[:, :3].T + e[:, 3] >= 0):
            raise AssertionError("serialized halfspace excludes patch vertex")
    # Check independence without changing the caller's mesh, using an owned
    # copy as builder input and then deliberately destroying that copy.
    class OwnedMesh:
        pass
    owned = OwnedMesh()
    owned.triangles, owned.material = triangles.copy(), np.asarray(mesh.material).copy()
    independent = build(owned)
    before = {name: array.tobytes() for name, array in buffers(independent).items()}
    owned.triangles[:] = np.nan
    owned.material[:] = -1
    if before != {name: array.tobytes() for name, array in buffers(independent).items()}:
        raise AssertionError("representation retained source aliases")
    if before != {name: array.tobytes() for name, array in packed.items()}:
        raise AssertionError("nondeterministic build")
    return {"containment_samples": sample_count, "boundary_samples": boundary_count,
            "outside_or_hole_samples": outside_count, "containment_mismatches": 0,
            "nearest_hit_rays": ray_count, "nearest_hit_count": hit_count,
            "nearest_hit_tied_source_samples": tie_count, "nearest_hit_mismatches": 0,
            "maximum_depth_error": max_error, "float32_stream_checks": True,
            "source_mutation_independence": True, "deterministic_rebuild": True,
            "validation_seconds": perf_counter() - start,
            "edge_policy": "CPU closed halfspaces; tied nearest source attributes accepted as a set. "
                           "GPU top-left rules, clipping edge ties, and MSAA coverage NOT resolved."}


def sha256(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--meshes", type=Path, default=ROOT / ".amp/in/artifacts/far-field-comparison/meshes.json")
    parser.add_argument("--output", type=Path, default=ROOT / ".amp/in/artifacts/analytic-gpu/input")
    args = parser.parse_args()
    # Reuse only the established loader, not the old CPU analytic representation.
    sys.path.insert(0, str(COMMON.parent))
    from common import load_meshes

    start = perf_counter()
    meshes = load_meshes(args.meshes)
    args.output.mkdir(parents=True, exist_ok=True)
    manifest = {
        "format_version": 1, "scope": "CPU input preparation and validation; no GPU results",
        "contract": {"byte_order": "little", "vertex_stride_bytes": 32,
                     "vertex_fields": ["x", "y", "z", "material", "nx", "ny", "nz", "patch_id"],
                     "index_type": "uint32", "edges_per_patch": 8, "edge_stride_bytes": 16,
                     "edge_test": "dot(e.xyz, world_position) + e.w >= 0",
                     "edge_padding": [0, 0, 0, 1], "patch_ids": "zero-based per fixture",
                     "analytic_proxy": "4 coplanar dominant-axis bounds corners and 6 indices per patch",
                     "merged_control": "fan triangulation of the identical convex domains; no clipping",
                     "max_polygon_edges": MAX_EDGES, "max_patch_axis_span_cells": MAX_SPAN / TICKS},
        "input": {"path": str(args.meshes), "sha256": sha256(args.meshes)},
        "sources": {str(path.relative_to(ROOT)): sha256(path) for path in [Path(__file__).resolve(), COMMON]},
        "environment": {"python": sys.version.split()[0], "numpy": np.__version__},
        "fixtures": {},
        "limitations": ["Greedy partition, not a minimum-polygon solution.",
                        "Assumes non-overlapping oriented atlas triangles; not an arbitrary soup repairer.",
                        "Eight-cell bound is on convex domains; coplanar bounding quads can extend beyond domains.",
                        "CPU tests do not establish GPU raster edge ownership, MSAA, performance, or image equivalence."],
    }
    for name, mesh in meshes.items():
        if not name or any(c not in "abcdefghijklmnopqrstuvwxyz0123456789-_" for c in name):
            raise ValueError("unsafe fixture filename")
        scene_start = perf_counter()
        rep = build(mesh)
        checks = validate(mesh, rep)
        serialization_start = perf_counter()
        streams = reference_buffers(mesh) | buffers(rep)
        files = {}
        for suffix, array in streams.items():
            filename = f"{name}-{suffix}"
            path = args.output / filename
            path.write_bytes(array.tobytes(order="C"))
            files[suffix] = {"file": filename, "bytes": array.nbytes, "sha256": sha256(path)}
        counts = {}
        for kind in ["reference", "analytic", "merged"]:
            vertices, indices = streams[f"{kind}-vertices.f32"], streams[f"{kind}-indices.u32"]
            counts[kind] = {"vertices": len(vertices), "indices": len(indices), "triangles": len(indices) // 3,
                            "geometry_bytes": vertices.nbytes + indices.nbytes}
        counts["analytic"]["edge_bytes"] = streams["edges.f32"].nbytes
        counts["analytic"]["total_bytes"] = counts["analytic"]["geometry_bytes"] + streams["edges.f32"].nbytes
        patch_spans = np.asarray([np.ptp(np.asarray(p.vertices), axis=0) / TICKS for p in rep.patches])
        quad_spans = np.asarray([np.ptp(quad(p), axis=0) for p in rep.patches])
        manifest["fixtures"][name] = {
            **rep.stats, "validation": checks, "counts": counts, "files": files,
            "bounds": {"min": mesh.bounds[0].tolist(), "max": mesh.bounds[1].tolist(),
                       "maximum_patch_axis_spans": patch_spans.max(axis=0).tolist(),
                       "maximum_quad_axis_spans": quad_spans.max(axis=0).tolist()},
            "serialization_seconds": perf_counter() - serialization_start,
            "total_prepare_seconds": perf_counter() - scene_start,
        }
        print(f"{name}: {len(mesh.triangles)} source triangles -> {len(rep.patches)} patches, "
              f"{counts['merged']['triangles']} merged / {counts['analytic']['triangles']} proxy triangles", flush=True)
    manifest["total_prepare_seconds"] = perf_counter() - start
    (args.output / "manifest.json").write_text(json.dumps(manifest, indent=2, sort_keys=True) + "\n")
    print(args.output / "manifest.json")


if __name__ == "__main__":
    main()
