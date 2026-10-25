"""CPU bounded-plane evaluator; analytic geometry, not analytic antialiasing.

Build cancels integer-lattice interior edges of coplanar, equal-material
triangles. Connected domains retain oriented boundary edges: winding preserves
concavities and holes without constructing a convex hull. Rendering uses only
these domains, never source triangles. The caller supplies spatial supersampling.
Timings describe this NumPy CPU experiment, not GPU performance.
"""
from collections import Counter, defaultdict
from dataclasses import dataclass
from functools import reduce
from math import gcd
from time import perf_counter

import numpy as np

from common import frame_from_hits


@dataclass
class Domain:
    edges: np.ndarray  # E,2,3, world-space oriented boundary segments
    owners: np.ndarray  # original order at boundary segments, for depth ties
    normal: np.ndarray
    offset: float
    material: int
    priority: int
    axes: tuple


@dataclass
class Representation:
    domains: list
    stats: dict


def build(mesh):
    """Extract exact unions on the exported 1/8-cell grid, without mutating MESH."""
    start = perf_counter()
    triangles = np.asarray(mesh.triangles)
    if triangles.ndim != 3 or triangles.shape[1:] != (3, 3):
        raise ValueError("expected T,3,3 triangles")
    if not np.all(np.isfinite(triangles)):
        raise ValueError("nonfinite geometry")
    ticks = np.rint(triangles * 8).astype(np.int64)
    if not np.array_equal(ticks / 8, triangles):
        raise ValueError("analytic build requires the exact 1/8-cell lattice")
    if np.asarray(mesh.material).shape != (len(ticks),):
        raise ValueError("one material ID is required per triangle")
    parent = list(range(len(ticks)))

    def root(i):
        while parent[i] != i:
            parent[i] = parent[parent[i]]
            i = parent[i]
        return i

    groups = defaultdict(list)
    atomic_visits = 0
    for i, triangle in enumerate(ticks):
        n = np.cross(triangle[1] - triangle[0], triangle[2] - triangle[0])
        scale = reduce(gcd, map(int, abs(n)))
        if not scale:
            raise ValueError("degenerate source triangle")
        n //= scale
        key = (*map(int, n), int(n @ triangle[0]), int(mesh.material[i]))
        for a, b in zip(triangle, np.roll(triangle, -1, axis=0)):
            delta = b - a
            steps = reduce(gcd, map(int, abs(delta)))
            for j in range(steps):
                p = tuple(map(int, a + delta * j // steps))
                q = tuple(map(int, a + delta * (j + 1) // steps))
                groups[key].append((p, q, i))
                atomic_visits += 1

    domains = []
    for key, records in groups.items():
        # Atomic edges match long/short neighbours, including T-junctions.
        seen = {}
        for a, b, i in records:
            edge = tuple(sorted((a, b)))
            if edge in seen:
                p, q = root(i), root(seen[edge])
                parent[max(p, q)] = min(p, q)
            else:
                seen[edge] = i
        components = defaultdict(list)
        for a, b, i in records:
            components[root(i)].append((a, b, i))
        normal = np.asarray(key[:3], dtype=float)
        length = np.linalg.norm(normal)
        for component in components.values():
            counts = Counter((a, b) for a, b, _ in component)
            owners = {}
            for a, b, i in component:
                owners[a, b] = min(i, owners.get((a, b), i))
            edges, priorities = [], []
            for (a, b), count in counts.items():
                # Retain multiplicities: winding of overlapping positive
                # regions is still nonzero, while hole boundaries subtract.
                for _ in range(max(0, count - counts[b, a])):
                    edges.append((a, b))
                    priorities.append(owners[a, b])
            if not edges:
                raise ValueError("coplanar component has no oriented boundary")
            # Undo tick subdivision along straight boundaries. Retain owner
            # changes so shared-edge depth ties keep the source's draw order.
            lines = defaultdict(list)
            for (a, b), owner in zip(edges, priorities):
                direction = tuple(y - x for x, y in zip(a, b))
                moment = tuple(np.cross(a, direction))
                lines[direction, moment, owner].append((a, b))
            edges, priorities = [], []
            for (direction, _, owner), segments in lines.items():
                merged = []
                for a, b in sorted(segments, key=lambda e: np.dot(e[0], direction)):
                    if merged and merged[-1][1] == a:
                        merged[-1] = (merged[-1][0], b)
                    else:
                        merged.append((a, b))
                edges.extend(merged)
                priorities.extend([owner] * len(merged))
            domains.append(Domain(
                np.asarray(edges, dtype=float) / 8,
                np.asarray(priorities, dtype=np.int32), normal.copy() / length,
                key[3] / (8 * length), key[4], min(i for _, _, i in component),
                tuple(axis for axis in range(3) if axis != np.argmax(abs(normal)))))
    domains.sort(key=lambda domain: domain.priority)
    size = sum(d.edges.nbytes + d.owners.nbytes + d.normal.nbytes for d in domains)
    return Representation(domains, {
        "source_triangle_count": len(triangles),
        "primitive_count": len(domains), "domain_count": len(domains),
        "boundary_edge_count": sum(len(d.edges) for d in domains),
        "numeric_array_bytes": int(size),
        "numeric_array_bytes_scope": "retained NumPy arrays; excludes Python containers",
        "build_atomic_edge_visits": atomic_visits,
        "build_seconds": perf_counter() - start,
        "antialiasing": "point samples; caller supersamples and resolves",
        "timing_scope": "CPU only, not GPU evidence",
    })


def render(rep, camera, width, height, light):
    """Nearest front-facing plane/domain hit at each orthographic pixel centre."""
    start = perf_counter()
    origins, direction = camera.rays(width, height)
    depth = np.full((height, width), np.inf)
    normals = np.zeros((height, width, 3))
    materials = np.full((height, width), -1, dtype=np.int32)
    priorities = np.full((height, width), np.iinfo(np.int32).max, dtype=np.int32)
    prepared, projected_vertices = [], 0
    for domain in rep.domains:
        facing = float(domain.normal @ direction)
        if facing >= -1e-12:  # Same front-face convention as the shared contract.
            continue
        projected = camera.project(domain.edges.reshape(-1, 3), width, height)
        projected_vertices += len(projected)
        # Expanded by one pixel; membership, not the proxy, determines coverage.
        lo = np.maximum(np.floor(projected[:, :2].min(axis=0)).astype(int) - 1, 0)
        hi = np.minimum(np.ceil(projected[:, :2].max(axis=0)).astype(int) + 1,
                        [width, height])
        if np.all(hi > lo):
            prepared.append((domain, facing, lo, hi))
    preparation_seconds = perf_counter() - start
    pixel_tests = edge_tests = 0
    intersection_start = perf_counter()
    for domain, facing, (x0, y0), (x1, y1) in prepared:
        local_origins = origins[y0:y1, x0:x1]
        z = (domain.offset - local_origins @ domain.normal) / facing
        points = local_origins + z[..., None] * direction
        q = points[..., list(domain.axes)]
        winding = np.zeros(z.shape, dtype=np.int32)
        boundary = np.zeros(z.shape, dtype=bool)
        rank = np.full(z.shape, np.iinfo(np.int32).max, dtype=np.int32)
        pixel_tests += z.size
        edge_tests += z.size * len(domain.edges)
        for edge, owner in zip(domain.edges, domain.owners):
            a, b = edge[:, list(domain.axes)]
            delta = b - a
            relative = q - a
            cross = delta[0] * relative[..., 1] - delta[1] * relative[..., 0]
            up = (a[1] <= q[..., 1]) & (q[..., 1] < b[1])
            down = (b[1] <= q[..., 1]) & (q[..., 1] < a[1])
            winding += (up & (cross > 0)).astype(np.int32)
            winding -= (down & (cross < 0)).astype(np.int32)
            along = relative @ delta
            epsilon = 1e-11 * max(1.0, np.linalg.norm(delta))
            on = ((abs(cross) <= epsilon) & (along >= -epsilon)
                  & (along <= delta @ delta + epsilon))
            boundary |= on
            rank[on] = np.minimum(rank[on], owner)
        rank[~boundary] = domain.priority
        region = depth[y0:y1, x0:x1]
        prior = priorities[y0:y1, x0:x1]
        valid = ((winding != 0) | boundary) & (z > 0)
        nearer = z < region - 1e-10
        tied = (abs(z - region) <= 1e-10) & (rank < prior)
        take = valid & (nearer | tied)
        region[take] = z[take]
        prior[take] = rank[take]
        normals[y0:y1, x0:x1][take] = domain.normal
        materials[y0:y1, x0:x1][take] = domain.material
    intersection_seconds = perf_counter() - intersection_start
    frame = frame_from_hits(camera, depth, normals, materials, light)
    stats = dict(rep.stats, pixel_domain_tests=int(pixel_tests),
                 pixel_edge_tests=int(edge_tests), tested_domain_count=len(prepared),
                 projected_bound_vertex_count=projected_vertices,
                 preparation_seconds=preparation_seconds,
                 intersection_seconds=intersection_seconds,
                 render_seconds=perf_counter() - start)
    return frame, stats
