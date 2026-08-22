#!/usr/bin/env python3
"""Generate an exact local oracle for Blender's one-segment Arc bevel.

Run this inside Blender through ``scripts/luft-blender-star-oracle``.  Each
case is the boundary of one of the 256 occupancy stars around a lattice
vertex.  The modifier is evaluated on a welded voxel boundary, but only the
faces wholly inside the central bevel cube are retained as the junction
answer.  Coordinates are quantized only after checking that Blender put them
on the integer offset lattice.  This is the independent corpus for #WJRRK7.
"""

from __future__ import annotations

import argparse
import hashlib
import itertools
import json
import math
import sys
from collections import Counter, defaultdict, deque
from pathlib import Path
from typing import Iterable, Sequence

import bpy
from mathutils import Vector


FORMAT_VERSION = 1
DEFAULT_CELL_SIZE = 8
DEFAULT_WIDTH = 1
DEFAULT_TOLERANCE = 1.0e-5

# These are the consequential settings dumped from the unsaved Blender 5.2
# scene behind #WK4YU4.  Properties concerned only with UI or object identity
# are deliberately absent.
BEVEL_SETTINGS = {
    "affect": "EDGES",
    "angle_limit": math.radians(30.0),
    "face_strength_mode": "FSTR_NONE",
    "harden_normals": False,
    "limit_method": "ANGLE",
    "loop_slide": True,
    "mark_seam": False,
    "mark_sharp": False,
    "material": -1,
    "miter_inner": "MITER_SHARP",
    "miter_outer": "MITER_ARC",
    "offset_type": "OFFSET",
    "profile": 0.5,
    "profile_type": "SUPERELLIPSE",
    "segments": 1,
    "use_clamp_overlap": True,
    "vmesh_method": "ADJ",
}


def direction(sample: int) -> tuple[int, int, int]:
    """The LUFT vertex-star convention: X is the low bit, then Y, then Z."""
    return tuple(1 if sample & (1 << axis) else -1 for axis in range(3))


DIRECTIONS = tuple(direction(sample) for sample in range(8))
DIRECTION_INDEX = {value: index for index, value in enumerate(DIRECTIONS)}


def cell_anchor(sample: int, cell_size: int) -> tuple[int, int, int]:
    return tuple(0 if component > 0 else -cell_size
                 for component in direction(sample))


def face_corners(
    anchor: tuple[int, int, int],
    cell_size: int,
    axis: int,
    side: int,
) -> tuple[tuple[int, int, int], ...]:
    x0, y0, z0 = anchor
    x1, y1, z1 = (x0 + cell_size, y0 + cell_size, z0 + cell_size)
    if axis == 0 and side < 0:
        return ((x0, y0, z0), (x0, y0, z1),
                (x0, y1, z1), (x0, y1, z0))
    if axis == 0:
        return ((x1, y0, z0), (x1, y1, z0),
                (x1, y1, z1), (x1, y0, z1))
    if axis == 1 and side < 0:
        return ((x0, y0, z0), (x1, y0, z0),
                (x1, y0, z1), (x0, y0, z1))
    if axis == 1:
        return ((x0, y1, z0), (x0, y1, z1),
                (x1, y1, z1), (x1, y1, z0))
    if side < 0:
        return ((x0, y0, z0), (x0, y1, z0),
                (x1, y1, z0), (x1, y0, z0))
    return ((x0, y0, z1), (x1, y0, z1),
            (x1, y1, z1), (x0, y1, z1))


def boundary_mesh(
    mask: int,
    cell_size: int,
) -> tuple[list[tuple[int, int, int]], list[tuple[int, ...]]]:
    occupied = {
        cell_anchor(sample, cell_size)
        for sample in range(8)
        if mask & (1 << sample)
    }
    vertex_indices: dict[tuple[int, int, int], int] = {}
    vertices: list[tuple[int, int, int]] = []
    faces: list[tuple[int, ...]] = []

    def vertex_index(point: tuple[int, int, int]) -> int:
        if point not in vertex_indices:
            vertex_indices[point] = len(vertices)
            vertices.append(point)
        return vertex_indices[point]

    for anchor in sorted(occupied):
        for axis in range(3):
            for side in (-1, 1):
                neighbor = list(anchor)
                neighbor[axis] += side * cell_size
                if tuple(neighbor) in occupied:
                    continue
                faces.append(tuple(vertex_index(point) for point in
                                   face_corners(anchor, cell_size, axis, side)))
    return vertices, faces


def edge_key(a: int, b: int) -> tuple[int, int]:
    return (a, b) if a < b else (b, a)


def edge_incidences(faces: Sequence[Sequence[int]]) -> Counter[tuple[int, int]]:
    incidences: Counter[tuple[int, int]] = Counter()
    for face in faces:
        for index, a in enumerate(face):
            incidences[edge_key(a, face[(index + 1) % len(face)])] += 1
    return incidences


def central_link_components(
    vertices: Sequence[tuple[int, int, int]],
    faces: Sequence[Sequence[int]],
) -> int:
    try:
        central = vertices.index((0, 0, 0))
    except ValueError:
        return 0
    incident = [index for index, face in enumerate(faces) if central in face]
    if not incident:
        return 0
    neighbors: dict[int, set[int]] = {index: set() for index in incident}
    central_edges: dict[tuple[int, int], list[int]] = defaultdict(list)
    for face_index in incident:
        face = faces[face_index]
        for index, a in enumerate(face):
            b = face[(index + 1) % len(face)]
            if central in (a, b):
                central_edges[edge_key(a, b)].append(face_index)
    for owners in central_edges.values():
        for left in owners:
            neighbors[left].update(owner for owner in owners if owner != left)
    return component_count(neighbors)


def component_count(neighbors: dict[int, set[int]]) -> int:
    unseen = set(neighbors)
    count = 0
    while unseen:
        count += 1
        queue = deque((unseen.pop(),))
        while queue:
            here = queue.popleft()
            for there in neighbors[here] & unseen:
                unseen.remove(there)
                queue.append(there)
    return count


def configure_bevel(modifier: bpy.types.BevelModifier, width: int) -> None:
    for name, value in BEVEL_SETTINGS.items():
        setattr(modifier, name, value)
    modifier.width = float(width)


def quantize_point(
    point: Sequence[float],
) -> tuple[tuple[int, int, int], float]:
    quantized = tuple(round(float(component)) for component in point)
    error = max(abs(float(component) - integer)
                for component, integer in zip(point, quantized))
    return quantized, error


def subtract(a: Sequence[int], b: Sequence[int]) -> tuple[int, int, int]:
    return tuple(left - right for left, right in zip(a, b))


def cross(a: Sequence[int], b: Sequence[int]) -> tuple[int, int, int]:
    return (a[1] * b[2] - a[2] * b[1],
            a[2] * b[0] - a[0] * b[2],
            a[0] * b[1] - a[1] * b[0])


def dot(a: Sequence[int], b: Sequence[int]) -> int:
    return sum(left * right for left, right in zip(a, b))


def primitive_normal(points: Sequence[Sequence[int]]) -> tuple[int, int, int]:
    origin = points[0]
    normal = (0, 0, 0)
    for left in range(1, len(points) - 1):
        normal = cross(subtract(points[left], origin),
                       subtract(points[left + 1], origin))
        if normal != (0, 0, 0):
            break
    divisor = math.gcd(*(abs(component) for component in normal))
    if divisor == 0:
        return normal
    return tuple(component // divisor for component in normal)


def normal_family(normal: Sequence[int]) -> str:
    signature = tuple(sorted(abs(component) for component in normal))
    return {
        (0, 0, 1): "axis",
        (0, 1, 1): "edge-diagonal",
        (1, 1, 1): "body-diagonal",
    }.get(signature, "other")


def canonical_cycle(
    points: Iterable[Sequence[int]],
) -> tuple[tuple[int, int, int], ...]:
    cycle = tuple(tuple(point) for point in points)
    return min(cycle[index:] + cycle[:index] for index in range(len(cycle)))


def unoriented_cycle(
    points: Iterable[Sequence[int]],
) -> tuple[tuple[int, int, int], ...]:
    cycle = tuple(tuple(point) for point in points)
    return min(canonical_cycle(cycle), canonical_cycle(reversed(cycle)))


def central_face_components(faces: Sequence[dict[str, object]]) -> list[list[int]]:
    neighbors: dict[int, set[int]] = {index: set() for index in range(len(faces))}
    owners: dict[tuple[tuple[int, int, int], tuple[int, int, int]], list[int]] = (
        defaultdict(list)
    )
    for face_index, face in enumerate(faces):
        points = face["points"]
        assert isinstance(points, list)
        for index, a in enumerate(points):
            b = points[(index + 1) % len(points)]
            edge = (tuple(a), tuple(b))
            owners[min(edge, edge[::-1])].append(face_index)
    for edge_owners in owners.values():
        for left in edge_owners:
            neighbors[left].update(right for right in edge_owners if right != left)

    components: list[list[int]] = []
    unseen = set(neighbors)
    while unseen:
        component: list[int] = []
        queue = deque((unseen.pop(),))
        while queue:
            here = queue.popleft()
            component.append(here)
            for there in neighbors[here] & unseen:
                unseen.remove(there)
                queue.append(there)
        components.append(sorted(component))
    return sorted(components)


def evaluate_case(mask: int, cell_size: int, width: int, tolerance: float) -> dict[str, object]:
    source_vertices, source_faces = boundary_mesh(mask, cell_size)
    source_edge_counts = edge_incidences(source_faces)
    nonmanifold_edges = sum(count != 2 for count in source_edge_counts.values())
    link_components = central_link_components(source_vertices, source_faces)
    regular_star = nonmanifold_edges == 0 and link_components <= 1
    input_summary = {
        "vertices": len(source_vertices),
        "faces": len(source_faces),
        "closed-edge-manifold": nonmanifold_edges == 0,
        "nonmanifold-edges": nonmanifold_edges,
        "central-link-components": link_components,
        "regular-star": regular_star,
    }
    if not source_faces:
        return {
            "mask": mask,
            "occupied-samples": [],
            "input": input_summary,
            "evaluated": {
                "vertices": 0,
                "edges": 0,
                "faces": 0,
                "max-integer-error": 0.0,
                "normal-families": {},
            },
            "junction": {
                "vertices": [],
                "faces": [],
                "components": [],
                "boundary-edges": 0,
                "internal-edges": 0,
                "nonmanifold-edges": 0,
            },
            "issues": [],
        }

    mesh = bpy.data.meshes.new(f"luft-star-{mask:02x}-source")
    mesh.from_pydata(source_vertices, [], source_faces)
    mesh.update()
    obj = bpy.data.objects.new(f"luft-star-{mask:02x}", mesh)
    bpy.context.scene.collection.objects.link(obj)
    modifier = obj.modifiers.new(name="LUFT Arc oracle", type="BEVEL")
    configure_bevel(modifier, width)
    bpy.context.view_layer.objects.active = obj
    obj.select_set(True)
    bpy.context.view_layer.update()

    evaluated_obj = obj.evaluated_get(bpy.context.evaluated_depsgraph_get())
    evaluated_mesh = evaluated_obj.to_mesh()
    try:
        quantized_vertices: list[tuple[int, int, int]] = []
        max_error = 0.0
        for vertex in evaluated_mesh.vertices:
            point, error = quantize_point(vertex.co)
            quantized_vertices.append(point)
            max_error = max(max_error, error)

        issues: list[str] = []
        if max_error > tolerance:
            issues.append(f"noninteger vertex error {max_error:.9g}")

        family_counts: Counter[str] = Counter()
        central_faces: list[dict[str, object]] = []
        for polygon in evaluated_mesh.polygons:
            raw_points = [evaluated_mesh.vertices[index].co
                          for index in polygon.vertices]
            points = [quantized_vertices[index] for index in polygon.vertices]
            normal = primitive_normal(points)
            family = normal_family(normal)
            family_counts[family] += 1
            if normal == (0, 0, 0):
                issues.append("degenerate evaluated polygon")
                continue
            plane = dot(normal, points[0])
            if any(dot(normal, point) != plane for point in points[1:]):
                issues.append("nonplanar quantized polygon")
            if family == "other":
                issues.append(f"normal outside 26 directions: {normal}")
            if all(max(abs(float(component)) for component in point)
                   <= width + tolerance for point in raw_points):
                cycle = canonical_cycle(points)
                central_faces.append({
                    "points": [list(point) for point in cycle],
                    "normal": list(normal),
                    "plane": plane,
                    "family": family,
                })

        central_faces.sort(key=lambda face: (
            face["family"], face["normal"], face["plane"], face["points"]
        ))
        junction_edges: Counter[
            tuple[tuple[int, int, int], tuple[int, int, int]]
        ] = Counter()
        junction_vertices: set[tuple[int, int, int]] = set()
        for face in central_faces:
            points = [tuple(point) for point in face["points"]]
            junction_vertices.update(points)
            for index, a in enumerate(points):
                b = points[(index + 1) % len(points)]
                junction_edges[min((a, b), (b, a))] += 1

        return {
            "mask": mask,
            "occupied-samples": [sample for sample in range(8)
                                 if mask & (1 << sample)],
            "input": input_summary,
            "evaluated": {
                "vertices": len(evaluated_mesh.vertices),
                "edges": len(evaluated_mesh.edges),
                "faces": len(evaluated_mesh.polygons),
                "max-integer-error": max_error,
                "normal-families": dict(sorted(family_counts.items())),
            },
            "junction": {
                "vertices": [list(point) for point in sorted(junction_vertices)],
                "faces": central_faces,
                "components": central_face_components(central_faces),
                "boundary-edges": sum(count == 1 for count in junction_edges.values()),
                "internal-edges": sum(count == 2 for count in junction_edges.values()),
                "nonmanifold-edges": sum(count > 2 for count in junction_edges.values()),
            },
            "issues": sorted(set(issues)),
        }
    finally:
        evaluated_obj.to_mesh_clear()
        bpy.data.objects.remove(obj, do_unlink=True)
        bpy.data.meshes.remove(mesh)


def permutation_parity(permutation: Sequence[int]) -> int:
    inversions = sum(permutation[left] > permutation[right]
                     for left in range(3) for right in range(left + 1, 3))
    return -1 if inversions % 2 else 1


def proper_rotations() -> list[tuple[tuple[int, int, int], ...]]:
    matrices: list[tuple[tuple[int, int, int], ...]] = []
    for permutation in itertools.permutations(range(3)):
        for signs in itertools.product((-1, 1), repeat=3):
            if permutation_parity(permutation) * math.prod(signs) != 1:
                continue
            rows = []
            for new_axis in range(3):
                row = [0, 0, 0]
                row[permutation[new_axis]] = signs[new_axis]
                rows.append(tuple(row))
            matrices.append(tuple(rows))
    return sorted(matrices)


ROTATIONS = proper_rotations()


def transform_point(
    matrix: Sequence[Sequence[int]],
    point: Sequence[int],
) -> tuple[int, int, int]:
    return tuple(dot(row, point) for row in matrix)


def transform_mask(matrix: Sequence[Sequence[int]], mask: int) -> int:
    transformed = 0
    for sample, old_direction in enumerate(DIRECTIONS):
        if mask & (1 << sample):
            new_direction = transform_point(matrix, old_direction)
            transformed |= 1 << DIRECTION_INDEX[new_direction]
    return transformed


def canonical_mask(mask: int) -> tuple[int, int]:
    return min((transform_mask(rotation, mask), index)
               for index, rotation in enumerate(ROTATIONS))


def junction_signature(case: dict[str, object]) -> tuple[object, ...]:
    junction = case["junction"]
    assert isinstance(junction, dict)
    faces = junction["faces"]
    assert isinstance(faces, list)
    return tuple(sorted(tuple(tuple(point) for point in face["points"])
                        for face in faces))


def transform_signature(
    signature: Sequence[Sequence[Sequence[int]]],
    matrix: Sequence[Sequence[int]],
) -> tuple[object, ...]:
    return tuple(sorted(canonical_cycle(transform_point(matrix, point)
                                        for point in face)
                        for face in signature))


def unoriented_signature(case: dict[str, object]) -> tuple[object, ...]:
    junction = case["junction"]
    assert isinstance(junction, dict)
    faces = junction["faces"]
    assert isinstance(faces, list)
    return tuple(sorted(unoriented_cycle(face["points"]) for face in faces))


def classify_cases(cases: list[dict[str, object]]) -> tuple[list[dict[str, object]], dict[str, object]]:
    by_mask = {int(case["mask"]): case for case in cases}
    classes: dict[int, list[dict[str, int]]] = defaultdict(list)
    for case in cases:
        mask = int(case["mask"])
        representative, rotation_index = canonical_mask(mask)
        case["canonical-mask"] = representative
        case["rotation-to-canonical"] = rotation_index
        classes[representative].append({
            "mask": mask,
            "rotation-to-canonical": rotation_index,
        })

    symmetry_mismatches: list[dict[str, int]] = []
    for mask, case in by_mask.items():
        signature = junction_signature(case)
        for rotation_index, rotation in enumerate(ROTATIONS):
            target_mask = transform_mask(rotation, mask)
            if transform_signature(signature, rotation) != junction_signature(by_mask[target_mask]):
                symmetry_mismatches.append({
                    "mask": mask,
                    "rotation": rotation_index,
                    "target-mask": target_mask,
                })

    complement_mismatches = [
        mask for mask in range(128)
        if unoriented_signature(by_mask[mask]) !=
           unoriented_signature(by_mask[mask ^ 0xFF])
    ]
    class_records = [
        {"representative": representative,
         "regular-star": bool(by_mask[representative]["input"]["regular-star"]),
         "members": sorted(members, key=lambda member: member["mask"])}
        for representative, members in sorted(classes.items())
    ]
    regular_masks = {
        mask for mask, case in by_mask.items()
        if bool(case["input"]["regular-star"])
    }
    regular_rotation_mismatches = [
        mismatch for mismatch in symmetry_mismatches
        if mismatch["mask"] in regular_masks
    ]
    singular_rotation_mismatches = [
        mismatch for mismatch in symmetry_mismatches
        if mismatch["mask"] not in regular_masks
    ]
    regular_complement_mismatches = [
        mask for mask in complement_mismatches
        if mask in regular_masks and (mask ^ 0xFF) in regular_masks
    ]
    issue_on_regular = [
        mask for mask in regular_masks if by_mask[mask]["issues"]
    ]
    checks = {
        "rotation-class-count": len(class_records),
        "regular-star-count": len(regular_masks),
        "singular-star-count": len(cases) - len(regular_masks),
        "regular-rotation-class-count": sum(
            bool(record["regular-star"]) for record in class_records
        ),
        "singular-rotation-class-count": sum(
            not bool(record["regular-star"]) for record in class_records
        ),
        "rotation-mismatch-count": len(symmetry_mismatches),
        "regular-rotation-mismatch-count": len(regular_rotation_mismatches),
        "singular-rotation-mismatch-count": len(singular_rotation_mismatches),
        "rotation-mismatches": symmetry_mismatches[:64],
        "complement-mismatch-count": len(complement_mismatches),
        "regular-complement-mismatch-count": len(regular_complement_mismatches),
        "complement-mismatches": complement_mismatches,
        "issue-case-count": sum(bool(case["issues"]) for case in cases),
        "issue-masks": [int(case["mask"]) for case in cases if case["issues"]],
        "issue-on-regular-count": len(issue_on_regular),
        "issue-on-regular-masks": sorted(issue_on_regular),
    }
    return class_records, checks


def lisp_key(key: str) -> str:
    return ":" + key.replace("_", "-")


def lisp(value: object, indent: int = 0) -> str:
    if value is None:
        return "NIL"
    if value is True:
        return "T"
    if value is False:
        return "NIL"
    if isinstance(value, str):
        return json.dumps(value)
    if isinstance(value, int):
        return str(value)
    if isinstance(value, float):
        return repr(value)
    if isinstance(value, dict):
        if not value:
            return "()"
        pieces = []
        child_indent = indent + 2
        for key, child in value.items():
            rendered = lisp(child, child_indent + len(str(key)) + 2)
            pieces.append(f"{lisp_key(str(key))} {rendered}")
        separator = "\n" + " " * child_indent
        return "(" + separator.join(pieces) + ")"
    if isinstance(value, (list, tuple)):
        if not value:
            return "()"
        rendered = [lisp(child, indent + 1) for child in value]
        if all("\n" not in child and len(child) < 72 for child in rendered):
            one_line = "(" + " ".join(rendered) + ")"
            if len(one_line) + indent <= 100:
                return one_line
        separator = "\n" + " " * (indent + 1)
        return "(" + separator.join(rendered) + ")"
    raise TypeError(f"Cannot serialize {type(value).__name__}")


def blender_build_hash() -> str:
    value = bpy.app.build_hash
    return value.decode() if isinstance(value, bytes) else str(value)


def make_material(
    name: str,
    color: tuple[float, float, float, float],
    *,
    emission: float = 0.0,
) -> bpy.types.Material:
    material = bpy.data.materials.new(name)
    material.use_nodes = True
    shader = material.node_tree.nodes.get("Principled BSDF")
    if shader is not None:
        shader.inputs["Base Color"].default_value = color
        shader.inputs["Roughness"].default_value = 0.74
        if emission:
            shader.inputs["Emission Color"].default_value = color
            shader.inputs["Emission Strength"].default_value = emission
    return material


def make_gallery_star(
    mask: int,
    location: tuple[float, float, float],
    material: bpy.types.Material,
    edge_material: bpy.types.Material,
) -> bpy.types.Object:
    vertices, faces = boundary_mesh(mask, 4)
    mesh = bpy.data.meshes.new(f"gallery-star-{mask:02x}-source")
    mesh.from_pydata(vertices, [], faces)
    mesh.update()
    obj = bpy.data.objects.new(f"star-{mask:02x}", mesh)
    bpy.context.scene.collection.objects.link(obj)
    obj.location = location
    obj.data.materials.append(material)
    obj.data.materials.append(edge_material)
    modifier = obj.modifiers.new(name="Arc bevel", type="BEVEL")
    configure_bevel(modifier, 1)
    wire = obj.modifiers.new(name="construction edges", type="WIREFRAME")
    wire.thickness = 0.035
    wire.use_even_offset = True
    wire.use_replace = False
    wire.material_offset = 1
    return obj


def point_at(obj: bpy.types.Object, target: Sequence[float]) -> None:
    obj.rotation_euler = (Vector(target) - obj.location).to_track_quat("-Z", "Y").to_euler()


def add_area_light(
    name: str,
    location: tuple[float, float, float],
    energy: float,
    size: float,
    target: tuple[float, float, float],
) -> None:
    light = bpy.data.lights.new(name, type="AREA")
    light.energy = energy
    light.shape = "DISK"
    light.size = size
    obj = bpy.data.objects.new(name, light)
    bpy.context.scene.collection.objects.link(obj)
    obj.location = location
    point_at(obj, target)


def add_gallery_label(
    text: str,
    location: tuple[float, float, float],
    camera: bpy.types.Object,
    material: bpy.types.Material,
) -> None:
    curve = bpy.data.curves.new(f"label-{text}", type="FONT")
    curve.body = text
    curve.align_x = "CENTER"
    curve.align_y = "CENTER"
    curve.size = 0.8
    curve.extrude = 0.015
    obj = bpy.data.objects.new(f"label-{text}", curve)
    bpy.context.scene.collection.objects.link(obj)
    obj.location = location
    obj.rotation_euler = (camera.location - obj.location).to_track_quat("Z", "Y").to_euler()
    obj.data.materials.append(material)


def render_gallery(path: Path) -> None:
    """Render regular representatives beside three singular counterexamples."""
    for obj in list(bpy.data.objects):
        bpy.data.objects.remove(obj, do_unlink=True)

    regular = make_material("regular star", (0.16, 0.48, 0.82, 1.0))
    singular = make_material("singular star", (0.82, 0.25, 0.16, 1.0))
    ground = make_material("ground", (0.12, 0.15, 0.20, 1.0))
    edges = make_material("edges", (0.012, 0.018, 0.03, 1.0))
    labels = make_material("labels", (0.82, 0.88, 0.98, 1.0), emission=0.8)
    cases = (
        (1, True), (7, True), (23, True),
        (27, True), (29, True), (31, True),
        (6, False), (24, False), (30, False),
    )
    pitch = 14.0
    for index, (mask, regular_p) in enumerate(cases):
        row, column = divmod(index, 3)
        x = (column - 1) * pitch
        y = (1 - row) * pitch
        make_gallery_star(
            mask, (x, y, 0.0), regular if regular_p else singular, edges
        )

    ground_mesh = bpy.data.meshes.new("ground")
    extent = 26.0
    ground_mesh.from_pydata(
        [(-extent, -extent, -5.0), (extent, -extent, -5.0),
         (extent, extent, -5.0), (-extent, extent, -5.0)],
        [],
        [(0, 1, 2, 3)],
    )
    ground_obj = bpy.data.objects.new("ground", ground_mesh)
    bpy.context.scene.collection.objects.link(ground_obj)
    ground_obj.data.materials.append(ground)

    camera_data = bpy.data.cameras.new("camera")
    camera = bpy.data.objects.new("camera", camera_data)
    bpy.context.scene.collection.objects.link(camera)
    camera.location = (42.0, -52.0, 44.0)
    camera.data.type = "ORTHO"
    camera.data.ortho_scale = 49.0
    point_at(camera, (0.0, 0.0, 0.0))
    bpy.context.scene.camera = camera

    for index, (mask, regular_p) in enumerate(cases):
        row, column = divmod(index, 3)
        x = (column - 1) * pitch
        y = (1 - row) * pitch
        kind = "regular" if regular_p else "singular"
        add_gallery_label(f"{mask:02X} {kind}", (x, y, 5.8), camera, labels)

    add_area_light("key", (-22.0, -28.0, 42.0), 2400.0, 18.0, (0.0, 0.0, 0.0))
    add_area_light("fill", (30.0, 14.0, 24.0), 1500.0, 14.0, (0.0, 0.0, 0.0))

    scene = bpy.context.scene
    scene.render.engine = "BLENDER_EEVEE"
    scene.render.resolution_x = 1600
    scene.render.resolution_y = 1100
    scene.render.resolution_percentage = 100
    scene.render.image_settings.file_format = "PNG"
    scene.render.filepath = str(path.resolve())
    scene.render.film_transparent = False
    scene.view_settings.exposure = 0.8
    scene.world.use_nodes = True
    background = scene.world.node_tree.nodes.get("Background")
    if background is not None:
        background.inputs["Color"].default_value = (0.055, 0.07, 0.10, 1.0)
        background.inputs["Strength"].default_value = 0.7
    path.parent.mkdir(parents=True, exist_ok=True)
    print(f"LUFT Blender oracle: rendering {path}", flush=True)
    bpy.ops.render.render(write_still=True)
    print(f"LUFT Blender oracle: rendered {path}", flush=True)


def generate(cell_size: int, width: int, tolerance: float) -> dict[str, object]:
    cases: list[dict[str, object]] = []
    for mask in range(256):
        cases.append(evaluate_case(mask, cell_size, width, tolerance))
        if mask % 16 == 15:
            print(f"LUFT Blender oracle: evaluated {mask + 1}/256 stars", flush=True)
    classes, checks = classify_cases(cases)
    return {
        "format-version": FORMAT_VERSION,
        "generator": "scripts/luft-blender-star-oracle.py",
        "blender": {
            "version": bpy.app.version_string,
            "build-hash": blender_build_hash(),
        },
        "parameters": {
            "cell-size": cell_size,
            "bevel-width": width,
            "integer-tolerance": tolerance,
            "source-scene-cell-size": 4,
            "source-scene-bevel-width": 1,
            "bevel-settings": BEVEL_SETTINGS,
        },
        "rotations": [[list(row) for row in rotation] for rotation in ROTATIONS],
        "classes": classes,
        "checks": checks,
        "cases": cases,
    }


def parse_arguments() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--output",
        type=Path,
        default=Path(__file__).resolve().parents[1] / "luft" / "blender-arc-stars.sexp",
    )
    parser.add_argument("--check", action="store_true")
    parser.add_argument("--render", type=Path)
    parser.add_argument("--cell-size", type=int, default=DEFAULT_CELL_SIZE)
    parser.add_argument("--width", type=int, default=DEFAULT_WIDTH)
    parser.add_argument("--tolerance", type=float, default=DEFAULT_TOLERANCE)
    arguments = sys.argv[sys.argv.index("--") + 1:] if "--" in sys.argv else []
    return parser.parse_args(arguments)


def main() -> int:
    arguments = parse_arguments()
    if not (0 < arguments.width * 2 < arguments.cell_size):
        raise SystemExit("bevel width must be positive and below half the cell size")
    data = generate(arguments.cell_size, arguments.width, arguments.tolerance)
    contents = (
        ";; Generated by scripts/luft-blender-star-oracle.py; do not edit.\n"
        + lisp(data)
        + "\n"
    )
    digest = hashlib.sha256(contents.encode()).hexdigest()
    if arguments.check:
        if not arguments.output.exists():
            print(f"LUFT Blender oracle: missing {arguments.output}", file=sys.stderr)
            return 1
        if arguments.output.read_text() != contents:
            print(f"LUFT Blender oracle: {arguments.output} is stale", file=sys.stderr)
            print(f"generated sha256 {digest}", file=sys.stderr)
            return 1
        print(f"LUFT Blender oracle: checked {arguments.output} ({digest})")
    else:
        arguments.output.write_text(contents)
        print(f"LUFT Blender oracle: wrote {arguments.output} ({digest})")
    checks = data["checks"]
    assert isinstance(checks, dict)
    print(
        "LUFT Blender oracle: "
        f"{checks['rotation-class-count']} rotation classes, "
        f"{checks['regular-star-count']} regular stars, "
        f"{checks['regular-rotation-mismatch-count']} regular rotation mismatches, "
        f"{checks['regular-complement-mismatch-count']} regular complement mismatches, "
        f"{checks['issue-on-regular-count']} regular stars with geometric issues; "
        f"{checks['singular-star-count']} singular stars retained as counterexamples"
    )
    if arguments.render is not None:
        render_gallery(arguments.render)
    return 1 if (checks["regular-rotation-mismatch-count"]
                 or checks["regular-complement-mismatch-count"]
                 or checks["issue-on-regular-count"]) else 0


if __name__ == "__main__":
    raise SystemExit(main())
