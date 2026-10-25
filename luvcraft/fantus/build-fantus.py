"""Fantus: sewn, weighted mesh; editable two-bone IK; verified runtime skinning.

Run with Blender 5.2+: blender -b --python luvcraft/fantus/build-fantus.py
Optional arguments after --: --render (three review stills), --preview (24 PNGs).
The script owns this directory only; review imagery goes to .amp/in/artifacts.
No downloaded models, textures, external Python packages, or add-ons are needed.
"""

import argparse
import json
import math
import os
import struct
import sys
from pathlib import Path

import bmesh
import bpy
import numpy as np
from mathutils import Matrix, Quaternion, Vector

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent.parent
ART = ROOT / ".amp/in/artifacts"
TAU = math.tau
FRAMES = 48
STRIDE = 0.80
STANCE = 0.58
# Blender (X right, Z up, -Y forward) -> runtime (X right, Y up, Z forward).
C = Matrix(((1, 0, 0, 0), (0, 0, 1, 0), (0, -1, 0, 0), (0, 0, 0, 1)))
PALETTE = {
    "felt": "794699", "ear": "B59AC7", "seam": "70458E",
    "cream": "F3DDB0", "peach": "E9BB94", "cloth": "F6F3E9",
    "green": "73A343", "green_dark": "46783A", "mouth": "46263F",
    "lip": "9866AA", "white": "FFF9E7", "hazel": "948445",
    "iris_dark": "51472D", "iris_gold": "BBA75D", "pupil": "231C22",
    "catch": "FFFFFF", "sole": "C7ABC8", "yellow": "EEC34B",
}
MATS = {}
PARTS = []


def smooth(a, b, x):
    t = max(0.0, min(1.0, (x - a) / (b - a)))
    return t * t * (3 - 2 * t)


def linear_rgb(h):
    def cv(v):
        s = int(v, 16) / 255
        return s / 12.92 if s <= 0.04045 else ((s + 0.055) / 1.055) ** 2.4
    return tuple(cv(h[i:i + 2]) for i in (0, 2, 4))


def materials():
    for name, col in PALETTE.items():
        mat = bpy.data.materials.new("Fantus / " + name)
        mat.diffuse_color = (*linear_rgb(col), 1)
        mat.use_nodes = True
        nodes = mat.node_tree.nodes
        bsdf = nodes.get("Principled BSDF")
        bsdf.inputs["Base Color"].default_value = mat.diffuse_color
        eye = name in {"white", "hazel", "iris_dark", "iris_gold", "pupil", "catch"}
        bsdf.inputs["Roughness"].default_value = 0.32 if eye else 0.86
        bsdf.inputs["Specular IOR Level"].default_value = 0.28 if eye else 0.22
        if not eye:
            bsdf.inputs["Sheen Weight"].default_value = 0.15
            bsdf.inputs["Sheen Roughness"].default_value = 0.75
            tex = nodes.new("ShaderNodeTexNoise")
            tex.inputs["Scale"].default_value = 185 if name == "cloth" else 220
            tex.inputs["Detail"].default_value = 2
            bump = nodes.new("ShaderNodeBump")
            bump.inputs["Strength"].default_value = 0.19
            bump.inputs["Distance"].default_value = 0.0012
            mat.node_tree.links.new(tex.outputs["Fac"], bump.inputs["Height"])
            mat.node_tree.links.new(bump.outputs["Normal"], bsdf.inputs["Normal"])
        MATS[name] = mat


def mesh(name, vertices, faces, material, weights=None, face_mats=None):
    data = bpy.data.meshes.new(name)
    data.from_pydata(vertices, [], faces)
    data.update()
    obj = bpy.data.objects.new(name, data)
    bpy.context.collection.objects.link(obj)
    keys = material if isinstance(material, list) else [material]
    for key in keys:
        data.materials.append(MATS[key])
    if face_mats:
        for p, mi in zip(data.polygons, face_mats):
            p.material_index = mi
    bm = bmesh.new()
    bm.from_mesh(data)
    bmesh.ops.recalc_face_normals(bm, faces=bm.faces)
    bm.to_mesh(data)
    bm.free()
    for p in data.polygons:
        p.use_smooth = True
    if weights:
        assign_weights(obj, weights)
    PARTS.append(obj)
    return obj


def assign_weights(obj, fn):
    for group in list(obj.vertex_groups):
        obj.vertex_groups.remove(group)
    for v in obj.data.vertices:
        pairs = {fn: 1.0} if isinstance(fn, str) else fn(v.co)
        pairs = {k: w for k, w in pairs.items() if w > 1e-7}
        total = sum(pairs.values())
        assert total > 0 and len(pairs) <= 4, pairs
        for name, value in pairs.items():
            group = obj.vertex_groups.get(name) or obj.vertex_groups.new(name=name)
            group.add([v.index], value / total, "REPLACE")


def active(obj):
    bpy.ops.object.select_all(action="DESELECT")
    obj.select_set(True)
    bpy.context.view_layer.objects.active = obj


def apply(obj, mod):
    active(obj)
    bpy.ops.object.modifier_apply(modifier=mod.name)


def ellipsoid(name, center, radius, mat, weight=None, segments=32, rings=20):
    verts, faces = [], []
    for j in range(rings + 1):
        lat = -math.pi / 2 + math.pi * j / rings
        for i in range(segments):
            a = TAU * i / segments
            verts.append((center[0] + radius[0] * math.cos(lat) * math.cos(a),
                          center[1] + radius[1] * math.cos(lat) * math.sin(a),
                          center[2] + radius[2] * math.sin(lat)))
    for j in range(rings):
        for i in range(segments):
            n = (i + 1) % segments
            faces.append((j * segments + i, j * segments + n,
                          (j + 1) * segments + n, (j + 1) * segments + i))
    obj = mesh(name, verts, faces, mat, weight)
    bm = bmesh.new()
    bm.from_mesh(obj.data)
    bmesh.ops.remove_doubles(bm, verts=bm.verts, dist=1e-6)
    bm.to_mesh(obj.data)
    bm.free()
    return obj


def catmull(points, steps=8, closed=False):
    pts = [Vector(p) for p in points]
    result = []
    end = len(pts) if closed else len(pts) - 1
    for i in range(end):
        a = pts[(i - 1) % len(pts)] if closed or i else pts[0]
        b, c = pts[i], pts[(i + 1) % len(pts)]
        d = pts[(i + 2) % len(pts)] if closed or i + 2 < len(pts) else pts[-1]
        for k in range(steps):
            t = k / steps
            result.append((2*b + (c-a)*t + (2*a-5*b+4*c-d)*t*t
                           + (-a+3*b-3*c+d)*t*t*t) * .5)
    if not closed:
        result.append(pts[-1])
    return result


def tube(name, points, radius, mat, weights, sides=8, closed=False):
    pts = [Vector(p) for p in points]
    verts, faces = [], []
    previous_u = None
    for j, p in enumerate(pts):
        prev = pts[j - 1] if j or closed else p
        nxt = pts[(j + 1) % len(pts)] if j + 1 < len(pts) or closed else p
        tangent = (nxt - prev).normalized()
        ref = previous_u if previous_u is not None else (Vector((1, 0, 0)) if abs(tangent.x) < .9 else Vector((0, 1, 0)))
        u = (ref - tangent * ref.dot(tangent)).normalized()
        previous_u = u
        v = tangent.cross(u)
        r = radius(j / (len(pts)-1)) if callable(radius) else radius
        for i in range(sides):
            a = TAU*i/sides
            verts.append(p + r * (u*math.cos(a) + v*math.sin(a)))
    for j in range(len(pts) if closed else len(pts)-1):
        for i in range(sides):
            faces.append((j*sides+i, j*sides+(i+1)%sides,
                          ((j+1)%len(pts))*sides+(i+1)%sides,
                          ((j+1)%len(pts))*sides+i))
    if not closed:
        faces.extend([tuple(reversed(range(sides))),
                      tuple((len(pts)-1)*sides+i for i in range(sides))])
    return mesh(name, verts, faces, mat, weights)


def union(name, objects, voxel, weight):
    bpy.ops.object.select_all(action="DESELECT")
    for obj in objects:
        obj.select_set(True)
        PARTS.remove(obj)
    bpy.context.view_layer.objects.active = objects[0]
    bpy.ops.object.join()
    obj = bpy.context.object
    obj.name = name
    obj.data.remesh_voxel_size = voxel
    bpy.ops.object.voxel_remesh()
    mod = obj.modifiers.new("Sculpted soft joins", "SMOOTH")
    mod.factor, mod.iterations = 1.0, 5
    apply(obj, mod)
    mod = obj.modifiers.new("Even silhouette budget", "DECIMATE")
    mod.ratio = .34
    apply(obj, mod)
    for p in obj.data.polygons:
        p.use_smooth = True
    assign_weights(obj, weight)
    PARTS.append(obj)
    return obj


def body_weights(p):
    x, y, z = p
    torso = smooth(.58, .80, z)
    arm = smooth(.16, .245, abs(x)) * (1 - smooth(.94, 1.02, z))
    if arm < .001:
        return {"hips": 1-torso, "chest": torso}
    side = "L" if x > 0 else "R"
    wrist = 1 - smooth(.405, .49, z)
    elbow = 1 - smooth(.64, .76, z)
    # Shoulder vertices meet the chest; wrist and elbow never overlap.
    if wrist > 0:
        limb = {"forearm."+side: 1-wrist, "hand."+side: wrist}
    else:
        limb = {"upper_arm."+side: 1-elbow, "forearm."+side: elbow}
    return {"chest": 1-arm, **{k: arm*w for k, w in limb.items()}}


def leg_weights(side):
    def fn(p):
        z = p.z
        foot = 1 - smooth(.105, .275, z)
        thigh = smooth(.275, .385, z)
        hip = smooth(.51, .62, z)
        if foot > 0:
            return {"foot."+side: foot, "shin."+side: 1-foot}
        return {"shin."+side: 1-thigh, "thigh."+side: thigh*(1-hip), "hips": thigh*hip}
    return fn


def build_body():
    pieces = [ellipsoid("Torso sculpt", (0, .025, .755), (.228, .168, .285), "felt"),
              ellipsoid("Neck sculpt", (0, .025, .995), (.115, .115, .13), "felt")]
    for s, side in ((1, "L"), (-1, "R")):
        path = catmull([(s*.155, .018, .95), (s*.23, .006, .87),
                        (s*.30, -.006, .70), (s*.355, -.027, .51),
                        (s*.38, -.045, .41)], 5)
        pieces.append(tube("Arm sculpt", path, lambda t: .066-.017*t,
                           "felt", None, sides=16))
        pieces.append(ellipsoid("Mitten sculpt", (s*.388, -.05, .397),
                                (.063, .050, .078), "felt", segments=20, rings=12))
        for j in range(3):
            x = s*(.353+.034*j)
            pieces.append(ellipsoid("Finger sculpt", (x, -.061, .343+.008*abs(j-1)),
                                    (.020, .032, .048), "felt", segments=12, rings=10))
    union("Body · continuous sewn felt", pieces, .013, body_weights)
    # Belly is a tailored inset following the pear-shaped chest, not a ball.
    def belly_y(x, z):
        q = max(.03, 1 - (x/.228)**2 - ((z-.755)/.285)**2)
        return .025 - .168*math.sqrt(q) - .002
    oval_patch("Belly applique", (0, .792), (.127, .18), belly_y, "cream", body_weights)
    for s, side in ((1, "L"), (-1, "R")):
        pieces = [tube("Leg sculpt", catmull([(s*.135, .01, .55),
                                              (s*.155, -.035, .33),
                                              (s*.17, 0, .115)], 8),
                       lambda t: .092-.024*t, "felt", None, sides=20),
                  ellipsoid("Foot sculpt", (s*.17, -.055, .087),
                            (.108, .169, .089), "felt", segments=32, rings=18)]
        obj = union("Leg and soft foot."+side, pieces, .010, leg_weights(side))
        # Squash the bottom into an actual level contact patch.
        for v in obj.data.vertices:
            if v.co.z < .031:
                v.co.z = max(0, (v.co.z-.018) * .35)
        obj.data.update()
        # Palm felt and a pair of finger stitching lines.
        ellipsoid("Palm patch."+side, (s*.389, -.091, .403),
                  (.043, .008, .047), "cream", "hand."+side, segments=24, rings=12)
        for j in (-1, 1):
            tube("Mitten stitch."+side, [(s*.388+j*.017, -.087, .335),
                                          (s*.388+j*.017, -.094, .358),
                                          (s*.388+j*.017, -.097, .377)],
                 .0013, "seam", "hand."+side, sides=6)
        # Cream cloven applique on the underside, visible during a lifted step.
        for j in (-1, 1):
            obj = ellipsoid("Cloven sole."+side, (s*.17+j*.022, -.089, .0015),
                            (.031, .055, .0015), "cream", "foot."+side,
                            segments=20, rings=8)
            for v in obj.data.vertices:
                v.co.x += j*.23*(v.co.y+.089)


def oval_patch(name, center, radii, y_fn, mat, weight, sectors=48, rings=9):
    verts = [(center[0], y_fn(*center), center[1])]
    for j in range(1, rings+1):
        r = j/rings
        for i in range(sectors):
            a = TAU*i/sectors
            x, z = center[0]+r*radii[0]*math.cos(a), center[1]+r*radii[1]*math.sin(a)
            verts.append((x, y_fn(x, z), z))
    faces = [(0, 1+i, 1+(i+1)%sectors) for i in range(sectors)]
    for j in range(rings-1):
        for i in range(sectors):
            a, b = 1+j*sectors+i, 1+j*sectors+(i+1)%sectors
            faces.append((a, b, b+sectors, a+sectors))
    return mesh(name, verts, faces, mat, weight)


def head_radii(z):
    h = max(-.99999, min(.99999, (z-1.265)/.335))
    c = math.sqrt(1-h*h)
    return .383*c**.80*(.94+.06*h), .276*c**.80


def face_y(x, z):
    rx, ry = head_radii(z)
    return .025 - ry*math.sqrt(max(.001, 1-(x/rx)**2))


def build_head():
    verts, faces = [], []
    n, m = 48, 32
    for j in range(m+1):
        lat = -math.pi/2 + math.pi*j/m
        z = 1.265+.335*math.sin(lat)
        rx, ry = head_radii(z)
        if j in (0, m):
            rx = ry = 0
        for i in range(n):
            a = TAU*i/n
            verts.append((rx*math.cos(a), .025+ry*math.sin(a), z))
    for j in range(m):
        for i in range(n):
            faces.append((j*n+i, j*n+(i+1)%n, (j+1)*n+(i+1)%n, (j+1)*n+i))
    head = mesh("Head · broad cheek-to-crown topology", verts, faces, "felt", "head")
    bm = bmesh.new()
    bm.from_mesh(head.data)
    bmesh.ops.remove_doubles(bm, verts=bm.verts, dist=1e-6)
    bm.to_mesh(head.data)
    bm.free()
    outline = catmull([(-.293, 1.34), (-.268, 1.425), (-.16, 1.487),
                       (0, 1.505), (.16, 1.487), (.268, 1.425), (.293, 1.34),
                       (.258, 1.253), (.176, 1.225), (.100, 1.248),
                       (.059, 1.305), (0, 1.323), (-.059, 1.305),
                       (-.100, 1.248), (-.176, 1.225), (-.258, 1.253)],
                      5, closed=True)
    # This notched outline is not star-convex: a radial fan overlaps itself
    # above the trunk. Ear-clip the real boundary, refine, then drape it.
    mask = mesh("Mask · one shaped cream felt panel", [(x, 0, z) for x, z in outline],
                [tuple(range(len(outline)))], "cream")
    bm = bmesh.new()
    bm.from_mesh(mask.data)
    bmesh.ops.triangulate(bm, faces=list(bm.faces))
    bmesh.ops.subdivide_edges(bm, edges=list(bm.edges), cuts=3, use_grid_fill=True)
    for v in bm.verts:
        v.co.y = face_y(v.co.x, v.co.z)-.006
    bmesh.ops.recalc_face_normals(bm, faces=list(bm.faces))
    bm.to_mesh(mask.data)
    bm.free()
    for p in mask.data.polygons:
        p.use_smooth = True
    assign_weights(mask, "head")
    mask.data.calc_loop_triangles()
    boundary_area = abs(sum(a.x*b.y-b.x*a.y for a, b in zip(outline, outline[1:]+outline[:1])))*.5
    projected_area = 0.0
    for tri in mask.data.loop_triangles:
        a, b, c = [mask.data.vertices[i].co for i in tri.vertices]
        projected_area += abs((b.x-a.x)*(c.z-a.z)-(c.x-a.x)*(b.z-a.z))*.5
    assert abs(projected_area-boundary_area) < 1e-7, "Mask triangles overlap in projection"
    tube("Mask piping", [(x, face_y(x, z)-.0074, z) for x, z in outline],
         .0015, "peach", "head", sides=6, closed=True)
    # Back head seam is subtle but gives the editable plush a sewn construction.
    seam = []
    for z in np.linspace(.995, 1.598, 60):
        rx, ry = head_radii(z)
        seam.append((0, .025+ry+.0005, z))
    tube("Crown back seam", seam, .0016, "seam", "head", sides=6)
    build_eyes()
    build_trunk()
    # Parted smile, with corners lifted; a dark recessed-looking ribbon and lips.
    verts, faces, upper, lower = [], [], [], []
    for j in range(49):
        x = -.185+.370*j/48
        t = x/.185
        mid = 1.075+.062*t*t
        width = .002+.010*(1-t*t)
        for z in (mid-width, mid+width):
            verts.append((x, face_y(x, z)-.003, z))
        upper.append((x, face_y(x, mid+width)-.004, mid+width))
        lower.append((x, face_y(x, mid-width)-.004, mid-width))
        if j:
            faces.append((2*j-2, 2*j-1, 2*j+1, 2*j))
    mesh("Gentle parted smile", verts, faces, "mouth", "head")
    tube("Soft lower lip", lower, .0032, "lip", "head", sides=8)
    tube("Upper smile seam", upper, .0019, "seam", "head", sides=6)


def build_eyes():
    for s, side in ((1, "L"), (-1, "R")):
        cx, cz = s*.17, 1.342
        rx, ry, rz = .083, .027, .065
        def surface(x, z):
            return face_y(x, z)-.011-ry*math.sqrt(max(0, 1-((x-cx)/rx)**2-((z-cz)/rz)**2))
        # Shallow lenses drape onto the face. Their outer edges are embedded in
        # the mask instead of exposing the sides of protruding spherical eyeballs.
        oval_patch("Eye felt socket."+side, (cx, cz), (rx+.0025, rz+.0025),
                   lambda x, z: face_y(x, z)-.009, "seam", "head", sectors=40, rings=5)
        oval_patch("Wide oval sclera."+side, (cx, cz), (rx, rz),
                   surface, "white", "head", sectors=48, rings=12)
        # Radially tessellated iris follows the sclera, with a restrained gold ring.
        verts = [(cx, surface(cx, cz)-.001, cz)]
        ids = []
        rings, sectors = 7, 40
        for j in range(1, rings+1):
            for i in range(sectors):
                a = TAU*i/sectors
                x = cx+.045*j/rings*math.cos(a)
                z = cz+.037*j/rings*math.sin(a)
                verts.append((x, surface(x, z)-.001, z))
        faces = [(0, i+1, (i+1)%sectors+1) for i in range(sectors)]
        ids.extend([0]*sectors)
        for j in range(rings-1):
            for i in range(sectors):
                a, b = 1+j*sectors+i, 1+j*sectors+(i+1)%sectors
                faces.append((a, b, b+sectors, a+sectors))
                ids.append(1 if j == rings-2 else (2 if (i*7)%11 < 3 else 0))
        mesh("Hazel iris."+side, verts, faces,
             ["hazel", "iris_dark", "iris_gold"], "head", ids)
        oval_patch("Pupil."+side, (cx, cz), (.0255, .025),
                   lambda x, z: surface(x, z)-.002, "pupil", "head", sectors=32, rings=4)
        for dx, dz, r in ((-.014, .018, .0085), (.012, -.011, .0032)):
            x, z = cx+dx, cz+dz
            ellipsoid("Eye catch."+side, (x, surface(x, z)-.003, z),
                      (r, .0025, r), "catch", "head", segments=16, rings=10)
        # Actual upper-lid shell, with its edge above the pupil, never a visor.
        verts, faces, edge = [], [], []
        for j in range(8):
            t = j/7
            z = cz+.044+(.0658-.044)*t
            half_width = rx*math.sqrt(max(0, 1-((z-cz)/.066)**2))
            for i in range(33):
                x = cx+half_width*(2*i/32-1)
                y = surface(x, z)-.0018
                verts.append((x, y, z))
                if j == 0:
                    edge.append((x, y-.0005, z))
        for j in range(7):
            for i in range(32):
                a = j*33+i
                faces.append((a, a+1, a+34, a+33))
        mesh("Upper felt lid."+side, verts, faces, "cream", "head")
        tube("Lid rolled hem."+side, edge, .0015, "peach", "head", sides=6)


def trunk_point(t):
    return Vector((0, -.225-.265*t, 1.264-.195*t+.063*t*t))


def trunk_weights(p):
    t = max(0, min(1, (-p.y-.225)/.265))
    a, b = smooth(.08, .32, t), smooth(.49, .78, t)
    return {"head": 1-a, "trunk.01": a*(1-b), "trunk.02": a*b}


def build_trunk():
    pts = [trunk_point(t) for t in np.linspace(0, 1, 49)]
    def radius(t):
        return .103-.048*t + .0028*math.sin(7*TAU*t)*math.sin(math.pi*t)**.5
    tube("Trunk · curved seven-ring felt", pts, radius, "felt", trunk_weights, sides=24)
    tip, tangent = pts[-1], (pts[-1]-pts[-2]).normalized()
    u, v = Vector((1, 0, 0)), tangent.cross(Vector((1, 0, 0)))
    verts = [tip+tangent*.001]
    n, m = 40, 6
    for j in range(1, m+1):
        r = .042*j/m
        for i in range(n):
            a = TAU*i/n
            verts.append(tip + r*(u*math.cos(a)+v*math.sin(a))
                         + tangent*(.0015-.002*(1-j/m)**2))
    faces = [(0, i+1, (i+1)%n+1) for i in range(n)]
    for j in range(m-1):
        for i in range(n):
            a, b = 1+j*n+i, 1+j*n+(i+1)%n
            faces.append((a, b, b+n, a+n))
    mesh("Peach trunk end inset", verts, faces, "peach", "trunk.02")
    for s in (-1, 1):
        center = tip+u*s*.014+v*.004+tangent*.003
        ellipsoid("Trunk tip stitch dimple", center, (.004, .0017, .003),
                  "cream", "trunk.02", segments=12, rings=8)


def ear_point(s, r, a, back=False):
    # Paddle: narrow lower attachment, broad top; cupped rather than a flat disk.
    q = math.sin(a)
    u = .177*r*math.cos(a)*(.91+.18*q*r)
    v = .295*r*q
    angle = math.radians(28)
    x = s*(.49 + u*math.cos(angle)+v*math.sin(angle))
    z = 1.30 - u*math.sin(angle)+v*math.cos(angle)
    depth = (.028+.031*(1-r*r)) if back else (-.008-.052*r*r)
    y = .037 + depth + .20*u
    return Vector((x, y, z))


def ear_weights(side):
    def fn(p):
        t = smooth(.37, .65, abs(p.x))
        return {"ear_base."+side: 1-t, "ear_tip."+side: t}
    return fn


def build_ears():
    n, rings = 48, 10
    for s, side in ((1, "L"), (-1, "R")):
        verts, faces, ids = [], [], []
        for back in (False, True):
            offset = len(verts)
            verts.append(ear_point(s, 0, 0, back))
            for j in range(1, rings+1):
                for i in range(n):
                    verts.append(ear_point(s, j/rings, TAU*i/n, back))
            for i in range(n):
                faces.append((offset, offset+1+i, offset+1+(i+1)%n))
                ids.append(0 if back else 1)
            for j in range(rings-1):
                for i in range(n):
                    a, b = offset+1+j*n+i, offset+1+j*n+(i+1)%n
                    faces.append((a, b, b+n, a+n))
                    ids.append(0 if back or j >= rings-3 else 1)
        back_start = 1+rings*n
        front_outer = 1+(rings-1)*n
        for i in range(n):
            a, b = front_outer+i, front_outer+(i+1)%n
            faces.append((a, b, b+back_start, a+back_start))
            ids.append(0)
        mesh("Cupped paddle ear."+side, verts, faces, ["felt", "ear"], ear_weights(side), ids)
        tube("Ear rolled binding."+side,
             [ear_point(s, .855, TAU*i/72) - Vector((0, .001, 0)) for i in range(72)],
             .0027, "lip", ear_weights(side), sides=6, closed=True)
        # Tiny broken running stitches, not a heavy decorative outline.
        for i in range(28):
            a = TAU*i/28
            tube("Ear stitch."+side,
                 [ear_point(s, .91, a+d)-Vector((0, .003, 0)) for d in (0, .035)],
                 .0009, "ear", ear_weights(side), sides=5)


def build_nappy():
    first_part = len(PARTS)
    n, rings = 64, 7
    verts, faces = [], []
    for j in range(rings):
        t = j/(rings-1)
        for i in range(n):
            a = TAU*i/n
            x = (.235+.028*math.sin(math.pi*t*.8))*math.cos(a)
            y = .018+(.166+.015*math.sin(math.pi*t))*math.sin(a)
            bottom = .385+.132*abs(math.cos(a))**1.3
            z = .665*(1-t)+bottom*t
            # A restrained soft gathered fold under the elastic waistband.
            ruffle = .0024*math.cos(a*12)*math.sin(math.pi*t)**2
            verts.append((x+ruffle*math.cos(a), y+ruffle*math.sin(a), z))
    for j in range(rings-1):
        for i in range(n):
            faces.append((j*n+i, j*n+(i+1)%n, (j+1)*n+(i+1)%n, (j+1)*n+i))
    # Bridge the middle of the lower ring: two genuine leg openings, not a skirt.
    last = (rings-1)*n
    rows = [[last+i for i in (14, 15, 16, 17, 18)]]
    front = [last+i for i in (50, 49, 48, 47, 46)]
    for j in range(1, 9):
        row = []
        t = j/9
        for ia, ib in zip(rows[0], front):
            p = Vector(verts[ia]).lerp(Vector(verts[ib]), t)
            p.z -= .012*math.sin(math.pi*t)
            row.append(len(verts))
            verts.append(p)
        rows.append(row)
    rows.append(front)
    for a, b in zip(rows[:-1], rows[1:]):
        for i in range(4):
            faces.append((a[i], a[i+1], b[i+1], b[i]))
    obj = mesh("Nappy · tailored cloth with two leg openings", verts, faces, "cloth", "hips")
    mod = obj.modifiers.new("Soft tailored cloth", "SUBSURF")
    mod.levels = 1
    apply(obj, mod)
    mod = obj.modifiers.new("Fabric thickness", "SOLIDIFY")
    mod.thickness = .004
    mod.offset = 0
    apply(obj, mod)
    waist = [Vector(verts[i])+Vector((0, 0, .001)) for i in range(n)]
    tube("Green elastic waistband", waist, .010, "green", "hips", sides=10, closed=True)
    for s, side in ((1, "L"), (-1, "R")):
        # Existing lower-ring arcs plus the bridge edge give an exact leg hem.
        indices = (list(range(50, 64))+list(range(0, 15))) if s == 1 else list(range(18, 47))
        hem = [Vector(verts[last+i]) for i in indices]
        edge_col = 0 if s == 1 else 4
        bridge = [Vector(verts[row[edge_col]]) for row in rows[1:-1]]
        if s == 1:
            hem.extend(bridge)
        else:
            hem.extend(reversed(bridge))
        tube("Green leg binding."+side, hem, .006, "green", "hips", sides=8, closed=True)
        cx, zc = s*.176, .627
        def tab_y(x, z):
            return .018-.180*math.sqrt(max(.05, 1-(x/.267)**2))-.014
        oval_patch("Nappy fastening tab."+side, (cx, zc), (.063, .023),
                   tab_y, "cloth", "hips", sectors=32, rings=5)
        border = []
        for a in np.linspace(0, TAU, 48, endpoint=False):
            x, z = cx+.063*math.cos(a), zc+.023*math.sin(a)
            border.append((x, tab_y(x, z)-.001, z))
        tube("Tab green binding."+side, border, .0045, "green", "hips", closed=True)
        x = s*.153
        ellipsoid("Nappy green button."+side, (x, tab_y(x, zc)-.007, zc),
                  (.014, .006, .014), "green_dark", "hips", segments=24, rings=10)
        for dx in (-.004, .004):
            ellipsoid("Button stitch", (x+dx, tab_y(x, zc)-.013, zc),
                      (.0015, .001, .003), "green", "hips", segments=8, rings=6)
    def cloth_weights(p):
        side = "L" if p.x > 0 else "R"
        follow = (1-smooth(.49, .625, p.z))*smooth(.025, .090, abs(p.x))
        return {"hips": 1-follow, "thigh."+side: follow}
    for obj in PARTS[first_part:]:
        assign_weights(obj, cloth_weights)


def build_rig():
    data = bpy.data.armatures.new("Fantus editable IK skeleton")
    arm = bpy.data.objects.new("FantusRig", data)
    bpy.context.collection.objects.link(arm)
    active(arm)
    arm.show_in_front = True
    data.display_type = "OCTAHEDRAL"
    bpy.ops.object.mode_set(mode="EDIT")
    def bone(name, h, t, parent=None, deform=True):
        b = data.edit_bones.new(name)
        b.head, b.tail, b.use_deform = h, t, deform
        if parent:
            b.parent = data.edit_bones[parent]
        return b
    bone("ROOT", (0, 0, 0), (0, 0, .15), deform=False)
    bone("hips", (0, 0, .55), (0, 0, .76), "ROOT")
    bone("chest", (0, 0, .76), (0, .02, 1.015), "hips")
    bone("head", (0, .02, 1.015), (0, .02, 1.53), "chest")
    bone("trunk.01", trunk_point(.02), trunk_point(.5), "head")
    bone("trunk.02", trunk_point(.5), trunk_point(1), "trunk.01")
    for s, side in ((1, "L"), (-1, "R")):
        bone("upper_arm."+side, (s*.175, .012, .945), (s*.30, -.006, .70), "chest")
        bone("forearm."+side, (s*.30, -.006, .70), (s*.37, -.04, .43), "upper_arm."+side)
        bone("hand."+side, (s*.37, -.04, .43), (s*.39, -.06, .34), "forearm."+side)
        bone("thigh."+side, (s*.135, 0, .55), (s*.155, -.035, .325), "hips")
        bone("shin."+side, (s*.155, -.035, .325), (s*.17, 0, .115), "thigh."+side)
        bone("foot."+side, (s*.17, 0, .115), (s*.17, -.17, .115), "shin."+side)
        bone("ear_base."+side, (s*.30, .035, 1.23), (s*.49, .035, 1.34), "head")
        bone("ear_tip."+side, (s*.49, .035, 1.34), (s*.66, .045, 1.51), "ear_base."+side)
        bone("CTRL_foot."+side, (s*.17, 0, .115), (s*.17, -.17, .115), "ROOT", False)
        bone("CTRL_knee."+side, (s*.17, -.60, .32), (s*.17, -.60, .40), "ROOT", False)
    bpy.ops.object.mode_set(mode="OBJECT")
    controls = data.collections.new("Animator · feet and poles")
    deform = data.collections.new("Deformation · weighted surface")
    for b in data.bones:
        (deform if b.use_deform else controls).assign(b)
    for pb in arm.pose.bones:
        pb.rotation_mode = "XYZ"
        pb.ik_stretch = 0
        pb.bone.color.palette = "THEME04" if pb.name.startswith("CTRL") else "THEME03"
    for side in ("L", "R"):
        ik = arm.pose.bones["shin."+side].constraints.new("IK")
        ik.name = "Two-bone planted-foot IK"
        ik.target, ik.subtarget = arm, "CTRL_foot."+side
        ik.pole_target, ik.pole_subtarget = arm, "CTRL_knee."+side
        ik.chain_count, ik.use_stretch, ik.iterations = 2, False, 512
        # Select the pole angle which reproduces the forward-pointing rest knee.
        best = None
        rest = data.bones["shin."+side].head_local
        for angle in (0, math.pi/2, -math.pi/2, math.pi):
            ik.pole_angle = angle
            bpy.context.view_layer.update()
            error = (arm.pose.bones["shin."+side].head-rest).length
            if best is None or error < best[0]:
                best = error, angle
        ik.pole_angle = best[1]
        rot = arm.pose.bones["foot."+side].constraints.new("COPY_ROTATION")
        rot.name = "Keep soft sole level in world space"
        rot.target, rot.subtarget = arm, "CTRL_foot."+side
        rot.owner_space = rot.target_space = "WORLD"
        # Standard independent foot control: contact stays exact even within the
        # IK solver's convergence tolerance. We separately verify the shin tail.
        loc = arm.pose.bones["foot."+side].constraints.new("COPY_LOCATION")
        loc.name = "Exact sole contact at the IK target"
        loc.target, loc.subtarget = arm, "CTRL_foot."+side
        loc.owner_space = loc.target_space = "WORLD"
    bpy.context.view_layer.update()
    for obj in PARTS:
        obj.parent = arm
        mod = obj.modifiers.new("Fantus weighted linear skin", "ARMATURE")
        mod.object = arm
        mod.use_deform_preserve_volume = False
    arm["Walk stride (figure units)"] = STRIDE
    arm["Walk cycle samples"] = FRAMES
    arm["Rig notes"] = "Move CTRL_foot.L/R; knees track CTRL_knee.L/R. Foot rotations are world-space. No IK stretch."
    return arm


def fcurves(action):
    for layer in action.layers:
        for strip in layer.strips:
            for bag in strip.channelbags:
                yield from bag.fcurves


def world_location(arm, name, delta):
    arm.pose.bones[name].location = arm.data.bones[name].matrix_local.to_3x3().inverted() @ Vector(delta)


def foot_path(phase):
    q = phase % 1.0
    half = STRIDE*STANCE*.5
    if q <= STANCE:
        return -half+STRIDE*q, 0.0
    t = (q-STANCE)/(1-STANCE)
    # Hermite return matches stance velocity at both contacts (no foot pop).
    h00, h10 = 2*t**3-3*t*t+1, t**3-2*t*t+t
    h01, h11 = -2*t**3+3*t*t, t**3-t*t
    y = h00*half+h10*STRIDE*(1-STANCE)-h01*half+h11*STRIDE*(1-STANCE)
    lift = .078*math.sin(math.pi*t)**2
    return y, lift


def animate(arm):
    arm.animation_data_create()
    idle = bpy.data.actions.new("Idle · grounded rest")
    idle.use_fake_user = True
    walk = bpy.data.actions.new("Walk · foot IK · 0.80 units")
    walk.use_fake_user = True
    for action in (idle, walk):
        arm.animation_data.action = action
        for frame in (range(1, FRAMES+2) if action == walk else (1, FRAMES+1)):
            phase = (frame-1)/FRAMES
            wave = math.sin(TAU*phase)
            for pb in arm.pose.bones:
                pb.location = (0, 0, 0)
                pb.rotation_euler = (0, 0, 0)
                pb.scale = (1, 1, 1)
            if action == walk:
                world_location(arm, "hips", (.020*math.sin(TAU*(phase-.10)), 0,
                                              -.075+.007*math.cos(TAU*phase*2)))
                arm.pose.bones["hips"].rotation_euler = (0, 0, math.radians(2.0)*wave)
                arm.pose.bones["chest"].rotation_euler = (math.radians(1.8), 0, -math.radians(2.6)*wave)
                arm.pose.bones["head"].rotation_euler = (-math.radians(1.2),
                                                        math.radians(1.3)*math.sin(TAU*phase-.4),
                                                        math.radians(.8)*wave)
                for s, side, offset in ((1, "L", 0), (-1, "R", .5)):
                    y, z = foot_path(phase+offset)
                    world_location(arm, "CTRL_foot."+side, (0, y, z))
                    arm.pose.bones["upper_arm."+side].rotation_euler.x = math.radians(16)*math.cos(TAU*(phase+offset))
                    arm.pose.bones["forearm."+side].rotation_euler.x = math.radians(-5-4*math.sin(TAU*(phase+offset)))
                    arm.pose.bones["hand."+side].rotation_euler.z = s*math.radians(3)*math.sin(TAU*phase-.5)
                    arm.pose.bones["ear_base."+side].rotation_euler.y = s*math.radians(3.2)*math.sin(TAU*phase-.7)
                    arm.pose.bones["ear_tip."+side].rotation_euler.z = math.radians(3)*math.sin(TAU*phase-1.1)
                arm.pose.bones["trunk.01"].rotation_euler.x = math.radians(2)*math.sin(TAU*phase-.5)
                arm.pose.bones["trunk.02"].rotation_euler.z = math.radians(3)*math.sin(TAU*phase-1)
            else:
                world_location(arm, "hips", (0, 0, -.014))
            for pb in arm.pose.bones:
                pb.keyframe_insert("location", frame=frame, group=pb.name)
                pb.keyframe_insert("rotation_euler", frame=frame, group=pb.name)
        for fc in fcurves(action):
            for key in fc.keyframe_points:
                key.interpolation = "LINEAR"
            fc.modifiers.new("CYCLES")
    arm.animation_data.action = idle
    bpy.context.scene.frame_set(1)
    return idle, walk


def join_skin(arm):
    print("PART_VERTICES", sorted([(o.name, len(o.data.vertices)) for o in PARTS], key=lambda x: -x[1])[:16], flush=True)
    # Keep the carefully tessellated face panels intact; collapse only less
    # sensitive cloth/body surfaces. In particular, don't turn irises into hexagons.
    protected = ("Mask", "Eye", "Wide oval", "Hazel", "Pupil", "Upper felt", "Lid")
    for part in PARTS:
        if not part.name.startswith(protected) and len(part.data.vertices) > 100:
            active(part)
            mod = part.modifiers.new("Runtime surface budget", "DECIMATE")
            mod.ratio = .70
            mod.delimit = {"MATERIAL"}
            bpy.ops.object.modifier_move_up(modifier=mod.name)
            apply(part, mod)
    bpy.ops.object.select_all(action="DESELECT")
    for obj in PARTS:
        obj.select_set(True)
    bpy.context.view_layer.objects.active = PARTS[0]
    bpy.ops.object.join()
    obj = bpy.context.object
    obj.name = "Fantus · felt, face and tailored cloth"
    obj.data.name = "Fantus editable surface"
    # Collapse can move an edge a few microns through the contact plane.
    # Preserve the explicitly flattened sole after final mesh reduction.
    for v in obj.data.vertices:
        v.co.z = max(0.0, v.co.z)
    # The only live modifier is skinning; .fmesh exports this undeformed base.
    assert len(obj.modifiers) == 1 and obj.modifiers[0].type == "ARMATURE"
    obj.data.validate()
    obj.data.update()
    obj.data.calc_loop_triangles()
    # A conventional UV map is useful to artists; all shipped shading is material based.
    active(obj)
    bpy.ops.object.mode_set(mode="EDIT")
    bpy.ops.mesh.select_all(action="SELECT")
    bpy.ops.uv.smart_project(angle_limit=math.radians(70), island_margin=.008)
    bpy.ops.object.mode_set(mode="OBJECT")
    return obj


def skin_matrices(arm, names):
    deps = bpy.context.evaluated_depsgraph_get()
    evaluated = arm.evaluated_get(deps)
    return [C @ evaluated.matrix_world @ evaluated.pose.bones[name].matrix
            @ arm.data.bones[name].matrix_local.inverted() @ C.inverted() for name in names]


def export_fmesh(obj, arm, idle, walk):
    names = [b.name for b in arm.data.bones if b.use_deform]
    bone_ids = {name: i for i, name in enumerate(names)}
    data = obj.data
    data.calc_loop_triangles()
    records, indices, lookup, source = [], [], {}, []
    for tri in data.loop_triangles:
        for vi in tri.vertices:
            key = (vi, tri.material_index)
            if key not in lookup:
                lookup[key] = len(records)
                source.append(vi)
                v = data.vertices[vi]
                pos, normal = C @ v.co, C.to_3x3() @ v.normal
                col = data.materials[tri.material_index].diffuse_color[:3]
                pairs = [(bone_ids[obj.vertex_groups[g.group].name], g.weight)
                         for g in v.groups if obj.vertex_groups[g.group].name in bone_ids and g.weight > 1e-7]
                pairs.sort(key=lambda x: -x[1])
                assert 0 < len(pairs) <= 4
                total = sum(w for _, w in pairs)
                ids = [i for i, _ in pairs]+[0]*(4-len(pairs))
                weights = [w/total for _, w in pairs]+[0]*(4-len(pairs))
                records.append((*pos, *normal, *col, *ids, *weights))
            indices.append(lookup[key])
    poses = []
    arm.animation_data.action = idle
    bpy.context.scene.frame_set(1)
    poses.append(skin_matrices(arm, names))
    arm.animation_data.action = walk
    for frame in range(1, FRAMES+1):
        bpy.context.scene.frame_set(frame)
        poses.append(skin_matrices(arm, names))
    path = HERE / "fantus.fmesh"
    with path.open("wb") as f:
        f.write(struct.pack("<4s4If", b"FNT1", len(records), len(indices), len(names), FRAMES, STRIDE))
        for record in records:
            f.write(struct.pack("<9f4I4f", *record))
        f.write(struct.pack("<"+str(len(indices))+"I", *indices))
        for pose in poses:
            for mat in pose:
                f.write(struct.pack("<12f", *(mat[r][c] for r in range(3) for c in range(4))))
    return names, np.array(source, dtype=np.int32)


def verify(obj, arm, idle, walk, names, source):
    # Parse the actual on-disk bytes, independently of the writer's arrays.
    payload = (HERE/"fantus.fmesh").read_bytes()
    magic, nv, ni, nb, nf, stride = struct.unpack_from("<4s4If", payload)
    assert magic == b"FNT1" and (nb, nf) == (len(names), FRAMES)
    dt = np.dtype([("p", "<f4", 3), ("n", "<f4", 3), ("c", "<f4", 3),
                   ("b", "<u4", 4), ("w", "<f4", 4)])
    assert dt.itemsize == 68
    vertices = np.frombuffer(payload, dtype=dt, count=nv, offset=24)
    indices = np.frombuffer(payload, dtype="<u4", count=ni, offset=24+nv*68)
    offset = 24+nv*68+ni*4
    mats = np.frombuffer(payload, dtype="<f4", offset=offset).reshape(nf+1, nb, 3, 4)
    assert len(payload) == offset+(nf+1)*nb*48
    assert indices.max() < nv and vertices["b"].max() < nb
    assert np.isfinite(vertices["p"]).all() and np.isfinite(mats).all()
    assert np.max(abs(vertices["w"].sum(axis=1)-1)) < 2e-7
    assert np.max(abs(np.linalg.norm(vertices["n"], axis=1)-1)) < 1e-5
    xyz1 = np.column_stack([vertices["p"], np.ones(nv)])
    checks = []
    wrong_pose_errors = []
    for action, frame, pose_index in [(idle, 1, 0)] + [(walk, f, f) for f in (4, 11, 19, 28, 37, 45)]:
        arm.animation_data.action = action
        bpy.context.scene.frame_set(frame)
        deps = bpy.context.evaluated_depsgraph_get()
        evaluated = obj.evaluated_get(deps)
        actual_mesh = evaluated.to_mesh()
        actual = np.array([C @ evaluated.matrix_world @ v.co for v in actual_mesh.vertices])[source]
        predicted = np.einsum("nijk,nk,ni->nj", mats[pose_index][vertices["b"]], xyz1, vertices["w"])
        error = np.linalg.norm(actual-predicted, axis=1)
        checks.append({"action": "idle" if action == idle else "walk", "frame": frame,
                       "max_position_error": float(error.max()), "rms_position_error": float(np.sqrt(np.mean(error**2)))})
        assert error.max() < 2e-6, checks[-1]
        wrong = np.einsum("nijk,nk,ni->nj", mats[0][vertices["b"]], xyz1, vertices["w"])
        wrong_pose_errors.append(float(np.linalg.norm(actual-wrong, axis=1).max()))
        evaluated.to_mesh_clear()
    assert max(wrong_pose_errors) > .07, "Negative control failed to detect an idle matrix substituted for walking"
    arm.animation_data.action = walk
    max_target, max_ground, max_level, max_slide, min_floor = 0, 0, 0, 0, 1
    max_shear, max_chain = 0, 0
    stance_samples, swing_heights = 0, []
    for step in range(FRAMES*4):
        frame = 1+step/4
        bpy.context.scene.frame_set(int(frame), subframe=frame%1)
        phase = (frame-1)/FRAMES
        for side, offset_phase in (("L", 0), ("R", .5)):
            foot = arm.pose.bones["foot."+side]
            target = arm.pose.bones["CTRL_foot."+side]
            max_target = max(max_target, (foot.head-target.head).length)
            max_chain = max(max_chain, (arm.pose.bones["shin."+side].tail-target.head).length)
            # The undeformed sole is z=0; apply the full solved foot skin matrix.
            sm = foot.matrix @ arm.data.bones[foot.name].matrix_local.inverted()
            sole = sm @ Vector((arm.data.bones[foot.name].head_local.x, -.055, 0))
            min_floor = min(min_floor, sole.z)
            normal = sm.to_3x3() @ Vector((0, 0, 1))
            max_level = max(max_level, (normal-Vector((0, 0, 1))).length)
            local_phase = (phase+offset_phase)%1
            if local_phase < STANCE-1/FRAMES:
                stance_samples += 1
                max_ground = max(max_ground, abs(sole.z))
                # Root travels -Y with STRIDE/cycle: this cancels planted-foot velocity.
                expected = -STRIDE*STANCE*.5+STRIDE*local_phase
                max_slide = max(max_slide, abs((foot.head.y-STRIDE*local_phase)-(-STRIDE*STANCE*.5)))
                assert abs(target.head.y-expected) < 1e-6
            else:
                swing_heights.append(sole.z)
        for b in arm.pose.bones:
            m = np.array(b.matrix.to_3x3())
            max_shear = max(max_shear, float(np.max(abs(m.T@m-np.eye(3)))))
    print("IK_DIAGNOSTICS", max_target, max_ground, max_slide, max_level, max_shear, min_floor, "shin", max_chain, flush=True)
    assert max_target < 3e-5 and max_ground < 3e-5 and max_slide < 3e-5, (max_target, max_ground, max_slide)
    assert max_chain < 5e-5, ("IK shin-tail convergence error", max_chain)
    assert max_level < 2e-5 and max_shear < 2e-5
    assert min_floor > -3e-5 and max(swing_heights) > .07
    # A loop endpoint is stored in Blender/glTF, but explicitly excluded from FNT1.
    bpy.context.scene.frame_set(1)
    first = np.array(skin_matrices(arm, names))
    bpy.context.scene.frame_set(FRAMES+1)
    loop_error = float(np.max(abs(np.array(skin_matrices(arm, names))-first)))
    assert loop_error < 1e-6
    bounds = np.array([C @ v.co for v in obj.data.vertices])
    report = {"format": "FNT1", "runtime_vertices": nv, "editable_vertices": len(obj.data.vertices),
              "indices": ni, "triangles": ni//3, "deform_bones": nb,
              "total_bones": len(arm.data.bones), "bone_order": names,
              "materials": len(obj.data.materials), "walk_frames": nf, "fps": 60,
              "stride_length": float(stride), "stance_fraction": STANCE,
              "bind_bounds_xyz": [bounds.min(axis=0).tolist(), bounds.max(axis=0).tolist()],
              "skinning_checks": checks, "negative_control_wrong_pose_max_error": max(wrong_pose_errors),
              "ik_foot_target_max_error": max_target, "stance_ground_max_error": max_ground,
              "ik_shin_tail_max_error": max_chain,
              "stance_world_slide_max_error": max_slide, "foot_level_normal_max_error": max_level,
              "minimum_sole_height": min_floor, "max_swing_height": max(swing_heights),
              "stance_subframe_samples": stance_samples, "bone_orthonormality_max_error": max_shear,
              "loop_matrix_max_error": loop_error, "file_bytes": len(payload)}
    (HERE/"verification.json").write_text(json.dumps(report, indent=2)+"\n")
    print("FANTUS_VERIFICATION " + json.dumps(report), flush=True)
    return report


def export_glb(obj, arm, idle, walk):
    # Blender's glTF exporter samples final evaluated joint transforms, including IK.
    active(arm)
    obj.select_set(True)
    bpy.ops.export_scene.gltf(filepath=str(HERE/"fantus.glb"), export_format="GLB",
                              use_selection=True, export_animations=True,
                              export_animation_mode="ACTIONS", export_force_sampling=True,
                              export_anim_slide_to_zero=True,
                              export_optimize_animation_size=False,
                              export_frame_range=False, export_frame_step=1,
                              export_def_bones=True, export_yup=True,
                              export_skins=True, export_materials="EXPORT",
                              export_cameras=False, export_lights=False)
    # Inspect the actual GLB container and require a skin plus baked channels.
    raw = (HERE/"fantus.glb").read_bytes()
    json_len = struct.unpack_from("<I", raw, 12)[0]
    gltf = json.loads(raw[20:20+json_len])
    assert len(gltf["skins"]) == 1 and len(gltf.get("animations", [])) == 2
    binary = raw[20+json_len+8:]
    # The exporter writes scale tracks even for unscaled rigs, including IK
    # roundoff around 1.0. Assert they are identity, then omit those redundant
    # tracks so consumers cannot interpret numerical noise as authored scaling.
    for animation in gltf["animations"]:
        assert len(animation["channels"]) >= 20
        channels = []
        for channel in animation["channels"]:
            if channel["target"]["path"] == "scale":
                accessor = gltf["accessors"][animation["samplers"][channel["sampler"]]["output"]]
                view = gltf["bufferViews"][accessor["bufferView"]]
                values = np.frombuffer(binary, dtype="<f4", count=accessor["count"]*3,
                                       offset=view.get("byteOffset", 0)+accessor.get("byteOffset", 0))
                assert np.max(abs(values-1)) < 1e-5, "Nonidentity animated scale"
            else:
                channels.append(channel)
        animation["channels"] = channels
    for node in gltf["nodes"]:
        if "scale" in node:
            assert max(abs(s-1) for s in node["scale"]) < 1e-5
            del node["scale"]
    encoded = json.dumps(gltf, separators=(",", ":")).encode()
    encoded += b" "*((-len(encoded))%4)
    with (HERE/"fantus.glb").open("wb") as f:
        f.write(struct.pack("<4sII", b"glTF", 2, 12+8+len(encoded)+8+len(binary)))
        f.write(struct.pack("<I4s", len(encoded), b"JSON"))
        f.write(encoded)
        f.write(struct.pack("<I4s", len(binary), b"BIN\0"))
        f.write(binary)
    print("GLB_ACTIONS", [(a["name"], len(a["channels"])) for a in gltf["animations"]], flush=True)
    verify_glb(arm, idle, walk)


def verify_glb(arm, idle, walk):
    """Evaluate the shipped glTF hierarchy and baked tracks, not Blender's exporter."""
    raw = (HERE/"fantus.glb").read_bytes()
    length = struct.unpack_from("<I", raw, 12)[0]
    gltf = json.loads(raw[20:20+length])
    binary = raw[28+length:]
    def accessor(index):
        a = gltf["accessors"][index]
        v = gltf["bufferViews"][a["bufferView"]]
        width = {"SCALAR": 1, "VEC3": 3, "VEC4": 4, "MAT4": 16}[a["type"]]
        assert a["componentType"] == 5126 and "byteStride" not in v
        return np.frombuffer(binary, dtype="<f4", count=a["count"]*width,
                             offset=v.get("byteOffset", 0)+a.get("byteOffset", 0)).reshape(a["count"], width)
    parents = {child: i for i, n in enumerate(gltf["nodes"]) for child in n.get("children", [])}
    skin = gltf["skins"][0]
    inverse_bind = accessor(skin["inverseBindMatrices"])
    names = [gltf["nodes"][j]["name"] for j in skin["joints"]]
    errors = []
    for animation, action in zip(gltf["animations"], (idle, walk)):
        assert animation["name"] == action.name
        arm.animation_data.action = action
        for frame in (1, 4, 11, 19, 28, 37, 45, 49):
            time = (frame-1)/60
            bpy.context.scene.frame_set(frame)
            nodes = [dict(n) for n in gltf["nodes"]]
            for channel in animation["channels"]:
                sampler = animation["samplers"][channel["sampler"]]
                times = accessor(sampler["input"])[:, 0]
                values = accessor(sampler["output"])
                k = max(0, min(len(times)-2, int(np.searchsorted(times, time))-1))
                t = max(0, min(1, (time-times[k])/(times[k+1]-times[k])))
                # Sample frames coincide with baked rotation keys; linear
                # interpolation is only needed for optimized constant tracks.
                value = values[k]*(1-t)+values[k+1]*t
                nodes[channel["target"]["node"]][channel["target"]["path"]] = value
            cache = {}
            def world(i):
                if i not in cache:
                    n = nodes[i]
                    q = n.get("rotation", (0, 0, 0, 1))
                    rot = Quaternion((q[3], q[0], q[1], q[2])).normalized()
                    local = Matrix.LocRotScale(Vector(n.get("translation", (0, 0, 0))), rot,
                                               Vector(n.get("scale", (1, 1, 1))))
                    cache[i] = world(parents[i]) @ local if i in parents else local
                return cache[i]
            expected = skin_matrices(arm, names)
            error = 0
            for j, node in enumerate(skin["joints"]):
                ib = Matrix(inverse_bind[j].reshape(4, 4).T.tolist())
                bone_error = float(np.max(abs(np.array(world(node)@ib)-np.array(expected[j]))))
                if bone_error > 2e-5:
                    print("GLB_MISMATCH", frame, names[j], bone_error, flush=True)
                error = max(error, bone_error)
            assert error < 2e-5, (action.name, frame, "GLB evaluated skin mismatch", error)
            errors.append(error)
    report = json.loads((HERE/"verification.json").read_text())
    report["glb_baked_skin_matrix_max_error"] = max(errors)
    report["glb_baked_skin_pose_checks"] = len(errors)
    (HERE/"verification.json").write_text(json.dumps(report, indent=2)+"\n")
    print("GLB_SKIN_VERIFIED", max(errors), "in", len(errors), "poses", flush=True)


def stage():
    scene = bpy.context.scene
    scene.render.engine = "CYCLES"
    scene.cycles.samples = 48
    scene.cycles.use_denoising = True
    scene.render.resolution_x, scene.render.resolution_y = 1000, 1100
    scene.render.resolution_percentage = 100
    scene.render.fps = 60
    scene.frame_start, scene.frame_end = 1, FRAMES
    scene.view_settings.view_transform = "AgX"
    if scene.world is None:
        scene.world = bpy.data.worlds.new("Fantus studio world")
    scene.world.use_nodes = True
    scene.world.node_tree.nodes["Background"].inputs["Color"].default_value = (.31, .36, .40, 1)
    scene.world.node_tree.nodes["Background"].inputs["Strength"].default_value = .35
    collection = bpy.data.collections.new("Studio · not exported")
    scene.collection.children.link(collection)
    def move_to_stage(obj):
        for coll in list(obj.users_collection):
            coll.objects.unlink(obj)
        collection.objects.link(obj)
    bpy.ops.mesh.primitive_plane_add(size=200)
    floor = bpy.context.object
    floor.name = "Studio floor"
    mat = bpy.data.materials.new("Studio warm grey")
    mat.diffuse_color = (.24, .275, .28, 1)
    mat.use_nodes = True
    mat.node_tree.nodes["Principled BSDF"].inputs["Base Color"].default_value = mat.diffuse_color
    mat.node_tree.nodes["Principled BSDF"].inputs["Roughness"].default_value = .94
    floor.data.materials.append(mat)
    floor.location.z = -.003
    move_to_stage(floor)
    def aim(obj, target):
        obj.rotation_euler = (Vector(target)-obj.location).to_track_quat("-Z", "Y").to_euler()
    for name, loc, power, size, col in [
            ("Key softbox", (-2.2, -3, 3.7), 290, 2.8, (1, .89, .78)),
            ("Cool fill", (2.8, -1.4, 2.0), 145, 2.5, (.78, .86, 1)),
            ("Felt rim", (0, 2, 3), 240, 2.0, (1, .83, .73))]:
        data = bpy.data.lights.new(name, "AREA")
        data.energy, data.shape, data.size, data.color = power, "DISK", size, col
        obj = bpy.data.objects.new(name, data)
        collection.objects.link(obj)
        obj.location = loc
        aim(obj, (0, 0, .8))
    cam = bpy.data.objects.new("Fantus portrait camera", bpy.data.cameras.new("Fantus portrait camera"))
    collection.objects.link(cam)
    cam.data.type, cam.data.ortho_scale = "ORTHO", 2.05
    scene.camera = cam
    cam.location = (2.45, -4.8, 2.25)
    aim(cam, (0, -.02, .81))
    return cam, aim


def render(arm, idle, walk, cam, aim, preview=False):
    ART.mkdir(parents=True, exist_ok=True)
    scene = bpy.context.scene
    shots = [("fantus-front", idle, 1, (0, -5, 1.25)),
             ("fantus-three-quarter", idle, 1, (2.45, -4.8, 2.25)),
             ("fantus-walk", walk, 38, (2.7, -4.4, 1.85))]
    for name, action, frame, loc in shots:
        arm.animation_data.action = action
        scene.frame_set(frame)
        cam.location = loc
        aim(cam, (0, -.02, .80))
        scene.render.filepath = str(ART/(name+".png"))
        bpy.ops.render.render(write_still=True)
    if preview:
        scene.cycles.samples = 16
        scene.render.resolution_x = scene.render.resolution_y = 540
        cam.data.ortho_scale = 1.95
        cam.location = (2.5, -4.8, 1.85)
        aim(cam, (0, -.025, .80))
        arm.animation_data.action = walk
        for frame in range(1, FRAMES+1, 2):
            scene.frame_set(frame)
            scene.render.filepath = str(ART/f"fantus-preview-{frame:02d}.png")
            bpy.ops.render.render(write_still=True)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--render", action="store_true")
    parser.add_argument("--preview", action="store_true")
    args = parser.parse_args(sys.argv[sys.argv.index("--")+1:] if "--" in sys.argv else [])
    bpy.ops.wm.read_factory_settings(use_empty=True)
    bpy.context.scene.render.fps = 60
    materials()
    build_body()
    build_head()
    build_ears()
    build_nappy()
    arm = build_rig()
    idle, walk = animate(arm)
    obj = join_skin(arm)
    names, source = export_fmesh(obj, arm, idle, walk)
    verify(obj, arm, idle, walk, names, source)
    export_glb(obj, arm, idle, walk)
    cam, aim = stage()
    arm.animation_data.action = idle
    bpy.context.scene.frame_set(1)
    active(arm)
    # Open on the model, not on the studio: camera and lights remain editable.
    for screen in bpy.data.screens:
        for area in screen.areas:
            if area.type == "VIEW_3D":
                area.spaces.active.region_3d.view_distance = 2.8
                area.spaces.active.region_3d.view_location = (0, 0, .8)
                area.spaces.active.shading.type = "MATERIAL"
    bpy.ops.wm.save_as_mainfile(filepath=str(HERE/"fantus.blend"))
    if args.render or args.preview:
        render(arm, idle, walk, cam, aim, args.preview)


if __name__ == "__main__":
    main()
