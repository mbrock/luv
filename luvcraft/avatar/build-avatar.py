"""Build Claude's luvcraft avatar: mesh, materials, rig, animations, export.

Run inside Blender:

    blender --background --python luvcraft/avatar/build-avatar.py

or from a live session (this is how it is normally iterated on):

    exec(compile(open(PATH).read(), PATH, 'exec'))

The script is the source of truth; claude-avatar.blend and claude-avatar.glb
are outputs and can always be regenerated from here.

Construction notes
------------------
Every limb joint (shoulder, elbow, wrist, hip, knee, ankle) is a sphere whose
radius matches the limb's half-width, so segments always overlap and the
silhouette never breaks open when the rig bends.

The torso is a cream shirt core wrapped in separate coat panels that leave a
gap down the front, so the shirt is *seen through* the coat rather than pasted
on top of it.  The coat continues below the belt as a flared skirt; those
vertices are tagged and hard-weighted to the hips so the skirt swings as one
piece instead of being torn apart by the thighs.

Arms are modelled in an A-pose (built vertically, then rotated outward about
the shoulder) so the hands hang clear of the coat skirt in the bind pose.

Coordinates below are "raw" build units; main() normalises the finished figure
to HEIGHT metres with the feet on z=0.
"""

import math
import os

import bmesh
import bpy
from mathutils import Matrix, Vector

HEIGHT = 1.85          # final height in metres
ARM_PIVOT_X = 0.30     # shoulder pivot
ARM_PIVOT_Z = 1.51
ARM_SPLAY = 9.0        # degrees the arms hang away from the body

OUT_DIR = os.path.dirname(os.path.abspath(__file__))

# ---------------------------------------------------------------- palette --

PALETTE = {
    "coat":     ("#C0562F", 0.62, 0.0),   # rust frock coat
    "coat_dk":  ("#8E3A1E", 0.60, 0.0),   # lapels, cuffs, collar
    "shirt":    ("#F2E4C9", 0.70, 0.0),   # warm cream
    "cravat":   ("#FBF3E4", 0.65, 0.0),
    "skin":     ("#E8B48C", 0.55, 0.0),
    "hair":     ("#3A2A22", 0.72, 0.0),
    "trousers": ("#2E3446", 0.75, 0.0),
    "boot":     ("#4A3428", 0.45, 0.0),
    "leather":  ("#5A3E2B", 0.50, 0.0),
    "brass":    ("#D9A441", 0.28, 0.0),
    "spark":    ("#FFC24A", 0.35, 5.0),   # emissive emblem
    "eye":      ("#221A16", 0.30, 0.0),
    "white":    ("#FFFFFF", 0.25, 0.0),
}


def _srgb_to_linear(c):
    return c / 12.92 if c <= 0.04045 else ((c + 0.055) / 1.055) ** 2.4


def _hex_to_linear(h):
    h = h.lstrip("#")
    return tuple(_srgb_to_linear(int(h[i:i + 2], 16) / 255.0) for i in (0, 2, 4))


def build_materials():
    mats = {}
    for name, (hexcol, rough, emit) in PALETTE.items():
        key = f"claude.{name}"
        m = bpy.data.materials.get(key) or bpy.data.materials.new(key)
        m.use_nodes = True
        bsdf = m.node_tree.nodes["Principled BSDF"]
        rgb = _hex_to_linear(hexcol)
        bsdf.inputs["Base Color"].default_value = (*rgb, 1.0)
        bsdf.inputs["Roughness"].default_value = rough
        if "Metallic" in bsdf.inputs:
            bsdf.inputs["Metallic"].default_value = 0.85 if name == "brass" else 0.0
        if emit:
            bsdf.inputs["Emission Color"].default_value = (*rgb, 1.0)
            bsdf.inputs["Emission Strength"].default_value = emit
        mats[name] = m
    return mats


# ------------------------------------------------------------- primitives --

class Builder:
    """Collects primitive parts so they can be joined into one mesh.

    ``tag`` names a bone: every vertex of the part goes into a ``TAG:<bone>``
    vertex group, which survives the join and lets skin() hard-bind the part to
    that bone.  Automatic weights are good for the big body volumes but drag
    small applied details (a lapel, an emblem, a satchel) toward whichever limb
    happens to be near, so anything applied gets pinned explicitly.
    """

    def __init__(self, mats):
        self.mats = mats
        self.parts = []

    def _finish(self, obj, material, bevel, segments, smooth, tag):
        if tag:
            vg = obj.vertex_groups.new(name=f"TAG:{tag}")
            vg.add(range(len(obj.data.vertices)), 1.0, "REPLACE")
        if bevel:
            mod = obj.modifiers.new("Bevel", "BEVEL")
            mod.width = bevel
            mod.segments = segments
            mod.limit_method = "ANGLE"
            mod.angle_limit = math.radians(30)
        obj.data.materials.append(self.mats[material])
        for poly in obj.data.polygons:
            poly.use_smooth = smooth
        self.parts.append(obj)
        return obj

    def box(self, name, size, loc, material, bevel=0.03, segments=4,
            rot=(0, 0, 0), tag=None):
        bpy.ops.mesh.primitive_cube_add(size=1, location=loc)
        obj = bpy.context.active_object
        obj.name = name
        obj.scale = size
        if any(rot):
            obj.rotation_euler = tuple(math.radians(a) for a in rot)
        bpy.ops.object.transform_apply(location=False, rotation=True, scale=True)
        return self._finish(obj, material, bevel, segments, False, tag)

    def ball(self, name, radius, loc, material, scale=(1, 1, 1), rot=(0, 0, 0),
             tag=None, subdivisions=3):
        # icospheres, not UV spheres: no poles means no dimple artefact where a
        # joint sphere emerges from a limb
        bpy.ops.mesh.primitive_ico_sphere_add(
            radius=radius, location=loc, subdivisions=subdivisions
        )
        obj = bpy.context.active_object
        obj.name = name
        obj.scale = scale
        if any(rot):
            obj.rotation_euler = tuple(math.radians(a) for a in rot)
        bpy.ops.object.transform_apply(location=False, rotation=True, scale=True)
        return self._finish(obj, material, 0, 0, True, tag)

    def tube(self, name, radius, depth, loc, material, rot=(0, 0, 0), tag=None):
        bpy.ops.mesh.primitive_cylinder_add(
            radius=radius, depth=depth, location=loc, vertices=18
        )
        obj = bpy.context.active_object
        obj.name = name
        if any(rot):
            obj.rotation_euler = tuple(math.radians(a) for a in rot)
            bpy.ops.object.transform_apply(location=False, rotation=True, scale=False)
        return self._finish(obj, material, 0.012, 2, True, tag)

    def star(self, name, loc, material, points=4, r_out=0.075, r_in=0.026,
             depth=0.024, tag=None):
        """A flat n-pointed star in the XZ plane, facing -Y."""
        bm = bmesh.new()
        ring = []
        n = points * 2
        for i in range(n):
            ang = math.pi / 2 + i * 2 * math.pi / n
            r = r_out if i % 2 == 0 else r_in
            ring.append(bm.verts.new((math.cos(ang) * r, 0.0, math.sin(ang) * r)))
        face = bm.faces.new(ring)
        res = bmesh.ops.extrude_face_region(bm, geom=[face])
        moved = [e for e in res["geom"] if isinstance(e, bmesh.types.BMVert)]
        bmesh.ops.translate(bm, verts=moved, vec=(0, depth, 0))
        bmesh.ops.recalc_face_normals(bm, faces=bm.faces)
        mesh = bpy.data.meshes.new(name)
        bm.to_mesh(mesh)
        bm.free()
        obj = bpy.data.objects.new(name, mesh)
        bpy.context.collection.objects.link(obj)
        obj.location = loc
        bpy.context.view_layer.objects.active = obj
        return self._finish(obj, material, 0, 0, False, tag)

    def mirror(self, fn):
        """Call fn(sign, suffix) for left and right."""
        for sign, suffix in ((1, "L"), (-1, "R")):
            fn(sign, suffix)

    def mark(self):
        return len(self.parts)

    def since(self, mark):
        return self.parts[mark:]


def rotate_about(objs, pivot, angle_deg, axis="Y"):
    """Rigidly rotate finished parts about a world-space pivot."""
    m = (Matrix.Translation(Vector(pivot))
         @ Matrix.Rotation(math.radians(angle_deg), 4, axis)
         @ Matrix.Translation(-Vector(pivot)))
    for obj in objs:
        obj.matrix_world = m @ obj.matrix_world


def rotate_point(p, pivot, angle_deg, axis="Y"):
    m = (Matrix.Translation(Vector(pivot))
         @ Matrix.Rotation(math.radians(angle_deg), 4, axis)
         @ Matrix.Translation(-Vector(pivot)))
    return m @ Vector(p)


# ------------------------------------------------------------------ build --

def build_head(b):
    # the head sits clear of the collar so a little neck shows
    b.box("head", (0.45, 0.44, 0.42), (0, 0.01, 1.95), "skin", bevel=0.09, segments=6)
    b.mirror(lambda s, side: b.box(
        f"ear.{side}", (0.05, 0.10, 0.115), (s * 0.23, 0.02, 1.935), "skin",
        bevel=0.02, segments=3, tag="head"))

    # eyes: rounded lenses standing proud of the face, each with a catchlight
    # sitting on the lens surface rather than buried inside it
    b.mirror(lambda s, side: b.ball(
        f"eye.{side}", 0.055, (s * 0.108, -0.190, 1.945), "eye",
        scale=(1, 0.5, 1.2), tag="head"))
    b.mirror(lambda s, side: b.ball(
        f"glint.{side}", 0.013, (s * 0.130, -0.218, 1.972), "white",
        subdivisions=2, tag="head"))
    # brows well above the eyes, angled up-and-out: curious rather than cross
    b.mirror(lambda s, side: b.box(
        f"brow.{side}", (0.10, 0.028, 0.02), (s * 0.11, -0.202, 2.025), "hair",
        bevel=0.007, segments=2, rot=(0, s * 9, 0), tag="head"))
    b.box("nose", (0.058, 0.062, 0.07), (0, -0.212, 1.90), "skin",
          bevel=0.018, segments=3, tag="head")
    # a wide, thin closed smile: centre bar plus two lifted corners
    b.box("mouth_c", (0.072, 0.02, 0.015), (0, -0.200, 1.826), "coat_dk",
          bevel=0.005, segments=2, tag="head")
    b.mirror(lambda s, side: b.box(
        f"mouth.{side}", (0.045, 0.02, 0.015), (s * 0.050, -0.195, 1.840), "coat_dk",
        bevel=0.005, segments=2, rot=(0, s * 26, 0), tag="head"))

    # hair: a hairline that frames the face, leaving cheeks and ears visible
    b.box("hair_cap", (0.47, 0.44, 0.17), (0, 0.015, 2.135), "hair",
          bevel=0.06, segments=5, tag="head")
    b.box("hair_back", (0.46, 0.18, 0.40), (0, 0.185, 1.96), "hair",
          bevel=0.05, segments=4, tag="head")
    b.mirror(lambda s, side: b.box(
        f"hair_temple.{side}", (0.075, 0.30, 0.22), (s * 0.222, 0.06, 2.07), "hair",
        bevel=0.032, segments=3, tag="head"))
    b.box("hair_fringe", (0.41, 0.14, 0.13), (0, -0.155, 2.115), "hair",
          bevel=0.035, segments=3, tag="head")
    b.mirror(lambda s, side: b.box(
        f"hair_sweep.{side}", (0.19, 0.12, 0.10), (s * 0.125, -0.15, 2.145), "hair",
        bevel=0.03, segments=3, rot=(0, s * 14, 0), tag="head"))
    b.box("hair_tuft_a", (0.15, 0.17, 0.11), (0.07, 0.04, 2.205), "hair",
          bevel=0.04, segments=3, rot=(7, 0, 8), tag="head")
    b.box("hair_tuft_b", (0.10, 0.12, 0.09), (-0.10, 0.10, 2.19), "hair",
          bevel=0.032, segments=3, rot=(-6, 0, -10), tag="head")


def build_torso(b):
    # neck, visible between collar and chin
    b.tube("neck", 0.095, 0.24, (0, 0.01, 1.70), "skin")

    # The torso shell is pinned to the chest bone rather than automatically
    # weighted: the coat sits close to the shoulders, and any arm influence
    # there tears the garment open when an arm lifts.  Pinning keeps the
    # torso rigid and lets the belt hide the chest/hips seam.
    b.box("shirt_core", (0.45, 0.36, 0.50), (0, 0, 1.37), "shirt",
          bevel=0.05, segments=4, tag="chest")
    b.box("pelvis", (0.45, 0.36, 0.20), (0, 0, 1.06), "trousers",
          bevel=0.04, segments=4, tag="hips")
    # fills the crotch so the inside of the coat is never visible through it
    b.box("seat", (0.31, 0.32, 0.30), (0, 0, 0.93), "trousers",
          bevel=0.05, segments=4, tag="hips")

    # coat body: back, sides, and two front panels leaving a centre gap
    b.box("coat_back", (0.55, 0.12, 0.48), (0, 0.17, 1.38), "coat",
          bevel=0.04, segments=4, tag="chest")
    b.mirror(lambda s, side: b.box(
        f"coat_side.{side}", (0.10, 0.40, 0.48), (s * 0.225, 0, 1.38), "coat",
        bevel=0.04, segments=4, tag="chest"))
    b.mirror(lambda s, side: b.box(
        f"coat_front.{side}", (0.185, 0.12, 0.48), (s * 0.175, -0.175, 1.38), "coat",
        bevel=0.04, segments=4, tag="chest"))

    # collar wrapping from the nape round to the front, where the lapels fold
    # back off it and run down the chest
    b.box("collar_back", (0.34, 0.14, 0.10), (0, 0.145, 1.635), "coat_dk",
          bevel=0.03, segments=4, tag="chest")
    b.mirror(lambda s, side: b.box(
        f"collar_wing.{side}", (0.095, 0.32, 0.10), (s * 0.14, -0.01, 1.635),
        "coat_dk", bevel=0.028, segments=4, rot=(0, 0, s * 13), tag="chest"))
    b.mirror(lambda s, side: b.box(
        f"lapel.{side}", (0.155, 0.045, 0.26), (s * 0.152, -0.212, 1.52), "coat_dk",
        bevel=0.02, segments=3, rot=(0, s * 15, 0), tag="chest"))

    # cravat at the throat
    b.box("cravat", (0.17, 0.13, 0.15), (0, -0.14, 1.585), "cravat",
          bevel=0.038, segments=4, tag="chest")
    b.ball("cravat_knot", 0.055, (0, -0.19, 1.55), "cravat", scale=(1, 0.8, 0.9),
           tag="chest")

    # belt and buckle
    b.box("belt", (0.58, 0.48, 0.09), (0, 0, 1.155), "leather",
          bevel=0.022, segments=3, tag="hips")
    b.box("buckle", (0.10, 0.035, 0.07), (0, -0.252, 1.155), "brass",
          bevel=0.013, segments=3, tag="hips")

    # flared coat skirt below the belt, weighted rigidly to the hips
    b.box("skirt_back", (0.57, 0.13, 0.46), (0, 0.20, 0.97), "coat",
          bevel=0.04, segments=4, tag="hips")
    b.mirror(lambda s, side: b.box(
        f"skirt_side.{side}", (0.105, 0.42, 0.46), (s * 0.245, 0.005, 0.97), "coat",
        bevel=0.04, segments=4, rot=(0, 0, s * 2), tag="hips"))
    b.mirror(lambda s, side: b.box(
        f"skirt_front.{side}", (0.175, 0.13, 0.42), (s * 0.18, -0.195, 0.99), "coat",
        bevel=0.04, segments=4, rot=(0, 0, s * 3), tag="hips"))

    # back tailoring: a half-belt at the waist and a vent down the skirt, so
    # the back reads as a garment rather than one flat slab
    b.box("half_belt", (0.36, 0.035, 0.075), (0, 0.238, 1.20), "coat_dk",
          bevel=0.018, segments=3, tag="hips")
    b.mirror(lambda s, side: b.box(
        f"half_belt_button.{side}", (0.038, 0.025, 0.038), (s * 0.13, 0.258, 1.20),
        "brass", bevel=0.009, segments=3, tag="hips"))
    b.box("vent", (0.03, 0.03, 0.32), (0, 0.262, 0.90), "coat_dk",
          bevel=0.01, segments=2, tag="hips")

    # satchel riding on the belt behind the hip
    b.box("satchel", (0.20, 0.16, 0.21), (-0.20, 0.31, 1.03), "leather",
          bevel=0.04, segments=4, tag="hips")
    b.box("satchel_flap", (0.215, 0.17, 0.06), (-0.20, 0.305, 1.135), "coat_dk",
          bevel=0.022, segments=3, tag="hips")
    b.box("satchel_clasp", (0.045, 0.03, 0.045), (-0.20, 0.395, 1.10), "brass",
          bevel=0.011, segments=3, tag="hips")

    # brass buttons: one on the coat body, three down the skirt
    b.box("button_0", (0.042, 0.028, 0.042), (0.11, -0.25, 1.30), "brass",
          bevel=0.01, segments=3, tag="chest")
    for i, z in enumerate((1.05, 0.93, 0.81)):
        b.box(f"button_s{i}", (0.042, 0.028, 0.042), (0.12, -0.27, z), "brass",
              bevel=0.01, segments=3, tag="hips")

    # the spark: a glowing four-point star on the right breast
    b.star("spark", (-0.17, -0.242, 1.30), "spark", r_out=0.075, tag="chest")


def build_arms(b):
    def arm(s, side):
        mark = b.mark()
        x = s * ARM_PIVOT_X
        b.ball(f"shoulder.{side}", 0.115, (x, 0, 1.51), "coat")
        b.box(f"upper_arm.{side}", (0.185, 0.215, 0.34), (x, 0, 1.34), "coat",
              bevel=0.05, segments=4)
        b.ball(f"elbow.{side}", 0.10, (x, 0, 1.17), "coat")
        b.box(f"forearm.{side}", (0.17, 0.20, 0.30), (x, 0, 1.02), "coat",
              bevel=0.045, segments=4)
        b.box(f"cuff.{side}", (0.20, 0.23, 0.09), (x, 0, 0.865), "coat_dk",
              bevel=0.024, segments=3)
        b.ball(f"wrist.{side}", 0.075, (x, 0, 0.84), "skin")
        b.box(f"palm.{side}", (0.15, 0.14, 0.17), (x, -0.008, 0.76), "skin",
              bevel=0.038, segments=4)
        for i, off in enumerate((-0.05, -0.017, 0.017, 0.05)):
            b.box(f"finger{i}.{side}", (0.031, 0.115, 0.078),
                  (x + s * off, -0.022, 0.635), "skin", bevel=0.014, segments=3)
        b.box(f"thumb.{side}", (0.057, 0.068, 0.11),
              (x - s * 0.088, -0.048, 0.775), "skin",
              bevel=0.024, segments=3, rot=(0, s * -22, 0))
        rotate_about(b.since(mark), (x, 0, ARM_PIVOT_Z), -s * ARM_SPLAY)

    b.mirror(arm)


def build_legs(b):
    def leg(s, side):
        x = s * 0.15
        b.ball(f"hip.{side}", 0.115, (x, 0, 1.00), "trousers")
        b.box(f"thigh.{side}", (0.215, 0.27, 0.40), (x, 0, 0.82), "trousers",
              bevel=0.05, segments=4)
        b.ball(f"knee.{side}", 0.105, (x, 0, 0.62), "trousers")
        b.box(f"shin.{side}", (0.19, 0.235, 0.38), (x, 0, 0.43), "trousers",
              bevel=0.045, segments=4)
        b.box(f"boot_cuff.{side}", (0.235, 0.275, 0.13), (x, 0, 0.255), "boot",
              bevel=0.03, segments=4)
        b.ball(f"ankle.{side}", 0.09, (x, 0, 0.24), "boot")
        b.box(f"boot.{side}", (0.205, 0.32, 0.16), (x, -0.05, 0.105), "boot",
              bevel=0.042, segments=4, tag=f"foot.{side}")
        b.box(f"sole.{side}", (0.215, 0.335, 0.055), (x, -0.055, 0.028), "leather",
              bevel=0.02, segments=3, tag=f"foot.{side}")

    b.mirror(leg)


def build_mesh(b):
    build_head(b)
    build_torso(b)
    build_arms(b)
    build_legs(b)


# ------------------------------------------------------------------- rig ---

SPINE = [
    ("root",  (0, 0, 0),      (0, -0.35, 0), None,    False),
    ("hips",  (0, 0, 1.03),   (0, 0, 1.20),  "root",  False),
    ("spine", (0, 0, 1.20),   (0, 0, 1.37),  "hips",  True),
    ("chest", (0, 0, 1.37),   (0, 0, 1.53),  "spine", True),
    ("neck",  (0, 0, 1.53),   (0, 0, 1.68),  "chest", True),
    ("head",  (0, 0, 1.68),   (0, 0, 2.18),  "neck",  True),
]


def build_rig():
    bpy.ops.object.select_all(action="DESELECT")
    bpy.ops.object.armature_add(enter_editmode=True, location=(0, 0, 0))
    arm = bpy.context.active_object
    arm.name = "ClaudeRig"
    arm.data.name = "ClaudeRig"
    eb = arm.data.edit_bones
    for bone in list(eb):
        eb.remove(bone)

    def add(name, head, tail, parent=None, connect=False):
        bone = eb.new(name)
        bone.head = Vector(head)
        bone.tail = Vector(tail)
        if parent:
            bone.parent = eb[parent]
            bone.use_connect = connect
        return bone

    for spec in SPINE:
        add(*spec)

    for s, side in ((1, "L"), (-1, "R")):
        x = s * ARM_PIVOT_X
        pivot = (x, 0, ARM_PIVOT_Z)
        splay = -s * ARM_SPLAY

        def a(p):
            """Arm point, laid out vertically then splayed into the A-pose."""
            return rotate_point(p, pivot, splay)

        add(f"clavicle.{side}", (s * 0.07, 0, 1.47), (x, 0, 1.51), "chest")
        add(f"upper_arm.{side}", a((x, 0, 1.51)), a((x, 0, 1.17)),
            f"clavicle.{side}")
        add(f"forearm.{side}", a((x, 0, 1.17)), a((x, 0, 0.86)),
            f"upper_arm.{side}", True)
        add(f"hand.{side}", a((x, 0, 0.86)), a((x, 0, 0.70)),
            f"forearm.{side}", True)
        add(f"fingers.{side}", a((x, 0, 0.70)), a((x, -0.02, 0.59)),
            f"hand.{side}", True)
        add(f"thumb.{side}", a((x - s * 0.07, -0.02, 0.82)),
            a((x - s * 0.125, -0.075, 0.73)), f"hand.{side}")

        add(f"thigh.{side}", (s * 0.15, 0, 1.01), (s * 0.15, 0, 0.62), "hips")
        add(f"shin.{side}", (s * 0.15, 0, 0.62), (s * 0.15, 0, 0.24),
            f"thigh.{side}", True)
        add(f"foot.{side}", (s * 0.15, 0, 0.24), (s * 0.15, -0.14, 0.06),
            f"shin.{side}", True)
        add(f"toe.{side}", (s * 0.15, -0.14, 0.06), (s * 0.15, -0.25, 0.06),
            f"foot.{side}", True)

    bpy.ops.object.mode_set(mode="OBJECT")
    # root is a placement handle, not a deform bone: leaving it deforming lets
    # automatic weights bind the boot soles to it, so they stay on the ground
    # while the rest of the leg walks away
    arm.data.bones["root"].use_deform = False
    arm.data.display_type = "OCTAHEDRAL"
    arm.show_in_front = True
    return arm


def skin(body, arm):
    bpy.ops.object.select_all(action="DESELECT")
    body.select_set(True)
    arm.select_set(True)
    bpy.context.view_layer.objects.active = arm
    bpy.ops.object.parent_set(type="ARMATURE_AUTO")

    bones = [(b.name, b.head_local, b.tail_local)
             for b in arm.data.bones if b.name != "root"]

    def nearest(p):
        best, best_d = None, 1e9
        for name, h, t in bones:
            d = t - h
            length_sq = d.length_squared
            u = max(0.0, min(1.0, (p - h).dot(d) / length_sq)) if length_sq else 0.0
            dist = (p - (h + d * u)).length
            if dist < best_d:
                best, best_d = name, dist
        return best

    fixed = 0
    for v in body.data.vertices:
        if not v.groups or sum(g.weight for g in v.groups) < 1e-4:
            body.vertex_groups[nearest(v.co)].add([v.index], 1.0, "REPLACE")
            fixed += 1

    # applied details are pinned to the bone named by their tag
    tagged = [(g.name[4:], g.index) for g in body.vertex_groups
              if g.name.startswith("TAG:")]
    pinned = {}
    for bone_name, group_index in tagged:
        idx = [v.index for v in body.data.vertices
               if any(g.group == group_index for g in v.groups)]
        for other in body.vertex_groups:
            if other.index != group_index:
                other.remove(idx)
        body.vertex_groups[bone_name].add(idx, 1.0, "REPLACE")
        pinned[bone_name] = pinned.get(bone_name, 0) + len(idx)
    for group in [g for g in body.vertex_groups if g.name.startswith("TAG:")]:
        body.vertex_groups.remove(group)

    return {"weight_fallbacks": fixed, "pinned": pinned}


# ------------------------------------------------------------ animations --

def action_fcurves(action):
    """Blender 5 keeps fcurves inside layered action channelbags."""
    if hasattr(action, "fcurves"):
        yield from action.fcurves
        return
    for layer in action.layers:
        for strip in layer.strips:
            for bag in getattr(strip, "channelbags", []):
                yield from bag.fcurves


def make_action(arm, name, keys):
    """keys: list of (frame, {bone: (rx, ry, rz) degrees}[, {bone: (x, y, z)}]).

    The optional third element keyframes bone translation, which the walk
    cycle needs: rotating the legs about a fixed hip lifts the rear foot off
    the floor, so the hips have to drop as the stride opens.
    """
    action = bpy.data.actions.new(name)
    action.use_fake_user = True
    arm.animation_data.action = action
    pb = arm.pose.bones
    moved = sorted({n for key in keys if len(key) > 2 for n in key[2]})
    for key in keys:
        frame, poses = key[0], key[1]
        locs = key[2] if len(key) > 2 else {}
        for b in pb:
            b.rotation_euler = (0, 0, 0)
            b.location = (0, 0, 0)
        for bone_name, rot in poses.items():
            pb[bone_name].rotation_euler = tuple(math.radians(a) for a in rot)
        for bone_name, loc in locs.items():
            pb[bone_name].location = loc
        for b in pb:
            b.keyframe_insert("rotation_euler", frame=frame)
        for bone_name in moved:
            pb[bone_name].keyframe_insert("location", frame=frame)
    for fc in action_fcurves(action):
        for kp in fc.keyframe_points:
            kp.interpolation = "BEZIER"
    return action


def build_animations(arm):
    bpy.context.view_layer.objects.active = arm
    bpy.ops.object.mode_set(mode="POSE")
    for b in arm.pose.bones:
        b.rotation_mode = "XYZ"
    if not arm.animation_data:
        arm.animation_data_create()

    make_action(arm, "Idle", [
        (1,   {}),
        (30,  {"chest": (-2.5, 0, 0), "head": (2, 4, 0),
               "upper_arm.L": (0, 0, 4), "upper_arm.R": (0, 0, -4)}),
        (60,  {}),
        (90,  {"chest": (-2, 0, 0), "head": (1, -5, 0),
               "upper_arm.L": (0, 0, 3), "upper_arm.R": (0, 0, -3)}),
        (120, {}),
    ])

    wave_up = {"upper_arm.R": (0, 0, -118), "forearm.R": (-28, 0, -38),
               "head": (0, 9, -6), "chest": (0, 0, -4)}
    wave_out = dict(wave_up)
    wave_out.update({"forearm.R": (-28, 0, 2), "hand.R": (0, 0, 18)})
    make_action(arm, "Wave", [
        (1, {}), (10, wave_up), (18, wave_out), (26, wave_up),
        (34, wave_out), (42, wave_up), (52, {}),
    ])

    def contact(f):
        """f = +1 puts the left leg forward."""
        return {
            "thigh.L": (28 * f, 0, 0), "shin.L": (-18 * max(f, 0), 0, 0),
            "thigh.R": (-28 * f, 0, 0), "shin.R": (-18 * max(-f, 0), 0, 0),
            "foot.L": (-10 * f, 0, 0), "foot.R": (10 * f, 0, 0),
            "upper_arm.L": (-26 * f, 0, 0), "upper_arm.R": (26 * f, 0, 0),
            "forearm.L": (18, 0, 0), "forearm.R": (18, 0, 0),
            "chest": (0, 0, -3 * f), "hips": (0, 0, 2 * f),
        }

    def passing(f):
        return {
            "thigh.L": (7 * f, 0, 0), "shin.L": (-32 * max(-f, 0), 0, 0),
            "thigh.R": (-7 * f, 0, 0), "shin.R": (-32 * max(f, 0), 0, 0),
            "upper_arm.L": (-7 * f, 0, 0), "upper_arm.R": (7 * f, 0, 0),
            "forearm.L": (15, 0, 0), "forearm.R": (15, 0, 0),
            "spine": (-2, 0, 0),
        }

    # the hips bone points +Z, so its local -Y lowers the body
    down = {"hips": (0, -0.044, 0)}
    up = {"hips": (0, 0.014, 0)}
    make_action(arm, "Walk", [
        (1, contact(1), down), (7, passing(-1), up), (13, contact(-1), down),
        (19, passing(1), up), (25, contact(1), down),
    ])

    arm.animation_data.action = bpy.data.actions["Idle"]
    bpy.ops.object.mode_set(mode="OBJECT")


# ---------------------------------------------------------------- scene ----

def clear_scene():
    for obj in list(bpy.data.objects):
        if obj.type in {"MESH", "ARMATURE", "EMPTY"}:
            bpy.data.objects.remove(obj, do_unlink=True)
    for mesh in list(bpy.data.meshes):
        if mesh.users == 0:
            bpy.data.meshes.remove(mesh)
    for act in list(bpy.data.actions):
        bpy.data.actions.remove(act)


def setup_render():
    scene = bpy.context.scene
    scene.render.engine = "BLENDER_EEVEE"
    scene.view_settings.view_transform = "Standard"
    scene.render.resolution_x = 720
    scene.render.resolution_y = 900
    scene.render.resolution_percentage = 100
    scene.render.fps = 24

    world = scene.world
    world.use_nodes = True
    bg = world.node_tree.nodes["Background"]
    bg.inputs[0].default_value = (0.09, 0.12, 0.17, 1.0)
    bg.inputs[1].default_value = 0.9

    for obj in list(bpy.data.objects):
        if obj.type == "LIGHT":
            bpy.data.objects.remove(obj, do_unlink=True)

    def light(name, energy, loc, rot, color, size):
        data = bpy.data.lights.new(name, "AREA")
        data.energy = energy
        data.color = color
        data.size = size
        obj = bpy.data.objects.new(name, data)
        obj.location = loc
        obj.rotation_euler = tuple(math.radians(a) for a in rot)
        bpy.context.collection.objects.link(obj)

    light("Key", 480, (-2.4, -2.6, 3.0), (46, 0, -42), (1.0, 0.96, 0.9), 3.0)
    light("Fill", 130, (3.0, -2.0, 1.6), (72, 0, 56), (0.75, 0.85, 1.0), 3.5)
    light("Rim", 300, (1.2, 3.2, 2.8), (118, 0, 200), (1.0, 0.85, 0.7), 2.5)

    if "Camera" not in bpy.data.objects:
        cam_data = bpy.data.cameras.new("Camera")
        cam = bpy.data.objects.new("Camera", cam_data)
        bpy.context.collection.objects.link(cam)
        scene.camera = cam
    return bpy.data.objects["Camera"]


VIEWS = {
    "front": ((0, -4.8, 1.0), (90, 0, 0)),
    "side":  ((4.8, 0, 1.0), (90, 0, 90)),
    "hero":  ((-2.6, -3.4, 1.65), (79, 0, -37)),
    "back":  ((0, 4.8, 1.0), (90, 0, 180)),
}


POSES = {
    "hero": ("Idle", 1, (-2.7, -3.5, 1.68), (79, 0, -37)),
    "wave": ("Wave", 14, (-2.9, -3.6, 1.70), (79, 0, -38)),
    "walk": ("Walk", 4, (3.2, -2.8, 1.50), (81, 0, 49)),
}


def render_sheet(out_path, cell=(560, 700)):
    """Render a two-row contact sheet: turnaround above, poses below."""
    import numpy as np

    scene = bpy.context.scene
    arm = bpy.data.objects["ClaudeRig"]
    cam = bpy.data.objects["Camera"]
    cam.data.type = "PERSP"
    cam.data.lens = 65
    old = (scene.render.resolution_x, scene.render.resolution_y)
    scene.render.resolution_x, scene.render.resolution_y = cell

    tmp = bpy.app.tempdir
    tiles = []

    def render_to(name):
        path = os.path.join(tmp, f"sheet_{name}.png")
        scene.render.filepath = path
        bpy.ops.render.render(write_still=True)
        return path

    arm.animation_data.action = bpy.data.actions["Idle"]
    scene.frame_set(1)
    for name in ("front", "side", "back"):
        loc, rot = VIEWS[name]
        cam.location = loc
        cam.rotation_euler = tuple(math.radians(a) for a in rot)
        tiles.append(render_to(name))
    for name, (action, frame, loc, rot) in POSES.items():
        arm.animation_data.action = bpy.data.actions[action]
        scene.frame_set(frame)
        cam.location = loc
        cam.rotation_euler = tuple(math.radians(a) for a in rot)
        tiles.append(render_to(name))

    w, h = cell
    sheet = np.zeros((h * 2, w * 3, 4), dtype=np.float32)
    for i, path in enumerate(tiles):
        img = bpy.data.images.load(path)
        px = np.array(img.pixels[:], dtype=np.float32).reshape(h, w, 4)
        row, col = divmod(i, 3)
        # image rows run bottom-up, so the first sheet row is the upper band
        sheet[(1 - row) * h:(2 - row) * h, col * w:(col + 1) * w] = px
        bpy.data.images.remove(img)

    out = bpy.data.images.new("sheet", width=w * 3, height=h * 2, alpha=True)
    out.pixels = sheet.ravel()
    out.filepath_raw = out_path
    out.file_format = "PNG"
    out.save()
    bpy.data.images.remove(out)

    scene.render.resolution_x, scene.render.resolution_y = old
    arm.animation_data.action = bpy.data.actions["Idle"]
    scene.frame_set(1)
    return out_path


def render_views(paths_dir, tag="", views=None):
    scene = bpy.context.scene
    cam = bpy.data.objects["Camera"]
    cam.data.type = "PERSP"
    cam.data.lens = 65
    os.makedirs(paths_dir, exist_ok=True)
    out = {}
    for name, (loc, rot) in (views or VIEWS).items():
        cam.location = loc
        cam.rotation_euler = tuple(math.radians(a) for a in rot)
        path = os.path.join(paths_dir, f"{tag}{name}.png")
        scene.render.filepath = path
        bpy.ops.render.render(write_still=True)
        out[name] = path
    return out


# ----------------------------------------------------------------- main ----

def main(export=True, save=True):
    clear_scene()
    mats = build_materials()
    b = Builder(mats)
    build_mesh(b)

    # join every part into one mesh
    bpy.ops.object.select_all(action="DESELECT")
    for obj in b.parts:
        bpy.context.view_layer.objects.active = obj
        for mod in list(obj.modifiers):
            bpy.ops.object.modifier_apply(modifier=mod.name)
        obj.select_set(True)
    bpy.context.view_layer.objects.active = b.parts[0]
    bpy.ops.object.join()
    body = bpy.context.active_object
    body.name = "ClaudeAvatar"
    body.data.name = "ClaudeAvatarMesh"
    bpy.ops.object.shade_smooth_by_angle(angle=math.radians(32))

    # put the origin at the world origin so scaling happens about the feet
    bpy.context.scene.cursor.location = (0, 0, 0)
    bpy.ops.object.origin_set(type="ORIGIN_CURSOR")

    # normalise height with the feet on the ground
    body.data.update()
    zs = [v.co.z for v in body.data.vertices]
    scale = HEIGHT / (max(zs) - min(zs))

    arm = build_rig()
    for obj in (body, arm):
        obj.scale = (scale, scale, scale)
        obj.location.z -= min(zs) * scale
    for obj in (body, arm):
        bpy.ops.object.select_all(action="DESELECT")
        obj.select_set(True)
        bpy.context.view_layer.objects.active = obj
        bpy.ops.object.transform_apply(location=True, rotation=True, scale=True)

    # UVs so the mesh is ready for textures later
    bpy.context.view_layer.objects.active = body
    bpy.ops.object.mode_set(mode="EDIT")
    bpy.ops.mesh.select_all(action="SELECT")
    bpy.ops.uv.smart_project(angle_limit=math.radians(66), island_margin=0.008)
    bpy.ops.object.mode_set(mode="OBJECT")

    weights = skin(body, arm)
    body.data.validate()
    build_animations(arm)
    setup_render()

    zs = [(body.matrix_world @ v.co).z for v in body.data.vertices]
    stats = {
        "verts": len(body.data.vertices),
        "faces": len(body.data.polygons),
        "materials": len(body.data.materials),
        "bones": len(arm.data.bones),
        "height": round(max(zs) - min(zs), 3),
        "floor": round(min(zs), 4),
        "actions": [a.name for a in bpy.data.actions],
        **weights,
    }

    if save:
        os.makedirs(OUT_DIR, exist_ok=True)
        bpy.ops.wm.save_as_mainfile(
            filepath=os.path.join(OUT_DIR, "claude-avatar.blend"))
    if export:
        bpy.ops.object.select_all(action="DESELECT")
        body.select_set(True)
        arm.select_set(True)
        bpy.context.view_layer.objects.active = arm
        bpy.ops.export_scene.gltf(
            filepath=os.path.join(OUT_DIR, "claude-avatar.glb"),
            export_format="GLB",
            use_selection=True,
            export_animations=True,
            export_animation_mode="ACTIONS",
            export_skins=True,
            export_apply=True,
        )
    return stats


if __name__ == "__main__":
    print(main())
