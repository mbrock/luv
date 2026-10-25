"""Editable gnome and tabby: sculpted weighted bodies, live IK and verified FNT1.

Blender 5.2: blender -b --python-exit-code 1 --python this-file.py -- --render --preview
Imports the existing Fantus construction/export library without running its main.
Owns this directory and .amp/in/artifacts/{gnome,cat}-* only.
"""

import argparse
import importlib.util
import json
import math
import struct
import sys
from pathlib import Path

import bmesh
import bpy
import numpy as np
from mathutils import Vector

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent.parent
ART = ROOT / ".amp/in/artifacts"
sys.dont_write_bytecode = True
spec = importlib.util.spec_from_file_location("felt_library", HERE.parent / "fantus/build-fantus.py")
F = importlib.util.module_from_spec(spec)
spec.loader.exec_module(F)
TAU, FRAMES, STRIDE = math.tau, 48, .8
PALETTE = {
    "coat": "526D87", "cuff": "7892A6", "trousers": "354653",
    "hat": "B92F37", "hat_band": "922B33", "cream": "F2DFB4",
    "hair_shadow": "D9C596", "skin": "E6B08C", "blush": "D28F77",
    "leather": "694638", "sole": "3F342E", "brass": "C9A35A",
    "orange": "D9964E", "stripe": "9E582E", "fur_light": "E5AE68",
    "pink": "C88383", "mouth": "6C4540", "white": "F8F0D8",
    "green": "83B16A", "iris_dark": "466B40", "pupil": "292E27", "catch": "FFFFFF",
}


class AssetPaths:
    """Route imported exporters' filenames; never write in the Fantus directory."""
    def __init__(self, name):
        self.name = name

    def __truediv__(self, filename):
        return HERE / filename.replace("fantus.", self.name + ".").replace("verification.json", self.name + "-verification.json")


def setup(name):
    bpy.ops.wm.read_factory_settings(use_empty=True)
    F.PARTS.clear()
    F.MATS.clear()
    F.PALETTE = PALETTE
    F.HERE = AssetPaths(name)
    F.FRAMES, F.STRIDE = FRAMES, STRIDE
    F.materials()
    for key, mat in F.MATS.items():
        mat.name = name.title() + " / " + key
    bpy.context.scene.render.fps = 60


def ell(name, center, radius, mat, weights=None, segments=32, rings=20):
    return F.ellipsoid(name, center, radius, mat, weights, segments, rings)


def curve(name, points, radius, mat, weights, sides=8, steps=6):
    return F.tube(name, F.catmull(points, steps), radius, mat, weights, sides)


def blend(a, b, t):
    return {a: 1-t, b: t}


def gnome_leg(side):
    def weights(p):
        if p.z < .24:
            return blend("foot."+side, "lower."+side, F.smooth(.13, .23, p.z))
        return blend("lower."+side, "upper."+side, F.smooth(.28, .40, p.z))
    return weights


def sleeve(side):
    def weights(p):
        if p.z < .61:
            return blend("hand."+side, "forearm."+side, F.smooth(.50, .61, p.z))
        return blend("forearm."+side, "arm."+side, F.smooth(.66, .77, p.z))
    return weights


def gnome_coat(p):
    torso = F.smooth(.57, .82, p.z)
    return blend("hips", "chest", torso)


def loft(name, rings, mat, weights, sides=64, cap=True):
    """Elliptical sculpt rings (center x,y,z and radii x,y); actual editable quads."""
    vertices, faces = [], []
    for x, y, z, rx, ry in rings:
        for i in range(sides):
            a = TAU*i/sides
            vertices.append((x+rx*math.cos(a), y+ry*math.sin(a), z))
    for j in range(len(rings)-1):
        for i in range(sides):
            faces.append((j*sides+i, j*sides+(i+1)%sides,
                          (j+1)*sides+(i+1)%sides, (j+1)*sides+i))
    if cap:
        faces.extend([tuple(reversed(range(sides))), tuple((len(rings)-1)*sides+i for i in range(sides))])
    return F.mesh(name, vertices, faces, mat, weights)


def eye(name, x, z, rx, rz, surface, iris, cat=False):
    """Conformal lenses and lids, never exposed eyeball spheres."""
    def layer(label, cx, cz, sx, sz, depth, material):
        return F.oval_patch(label, (cx, cz), (sx, sz),
                           lambda xx, zz: surface(xx, zz)-depth, material, "head", 40, 5)
    layer("Eye socket "+name, x, z, rx*1.12, rz*1.16, .004, "iris_dark" if cat else "hair_shadow")
    layer("Eye lens "+name, x, z, rx, rz, .008, iris if cat else "white")
    if not cat:
        layer("Iris "+name, x, z+.001, rx*.51, rz*.81, .010, iris)
    layer("Pupil "+name, x, z+.001, rx*(.14 if cat else .27), rz*.75, .012, "pupil")
    layer("Eye glint "+name, x-rx*.18, z+rz*.30, rx*.14, rz*.18, .014, "catch")
    points = [(x+rx*math.cos(a), surface(x+rx*math.cos(a), z+rz*math.sin(a))-.012,
               z+rz*math.sin(a)) for a in np.linspace(.06, math.pi-.06, 18)]
    F.tube("Upper lid "+name, points, .005 if cat else .006, "orange" if cat else "skin", "head", 8)


def build_gnome():
    # A short, open-hem work coat: legs occupy the lower third of the silhouette.
    rings = []
    for z, rx, ry in ((.47,.26,.175),(.49,.269,.184),(.54,.273,.19),(.62,.258,.19),
                      (.72,.236,.185),(.80,.209,.16),(.88,.19,.13),(.92,.13,.10)):
        rings.append((0,.02,z,rx,ry))
    coat = loft("Short tailored work coat", rings, "coat", gnome_coat, cap=False)
    # Inward hem thickness, rather than a solid cone base.
    loft("Turned coat hem", [(0,.02,.47,.26,.175),(0,.02,.482,.248,.163)], "cuff", "hips", cap=False)
    pants=[ell("Undercoat pelvis", (0,.025,.55), (.22,.15,.13), "trousers")]
    for s, side in ((1,"L"),(-1,"R")):
        pants.extend([ell("Upper trouser",(s*.135,.0,.46),(.10,.095,.18),"trousers"),
                      ell("Lower trouser",(s*.145,-.012,.285),(.078,.077,.13),"trousers")])
        boot = F.union("Leather boot."+side,
                       [ell("Boot ankle",(s*.15,.0,.15),(.091,.10,.13),"leather"),
                        ell("Boot toe",(s*.15,-.065,.085),(.106,.173,.085),"leather")],
                       .010,"foot."+side)
        for v in boot.data.vertices:
            v.co.z = max(.022,v.co.z)
        sole = ell("Flat leather sole."+side,(s*.15,-.065,.022),(.107,.172,.024),"sole","foot."+side)
        for v in sole.data.vertices:
            v.co.z = max(0,v.co.z)
        # Sleeves are blended into shoulder caps; no animated rigid objects.
        pieces = [ell("Shoulder",(s*.19,.015,.83),(.11,.12,.12),"coat"),
                  ell("Sleeve upper",(s*.255,.01,.75),(.10,.105,.14),"coat"),
                  ell("Sleeve lower",(s*.30,-.015,.62),(.082,.09,.13),"coat")]
        F.union("Sewn sleeve."+side,pieces,.012,sleeve(side))
        ell("Cuff."+side,(s*.32,-.024,.535),(.084,.086,.035),"cuff",sleeve(side))
        F.union("Soft mitten."+side,
                [ell("Palm",(s*.327,-.026,.47),(.080,.073,.092),"leather"),
                 ell("Thumb",(s*.275,-.067,.485),(.04,.045,.055),"leather")],.009,"hand."+side)
        curve("Boot stitching."+side,[(s*.15-.065,-.16,.065),(s*.15,-.205,.07),(s*.15+.065,-.16,.065)],
              .002,"brass","foot."+side)
        # Patch pockets on the side-front, away from the beard.
        ell("Patch pocket."+side,(s*.19,-.132,.59),(.055,.018,.066),"cuff",gnome_coat,24,14)
    def pants_weights(p):
        side="L" if p.x>0 else "R"
        hip=F.smooth(.43,.56,p.z)
        return {"hips":hip,**{k:w*(1-hip) for k,w in gnome_leg(side)(p).items()}}
    F.union("Continuous sewn trousers and crotch",pants,.012,pants_weights)
    curve("Coat front piping",[(0,-.17,.49),(0,-.177,.60),(0,-.165,.72)],.005,"cuff",gnome_coat)
    for z in (.53,.62,.71):
        ell("Brass coat button",(.022,-.178,z),(.013,.007,.013),"brass",gnome_coat,16,10)

    # Warm face and ears; the nose is joined to the cheeks, not a pasted sphere.
    faceparts = [ell("Face",(0,-.012,.946),(.242,.183,.201),"skin"),
                 ell("Warm nose",(0,-.213,.908),(.086,.080,.067),"skin")]
    for s in (-1,1):
        faceparts.append(ell("Ear",(s*.224,-.005,.94),(.047,.052,.074),"skin"))
    F.union("Sculpted face and round nose",faceparts,.008,"head")
    def surface(x,z):
        return -.012-.183*math.sqrt(max(.05,1-(x/.242)**2-((z-.946)/.201)**2))
    for s, side in ((1,"L"),(-1,"R")):
        eye(side,s*.096,.979,.048,.033,surface,"green")
        curve("Cream expressive brow."+side,
              [(s*.045,surface(s*.045,1.027)-.01,1.027),
               (s*.096,surface(s*.096,1.041)-.01,1.041),
               (s*.155,surface(s*.155,1.027)-.01,1.027)],.013,"cream","head",10)
        ell("Ear inner."+side,(s*.246,-.041,.946),(.018,.01,.038),"blush","head",20,12)

    def beard_weights(p):
        return blend("head","beard",1-F.smooth(.65,.88,p.z))
    locks = [ell("Beard heart",(0,-.16,.746),(.193,.127,.20),"cream")]
    for s in (-1,1):
        locks.extend([ell("Beard cheek",(s*.157,-.12,.844),(.085,.092,.13),"cream"),
                      ell("Beard lock",(s*.102,-.184,.706),(.09,.09,.145),"cream")])
    locks.append(ell("Beard point",(.018,-.174,.614),(.075,.072,.074),"cream"))
    F.union("Sculpted scalloped beard",locks,.008,beard_weights)
    # Shallow raised combed locks, embedded halfway in the beard surface.
    for s in (-1,1):
        for j in range(3):
            x=s*(.04+j*.044)
            curve("Combed beard strand",[(x,-.269,.81),(x*.86,-.282,.735),(x*.65,-.248,.652)],
                  lambda t:.004*(math.sin(math.pi*t)**.5+.15),"hair_shadow",beard_weights,6)
        curve("Swept moustache",[(s*.018,-.269,.872),(s*.075,-.278,.857),
                                 (s*.13,-.244,.852),(s*.17,-.210,.875)],
              lambda t:.007+.025*math.sin(math.pi*t)**.7,"cream","head",12,8)
    curve("Smile below moustache",[(-.046,-.278,.837),(0,-.285,.829),(.046,-.278,.837)],
          .004,"mouth","head",8)

    def hat_weights(p):
        return blend("head","hat",F.smooth(1.10,1.40,p.z))
    # A curved, asymmetrical felt cap, with a folded-over soft tip.
    controls = [(0,.006,1.075,.267,.205),(0,.012,1.11,.267,.205),
                (.008,.023,1.18,.228,.181),(.024,.025,1.27,.192,.151),
                (.048,.006,1.36,.149,.119),(.075,-.031,1.43,.105,.091),
                (.105,-.080,1.49,.068,.060),(.129,-.132,1.535,.038,.036),
                (.143,-.180,1.56,.010,.012),(.149,-.204,1.55,.003,.004)]
    interpolated = F.catmull(controls,5)
    loft("Leaning scarlet felt hat",interpolated,"hat",hat_weights,64)
    brim=[]
    for t in np.linspace(0,TAU,17):
        brim.append((0,.005,1.084+.026*math.sin(t),.269+.019*math.cos(t),.207+.017*math.cos(t)))
    loft("Rolled scarlet brim",brim,"hat_band","head",64,False)
    curve("Hat seam",[(.17,-.157,1.13),(.159,-.122,1.25),(.141,-.114,1.37),(.145,-.135,1.46)],
          .0025,"hat_band",hat_weights,6)
    limbs=[]
    for s,side,phase in ((1,"L",0),(-1,"R",.5)):
        limbs.append(dict(id=side,hip=(s*.15,0,.61),knee=(s*.15,-.10,.355),
                          ankle=(s*.15,0,.12),pole=(s*.15,-.70,.36),parent="hips",phase=phase))
    return limbs


TAIL = [(0,.43,.63),(.10,.68,.64),(.17,.94,.75),(.10,1.18,.90),(-.055,1.28,.96)]


def tail_weights(p):
    # Smoothly distribute around each bend; arc parameters are monotonic in Y.
    t = max(0,min(3.999,(p.y-.43)/(.85)*4))
    i = int(t)
    j = min(3,i+1)
    return {"tail.%02d"%(i+1):1-(t-i),"tail.%02d"%(j+1):(t-i)} if i!=j else {"tail.04":1}


def cat_weights(p):
    head = F.smooth(.48,.72,p.z)*(1-F.smooth(-.42,-.20,p.y))
    side = "L" if p.x>0 else "R"
    rear = p.y>.03
    label=("hind." if rear else "front.")+side
    if p.z<.14:
        return {"foot."+label:1}
    s=1 if p.x>0 else -1
    hip=Vector((s*.18,.30 if rear else -.31,.59 if rear else .61))
    knee=Vector((s*.19,.12 if rear else -.23,.34))
    ankle=Vector((s*.20,.34 if rear else -.37,.10))
    distance=float("inf")
    for a,b in ((hip,knee),(knee,ankle)):
        axis=b-a
        t=max(0,min(1,(p-a).dot(axis)/axis.length_squared))
        distance=min(distance,(p-a-axis*t).length)
    limb=(1-F.smooth(.095,.175,distance))*(1-F.smooth(.38,.60,p.z))
    if p.z<.22:
        chain=blend("foot."+label,"lower."+label,F.smooth(.14,.23,p.z))
    else:
        chain=blend("lower."+label,"upper."+label,F.smooth(.27,.41,p.z))
    # Fade the belly to torso without dragging the forward hind-knee back to
    # the pelvis. Distance to the actual bent chain follows the anatomy.
    limb*=F.smooth(.015,.085,p.y) if rear else 1-F.smooth(-.12,.015,p.y)
    torso=blend("chest","hips",F.smooth(-.05,.23,p.y))
    if limb>.001:
        return {**{k:w*(1-limb) for k,w in torso.items()},**{k:w*limb for k,w in chain.items()}}
    if head>.001:
        return blend("chest","head",head)
    return torso


def paint_boundaries(obj, fields, weights):
    """Cut real color boundaries through triangles, not jagged polygon-center paint.

    Fields are signed distances/implicit patterns: positive chooses that material.
    Shared cut vertices remain welded; FNT1 splits only material-color records.
    """
    mod=obj.modifiers.new("Budget before cutting color boundaries","DECIMATE")
    mod.ratio=.60
    F.apply(obj,mod)
    # A decimated long triangle can skip a whole stripe between its vertices.
    # Refine only the pattern's Y/Z sampling directions before clipping.
    bm=bmesh.new()
    bm.from_mesh(obj.data)
    for _ in range(5):
        edges=[e for e in bm.edges if abs(e.verts[0].co.y-e.verts[1].co.y)>.030 or
               abs(e.verts[0].co.z-e.verts[1].co.z)>(.018 if min(v.co.z for v in e.verts)<.40 else .030)]
        if not edges:
            break
        bmesh.ops.subdivide_edges(bm,edges=edges,cuts=1,use_grid_fill=True)
        bmesh.ops.triangulate(bm,faces=list(bm.faces))
    bm.to_mesh(obj.data)
    bm.free()
    obj.data.update()
    obj.data.calc_loop_triangles()
    polygons=[([obj.data.vertices[i].co.copy() for i in tri.vertices],0)
              for tri in obj.data.loop_triangles]
    for field,material in fields:
        result=[]
        for points,old_material in polygons:
            values=[field(p) for p in points]
            if min(values)>=0:
                result.append((points,material))
            elif max(values)<=0:
                result.append((points,old_material))
            else:
                positive,negative=[],[]
                for i,p in enumerate(points):
                    q=points[(i+1)%len(points)]
                    a,b=values[i],values[(i+1)%len(points)]
                    (positive if a>=0 else negative).append(p)
                    if (a>=0)!=(b>=0):
                        # Solve the true pattern boundary, not a linearly
                        # interpolated sine value (which gives serrated bands).
                        lo,hi=0.0,1.0
                        for _ in range(20):
                            mid=(lo+hi)*.5
                            if (field(p+(q-p)*mid)>=0)==(a>=0):
                                lo=mid
                            else:
                                hi=mid
                        crossing=p+(q-p)*((lo+hi)*.5)
                        positive.append(crossing)
                        negative.append(crossing)
                if len(positive)>=3:
                    result.append((positive,material))
                if len(negative)>=3:
                    result.append((negative,old_material))
        polygons=result
    vertices,faces,materials,lookup=[],[],[],{}
    for points,material in polygons:
        face=[]
        for p in points:
            key=tuple(round(float(c),7) for c in p)
            if key not in lookup:
                lookup[key]=len(vertices)
                vertices.append(p)
            face.append(lookup[key])
        if len(set(face))>=3:
            faces.append(face)
            materials.append(material)
    replacement=F.mesh(obj.name,vertices,faces,["orange","stripe","cream"],weights,materials)
    F.PARTS.remove(obj)
    bpy.data.objects.remove(obj,do_unlink=True)
    return replacement


def build_cat():
    # Remesh one continuous organic surface: head, chest, shoulders, haunches,
    # four articulated limbs and paws. Color changes never require seams.
    pieces=[ell("Long ribcage",(0,.025,.57),(.225,.40,.23),"orange"),
            ell("Chest",(0,-.29,.57),(.205,.22,.25),"orange"),
            ell("Neck",(0,-.36,.70),(.177,.18,.22),"orange"),
            ell("Rounded feline head",(0,-.48,.775),(.245,.216,.212),"orange")]
    for s in (-1,1):
        pieces.extend([ell("Cheek",(s*.147,-.53,.735),(.117,.142,.106),"orange"),
                       ell("Haunch",(s*.15,.29,.51),(.15,.22,.225),"orange"),
                       ell("Shoulder",(s*.155,-.285,.49),(.104,.145,.20),"orange")])
        # Slightly rounded triangular pinnae, with thick roots growing from head.
        ear=loft("Outer ear",[(s*.155,-.451,.89,.112,.085),
                             (s*.183,-.44,.95,.077,.057),
                             (s*.207,-.427,1.015,.034,.033),
                             (s*.218,-.42,1.05,.006,.009)],"orange",None,32)
        pieces.append(ear)
    limbs=[]
    for s,side in ((1,"L"),(-1,"R")):
        for rear in (False,True):
            label=("hind." if rear else "front.")+side
            hip=(s*.18,.30 if rear else -.31,.59 if rear else .61)
            knee=(s*.19,.12 if rear else -.23,.34)
            ankle=(s*.20,.34 if rear else -.37,.10)
            pole=(s*.20,-.65 if rear else .32,.34)
            phase={"hind.L":0,"front.L":.75,"hind.R":.5,"front.R":.25}[label]
            limbs.append(dict(id=label,hip=hip,knee=knee,ankle=ankle,pole=pole,
                              parent="hips" if rear else "chest",phase=phase))
            points=[hip,knee,ankle]
            pieces.append(F.tube("Organic leg",F.catmull(points,10),
                                 lambda t:.09-.033*t,"orange",None,20))
            pieces.append(ell("Paw",(ankle[0],ankle[1]-.033,.065),(.088,.135,.067),"orange"))
    body=F.union("Continuous tabby body and four legs",pieces,.0095,cat_weights)
    # Respect exact size and the contact plane after voxel smoothing.
    for v in body.data.vertices:
        if v.co.z<.019:
            v.co.z=0
    # Material boundaries are exported as linear per-vertex colors in FNT1.
    def stripes(p):
        x,y,z=p
        taper=.64+.34*(1-F.smooth(-.29,-.17,y))
        flank=min(y+.33,z-.33,(math.sin(36*y+3.8*z+2*math.sin(x*9))-taper)*.035)
        legs=min(.38-z,z-.19,(math.sin(z*65+(2 if y>0 else 0))-.65)*.025)
        return max(flank,legs)
    def cream(p):
        x,y,z=p
        bib=min(-.39-y,.10*(1-(x/.155)**2-((z-.51)/.19)**2))
        return max(.145-z,bib)
    paint_boundaries(body,[(stripes,1),(cream,2)],cat_weights)
    # Paired muzzle pads and chin are sculpted into a single cream piece.
    muzzle=F.union("Cream muzzle and chin",
                   [ell("Muzzle pad",(s*.064,-.663,.708),(.085,.064,.061),"cream") for s in (-1,1)]+
                   [ell("Chin",(0,-.632,.661),(.090,.06,.045),"cream")],.0065,"head")
    def surface(x,z):
        return -.48-.216*math.sqrt(max(.05,1-(x/.245)**2-((z-.775)/.212)**2))
    for s,side in ((1,"L"),(-1,"R")):
        eye(side,s*.105,.801,.065,.041,surface,"green",True)
        # Pink ear inset inset into the actual pinna, with rounded corners.
        inset=loft("Pink inner ear."+side,
                   [(s*.159,-.514,.916,.051,.008),(s*.18,-.489,.952,.036,.009),
                    (s*.207,-.454,1.015,.007,.005)],"pink","head",24)
        for j in range(3):
            z=.718-j*.020
            curve("Tabby cheek stripe",[(s*.164,surface(s*.164,z)-.003,z),
                                         (s*.205,surface(s*.205,z+.016)-.003,z+.016)],
                  lambda t:.001+.007*math.sin(math.pi*t),"stripe","head",6,8)
            # Three tapered whiskers per cheek, rooted in small follicle dots.
            xx=s*(.088+j*.011)
            ell("Whisker follicle",(xx,-.720+j*.006,.710-j*.013),(.004,.002,.004),"mouth","head",12,6)
            curve("Whisker",[(xx,-.723+j*.006,.708-j*.013),
                             (s*.22,-.718,.71+(j-1)*.023),
                             (s*.36,-.67,.72+(j-1)*.05)],
                  lambda t:.0018*(1-.8*t),"cream","head",6,8)
    # Three tapered forehead marks and an M-shaped brow marking.
    for x in (-.075,0,.075):
        pts=[(x*(1-.15*t),surface(x*(1-.15*t),.868+.089*t)-.002,.868+.089*t) for t in np.linspace(0,1,18)]
        F.tube("Tabby forehead mark",pts,lambda t:.002+.008*math.sin(math.pi*t),"stripe","head",8)
    nose=loft("Pink heart nose",[(0,-.727,.718,.009,.005),(0,-.73,.738,.029,.012),
                                (0,-.725,.746,.023,.008)],"pink","head",32)
    curve("Philtrum",[(0,-.735,.719),(0,-.735,.697)],.0025,"mouth","head",6)
    for s in (-1,1):
        curve("Cat smile",[(0,-.734,.698),(s*.026,-.724,.688),(s*.051,-.716,.696)],.002,"mouth","head",6)
        for rear in (False,True):
            label=("hind." if rear else "front.")+("L" if s>0 else "R")
            y=.34 if rear else -.37
            for dx in (-.025,.025):
                curve("Paw toe seam",[(s*.20+dx,y-.155,.044),(s*.20+dx,y-.151,.068),
                                      (s*.20+dx,y-.12,.088)],.0017,"hair_shadow","foot."+label,6)
    tail=F.tube("Articulated ringed tail",F.catmull(TAIL,14),lambda t:.061*(1-.66*t),
                "orange",tail_weights,20)
    paint_boundaries(tail,[(lambda p:(math.sin((p.y-.43)*49)-.55),1)],tail_weights)
    return limbs


def rig(name,limbs):
    data=bpy.data.armatures.new(name.title()+" editable two-bone IK skeleton")
    arm=bpy.data.objects.new(name.title()+"Rig",data)
    bpy.context.collection.objects.link(arm)
    F.active(arm)
    arm.show_in_front=True
    bpy.ops.object.mode_set(mode="EDIT")
    def bone(label,head,tail,parent=None,deform=True):
        b=data.edit_bones.new(label)
        b.head,b.tail,b.use_deform=head,tail,deform
        if parent:
            b.parent=data.edit_bones[parent]
        return b
    bone("ROOT",(0,0,0),(0,0,.15),deform=False)
    if name=="gnome":
        bone("hips",(0,0,.61),(0,0,.75),"ROOT")
        bone("chest",(0,0,.75),(0,0,.9),"hips")
        bone("head",(0,0,.9),(0,0,1.1),"chest")
        bone("beard",(0,-.18,.85),(0,-.18,.60),"head")
        bone("hat",(0,.015,1.12),(.13,-.13,1.52),"head")
        for s,side in ((1,"L"),(-1,"R")):
            bone("arm."+side,(s*.19,.015,.85),(s*.28,0,.68),"chest")
            bone("forearm."+side,(s*.28,0,.68),(s*.325,-.025,.53),"arm."+side)
            bone("hand."+side,(s*.325,-.025,.53),(s*.33,-.035,.43),"forearm."+side)
    else:
        bone("hips",(0,.30,.59),(0,.10,.60),"ROOT")
        bone("chest",(0,.10,.60),(0,-.31,.61),"hips")
        bone("head",(0,-.31,.61),(0,-.48,.90),"chest")
        for i in range(4):
            bone("tail.%02d"%(i+1),TAIL[i],TAIL[i+1],"hips" if i==0 else "tail.%02d"%i)
    for limb in limbs:
        k=limb["id"]
        bone("upper."+k,limb["hip"],limb["knee"],limb["parent"])
        bone("lower."+k,limb["knee"],limb["ankle"],"upper."+k)
        toe=Vector(limb["ankle"])+Vector((0,-.12,0))
        bone("foot."+k,limb["ankle"],toe,"lower."+k)
        bone("CTRL_foot."+k,limb["ankle"],toe,"ROOT",False)
        bone("CTRL_pole."+k,limb["pole"],Vector(limb["pole"])+Vector((0,0,.1)),"ROOT",False)
    bpy.ops.object.mode_set(mode="OBJECT")
    controls=data.collections.new("Animator · foot targets and knee-elbow poles")
    deform=data.collections.new("Deformation · weighted body")
    secondary=data.collections.new("Animator · secondary motion")
    for b in data.bones:
        b.inherit_scale="NONE"
        (deform if b.use_deform else controls).assign(b)
        if b.name.startswith(("tail.","hat","beard","head","arm.")):
            secondary.assign(b)
    for pb in arm.pose.bones:
        pb.rotation_mode="XYZ"
        pb.ik_stretch=0
        pb.bone.color.palette="THEME04" if not pb.bone.use_deform else "THEME03"
    for limb in limbs:
        k=limb["id"]
        ik=arm.pose.bones["lower."+k].constraints.new("IK")
        ik.name="Nonstretching two-bone leg IK"
        ik.target,ik.subtarget=arm,"CTRL_foot."+k
        ik.pole_target,ik.pole_subtarget=arm,"CTRL_pole."+k
        ik.chain_count,ik.use_stretch,ik.iterations=2,False,512
        best=(float("inf"),0)
        for angle in (0,math.pi/2,-math.pi/2,math.pi):
            ik.pole_angle=angle
            bpy.context.view_layer.update()
            error=(arm.pose.bones["lower."+k].head-Vector(limb["knee"])).length
            if error<best[0]:
                best=(error,angle)
        ik.pole_angle=best[1]
        for kind,label in (("COPY_ROTATION","Level sole"),("COPY_LOCATION","Exact sole contact")):
            c=arm.pose.bones["foot."+k].constraints.new(kind)
            c.name=label
            c.target,c.subtarget=arm,"CTRL_foot."+k
            c.target_space=c.owner_space="WORLD"
    bpy.context.view_layer.update()
    arm["Walk stride (figure units)"]=STRIDE
    arm["Walk target speed (figure units/sec)"]=1.0
    arm["Rig notes"]="Live two-bone IK. Move CTRL_foot targets, CTRL_pole knee/elbow controls. Feet stay level, chains never stretch."
    arm["limbs_json"]=json.dumps(limbs)
    return arm


def path(phase,stance,lift):
    q=phase%1
    half=STRIDE*stance*.5
    if q<=stance:
        return -half+STRIDE*q,0
    t=(q-stance)/(1-stance)
    y=(2*t**3-3*t*t+1)*half+(t**3-2*t*t+t)*STRIDE*(1-stance)
    y-=( -2*t**3+3*t*t)*half
    y+=(t**3-t*t)*STRIDE*(1-stance)
    return y,lift*math.sin(math.pi*t)**2


def animate(name,arm,limbs):
    arm.animation_data_create()
    idle=bpy.data.actions.new("Idle")
    walk=bpy.data.actions.new("Walk")
    stance=.58 if name=="gnome" else .66
    for action in (idle,walk):
        action.use_fake_user=True
        arm.animation_data.action=action
        for frame in (range(1,FRAMES+2) if action==walk else (1,FRAMES+1)):
            phase=(frame-1)/FRAMES
            wave=math.sin(TAU*phase)
            for pb in arm.pose.bones:
                pb.location=(0,0,0)
                pb.rotation_euler=(0,0,0)
                pb.scale=(1,1,1)
            if action==walk:
                F.world_location(arm,"hips",((.012 if name=="gnome" else .007)*math.sin(TAU*(phase-.1)),0,
                                             (-.07 if name=="gnome" else -.07)+.004*math.cos(TAU*phase*2)))
                arm.pose.bones["head"].rotation_euler.y=.018*math.sin(TAU*phase-.4)
                for limb in limbs:
                    y,z=path(phase+limb["phase"],stance,.078 if name=="gnome" else .075)
                    F.world_location(arm,"CTRL_foot."+limb["id"],(0,y,z))
                if name=="gnome":
                    arm.pose.bones["chest"].rotation_euler.z=-.025*wave
                    for s,side,off in ((1,"L",0),(-1,"R",.5)):
                        arm.pose.bones["arm."+side].rotation_euler.x=.30*math.cos(TAU*(phase+off))
                        arm.pose.bones["forearm."+side].rotation_euler.x=-.07-.05*math.sin(TAU*(phase+off))
                    arm.pose.bones["beard"].rotation_euler.x=.035*math.sin(TAU*phase-.6)
                    arm.pose.bones["hat"].rotation_euler.z=.035*math.sin(TAU*phase-1)
                else:
                    for i in range(4):
                        arm.pose.bones["tail.%02d"%(i+1)].rotation_euler.z=.065*math.sin(TAU*phase-i*.55)
                    arm.pose.bones["chest"].rotation_euler.y=.012*wave
            else:
                F.world_location(arm,"hips",(0,0,-.016))
            for pb in arm.pose.bones:
                pb.keyframe_insert("location",frame=frame,group=pb.name)
                pb.keyframe_insert("rotation_euler",frame=frame,group=pb.name)
        for fc in F.fcurves(action):
            for key in fc.keyframe_points:
                key.interpolation="LINEAR"
            fc.modifiers.new("CYCLES")
    arm.animation_data.action=idle
    bpy.context.scene.frame_set(1)
    return idle,walk


def join(name,arm):
    protected=("Eye","Iris","Pupil","Upper lid","Whisker","Pink","Tabby forehead",
               "Continuous tabby body","Articulated ringed tail")
    for part in F.PARTS:
        if len(part.data.vertices)>300 and not part.name.startswith(protected):
            mod=part.modifiers.new("Economical silhouette and preserved color boundaries","DECIMATE")
            mod.ratio=.65 if name=="gnome" else .60
            mod.delimit={"MATERIAL"}
            F.apply(part,mod)
    bpy.ops.object.select_all(action="DESELECT")
    for part in F.PARTS:
        part.select_set(True)
    bpy.context.view_layer.objects.active=F.PARTS[0]
    bpy.ops.object.join()
    obj=bpy.context.object
    obj.name=name.title()+" · editable weighted surface"
    obj.parent=arm
    mod=obj.modifiers.new("Linear skeletal deformation","ARMATURE")
    mod.object=arm
    mod.use_deform_preserve_volume=False
    for v in obj.data.vertices:
        v.co.z=max(0,v.co.z)
        if name=="gnome":
            v.co.z=min(1.56,v.co.z)
    obj.data.validate()
    obj.data.update()
    F.active(obj)
    bpy.ops.object.mode_set(mode="EDIT")
    bpy.ops.mesh.select_all(action="SELECT")
    bpy.ops.uv.smart_project(angle_limit=math.radians(70),island_margin=.008)
    bpy.ops.object.mode_set(mode="OBJECT")
    return obj


def verify(name,obj,arm,idle,walk,names,source,reopened=False):
    """Independent binary skinning, contacts, solver endpoints and negative controls."""
    raw=(HERE/(name+".fmesh")).read_bytes()
    magic,nv,ni,nb,nf,stride=struct.unpack_from("<4s4If",raw)
    assert magic==b"FNT1" and nf==FRAMES and nb==len(names)
    dtype=np.dtype([("p","<f4",3),("n","<f4",3),("c","<f4",3),("b","<u4",4),("w","<f4",4)])
    vertices=np.frombuffer(raw,dtype=dtype,count=nv,offset=24)
    indices=np.frombuffer(raw,dtype="<u4",count=ni,offset=24+nv*68)
    mats=np.frombuffer(raw,dtype="<f4",offset=24+nv*68+ni*4).reshape(nf+1,nb,3,4)
    assert len(raw)==24+nv*68+ni*4+(nf+1)*nb*48
    assert indices.max()<nv and vertices["b"].max()<nb
    assert np.isfinite(vertices["p"]).all() and np.isfinite(vertices["n"]).all() and np.isfinite(mats).all()
    assert np.max(abs(vertices["w"].sum(axis=1)-1))<2e-7
    assert np.max(abs(np.linalg.norm(vertices["n"],axis=1)-1))<1e-5
    xyz1=np.column_stack([vertices["p"],np.ones(nv)])
    checks,negative=[],[]
    for action,frame,pi in [(idle,1,0)]+[(walk,f,f) for f in (4,11,19,28,37,45)]:
        arm.animation_data.action=action
        bpy.context.scene.frame_set(frame)
        evaluated=obj.evaluated_get(bpy.context.evaluated_depsgraph_get())
        actual_mesh=evaluated.to_mesh()
        actual=np.array([F.C@evaluated.matrix_world@v.co for v in actual_mesh.vertices])[source]
        predicted=np.einsum("nijk,nk,ni->nj",mats[pi][vertices["b"]],xyz1,vertices["w"])
        error=np.linalg.norm(actual-predicted,axis=1)
        checks.append(dict(action=action.name,frame=frame,max_position_error=float(error.max())))
        assert error.max()<2e-6,checks[-1]
        wrong=np.einsum("nijk,nk,ni->nj",mats[0][vertices["b"]],xyz1,vertices["w"])
        negative.append(float(np.linalg.norm(actual-wrong,axis=1).max()))
        evaluated.to_mesh_clear()
    assert max(negative)>.1
    limbs=json.loads(arm["limbs_json"])
    stance=.58 if name=="gnome" else .66
    arm.animation_data.action=walk
    metrics=dict(target=0,chain=0,ground=0,slide=0,level=0,orthonormality=0,minimum_floor=1,swing=0)
    per_limb={limb["id"]:dict(chain=0,swing=0,stance_samples=0) for limb in limbs}
    for step in range(FRAMES*4):
        frame=1+step/4
        bpy.context.scene.frame_set(int(frame),subframe=frame%1)
        phase=(frame-1)/FRAMES
        for limb in limbs:
            k=limb["id"]
            foot,target=arm.pose.bones["foot."+k],arm.pose.bones["CTRL_foot."+k]
            endpoint=(arm.pose.bones["lower."+k].tail-target.head).length
            metrics["chain"]=max(metrics["chain"],endpoint)
            per_limb[k]["chain"]=max(per_limb[k]["chain"],endpoint)
            metrics["target"]=max(metrics["target"],(foot.head-target.head).length)
            sm=foot.matrix@arm.data.bones[foot.name].matrix_local.inverted()
            sole=sm@Vector((limb["ankle"][0],limb["ankle"][1]-.04,0))
            normal=sm.to_3x3()@Vector((0,0,1))
            metrics["minimum_floor"]=min(metrics["minimum_floor"],sole.z)
            metrics["level"]=max(metrics["level"],(normal-Vector((0,0,1))).length)
            q=(phase+limb["phase"])%1
            if q<stance-1/FRAMES:
                per_limb[k]["stance_samples"]+=1
                metrics["ground"]=max(metrics["ground"],abs(sole.z))
                slide=abs((foot.head.y-limb["ankle"][1]-STRIDE*q)-(-STRIDE*stance*.5))
                metrics["slide"]=max(metrics["slide"],slide)
            else:
                metrics["swing"]=max(metrics["swing"],sole.z)
                per_limb[k]["swing"]=max(per_limb[k]["swing"],sole.z)
        for pb in arm.pose.bones:
            m=np.array(pb.matrix.to_3x3())
            error=float(np.max(abs(m.T@m-np.eye(3))))
            if error>3e-5:
                print("NONORTHOGONAL",frame,pb.name,error,flush=True)
            metrics["orthonormality"]=max(metrics["orthonormality"],error)
    print(name,"IK_METRICS",metrics,per_limb,flush=True)
    assert metrics["chain"]<5e-5,metrics
    assert max(metrics[k] for k in ("target","ground","slide","level","orthonormality"))<3e-5,metrics
    assert metrics["minimum_floor"]> -3e-5
    assert min(m["swing"] for m in per_limb.values())>.06
    bpy.context.scene.frame_set(1)
    first=np.array(F.skin_matrices(arm,names))
    bpy.context.scene.frame_set(FRAMES+1)
    loop=float(np.max(abs(np.array(F.skin_matrices(arm,names))-first)))
    assert loop<1e-6
    bounds=np.array([F.C@v.co for v in obj.data.vertices])
    arm.animation_data.action=idle
    bpy.context.scene.frame_set(1)
    evaluated=obj.evaluated_get(bpy.context.evaluated_depsgraph_get())
    evaluated_mesh=evaluated.to_mesh()
    idle_bounds=np.array([F.C@evaluated.matrix_world@v.co for v in evaluated_mesh.vertices])
    evaluated.to_mesh_clear()
    center=np.array((0,.78,0) if name=="gnome" else (0,.53,-.25))
    radius=float(np.max(np.linalg.norm(bounds-center,axis=1)))+.07
    # Check the actual runtime skin, including interpolation, not only an ideal
    # sole point. This catches stray torso weights on flattened paw vertices.
    runtime_floor=1
    runtime_contact=runtime_slide=0
    animated_min,animated_max=idle_bounds.min(axis=0),idle_bounds.max(axis=0)
    contacts={limb["id"]:np.where((abs(vertices["p"][:,1])<1e-7)&
              (vertices["b"][:,0]==names.index("foot."+limb["id"]))&
              (vertices["w"][:,0]>.99999))[0] for limb in limbs}
    assert all(len(ids)>0 for ids in contacts.values())
    for step in range(FRAMES*4):
        phase=step/(FRAMES*4)
        a=step//4
        t=(step%4)/4
        palette=mats[1+a]*(1-t)+mats[1+(a+1)%FRAMES]*t
        positions=np.einsum("nijk,nk,ni->nj",palette[vertices["b"]],xyz1,vertices["w"])
        runtime_floor=min(runtime_floor,float(positions[:,1].min()))
        animated_min=np.minimum(animated_min,positions.min(axis=0))
        animated_max=np.maximum(animated_max,positions.max(axis=0))
        radius=max(radius,float(np.max(np.linalg.norm(positions-center,axis=1)))+.07)
        for limb in limbs:
            q=(phase+limb["phase"])%1
            if q<stance-1/FRAMES:
                ids=contacts[limb["id"]]
                runtime_contact=max(runtime_contact,float(abs(positions[ids,1]).max()))
                displacement=positions[ids,2]-vertices["p"][ids,2]
                expected=STRIDE*stance*.5-STRIDE*q
                runtime_slide=max(runtime_slide,float(abs(displacement-expected).max()))
    assert runtime_floor> -3e-5,("Runtime vertices penetrate floor",runtime_floor)
    assert max(runtime_contact,runtime_slide)<3e-5,(runtime_contact,runtime_slide)
    report=dict(format="FNT1",model=name,runtime_vertices=nv,editable_vertices=len(obj.data.vertices),
                triangles=ni//3,indices=ni,deform_bones=nb,total_bones=len(arm.data.bones),bone_order=names,
                materials=len(obj.data.materials),walk_frames=nf,fps=60,stride_length=float(stride),
                cycle_seconds=.8,target_speed_figure_units_per_second=1,stance_fraction=stance,
                bind_bounds_xyz=[bounds.min(axis=0).tolist(),bounds.max(axis=0).tolist()],
                idle_bounds_xyz=[idle_bounds.min(axis=0).tolist(),idle_bounds.max(axis=0).tolist()],
                animated_bounds_xyz=[animated_min.tolist(),animated_max.tolist()],
                dimensions=(bounds.max(axis=0)-bounds.min(axis=0)).tolist(),
                face_height=.92 if name=="gnome" else .78,
                idle_face_height=.904 if name=="gnome" else .764,
                recommended_interaction_sphere=dict(center=center.tolist(),radius=radius),
                skinning_checks=checks,negative_control_wrong_pose_max_error=max(negative),
                ik=metrics,limbs=per_limb,gait_phase_offsets={l["id"]:l["phase"] for l in limbs},
                runtime_minimum_vertex_height=runtime_floor,runtime_contact_max_error=runtime_contact,
                runtime_stance_slide_max_error=runtime_slide,
                runtime_contact_vertices={k:len(v) for k,v in contacts.items()},
                loop_matrix_max_error=loop,file_bytes=len(raw),saved_blend_reopened=reopened)
    (HERE/(name+"-verification.json")).write_text(json.dumps(report,indent=2)+"\n")
    return report


def stage(name,arm,idle):
    cam,aim=F.stage()
    cam.name=name.title()+" portrait camera"
    scene=bpy.context.scene
    scene.cycles.samples=32
    scene.render.resolution_x=900
    scene.render.resolution_y=1000 if name=="gnome" else 800
    cam.data.ortho_scale=1.9 if name=="gnome" else 2.55
    center=(0,0,.78) if name=="gnome" else (0,.20,.52)
    cam.location=(2.7,-4.8,2.15) if name=="gnome" else (3.2,-4.5,2.15)
    aim(cam,center)
    arm.animation_data.action=idle
    scene.frame_set(1)
    F.active(arm)
    for screen in bpy.data.screens:
        for area in screen.areas:
            if area.type=="VIEW_3D":
                area.spaces.active.region_3d.view_distance=2.8
                area.spaces.active.region_3d.view_location=center
                area.spaces.active.shading.type="MATERIAL"
                area.spaces.active.region_3d.view_rotation=cam.rotation_euler.to_quaternion()
    return cam,aim


def render(name,arm,idle,walk,cam,aim,preview):
    ART.mkdir(parents=True,exist_ok=True)
    scene=bpy.context.scene
    target=(0,0,.76) if name=="gnome" else (0,.20,.50)
    shots=[("front",idle,1,(0,-5,1.1 if name=="gnome" else 1.35)),
           ("three-quarter",idle,1,(2.7,-4.8,2.05)),
           ("walk",walk,38,(2.9,-4.4,1.8))]
    for label,action,frame,location in shots:
        arm.animation_data.action=action
        scene.frame_set(frame)
        cam.location=location
        aim(cam,target)
        scene.render.filepath=str(ART/(name+"-"+label+".png"))
        bpy.ops.render.render(write_still=True)
    if preview:
        scene.cycles.samples=12
        scene.render.resolution_x=scene.render.resolution_y=480
        cam.location=(2.9,-4.8,1.65)
        aim(cam,target)
        arm.animation_data.action=walk
        for frame in range(1,FRAMES+1,2):
            scene.frame_set(frame)
            scene.render.filepath=str(ART/f"{name}-preview-{frame:02d}.png")
            bpy.ops.render.render(write_still=True)


def main():
    parser=argparse.ArgumentParser()
    parser.add_argument("--model",choices=("gnome","cat","both"),default="both")
    parser.add_argument("--render",action="store_true")
    parser.add_argument("--preview",action="store_true")
    args=parser.parse_args(sys.argv[sys.argv.index("--")+1:] if "--" in sys.argv else [])
    for name in (("gnome","cat") if args.model=="both" else (args.model,)):
        setup(name)
        limbs=build_gnome() if name=="gnome" else build_cat()
        arm=rig(name,limbs)
        idle,walk=animate(name,arm,limbs)
        obj=join(name,arm)
        names,source=F.export_fmesh(obj,arm,idle,walk)
        verify(name,obj,arm,idle,walk,names,source)
        F.export_glb(obj,arm,idle,walk)
        cam,aim=stage(name,arm,idle)
        bpy.context.preferences.filepaths.save_version=0
        bpy.ops.wm.save_as_mainfile(filepath=str(HERE/(name+".blend")))
        # Reopen the deliverable and repeat binary, solver and GLB checks on it.
        bpy.ops.wm.open_mainfile(filepath=str(HERE/(name+".blend")))
        arm=bpy.data.objects[name.title()+"Rig"]
        obj=bpy.data.objects[name.title()+" · editable weighted surface"]
        idle,walk=bpy.data.actions["Idle"],bpy.data.actions["Walk"]
        verify(name,obj,arm,idle,walk,names,source,True)
        F.verify_glb(arm,idle,walk)
        if args.render or args.preview:
            render(name,arm,idle,walk,bpy.context.scene.camera,aim,args.preview)
        print("DELIVERED",name,flush=True)


if __name__=="__main__":
    main()
