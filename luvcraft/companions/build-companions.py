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
    # Gnome-only dyed wool, ivory hair and saddle leather; leave the tabby palette alone.
    for key, color in {"coat":"496C85", "cuff":"68889B", "cream":"F3E3C4",
                       "hair_shadow":"DFCCAA", "skin":"EAB08D", "blush":"DFA084",
                       "leather":"80533D", "sole":"42372F", "hat":"C3333B",
                       "hat_band":"A82D36", "green":"779660"}.items():
        mat = F.MATS[key]
        mat.diffuse_color = (*F.linear_rgb(color), 1)
        mat.node_tree.nodes["Principled BSDF"].inputs["Base Color"].default_value = mat.diffuse_color
    thread = F.MATS["leather"].copy()
    thread.name = "Gnome / flax stitching"
    thread.diffuse_color = (*F.linear_rgb("BF9A69"), 1)
    thread.node_tree.nodes["Principled BSDF"].inputs["Base Color"].default_value = thread.diffuse_color
    F.MATS["gnome_thread"] = thread

    def sweep(name, controls, mat, weights, sides=32):
        """Sculpted elliptical sections following an XZ bend, not stacked horizontal discs."""
        sections = F.catmull(controls, 6)
        vertices, faces = [], []
        for j, section in enumerate(sections):
            p = Vector(section[:3])
            tangent = Vector(sections[min(j+1,len(sections)-1)][:3])-Vector(sections[max(0,j-1)][:3])
            across = Vector((tangent.z, 0, -tangent.x)).normalized()
            depth = tangent.normalized().cross(across).normalized()
            for i in range(sides):
                a = TAU*i/sides
                vertices.append(p + across*max(.0005,section[3])*math.cos(a)
                                + depth*max(.0005,section[4])*math.sin(a))
        for j in range(len(sections)-1):
            for i in range(sides):
                faces.append((j*sides+i,j*sides+(i+1)%sides,
                              (j+1)*sides+(i+1)%sides,(j+1)*sides+i))
        faces.extend([tuple(reversed(range(sides))),
                      tuple((len(sections)-1)*sides+i for i in range(sides))])
        return F.mesh(name,vertices,faces,mat,weights)

    # A short, open-hem work coat: legs occupy the lower third of the silhouette.
    rings = []
    for z, rx, ry in ((.47,.26,.175),(.49,.269,.184),(.54,.273,.19),(.62,.258,.19),
                      (.72,.236,.185),(.80,.209,.16),(.88,.19,.13),(.92,.13,.10)):
        rings.append((0,.02,z,rx,ry))
    coat = loft("Short tailored work coat", rings, "coat", gnome_coat, cap=False)
    # Inward hem thickness, rather than a solid cone base.
    loft("Turned coat hem", [(0,.02,.47,.26,.175),(0,.02,.482,.248,.163)], "cuff", "hips", cap=False)
    loft("Soft standing coat collar",F.catmull(
         [(0,.014,.850,.211,.164),(0,.014,.870,.222,.177),
          (0,.014,.893,.215,.170),(0,.014,.900,.207,.164)],4),
         "coat","chest",48,False)
    pants=[ell("Undercoat pelvis", (0,.025,.55), (.22,.15,.13), "trousers")]
    for s, side in ((1,"L"),(-1,"R")):
        pants.extend([ell("Upper trouser",(s*.135,.0,.46),(.10,.095,.18),"trousers"),
                      ell("Lower trouser",(s*.145,-.012,.285),(.078,.077,.13),"trousers")])
        loft("Last-shaped leather boot."+side,F.catmull(
             [(s*.15,-.055,.029,.106,.168),(s*.15,-.055,.05,.107,.168),
              (s*.15,-.053,.087,.106,.162),(s*.15,-.047,.115,.099,.147),
              (s*.15,-.015,.145,.085,.117),(s*.15,.005,.175,.077,.091),
              (s*.15,.005,.21,.077,.083),
              (s*.15,.005,.242,.077,.082)],5),"leather","foot."+side,48,False)
        loft("Leather boot shaft lining."+side,
             [(s*.15,.005,.242,.077,.082),(s*.15,.005,.238,.068,.074),
              (s*.15,.005,.17,.066,.072)],"leather","foot."+side,40,False)
        sole = ell("Flat leather sole."+side,(s*.15,-.055,.019),(.110,.174,.023),"sole","foot."+side)
        for v in sole.data.vertices:
            v.co.z = max(0,v.co.z)
        welt = [(s*.15+.108*math.cos(a),-.055+.172*math.sin(a),.034)
                for a in np.linspace(0,TAU,65)[:-1]]
        F.tube("Leather sole welt."+side,welt,.006,"leather","foot."+side,8,True)
        for a in np.linspace(math.pi,TAU,20):
            curve("Welt saddle stitch."+side,
                  [(s*.15+r*math.cos(a),-.055+r*1.59*math.sin(a),.039) for r in (.104,.110)],
                  .0013,"gnome_thread","foot."+side,6,2)
        loft("Turned boot collar."+side,
             [(s*.15,.005,.214,.082,.085),(s*.15,.005,.232,.083,.085),
              (s*.15,.005,.240,.077,.079),(s*.15,.005,.228,.074,.077)],
             "leather","foot."+side,40,False)
        # Sleeves are blended into shoulder caps; no animated rigid objects.
        sweep("Soft tailored sleeve."+side,
              [(s*.166,.015,.874,.076,.089),(s*.223,.015,.813,.101,.112),
               (s*.271,.001,.721,.086,.099),(s*.300,-.014,.622,.077,.088),
               (s*.320,-.024,.535,.073,.076)],"coat",sleeve(side))
        ell("Cuff."+side,(s*.32,-.024,.535),(.084,.086,.035),"cuff",sleeve(side))
        F.union("Soft mitten."+side,
                [ell("Palm",(s*.332,-.026,.468),(.073,.069,.09),"leather"),
                 ell("Thumb",(s*.276,-.075,.481),(.034,.040,.061),"leather")],.006,"hand."+side)
        for z in np.linspace(.423,.514,10):
            points=[]
            for zz in (z-.0025,z+.0025):
                x=s*(.356+.007*math.sin((zz-.423)/.091*math.pi))
                y=-.026-.069*math.sqrt(1-((x-s*.332)/.073)**2-((zz-.468)/.09)**2)
                points.append((x,y-.001,zz))
            curve("Mitten saddle stitch."+side,points,.0013,"gnome_thread","hand."+side,6,2)
        # Pockets are shaped cloth panels following the coat, not oval ornaments.
        outline = [(s*.145,.626),(s*.239,.626),(s*.238,.550),
                   (s*.213,.531),(s*.176,.534),(s*.150,.553)]
        def pocket_y(x,z):
            return .02-.19*math.sqrt(max(.08,1-(x/.271)**2))-.012
        border = F.catmull([(x,pocket_y(x,z),z) for x,z in outline],5,True)
        center = Vector((s*.192,pocket_y(s*.192,.579)-.004,.579))
        F.mesh("Tailored patch pocket."+side,[center]+border,
               [(0,1+i,1+(i+1)%len(border)) for i in range(len(border))],"coat",gnome_coat)
        F.tube("Pocket bound edge."+side,border,.002,"cuff",gnome_coat,8,True)
        curve("Pocket flap."+side,[(s*.148,pocket_y(s*.148,.616)-.005,.616),
                                  (s*.190,pocket_y(s*.190,.598)-.007,.598),
                                  (s*.236,pocket_y(s*.236,.616)-.005,.616)],
              .003,"cuff",gnome_coat,8)
        ell("Pocket brass button."+side,(s*.194,pocket_y(s*.194,.604)-.012,.604),
            (.010,.006,.010),"brass",gnome_coat,16,10)
    def pants_weights(p):
        side="L" if p.x>0 else "R"
        hip=F.smooth(.43,.56,p.z)
        return {"hips":hip,**{k:w*(1-hip) for k,w in gnome_leg(side)(p).items()}}
    F.union("Continuous sewn trousers and crotch",pants,.012,pants_weights)
    curve("Coat front piping",[(0,-.17,.49),(0,-.177,.60),(0,-.165,.72)],.005,"cuff",gnome_coat)
    for a in np.linspace(math.pi,TAU,37):
        points = [(.264*math.cos(t),.02+.179*math.sin(t),.487) for t in (a-.014,a+.014)]
        curve("Hem flax stitch",points,.0013,"gnome_thread","hips",6,2)
    for z in (.53,.62,.71):
        ell("Brass coat button",(.022,-.178,z),(.013,.007,.013),"brass",gnome_coat,16,10)

    # Warm face and ears; the nose is joined to the cheeks, not a pasted sphere.
    faceparts = [ell("Face",(0,-.012,.946),(.242,.183,.201),"skin"),
                 ell("Warm nose",(0,-.213,.919),(.082,.081,.064),"skin")]
    for s in (-1,1):
        faceparts.append(ell("Ear",(s*.224,-.005,.94),(.047,.052,.074),"skin"))
        faceparts.append(ell("Lifted smiling cheek",(s*.145,-.145,.935),(.075,.055,.062),"skin"))
    F.union("Sculpted face and round nose",faceparts,.006,"head")
    def surface(x,z):
        # The inner eye corners sit behind the nose, never projected onto its tip.
        y = -.012-.183*math.sqrt(max(.05,1-(x/.242)**2-((z-.946)/.201)**2))
        cheek = 1-((abs(x)-.145)/.075)**2-((z-.935)/.062)**2
        return min(y,-.145-.055*math.sqrt(cheek)) if cheek>0 else y
    for s, side in ((1,"L"),(-1,"R")):
        eye(side,s*.102,.988,.043,.029,surface,"green")
        lower = [(s*.102+.043*math.cos(a),surface(s*.102+.043*math.cos(a),.988+.026*math.sin(a))-.013,
                  .988+.026*math.sin(a)) for a in np.linspace(math.pi+.12,TAU-.12,20)]
        F.tube("Smiling lower lid."+side,lower,lambda t:.002+.003*math.sin(math.pi*t),"skin","head",8)
        curve("Cream expressive brow."+side,
              [(s*.044,surface(s*.044,1.030)-.012,1.030),
               (s*.083,surface(s*.083,1.047)-.014,1.047),
               (s*.123,surface(s*.123,1.044)-.012,1.044),
               (s*.157,surface(s*.157,1.031)-.009,1.031)],
              lambda t:.002+.012*math.sin(math.pi*t)**.5,"cream","head",12)
        ell("Ear inner."+side,(s*.246,-.041,.946),(.018,.01,.038),"blush","head",20,12)

    def beard_weights(p):
        return blend("head","beard",1-F.smooth(.65,.88,p.z))
    # The broad locks have real flowing volume; no contrasting scratches on a blob.
    beard_start = len(F.PARTS)
    loft("Ivory beard foundation",
         [(0,-.12,.894,.171,.069),(0,-.15,.85,.184,.090),
          (0,-.167,.79,.177,.102),(.003,-.162,.72,.152,.096),
          (.013,-.159,.66,.110,.074),(.018,-.169,.624,.056,.040),
          (.017,-.174,.610,.004,.004)],"cream",beard_weights)
    for s in (-1,1):
        sweep("Swept cheek whisker",[(s*.174,-.095,.952,.008,.008),
                                     (s*.203,-.155,.885,.054,.052),
                                     (s*.187,-.181,.805,.057,.055),
                                     (s*.150,-.189,.735,.043,.040),
                                     (s*.147,-.184,.697,.001,.001)],"cream",beard_weights)
        sweep("Outer flowing beard lock",[(s*.139,-.206,.856,.030,.024),
                                          (s*.144,-.234,.789,.053,.031),
                                          (s*.109,-.230,.711,.044,.030),
                                          (s*.089,-.209,.649,.023,.022),
                                          (s*.104,-.187,.620,.001,.001)],"cream",beard_weights)
        sweep("Inner flowing beard lock",[(s*.053,-.225,.864,.006,.006),
                                          (s*.064,-.255,.820,.029,.020),
                                          (s*.075,-.269,.786,.037,.025),
                                          (s*.056,-.253,.723,.033,.028),
                                          (s*.033,-.224,.670,.018,.020),
                                          (s*.041,-.206,.638,.001,.001)],"cream",beard_weights)
    sweep("Central swept beard tip",[(-.010,-.241,.830,.006,.005),
                                     (-.014,-.264,.790,.033,.020),
                                     (-.016,-.274,.744,.047,.026),
                                     (.009,-.248,.671,.044,.026),
                                     (.034,-.210,.606,.029,.022),
                                     (.016,-.171,.567,.001,.001)],"cream",beard_weights)
    beard = F.union("Continuous flowing ivory beard",list(F.PARTS[beard_start:]),.004,beard_weights)
    # Shallow comb marks are sculpted into the front surface, so they survive FNT1.
    grooves=[]
    for s in (-1,1):
        for path in [[(s*.185,.911),(s*.211,.864),(s*.192,.805),(s*.163,.761)],
                     [(s*.127,.839),(s*.143,.790),(s*.113,.728),(s*.090,.677)],
                     [(s*.060,.825),(s*.079,.782),(s*.061,.734),(s*.038,.687)]]:
            points=F.catmull(path,10)
            grooves.extend((p,math.sin(math.pi*i/(len(points)-1))**.6)
                           for i,p in enumerate(points))
    for v in beard.data.vertices:
        if v.normal.y<-.25:
            p=Vector((v.co.x,v.co.z))
            depth=max(.0038*fade*math.exp(-(p-q).length_squared/.000035) for q,fade in grooves)
            v.co.y+=depth*min(1,(-v.normal.y-.25)/.45)
    beard.data.update()
    for s in (-1,1):
        curve("Swept moustache",[(s*.009,-.271,.878),(s*.067,-.283,.861),
                                 (s*.124,-.260,.867),(s*.163,-.226,.900)],
              lambda t:.0015+.030*math.sin(math.pi*t)**.65,"cream","head",16,10)
    F.oval_patch("Warm smiling mouth",(0,.847),(.050,.009),
                 lambda x,z:-.276+.014*(x/.054)**2,"mouth","head",40,5)
    curve("Soft smiling lower lip",[(-.046,-.267,.845),(0,-.279,.838),(.046,-.267,.845)],
          lambda t:.001+.003*math.sin(math.pi*t),"skin","head",8)

    def hat_weights(p):
        return blend("head","hat",F.smooth(1.10,1.40,p.z))
    # A softly turned tip follows its centerline instead of pinching horizontal rings.
    controls = [(0,.006,1.075,.267,.205),(0,.012,1.115,.260,.201),
                (.004,.020,1.20,.222,.173),(.021,.020,1.29,.179,.142),
                (.050,.002,1.38,.131,.106),(.089,-.027,1.448,.082,.071),
                (.141,-.054,1.476,.049,.045),(.191,-.076,1.450,.031,.029),
                (.210,-.095,1.406,.018,.018),(.205,-.105,1.383,.001,.001)]
    sweep("Softly folded scarlet felt hat",controls,"hat",hat_weights,64)
    brim=[]
    for t in np.linspace(0,TAU,17):
        brim.append((0,.005,1.084+.026*math.sin(t),.269+.019*math.cos(t),.207+.017*math.cos(t)))
    loft("Rolled scarlet brim",brim,"hat_band","head",64,False)
    limbs=[]
    for s,side,phase in ((1,"L",0),(-1,"R",.5)):
        limbs.append(dict(id=side,hip=(s*.15,0,.61),knee=(s*.15,-.10,.355),
                          ankle=(s*.15,0,.12),pole=(s*.15,-.70,.36),parent="hips",phase=phase))
    return limbs


TAIL = [(0,.45,.68),(.065,.69,.75),(.09,.91,.93),(.055,1.09,1.08),(-.07,1.16,1.08)]
CAT_LIMBS = [dict(id=("hind." if rear else "front.")+side,
                  hip=(s*(.15 if rear else .14),.34 if rear else -.28,.63 if rear else .64),
                  knee=(s*(.16 if rear else .145),.16 if rear else -.22,.37 if rear else .35),
                  ankle=(s*(.16 if rear else .145),.40 if rear else -.29,.13 if rear else .10),
                  pole=(s*.16,-.65 if rear else .32,.35),
                  parent="hips" if rear else "chest",
                  phase={"hind.L":0,"front.L":.75,"hind.R":.5,"front.R":.25}[
                      ("hind." if rear else "front.")+side])
             for s,side in ((1,"L"),(-1,"R")) for rear in (False,True)]


def tail_weights(p):
    # Blend around the actual joints, not equally spaced world-Y slices.
    nearest=(float("inf"),0)
    for i in range(4):
        a,b=Vector(TAIL[i]),Vector(TAIL[i+1])
        axis=b-a
        t=max(0,min(1,(p-a).dot(axis)/axis.length_squared))
        nearest=min(nearest,((p-a-axis*t).length,i+t))
    u=max(0,min(3,nearest[1]-.5))
    i=min(2,int(u))
    weights=blend("tail.%02d"%(i+1),"tail.%02d"%(i+2),u-i)
    root=1-F.smooth(0,.35,nearest[1])
    return {"hips":root,**{k:w*(1-root) for k,w in weights.items()}}


def cat_weights(p):
    head=F.smooth(.62,.77,p.z)*(1-F.smooth(-.36,-.23,p.y))
    if head>=1:
        return {"head":1}
    nearest=(float("inf"),0)
    for i,limb in enumerate(CAT_LIMBS):
        hip,knee,ankle=[Vector(limb[k]) for k in ("hip","knee","ankle")]
        for a,b in ((hip,knee),(knee,ankle)):
            axis=b-a
            t=max(0,min(1,(p-a).dot(axis)/axis.length_squared))
            nearest=min(nearest,((p-a-axis*t).length,i))
    distance,i=nearest
    limb=CAT_LIMBS[i]
    label=limb["id"]
    # Blend across the ankle itself. A high, long sock of rigid foot weights
    # turns the lower shin into a ribbon when the level paw opposes a swing.
    cuff=limb["ankle"][2]-.01
    if p.z<cuff:
        return {"foot."+label:1}
    # The midline abdomen never follows a leg. On the lateral shoulder/thigh,
    # fade broadly into the torso to avoid the old hard epaulette-like crease.
    # Below the knee there is no torso: even toes far from the chain belong
    # entirely to the limb, never partially to chest/hips.
    amount=(1-F.smooth(.30,.48,p.z)*F.smooth(.065,.145,distance))*(1-F.smooth(.36,.67,p.z))
    amount*=(1-F.smooth(.30,.48,p.z)*(1-F.smooth(.02,.10,abs(p.x))))*(1-head)
    if p.z<cuff+.04:
        chain=blend("foot."+label,"lower."+label,F.smooth(cuff,cuff+.04,p.z))
    else:
        chain=blend("lower."+label,"upper."+label,
                    F.smooth(limb["knee"][2]-.065,limb["knee"][2]+.065,p.z))
    torso=blend("chest","head",head) if head>0 else blend("chest","hips",F.smooth(0,.29,p.y))
    return {**{k:w*(1-amount) for k,w in torso.items()},
            **{k:w*amount for k,w in chain.items()}}


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
    seen=set()
    for points,material in polygons:
        face=[]
        for p in points:
            key=tuple(round(float(c),7) for c in p)
            if key not in lookup:
                lookup[key]=len(vertices)
                vertices.append(p)
            face.append(lookup[key])
        # A cut can meet an existing vertex. Welding then collapses an edge;
        # repeated indices in the same face can hang BMesh's normal traversal.
        face=list(dict.fromkeys(face))
        key=tuple(sorted(face))
        if len(face)>=3 and key not in seen:
            seen.add(key)
            faces.append(face)
            materials.append(material)
    replacement=F.mesh(obj.name,vertices,faces,["orange","stripe","cream"],weights,materials)
    F.PARTS.remove(obj)
    bpy.data.objects.remove(obj,do_unlink=True)
    return replacement


def build_cat():
    # Cat-local colors: do not change the gnome's shared cream, eyes or skin.
    for key,color in {"orange":"D88B3F","stripe":"A46135","cream":"F3E4C5",
                      "green":"91B871","iris_dark":"475D36","pink":"CE8C8A"}.items():
        mat=F.MATS[key]
        mat.diffuse_color=(*F.linear_rgb(color),1)
        bsdf=mat.node_tree.nodes.get("Principled BSDF")
        bsdf.inputs["Base Color"].default_value=mat.diffuse_color
        if key in {"green","iris_dark"}:
            bsdf.inputs["Roughness"].default_value=.30
            bsdf.inputs["Normal"].default_value=(0,0,0)
            for link in list(bsdf.inputs["Normal"].links):
                mat.node_tree.links.remove(link)

    # The chest is deep, the waist is tucked, and the pelvis broadens again.
    # Longitudinal cross-sections avoid a barrel assembled from round blobs.
    sections=F.catmull([(-.43,.62,.035,.055),(-.34,.625,.13,.155),
                       (-.17,.63,.163,.183),(.02,.65,.15,.15),
                       (.21,.658,.128,.115),(.37,.645,.158,.143),
                       (.49,.635,.10,.10),(.535,.64,.012,.02)],5)
    vertices,faces=[],[]
    for y,z,rx,rz in sections:
        for i in range(48):
            a=TAU*i/48
            vertices.append((rx*math.cos(a),y,z+rz*math.sin(a)))
    for j in range(len(sections)-1):
        for i in range(48):
            faces.append((j*48+i,j*48+(i+1)%48,(j+1)*48+(i+1)%48,(j+1)*48+i))
    faces.extend([tuple(reversed(range(48))),tuple((len(sections)-1)*48+i for i in range(48))])
    pieces=[F.mesh("Tucked thorax and lumbar waist",vertices,faces,"orange"),
            ell("Rising neck",(0,-.343,.735),(.123,.135,.19),"orange"),
            F.tube("Neck into withers",F.catmull([(0,-.33,.78),(0,-.23,.738),(0,-.13,.70)],10),
                   .103,"orange",None,24),
            ell("Feline cranial wedge",(0,-.49,.817),(.205,.180,.183),"orange")]
    for s in (-1,1):
        pieces.extend([ell("Cheekbone",(s*.123,-.527,.758),(.093,.119,.088),"orange"),
                       ell("Scapula",(s*.112,-.24,.587),(.078,.125,.165),"orange")])
        thigh=ell("Folded haunch",(0,0,0),(.10,.105,.18),"orange")
        tilt=Vector((0,.18,.26)).to_track_quat("Z","Y")
        for v in thigh.data.vertices:
            v.co=tilt@v.co+Vector((s*.124,.275,.535))
        pieces.append(thigh)
        pieces.append(loft("Outer ear",[(s*.143,-.449,.88,.078,.061),
                                         (s*.168,-.439,.975,.063,.043),
                                         (s*.194,-.426,1.043,.028,.024),
                                         (s*.201,-.422,1.088,.004,.006)],"orange",None,40))
    for limb in CAT_LIMBS:
        hip,knee,ankle=[limb[k] for k in ("hip","knee","ankle")]
        rear=limb["id"].startswith("hind")
        # Tapered forearms and folded hind shanks, not four identical cylinders.
        # Bury the proximal cap inside the torso; the scapula/thigh, rather
        # than an exposed tube end, supplies the muscle's outer silhouette.
        root=(hip[0]*.60,hip[1],hip[2])
        pieces.append(F.tube("Articulated leg",F.catmull([root,knee,ankle],12),
                             lambda t:.035+(.060 if rear else .040)*math.sin(math.pi*t)*(1-t),
                             "orange",None,24))
        pieces.append(ell("Pastern",(ankle[0],ankle[1]-.004,.085),(.035,.036,.058),"orange"))
        pieces.append(ell("Oval paw",(ankle[0],ankle[1]-.035,.045),(.063,.095,.047),"orange"))
        for dx in (-.037,0,.037):
            pieces.append(ell("Rounded toe",(ankle[0]+dx,ankle[1]-.100+abs(dx)*.20,.038),
                              (.025,.033,.034),"orange",segments=24,rings=16))
    body=F.union("Continuous tabby body and four legs",pieces,.0075,cat_weights)
    transitions=body.vertex_groups.new(name="Sculpt shoulder and haunch transitions")
    for v in body.data.vertices:
        w=F.smooth(.28,.43,v.co.z)*(1-F.smooth(.70,.84,v.co.z))
        transitions.add([v.index],w,"REPLACE")
    mod=body.modifiers.new("Soften muscle insertions, preserve toes and ears","SMOOTH")
    mod.factor,mod.iterations,mod.vertex_group=.65,18,transitions.name
    F.apply(body,mod)
    for v in body.data.vertices:
        if v.co.z<.014:
            v.co.z=0
    def stripes(p):
        x,y,z=p
        taper=.58+.42*(1-F.smooth(.43,.64,z))
        flank=min(y+.29,z-.43,(math.sin(34*y+4.5*z+.8*math.cos(x*12))-taper)*.04)
        legs=min(.39-z,z-.20,(math.sin(z*61+(1 if y>0 else 0))-.72)*.025)
        return max(flank,legs)
    def cream(p):
        x,y,z=p
        bib=min(-.375-y,.10*(1-(x/.115)**2-((z-.575)/.185)**2))
        socks=(.14 if y>0 else .115)+.008*math.cos(x*35)-z
        return max(socks,bib)
    body=paint_boundaries(body,[(stripes,1),(cream,2)],cat_weights)
    F.union("Cream muzzle and chin",
            [ell("Muzzle pad",(s*.051,-.655,.731),(.064,.060,.048),"cream") for s in (-1,1)]+
            [ell("Chin",(0,-.631,.686),(.072,.052,.035),"cream")],.0045,"head")

    # All lenses and markings conform to the actual remeshed head, rather than
    # an ellipsoid approximation that floats above the cheeks in profile.
    def surface(x,z):
        hit,point,normal,index=body.ray_cast(Vector((x,-2,z)),Vector((0,1,0)))
        assert hit,(x,z)
        return point.y

    def almond(label,s,rx,rz,depth,material):
        vertices=[(s*.096,surface(s*.096,.833)-depth-.006,.833)]
        faces=[]
        for j in range(1,7):
            r=j/6
            for i in range(48):
                a=TAU*i/48
                x=s*.096+r*rx*math.cos(a)
                z=.833+r*rz*math.sin(a)*abs(math.sin(a))**.32+s*(x-s*.096)*.09
                vertices.append((x,surface(x,z)-depth-.006*(1-r*r),z))
        for i in range(48):
            faces.append((0,1+i,1+(i+1)%48))
        for j in range(5):
            for i in range(48):
                a=1+j*48+i
                b=1+j*48+(i+1)%48
                faces.append((a,b,b+48,a+48))
        F.mesh("Eye "+label,vertices,faces,material,"head")

    for s,side in ((1,"L"),(-1,"R")):
        almond("socket."+side,s,.068,.044,.002,"iris_dark")
        almond("jade lens."+side,s,.062,.039,.004,"green")
        F.oval_patch("Pupil."+side,(s*.096,.833),(.016,.030),
                     lambda x,z:surface(x,z)-.014,"pupil","head",40,6)
        F.oval_patch("Eye glint."+side,(s*.096-.010,.849),(.006,.008),
                     lambda x,z:surface(x,z)-.016,"catch","head",24,4)
        points=[]
        for a in np.linspace(.04,math.pi-.04,24):
            x=s*.096+.064*math.cos(a)
            z=.833+.042*math.sin(a)**1.32+s*(x-s*.096)*.09
            points.append((x,surface(x,z)-.005,z))
        F.tube("Upper lid."+side,points,.004,"orange","head",8)
        loft("Pink inner ear."+side,
             [(s*.146,-.501,.941,.043,.006),(s*.171,-.481,.981,.036,.006),
              (s*.194,-.447,1.044,.013,.005),(s*.199,-.433,1.066,.002,.003)],
             "pink","head",32)
        for j in range(3):
            z=.777-j*.021
            vertices=[]
            for t in np.linspace(0,1,16):
                x=s*(.145+.048*t)
                for edge in (-1,1):
                    zz=z+.020*t+edge*(.0002+.0035*math.sin(math.pi*t))
                    vertices.append((x,surface(x,zz)-.0006,zz))
            F.mesh("Tabby cheek stripe",vertices,[(2*i,2*i+1,2*i+3,2*i+2) for i in range(15)],
                   "stripe","head")
            xx=s*(.065+j*.009)
            yy=-.655-.060*math.sqrt(max(.1,1-((abs(xx)-.051)/.064)**2-((.739-j*.012-.731)/.048)**2))
            zz=.739-j*.012
            ell("Whisker follicle",(xx,yy-.001,zz),(.0025,.0015,.0025),"mouth","head",12,8)
            curve("Whisker",[(xx,yy-.002,zz),(s*.19,-.701,.73+(1-j)*.022),
                             (s*.325,-.655,.74+(1-j)*.052)],
                  lambda t:.0013*(1-.85*t),"cream","head",6,8)
    for x in (-.062,0,.062):
        vertices=[]
        for t in np.linspace(0,1,22):
            z=.891+.079*t
            for edge in (-1,1):
                xx=x*(1-.28*t)+edge*(.0002+.007*math.sin(math.pi*t))
                vertices.append((xx,surface(xx,z)-.0006,z))
        F.mesh("Tabby forehead mark",vertices,[(2*i,2*i+1,2*i+3,2*i+2) for i in range(21)],
               "stripe","head")
    loft("Pink heart nose",[(0,-.711,.747,.004,.004),(0,-.714,.764,.023,.010),
                             (0,-.709,.771,.019,.006)],"pink","head",32)
    curve("Philtrum",[(0,-.716,.747),(0,-.716,.725)],.0018,"mouth","head",6)
    for s in (-1,1):
        curve("Cat smile",[(0,-.716,.725),(s*.025,-.711,.713),(s*.045,-.707,.718)],
              .0017,"mouth","head",6)
    for limb in CAT_LIMBS:
        x,y,z=limb["ankle"]
        for dx in (-.019,.019):
            curve("Paw toe seam",[(x+dx,y-.129,.024),(x+dx,y-.126,.043),
                                  (x+dx,y-.107,.065)],.0012,"hair_shadow","foot."+limb["id"],6)
    tail=F.tube("Articulated ringed tail",F.catmull(TAIL,18),
                lambda t:.050*(1-.60*t)*math.sqrt(max(.005,1-(max(0,t-.90)/.10)**2)),
                "orange",tail_weights,24)
    paint_boundaries(tail,[(lambda p:math.sin((p.y-.45)*47+(p.z-.68)*8)-.62,1)],tail_weights)
    return CAT_LIMBS


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
    if name=="cat":
        # Toe/ankle vertices following the torso can tear while all soles
        # still pass contact tests. Check the exported influences themselves.
        low=vertices["p"][:,1]<.30
        torso=np.isin(vertices["b"],[names.index(k) for k in ("hips","chest","head")])
        assert not np.any((vertices["w"]>0)&torso&low[:,None]),"Cat lower legs follow torso"
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
