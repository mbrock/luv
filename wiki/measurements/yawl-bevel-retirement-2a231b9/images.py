"""Reproduce review images from exact 1920x1080 native captures (Pillow)."""
import json
import math
import pathlib
import sys

from PIL import Image, ImageChops, ImageDraw, ImageStat

source, destination = map(pathlib.Path, sys.argv[1:])
destination.mkdir(parents=True, exist_ok=True)
metrics = {}
control = Image.open(source / "control.png").convert("RGB")
assert control.size == (1920, 1080)
for mode in ("control", "p1", "p2", "p4"):
    path = source / f"{mode}.png"
    if not path.exists():
        continue
    image = Image.open(path).convert("RGB")
    assert image.size == control.size
    image.resize((1280, 720), Image.Resampling.LANCZOS).save(destination / f"{mode}-overview.png")
    image.crop((550, 300, 1350, 750)).save(destination / f"{mode}-detail.png")
    # Exclude changing HUD and fixed inventory. Image differences are not a
    # geometry/appearance acceptance oracle, just a reproducible error measure.
    diff = ImageChops.difference(control.crop((0, 32, 1920, 960)),
                                 image.crop((0, 32, 1920, 960)))
    stats = ImageStat.Stat(diff)
    metrics[mode] = {"mean_absolute_rgb8": sum(stats.mean) / 3,
                     "rms_rgb8": math.sqrt(sum(x * x for x in stats.rms) / 3)}
(destination / "image-metrics.json").write_text(json.dumps(metrics, indent=2) + "\n")

if (source / "control-motion-00.png").exists():
    frames = []
    for index in range(8):
        frame = Image.new("RGB", (1920, 388), "#1b2026")
        draw = ImageDraw.Draw(frame)
        for column, (mode, label) in enumerate((("control", "Control"), ("p2", "2 px"), ("p4", "4 px"))):
            image = Image.open(source / f"{mode}-motion-{index:02}.png").convert("RGB")
            assert image.size == (1920, 1080)
            frame.paste(image.resize((640, 360), Image.Resampling.LANCZOS), (column * 640, 28))
            draw.text((column * 640 + 12, 7), f"{label} — same pose {index + 1}/8", fill="white")
        frames.append(frame)
    for group in range(2):
        sheet = Image.new("RGB", (1920, 388 * 4))
        for row in range(4):
            sheet.paste(frames[group * 4 + row], (0, row * 388))
        sheet.save(destination / f"native-motion-poses-{group}.png")
    frames += frames[-2:0:-1]
    frames[0].save(destination / "native-motion.gif", save_all=True, append_images=frames[1:],
                   duration=250, loop=0, optimize=False)
