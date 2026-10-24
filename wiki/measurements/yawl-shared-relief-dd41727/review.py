from pathlib import Path
from PIL import Image, ImageDraw, ImageFont
import numpy as np
import json
import subprocess
import os

root = Path(os.environ.get('RELIEF_OUTPUT', 'build/shared-relief-quality'))
font = ImageFont.truetype('/System/Library/Fonts/Supplemental/Arial.ttf', 22)

def sheet(mode, names, output, crop=None):
    width, height = (640, 380) if crop else (640, 400)
    image = Image.new('RGB', (width * 2, (height + 36) * len(names)), '#111820')
    draw = ImageDraw.Draw(image)
    for row, name in enumerate(names):
        for col, variant in enumerate(('control', 'shared')):
            source = Image.open(root / f'{variant}-{mode}' / f'{name}-15.png').convert('RGB')
            if crop:
                source = source.crop(crop)
            source = source.resize((width, height), Image.Resampling.LANCZOS)
            x, y = col * width, row * (height + 36)
            image.paste(source, (x, y + 36))
            draw.text((x + 12, y + 5), f'{name} | {"Before" if col == 0 else "Shared relief"} | {mode}', font=font, fill='white')
    image.save(root / output)

for mode in ('native', 'temporal'):
    sheet(mode, [f'page-{i}' for i in range(1, 8)], f'palette-{mode}.jpg')
    sheet(mode, ['bricks-near', 'red-wool-near', 'floor-grazing', 'wall-grazing'],
          f'comparison-{mode}.jpg', (320, 360, 1600, 1120))
    sheet(mode, ['floor-near', 'floor-grazing', 'wall-grazing', 'isometric'],
          f'boundaries-{mode}.jpg')
    sheet(mode, ['grass-near', 'bricks-grazing', 'red-wool-grazing', 'wood-near',
                 'wood-grazing', 'iron-plate-near', 'iron-plate-grazing', 'mosaic-near'],
          f'other-{mode}.jpg', (320, 360, 1600, 1120))

    # One continuous traverse per clip, identical path and independent histories.
    for name in ('bricks-near', 'red-wool-grazing', 'grass-grazing', 'floor-grazing', 'wall-grazing'):
        subprocess.run(['ffmpeg', '-hide_banner', '-loglevel', 'error', '-y',
                        '-framerate', '30', '-start_number', '16', '-i', str(root / f'control-{mode}' / f'{name}-%02d.png'),
                        '-framerate', '30', '-start_number', '16', '-i', str(root / f'shared-{mode}' / f'{name}-%02d.png'),
                        '-filter_complex', '[0:v]scale=960:600[a];[1:v]scale=960:600[b];[a][b]hstack',
                        '-frames:v', '32', '-c:v', 'libx264', '-crf', '18', '-pix_fmt', 'yuv420p',
                        str(root / f'motion-{mode}-{name}.mp4')], check=True)

stats = {}
for mode in ('native', 'temporal'):
    for path in sorted((root / f'shared-{mode}').glob('*-15.png')):
        after = np.asarray(Image.open(path).convert('RGB'), dtype=float)[64:]
        before = np.asarray(Image.open(root / f'control-{mode}' / path.name).convert('RGB'), dtype=float)[64:]
        diff = np.abs(after - before)
        stats[f'{mode}/{path.stem}'] = {'mean_abs_rgb_255': float(diff.mean()),
                                      'max_channel_255': float(diff.max()),
                                      'pixels_over_8': int((diff.max(axis=2) > 8).sum())}
(root / 'image-diff.json').write_text(json.dumps(stats, indent=2) + '\n')

for variant in ('control', 'shared'):
    path = root / f'{variant}.png'
    if path.exists():
        image = Image.open(path).convert('RGB')
        image.resize((1280, 720), Image.Resampling.LANCZOS).save(root / f'{variant}-overview.jpg')
