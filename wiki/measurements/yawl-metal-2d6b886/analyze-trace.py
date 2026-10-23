import collections
import json
import os
import statistics
import sys
import xml.etree.ElementTree as ET
from pathlib import Path

root = Path(sys.argv[1] if len(sys.argv) > 1 else '.amp/in/artifacts')
def rows(name):
    refs = {}
    tree = ET.parse(root / name)
    def resolve(e):
        if 'ref' in e.attrib:
            return refs[e.attrib['ref']]
        if 'id' in e.attrib:
            refs[e.attrib['id']] = e
        for child in e:
            resolve(child)
        return e
    for row in tree.iter('row'):
        yield [resolve(e) for e in row]

pid = os.environ.get('TRACE_PID', '80881')
passes = collections.defaultdict(lambda: collections.defaultdict(list))
for r in rows('gpu.xml'):
    process = r[10].find('pid')
    if process is not None and process.text == pid:
        label = r[6].attrib.get('fmt', '').split('  (')[0]
        stage = r[2].text
        entry = (int(r[0].text), int(r[1].text) / 1e6,
                 int(r[17].text), int(r[3].text))
        encoder = (int(r[15].text), int(r[16].text))
        passes[label, stage][encoder].append(entry)
# The same frame numbers cover scene, shadows, MetalFX and finishing. Use
# the last 100 scene frames for every pass, including stages with missing
# events, so an incomplete shadow channel cannot pull warmup frames back in.
frame_numbers = sorted({e[3] for es in passes['Command Buffer 0:luft site streams', 'Vertex'].values()
                        for e in es})[-100:]
measured_frames = set(frame_numbers)
selected = set()
budgets = []
for (label, stage), encoders in sorted(passes.items()):
    # A stage can be split into several intervals in one encoder. Count the
    # encoder once and include every segment, rather than averaging segments
    # as if they were complete frames. Repeated same-label passes remain
    # separate encoders, not one combined per-frame cost.
    entries = [es for es in encoders.values() if es[0][3] in measured_frames]
    if not entries:
        continue
    selected.update(e[2] for es in entries for e in es)
    durations = [sum(e[1] for e in es) for es in entries]
    row = {'pass': label, 'stage': stage, 'n': len(entries),
           'segments': sum(len(es) for es in entries),
           'mean_ms': statistics.mean(durations), 'median_ms': statistics.median(durations)}
    budgets.append(row)
    print('PASS', row)
shaders = []
for r in rows('shader-list.xml'):
    process = r[8].find('pid')
    if process is not None and process.text == pid:
        shaders.append((int(r[0].text), int(r[5].text), int(r[6].text), r[1].text, r[7].text))
totals = collections.defaultdict(lambda: [0, 0])
for r in rows('shader-profile.xml'):
    time, duration, pc = [int(e.text) for e in r[:3]]
    submission = int(r[3].text)
    if submission not in selected:
        continue
    matches = [s for s in shaders if s[0] <= time and s[1] <= pc < s[2]]
    key = max(matches)[3:] if matches else ('unmapped', '')
    totals[key][0] += 1
    totals[key][1] += duration / 1e6
for k, (count, duration) in sorted(totals.items(), key=lambda p: -p[1][1]):
    print(k, count, round(duration, 3))
spills = collections.Counter(int(r[3].text) for r in rows('spills.xml')
                             if r[4].find('pid') is not None and r[4].find('pid').text == pid)
print('SPILLS', spills)
with open(root / 'summary.json', 'w') as out:
    json.dump({'pid': pid, 'frame_numbers': frame_numbers, 'passes': budgets,
               'shader_samples': [{'name': k[0], 'stage': k[1], 'n': v[0], 'duration_ms': v[1]}
                                  for k, v in sorted(totals.items(), key=lambda p: -p[1][1])],
               'spill_events_by_bytes': dict(spills)}, out, indent=2)
