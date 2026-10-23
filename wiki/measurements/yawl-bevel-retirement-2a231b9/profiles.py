"""Archive final structured gpudebug responses; extract unrounded lane counts."""
import gzip
import json
import pathlib
import sys

source, destination = map(pathlib.Path, sys.argv[1:])
destination.mkdir(parents=True, exist_ok=True)


def final_response(path):
    remaining = path.read_text().strip()
    value = None
    while remaining:
        value, end = json.JSONDecoder().raw_decode(remaining)
        remaining = remaining[end:].strip()
    return value


summary = {}
for mode in ("control", "p2", "p4"):
    raw = {path.stem: final_response(path)
           for path in sorted((source / f"retirement-profile-{mode}").glob("*.json"))}
    assert raw
    with gzip.open(destination / f"raw-profile-{mode}.json.gz", "wt") as output:
        json.dump(raw, output, indent=2, ensure_ascii=False)
    counters = {}
    for group in ("scene-fragment_shader", "scene-vertex_shader", "scene-primitives"):
        for counter in raw[group]["children"]:
            if counter.get("values"):
                counters[counter["name"]] = counter["values"][0]["value"]
    summary[mode] = counters
(destination / "counters.json").write_text(json.dumps(summary, indent=2, ensure_ascii=False) + "\n")
print(json.dumps(summary, indent=2, ensure_ascii=False))
