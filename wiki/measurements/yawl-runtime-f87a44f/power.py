"""Align powermetrics interval samples with frame-feedback wall timestamps."""

import bisect
import csv
import datetime
import gzip
import json
import plistlib
import re
import statistics
import sys


def summarize(power_path, frame_path):
    opener = gzip.open if power_path.endswith(".gz") else open
    with opener(power_path, "rb") as stream:
        samples = [plistlib.loads(part) for part in re.findall(rb"<\?xml.*?</plist>", stream.read(), re.S)]
    # Plist dates have whole-second resolution. Accumulated interval lengths
    # and all the dates together bound the origin to much less than a second.
    elapsed = []
    total = 0
    for sample in samples:
        total += sample["elapsed_ns"] / 1e9
        elapsed.append(total)
    dates = [sample["timestamp"].replace(tzinfo=datetime.timezone.utc).timestamp() for sample in samples]
    lower = max(date - time for date, time in zip(dates, elapsed))
    upper = min(date + 1 - time for date, time in zip(dates, elapsed))
    assert lower <= upper
    ends = [(lower + upper) / 2 + time for time in elapsed]
    with open(frame_path) as stream:
        frames = list(csv.DictReader(stream, delimiter="\t"))
    records = []
    for block in sorted({int(row["block"]) for row in frames}):
        rows = [row for row in frames if int(row["block"]) == block]
        for offset in range(0, len(rows), 30):
            window = rows[offset : offset + 30]
            matches = []
            for row in window:
                time = float(row["feedback_unix"])
                index = bisect.bisect_left(ends, time)
                assert 0 < index < len(samples), "Power samples must cover every frame"
                matches.append(samples[index])
            records.append({
                "block": block, "variant": window[0]["variant"], "first_frame": offset,
                "gpu_ms": statistics.mean(float(row["gpu_ms"]) for row in window),
                # Despite the plist key, this GPU field uses MHz on this OS;
                # the text-mode sample and DVFM state frequencies confirm it.
                "gpu_mhz": statistics.mean(sample["gpu"]["freq_hz"] for sample in matches),
                "gpu_idle_ratio": statistics.mean(sample["gpu"]["idle_ratio"] for sample in matches),
                "thermal_pressure": sorted({sample["thermal_pressure"] for sample in matches}),
            })
    return {"origin_uncertainty_seconds": upper - lower, "windows": records}


if __name__ == "__main__":
    print(json.dumps(summarize(*sys.argv[1:]), indent=2))
