"""Validate and summarize the per-frame TSV, without third-party packages."""

import csv
import json
import statistics
import sys
from collections import defaultdict


def summarize_adjacent(path, rows):
    variants = list(dict.fromkeys(row["variant"] for row in rows))
    order = variants + variants[::-1]
    assert [int(row["frame"]) for row in rows] == list(range(len(rows)))
    assert [row["variant"] for row in rows] == (order * (len(rows) // len(order)))
    assert len(rows) % 120 == 0
    windows = []
    for offset in range(0, len(rows), 120):
        group = rows[offset : offset + 120]
        windows.append({
            variant: {
                field: statistics.mean(float(row[field]) for row in group if row["variant"] == variant)
                for field in ("gpu_ms", "completion_ms", "cpu_encode_ms")
            }
            for variant in variants
        })
    result = {"path": path, "frames": len(rows), "windows_120_frames": windows, "variants": {}}
    for variant in variants:
        selected = [row for row in rows if row["variant"] == variant]
        stats = {"frames": len(selected)}
        for field in ("gpu_ms", "completion_ms", "cpu_encode_ms"):
            values = [float(row[field]) for row in selected]
            stats[field] = {"mean": statistics.mean(values), "median": statistics.median(values)}
            if variant != "control":
                deltas = [window["control"][field] - window[variant][field] for window in windows]
                stats[field]["window_saving_ms_range"] = [min(deltas), max(deltas)]
                stats[field]["faster_windows"] = sum(delta > 0 for delta in deltas)
        result["variants"][variant] = stats
    return result


def summarize(path):
    with open(path) as stream:
        rows = list(csv.DictReader(stream, delimiter="\t"))
    if "block" not in rows[0]:
        return summarize_adjacent(path, rows)
    blocks = defaultdict(list)
    for row in rows:
        blocks[int(row["block"])].append(row)
    assert list(blocks) == list(range(1, len(blocks) + 1))
    counts = {len(values) for values in blocks.values()}
    assert len(counts) == 1
    variants = list(dict.fromkeys(row["variant"] for row in rows))
    order = variants + variants[::-1]
    assert len(blocks) % len(order) == 0
    records = []
    for block, values in blocks.items():
        variant = values[0]["variant"]
        assert variant == order[(block - 1) % len(order)]
        assert {row["variant"] for row in values} == {variant}
        assert [int(row["frame"]) for row in values] == list(range(len(values)))
        record = {"block": block, "variant": variant}
        for field in ("gpu_ms", "completion_ms", "cpu_encode_ms"):
            samples = [float(row[field]) for row in values]
            assert all(value > 0 for value in samples)
            record[field] = statistics.mean(samples)
        records.append(record)
    result = {"path": path, "frames": len(rows), "blocks": records, "variants": {}}
    for variant in variants:
        selected = [record for record in records if record["variant"] == variant]
        stats = {"blocks": len(selected)}
        for field in ("gpu_ms", "completion_ms", "cpu_encode_ms"):
            means = [record[field] for record in selected]
            stats[field] = {"mean": statistics.mean(means), "block_range": [min(means), max(means)]}
        samples = [float(row["gpu_ms"]) for row in rows if row["variant"] == variant]
        stats["gpu_ms"]["median"] = statistics.median(samples)
        histogram = defaultdict(int)
        for value in samples:
            histogram[int(value)] += 1
        stats["gpu_ms"]["histogram_1ms"] = dict(sorted(histogram.items()))
        result["variants"][variant] = stats
    result["rounds"] = []
    for offset in range(0, len(records), len(order)):
        group = records[offset : offset + len(order)]
        result["rounds"].append({
            variant: {
                field: statistics.mean(record[field] for record in group if record["variant"] == variant)
                for field in ("gpu_ms", "completion_ms")
            }
            for variant in variants
        })
    return result


if __name__ == "__main__":
    print(json.dumps([summarize(path) for path in sys.argv[1:]], indent=2))
