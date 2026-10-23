"""Save the official CLI's actual responses, including missing/zero fields."""
import json
import pathlib
import subprocess
import sys
import time

session, destination = sys.argv[1:]
root = pathlib.Path(destination)
root.mkdir(parents=True, exist_ok=True)

def query(name, commands):
    args = ['/usr/bin/gpudebug', '-s', session, '--json']
    for command in commands:
        args += ['-c', command]
    for attempt in range(10):
        result = subprocess.run(args, capture_output=True, text=True, check=True)
        values = []
        remaining = result.stdout.strip()
        try:
            while remaining:
                value, end = json.JSONDecoder().raw_decode(remaining)
                values.append(value)
                remaining = remaining[end:].strip()
        except json.JSONDecodeError:
            # profile run returns before its asynchronous analysis has always
            # installed the info handlers. Retry only these read-only queries.
            (root / f'{name}.retry-{attempt}.txt').write_text(result.stdout + result.stderr)
            if attempt == 9:
                raise
            time.sleep(1)
            continue
        (root / (name + '.json')).write_text(result.stdout)
        if result.stderr:
            (root / (name + '.stderr')).write_text(result.stderr)
        return values[-1]

query('status', ['status'])
query('timeline', ['go /performance/timeline', 'info --all'])
query('encoders', ['go /performance/encoders', 'list --all'])
shaders = query('shaders', ['go /performance/shaders', 'list --all'])
for shader in shaders.get('children', []):
    query('shader-' + shader['name'], ['info ' + shader['name']])
groups = query('scene-counter-groups', ['go /performance/encoders/re1/counters', 'list --all'])
for group in groups.get('children', []):
    name = group['name']
    query('scene-' + name, ['go /performance/encoders/re1/counters/' + name, 'list --all'])
query('scene-encoder', ['go /performance/encoders/re1', 'info --all'])
query('shadow-encoder', ['go /performance/encoders/re0', 'info --all'])
