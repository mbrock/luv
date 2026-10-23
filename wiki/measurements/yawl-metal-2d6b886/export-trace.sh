#!/bin/sh
set -eu
trace=$1
out=${trace%.trace}
mkdir -p "$out"
for spec in 'shader-list:metal-shader-profiler-shader-list' 'shader-profile:gpu-shader-profiler-interval' 'spills:graphics-compiler-spill-events' 'gpu:metal-gpu-intervals' 'counters:gpu-counter-value' 'counter-info:gpu-counter-info'; do
  name=${spec%%:*}
  schema=${spec#*:}
  /Applications/Xcode.app/Contents/Developer/usr/bin/xctrace export --input "$trace" --xpath "/trace-toc/run/data/table[@schema=\"$schema\"]" > "$out/$name.xml"
done
