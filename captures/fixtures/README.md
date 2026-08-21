# Showcase film fixtures

`big-buck-bunny-meadow.mp4` is a four-second, silent excerpt from
*Big Buck Bunny* (2008), frames 00:47–00:51 of the Blender Foundation release.
It is 480×270 H.264 at 24 fps and exists so the world terminal's Film mode can
play recognizable authored content without a network request during capture.

Source: [Blender Foundation's Big Buck Bunny download page](https://peach.blender.org/download/)

License: [Creative Commons Attribution 3.0](https://creativecommons.org/licenses/by/3.0/)

Required attribution for reuse of part of the film:

> (c) copyright 2008, Blender Foundation / www.bigbuckbunny.org

The official YouTube upload is `https://www.youtube.com/watch?v=YE7VzlLtp-4`.
The current `yt-dlp` can identify that source, but YouTube returned HTTP 403
for its signed media URL on 2026-08-21. The checked-in excerpt was therefore
cut from the identical 320×180 MP4 ZIP on Blender Foundation's download host:

```sh
ffmpeg -ss 47 -t 4 -i BigBuckBunny_320x180.mp4 \
  -vf 'scale=480:270:flags=lanczos,fps=24' \
  -c:v libx264 -preset medium -crf 20 -pix_fmt yuv420p \
  -movflags +faststart -an -map_metadata -1 \
  big-buck-bunny-meadow.mp4
```
