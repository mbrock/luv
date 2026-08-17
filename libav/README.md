# FFmpeg's libav* from Lisp

The `luv/libav` ASDF system binds enough of FFmpeg to ask what this build can
do, to describe a picture, and to hold a decoded frame with explicit
ownership. It exists because hardware video decode is the one path that hands
luv a frame already on the GPU: VideoToolbox into a `CVPixelBuffer` on Darwin,
VAAPI into a dmabuf or Vulkan Video straight into a `VkImage` on Linux. FFmpeg
is the single front door to all of them, so the platform difference collapses
into unwrapping a different handle out of the same `AVFrame`.

There is no decoder here yet. This is the layer underneath one.

## The groveled layer

`abi.lisp` is a `cffi-grovel` file rather than a transcription. `AVFrame`'s
declaration order says nothing about its memory order -- `flags` lands between
`buf` and `hw_frames_ctx`, `duration` sits past everything else -- and the
layout moves between major versions. Groveling asks the compiler that will
actually be linked against.

Two things about grovel are worth knowing before editing that file:

- `constant` guards every name it emits with `#ifdef`, so it can only see
  preprocessor definitions. The `AV_PIX_FMT_*` and `AV_HWDEVICE_TYPE_*` names
  are enum members, and asking for them with `constant` fails *silently* --
  the variable is simply never defined. They need `cenum`.
- Header search comes from `pkg-config-cflags`, so no store path is written
  down. The Nix environment puts FFmpeg's `.pc` files on `PKG_CONFIG_PATH`;
  an ordinary system install works the same way.

## The version agreement

libavutil, libavcodec, and libavformat version together, and FFmpeg only
supports combinations from a single build. A mismatch between the headers this
system groveled and the libraries it loads does not fail at a call site -- it
quietly reads a field from the wrong offset. So `load-libav` compares the
groveled `LIBAV*_VERSION_MAJOR` against what each loaded library reports and
signals `libav-version-mismatch` when they disagree.

The sonames are built from those same groveled majors, so the version this
system was compiled against is stated exactly once.

`LUV_FFMPEG_LIBDIR`, which the Nix environment sets to the exact store path,
is consulted before the platform search. This matters more than it looks:
CFFI explodes `LD_LIBRARY_PATH` itself on both Darwin and Linux before
consulting the system loader, and without the pin an unqualified soname is
free to find a Homebrew FFmpeg instead.

## Running it

```sh
./scripts/dev sbcl --non-interactive \
  --eval '(require :asdf)' \
  --eval '(asdf:load-asd (truename "luv.asd"))' \
  --eval '(asdf:test-system :luv/libav)'
```

`make test` runs the same suite, since `scripts/test.lisp` lists the system.

From Lisp:

```lisp
(asdf:load-system :luv/libav)

(libav:libav-build)             ; => "8.1.2"
(libav:hardware-device-types)   ; => (:videotoolbox :opencl) on this Mac
(libav:decoder-available-p "av1")

(libav:with-frame (frame)
  (setf (libav:frame-width frame) 1920
        (libav:frame-height frame) 1080
        (libav:frame-pixel-format frame) :nv12)
  (libav:allocate-frame-buffer frame)
  (libav:frame-plane-pitch frame 0))
```

A frame carries two lifetimes: the struct, released by `release-frame`, and
the reference-counted buffers it points at, dropped by `unreference-frame`
without disturbing the struct. A decode loop reuses one frame across thousands
of pictures by unreferencing between them.

When `frame-hardware-p` is true the picture never touched the CPU, and
`frame-plane-pointer`'s third plane is a platform surface rather than pixels.
