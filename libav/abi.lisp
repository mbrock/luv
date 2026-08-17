;;;; FFmpeg's own headers, read by the C compiler rather than by hand.
;;;;
;;;; AVFrame is a struct whose declaration order says nothing about its memory
;;;; order -- `flags' lands between `buf' and `hw_frames_ctx', `duration' sits
;;;; past the end of everything interesting -- and its layout moves across
;;;; major versions.  Groveling asks the same compiler and headers the library
;;;; was built against, so the offsets are right by construction instead of by
;;;; transcription.
;;;;
;;;; The header search path comes from pkg-config, so nothing here names a
;;;; store path: the Nix environment puts FFmpeg's .pc files on
;;;; PKG_CONFIG_PATH and an ordinary system install works the same way.

(in-package #:luv.libav)

(pkg-config-cflags "libavutil")
(pkg-config-cflags "libavcodec")
(pkg-config-cflags "libavformat")
(pkg-config-cflags "libswscale")

(include "errno.h")
(include "libavutil/avutil.h")
(include "libavutil/buffer.h")
(include "libavutil/frame.h")
(include "libavutil/hwcontext.h")
(include "libavutil/pixfmt.h")
(include "libavutil/rational.h")
(include "libavcodec/avcodec.h")
(include "libavcodec/version.h")
(include "libavformat/avformat.h")
(include "libavformat/version.h")
(include "libswscale/swscale.h")
(include "libswscale/version.h")

;;; The compile-time half of the version agreement.  LOAD-LIBAV compares these
;;; against what the loaded shared libraries report at run time; see ffi.lisp.

(constant (+avutil-version-major+ "LIBAVUTIL_VERSION_MAJOR"))
(constant (+avcodec-version-major+ "LIBAVCODEC_VERSION_MAJOR"))
(constant (+avformat-version-major+ "LIBAVFORMAT_VERSION_MAJOR"))
(constant (+swscale-version-major+ "LIBSWSCALE_VERSION_MAJOR"))

;;; AV-FRAME below spells its two arrays as 8 elements because :COUNT wants a
;;; literal.  This constant is what lets FFI.LISP assert that the header still
;;; agrees with that 8 rather than trusting it.

(constant (+data-pointer-count+ "AV_NUM_DATA_POINTERS"))

(constant (+frame-flag-key+ "AV_FRAME_FLAG_KEY"))

;;; The two sentinels a decode loop is built around: EOF ends the stream, and
;;; EAGAIN means the codec wants the other half of the send/receive pair.
;;; AVERROR is a macro over an expression rather than a name, and CONSTANT can
;;; only ask about names -- it guards every one it emits with #ifdef -- so take
;;; errno's value and negate it in Lisp, which is all AVERROR does here.

(constant (+error-eof+ "AVERROR_EOF"))
(constant (+eagain+ "EAGAIN"))

;;; swscale's scaler choice.  Bilinear is what a video player would use to fit
;;; a picture to a surface that is not its native size.  These are members of
;;; `enum SwsFlags' rather than definitions, so CENUM again.

(cenum (swscale-flags :base-type :int)
  ((:fast-bilinear "SWS_FAST_BILINEAR"))
  ((:bilinear "SWS_BILINEAR"))
  ((:bicubic "SWS_BICUBIC"))
  ((:point "SWS_POINT"))
  ((:area "SWS_AREA"))
  ((:lanczos "SWS_LANCZOS")))

;;; Seeking lands on a keyframe, and BACKWARD picks the one at or before the
;;; timestamp rather than after it -- which is what "start over" means.

(constant (+seek-backward+ "AVSEEK_FLAG_BACKWARD"))

;;; The AV_PIX_FMT_* and AV_HWDEVICE_TYPE_* names are enum members, not
;;; preprocessor definitions, so CONSTANT cannot see them at all: it would
;;; silently emit nothing and leave the variable unbound.  CENUM reads the
;;; enumeration itself.
;;;
;;; Only the formats luv expects to meet are listed.  The software ones are
;;; what a decoder hands back on the CPU path; the three hardware ones are
;;; opaque frames whose `data[3]' carries a platform surface -- a
;;; CVPixelBuffer, a VASurfaceID, or an AVVkFrame -- which is the whole reason
;;; to decode through FFmpeg rather than around it.

(cenum (pixel-format :base-type :int)
  ((:none "AV_PIX_FMT_NONE"))
  ((:yuv420p "AV_PIX_FMT_YUV420P"))
  ((:nv12 "AV_PIX_FMT_NV12"))
  ((:p010 "AV_PIX_FMT_P010"))
  ((:rgba "AV_PIX_FMT_RGBA"))
  ((:bgra "AV_PIX_FMT_BGRA"))
  ((:videotoolbox "AV_PIX_FMT_VIDEOTOOLBOX"))
  ((:vaapi "AV_PIX_FMT_VAAPI"))
  ((:vulkan "AV_PIX_FMT_VULKAN")))

(cenum (media-type :base-type :int)
  ((:unknown "AVMEDIA_TYPE_UNKNOWN"))
  ((:video "AVMEDIA_TYPE_VIDEO"))
  ((:audio "AVMEDIA_TYPE_AUDIO"))
  ((:subtitle "AVMEDIA_TYPE_SUBTITLE")))

(cenum (hardware-device-type :base-type :int)
  ((:none "AV_HWDEVICE_TYPE_NONE"))
  ((:vdpau "AV_HWDEVICE_TYPE_VDPAU"))
  ((:cuda "AV_HWDEVICE_TYPE_CUDA"))
  ((:vaapi "AV_HWDEVICE_TYPE_VAAPI"))
  ((:videotoolbox "AV_HWDEVICE_TYPE_VIDEOTOOLBOX"))
  ((:drm "AV_HWDEVICE_TYPE_DRM"))
  ((:opencl "AV_HWDEVICE_TYPE_OPENCL"))
  ((:vulkan "AV_HWDEVICE_TYPE_VULKAN")))

(cstruct av-rational "AVRational"
  (numerator "num" :type :int)
  (denominator "den" :type :int))

(cstruct av-buffer-reference "AVBufferRef"
  (buffer "buffer" :type :pointer)
  (data "data" :type :pointer)
  (size "size" :type :size))

;;; Only the fields luv reads or writes.  Groveling a subset is safe precisely
;;; because every offset is computed rather than accumulated: leaving a field
;;; out cannot shift the ones that follow it.

(cstruct av-frame "AVFrame"
  (data "data" :type :pointer :count 8)
  (pitches "linesize" :type :int :count 8)
  (extended-data "extended_data" :type :pointer)
  (width "width" :type :int)
  (height "height" :type :int)
  (sample-count "nb_samples" :type :int)
  (format "format" :type :int)
  (flags "flags" :type :int)
  (presentation-timestamp "pts" :type :int64)
  (packet-decode-timestamp "pkt_dts" :type :int64)
  (duration "duration" :type :int64)
  (buffers "buf" :type :pointer :count 8)
  (hardware-frames-context "hw_frames_ctx" :type :pointer))

;;; The container side.  A demuxed file is an AVFormatContext holding an array
;;; of AVStreams; each stream describes its content with AVCodecParameters,
;;; which is the serializable half a decoder is configured from.

(cstruct av-format-context "AVFormatContext"
  (stream-count "nb_streams" :type :unsigned-int)
  (streams "streams" :type :pointer)
  (duration "duration" :type :int64))

(cstruct av-stream "AVStream"
  (index "index" :type :int)
  (codec-parameters "codecpar" :type :pointer)
  (time-base "time_base" :type (:struct av-rational))
  (average-frame-rate "avg_frame_rate" :type (:struct av-rational))
  (frame-count "nb_frames" :type :int64))

(cstruct av-codec-parameters "AVCodecParameters"
  (codec-type "codec_type" :type :int)
  (codec-id "codec_id" :type :int)
  (format "format" :type :int)
  (width "width" :type :int)
  (height "height" :type :int))

(cstruct av-codec-context "AVCodecContext"
  (width "width" :type :int)
  (height "height" :type :int)
  (pixel-format "pix_fmt" :type :int))

(cstruct av-packet "AVPacket"
  (data "data" :type :pointer)
  (size "size" :type :int)
  (stream-index "stream_index" :type :int)
  (presentation-timestamp "pts" :type :int64)
  (decode-timestamp "dts" :type :int64))
