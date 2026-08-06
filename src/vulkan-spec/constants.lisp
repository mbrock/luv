;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; generate.lisp --- generate CFFI bindings from vk.xml file.
;;;
;;; Copyright (c) 2016, Bart Botta  <00003b@gmail.com>
;;;   All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;

(in-package #:vulkan-spec.constants)

(defparameter *special-words*
  '("Bool32" "Win32"
    "16" "32" "64"
    "3d" "2d" "1d"
    "3D" "2D" "1D"
    "b32" "i64" "u64" "f64"
    "AABB" "AABBS" "AABBs"
    "RTE" "RTZ"
    "4444" "A4R4G4B4" "A4B4G4R4" ;; VK_EXT_4444_FORMATS
    "ID" "UUID" "LUID"
    "H264" "H265" ;; video encode/decode (since v1.2.175)
    "HINSTANCE" "HWND" "HMONITOR" "HANDLE" "SECURITY_ATTRIBUTES" "DWORD" "LPCWSTR" ;; windows.h
    "FB" ;; directfb.h
    "CA" ;; CAMetalLayer
    "OS" "IOS" ;; MacOS and iOS
    "NVX" ;; nvidia experimental extensions
    "SRT" ;; SRT 
    "ETC2" "ASTC" "ASTC_" "LDR" "BC" "RR")
  "A sequence of special words that are used in the specification of the Vulkan API.")

(defparameter *vk-platform*
  (alexandria:plist-hash-table
   '("void" :void
     "char" :char
     "float" :float
     "double" :double ;; added this after v1.1.119 failed (not sure which version added this)
     "uint8_t" :uint8
     "uint16_t" :uint16 ;; added this after v1.1.93 failed (not sure which version added this)
     "uint32_t" :uint32
     "uint64_t" :uint64
     "int8_t" :int8 ;; added in v1.2.175
     "int16_t" :int16
     "int32_t" :int32
     "int64_t" :int64 ;; added this after v1.1.119 failed (not sure which version added this)
     "int" :int
     "size_t" :size)
   :test 'equal))

(defparameter *opaque-types*
  '("ANativeWindow"
    "AHardwareBuffer" ;; added in v1.1.71
    "mir_connection"
    "mir_surface"
    "xcb_connection_t"
    "IDirectFB"
    "IDirectFBSurface"
    "CAMetalLayer"
    "Display"
    "wl_display"
    "wl_surface"))

(defparameter *opaque-struct-types*
  '("SECURITY_ATTRIBUTES"
    "_screen_context"  ;; added in v1.2.171
    "_screen_window" ;; added in v1.2.171
    ;; all StdVideo-structs have been added in v1.2.175
    "StdVideoDecodeH264PictureInfo"
    "StdVideoDecodeH264ReferenceInfo"
    "StdVideoDecodeH264Mvc"
    "StdVideoH264SequenceParameterSet"
    "StdVideoH264PictureParameterSet"
    "StdVideoEncodeH264SliceHeader"
    "StdVideoEncodeH264PictureInfo"
    "StdVideoEncodeH264RefPicMarkingEntry"
    "StdVideoEncodeH264RefListModEntry"
    "StdVideoEncodeH264RefMgmtFlags"
    "StdVideoEncodeH264PictureInfoFlags"
    "StdVideoEncodeH264RefMemMgmtCtrlOperations"
    "StdVideoEncodeH264SliceHeaderFlags"
    "StdVideoEncodeH265ReferenceModificationFlags"
    "StdVideoEncodeH265ReferenceInfoFlags"
    "StdVideoEncodeH265SliceHeaderFlags"
    "StdVideoEncodeH265ReferenceModifications"
    "StdVideoEncodeH265ReferenceInfo"
    "StdVideoEncodeH265SliceHeader"
    "StdVideoEncodeH265SliceSegmentHeader"
    "StdVideoEncodeH265PictureInfo"
    "StdVideoEncodeH265PictureInfoFlags"
    "StdVideoDecodeH264MvcElementFlags"
    "StdVideoDecodeH264MvcElement"
    "StdVideoDecodeH264ReferenceInfoFlags"
    "StdVideoDecodeH264PictureInfoFlags"
    "StdVideoDecodeH265PictureInfo"
    "StdVideoDecodeH265ReferenceInfo"
    "StdVideoDecodeH265ReferenceInfoFlags"
    "StdVideoDecodeH265PictureInfoFlags"
    "StdVideoH264PpsFlags"
    "StdVideoH264SpsVuiFlags"
    "StdVideoH264HrdParameters"
    "StdVideoH264SequenceParameterSetVui"
    "StdVideoH264ScalingLists"
    "StdVideoH264SpsFlags"
    "StdVideoH265VideoParameterSet"
    "StdVideoH265SequenceParameterSet"
    "StdVideoH265PictureParameterSet"
    "StdVideoH265SpsVuiFlags"
    "StdVideoH265HrdFlags"
    "StdVideoH265SubLayerHrdParameters"
    "StdVideoH265PpsFlags"
    "StdVideoH265PredictorPaletteEntries"
    "StdVideoH265SequenceParameterSetVui"
    "StdVideoH265ScalingLists"
    "StdVideoH265SpsFlags"
    "StdVideoH265VpsFlags"
    "StdVideoH265HrdParameters"
    "StdVideoH265DecPicBufMgr"))

(defparameter *fix-must-be*
  (alexandria:alist-hash-table
   '((:pipeline-iinput-assembly-state-create-info
      . :pipeline-input-assembly-state-create-info)
     (:debug-report-callback-create-info
      . :debug-report-create-info-ext))))

(defparameter *misc-os-types*
  (alexandria:plist-hash-table
   '("GgpStreamDescriptor" (:pointer :void) ;; 
     "GgpFrameToken" (:pointer :void)
     "HINSTANCE" (:pointer :void)
     "HWND" (:pointer :void)
     "HANDLE" (:pointer :void)
     "HMONITOR" (:pointer :void)
     "DWORD" :uint32
     "LPCWSTR" (:pointer :void)
     "RROutput" :ulong
     "xcb_window_t" :uint32
     "xcb_visualid_t" :uint32
     "zx_handle_t" (:pointer :void)
     "Window" :ulong
     "VisualID" :ulong
     "StdVideoH264ProfileIdc" :uint32 ;; an enum, added in v1.2.175
     "StdVideoH264MemMgmtControlOp" :uint32
     "StdVideoH264ModificationOfPicNumsIdc" :uint32
     "StdVideoH264PictureType" :uint32
     "StdVideoH264DisableDeblockingFilterIdc" :uint32
     "StdVideoH264CabacInitIdc" :uint32
     "StdVideoH264SliceType" :uint32
     "StdVideoH264WeightedBipredIdc" :uint32
     "StdVideoH264AspectRatioIdc" :uint32
     "StdVideoH264PocType" :uint32
     "StdVideoH264ChromaFormatIdc" :uint32
     "StdVideoH264Level" :uint32
     "StdVideoH265ProfileIdc" :uint32 ;; an enum, added in v1.2.175
     "StdVideoH265PictureType" :uint32
     "StdVideoH265SliceType" :uint32
     "StdVideoH265Level" :uint32)
   :test 'equal))

;; from generator.py
(defconstant +ext-base+ 1000000000)
(defconstant +ext-block-size+ 1000)

(defparameter *special-pointer-types*
  '("Display"
    "IDirectFB"
    "wl_display"
    "xcb_connection_t"
    "_screen_window")
  "A sequence of pointer types which are never used as const-qualified call arguments, but are never used as return arguments.")

(defparameter *special-base-types*
  '("ANativeWindow"
    "AHardwareBuffer"))

(defparameter *directly-exposed-extensions*
  '("VK_KHR_surface"
    "VK_KHR_display"
    "VK_KHR_xlib_surface"
    "VK_KHR_xcb_surface"
    "VK_KHR_wayland_surface"
    "VK_EXT_directfb_surface"
    "VK_KHR_win32_surface"
    "VK_KHR_android_surface"
    "VK_GGP_stream_descriptor_surface"
    "VK_MVK_macos_surface"
    "VK_MVK_ios_surface"
    "VK_EXT_headless_surface"
    "VK_EXT_metal_surface"
    "VK_FUCHSIA_imagepipe_surface"
    "VK_KHR_swapchain"
    "VK_KHR_display_swapchain"
    "VK_KHR_get_display_properties2"
    "VK_KHR_get_surface_capabilities2"
    "VK_QNX_screen_surface")
  "The names of all extensions that are directly exposed by the Vulkan loader.")
