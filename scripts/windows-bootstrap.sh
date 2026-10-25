#!/usr/bin/env bash
# Run in an updated MSYS2 UCRT64 shell (e.g. msys2/setup-msys2 with
# msystem: UCRT64 and update: true). These are build-machine packages only;
# windows-build.py ships the DLL import closure, never MSYS2 or its tools.
set -euo pipefail

if [[ ${MSYSTEM:-} != UCRT64 || ${MINGW_PREFIX:-} != /ucrt64 ]]; then
  echo 'Use the MSYS2 UCRT64 shell, not MSYS, MINGW64, or Cygwin.' >&2
  exit 1
fi

packages=(sbcl sdl3 sdl3-image sdl3-ttf harfbuzz ffmpeg libffi openssl
          vulkan-headers vulkan-loader gcc binutils pkgconf)
case ${1:-} in
  '') ;;
  --with-mesa) packages+=(mesa) ;;
  *) echo 'Usage: windows-bootstrap.sh [--with-mesa]' >&2; exit 2 ;;
esac
for i in "${!packages[@]}"; do
  packages[i]="mingw-w64-ucrt-x86_64-${packages[i]}"
done
pacman --sync --needed --noconfirm "${packages[@]}"
pacman --query "${packages[@]}"
/ucrt64/bin/sbcl --version
/ucrt64/bin/pkg-config --modversion libavutil libavcodec libavformat libswscale
# setup-msys2 can install outside C:/msys64. Pass this output as --msys-root
# when the Python build runs in the following native PowerShell step.
if [[ -n ${GITHUB_OUTPUT:-} ]]; then
  printf 'msys_root=%s\n' "$(cygpath -m /)" >>"$GITHUB_OUTPUT"
fi
