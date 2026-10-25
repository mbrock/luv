# Run on Linux, not Windows. No target Lisp is compiled by this expression:
# nix build --impure --expr 'import ./scripts/windows-sources.nix {}' \
#   --out-link build/windows-sources
# Upload build/windows-sources/lisp-sources.tar.gz to the native Windows job.
{ root ? ../. }:
let
  flake = builtins.getFlake (toString root);
  # Resolve the same lispLibrariesFor closure as the checked-in flake instead
  # of maintaining a second Quicklisp dependency list or updating its pins.
  pkgs = flake.inputs.nixpkgs.legacyPackages.x86_64-linux;
  lib = pkgs.lib;
  lisps = builtins.filter (p: p ? lispLibs)
    flake.devShells.x86_64-linux.slim.nativeBuildInputs;
  lisp = assert builtins.length lisps == 1; builtins.head lisps;
  node = package: { key = package.outPath; inherit package; };
  closure = lib.genericClosure {
    # Deploy is a build dependency of the portable-image builder, also
    # pinned by our existing nixpkgs lock rather than fetched from HEAD.
    startSet = map node (lisp.lispLibs ++ [ pkgs.sbcl.pkgs.deploy ]);
    operator = n: map node (n.package.lispLibs or [ ]);
  };
  packages = map (n: n.package) closure;
  cffi = builtins.head (builtins.filter (p: p.pname == "cffi") packages);
  sourceFor = p:
    # The nixpkgs libffi variant is a second copy of CFFI, patched for the
    # build host's headers. Use one unmodified source tree for Windows.
    if p.pname == "cffi-libffi" then cffi.src else p.src;
  sources = lib.unique (map sourceFor packages);
  sdl = pkgs.applyPatches {
    name = "cl-sdl3-windows-sources";
    src = flake.inputs.cl-sdl3;
    patches = [ ../nix/cl-sdl3-no-mixer.patch ./windows-cl-sdl3.patch ];
  };
  manifest = pkgs.writeText "lisp-sources.json" (builtins.toJSON {
    format = 1;
    lockSha256 = builtins.hashFile "sha256" (root + "/flake.lock");
    mcclimRevision = flake.inputs.mcclim.rev;
    clSdl3Revision = flake.inputs.cl-sdl3.rev;
    patches = [ "cl-sdl3-no-mixer.patch" "windows-cl-sdl3.patch" ];
    packages = map (p: {
      name = p.pname;
      version = p.version or "unknown";
      directory = builtins.baseNameOf (sourceFor p);
    }) packages;
  });
in
pkgs.runCommand "luv-windows-lisp-sources" { } ''
  mkdir -p "$out" sources
  ${lib.concatMapStringsSep "\n" (src: ''
    cp -RL ${src} sources/${builtins.baseNameOf src}
  '') sources}
  cp -RL ${flake.inputs.mcclim} sources/mcclim
  cp -RL ${sdl} sources/cl-sdl3
  chmod -R u+w sources
  # Source directories built by nixpkgs may retain metadata or native caches.
  # Nothing compiled on Linux belongs in the Windows ASDF source registry.
  find sources -type d -name nix-support -prune -exec rm -rf {} +
  find sources -type f \( -iname '*.fasl' -o -iname '*.so' -o \
    -iname '*.dll' -o -iname '*.dylib' -o -iname '*.o' -o -iname '*.exe' \) -delete
  cp ${manifest} sources/lisp-sources.json
  cp ${root + "/flake.lock"} sources/flake.lock
  tar --sort=name --mtime=@1 --owner=0 --group=0 --numeric-owner \
    -czf "$out/lisp-sources.tar.gz" -C sources .
''
