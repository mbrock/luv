{
  description = "luv — a Common Lisp Vulkan atelier";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.ghostty.url = "github:ghostty-org/ghostty";
  inputs.mcclim = {
    url = "git+https://codeberg.org/McCLIM/McCLIM.git?ref=master";
    flake = false;
  };
  inputs.cl-sdl3 = {
    url = "git+https://github.com/aiffc/cl-sdl3.git?rev=47c90b54715aba23752b70d382a3eb310172cd34";
    flake = false;
  };
  # Keep this tag equal to the Tracy profiler you actually run: the client and
  # the viewer negotiate an exact protocol version and refuse to talk across a
  # mismatch.
  inputs.tracy = {
    url = "github:wolfpld/tracy/v0.13.1";
    flake = false;
  };
  # WPE WebKit, the embeddable browser engine, packaged out of tree.  Linux
  # only, and by design: WPE's whole point is the accelerated-compositing
  # handoff over EGL and linux-dmabuf, which has no Darwin equivalent (the
  # macOS side of an embedded-browser surface would be WKWebView into an
  # IOSurface instead).  Nothing here depends on it yet -- it is exposed as a
  # deliberately off-to-the-side package so a Linux machine can build it into
  # a shared cache ahead of the work that will use it.
  inputs.nix-wpe-webkit.url = "github:eval-exec/nix-wpe-webkit";

  outputs = { nixpkgs, ghostty, mcclim, cl-sdl3, tracy, nix-wpe-webkit, ... }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
      environmentFor = system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          libghosttyVt =
            ghostty.packages.${system}.libghostty-vt-releasesafe;
          libghosttyVtLibrary =
            "${libghosttyVt}/lib/libghostty-vt${pkgs.stdenv.hostPlatform.extensions.sharedLibrary}";
          sbclVersion = "2.6.7";
          needsSimdSbcl = system == "aarch64-linux" || system == "aarch64-darwin";
          sbclUnwrapped =
            if needsSimdSbcl then
              pkgs.sbcl.overrideAttrs (_finalAttrs: _previousAttrs: {
                version = sbclVersion;
                src = pkgs.fetchurl {
                  url = "mirror://sourceforge/project/sbcl/sbcl/${sbclVersion}/sbcl-${sbclVersion}-source.tar.bz2";
                  hash = "sha256-Hr3DXJ3I4nG4zRrESWXgC/JV+cAiFlD8t38Ps0wtOt4=";
                };
              })
            else
              pkgs.sbcl;
          sbcl = pkgs.wrapLisp {
            pkg = sbclUnwrapped;
            faslExt = "fasl";
            flags = [
              "--dynamic-space-size"
              "3000"
            ];
          };
          # Tracy ships its whole client as one translation unit, so the profiled
          # build is a single compiler invocation rather than a CMake project.
          # The options matter more than the build does:
          #
          #   ON_DEMAND       only collect while a viewer is attached, so the
          #                   durable ./sly image does not accumulate a trace
          #                   nobody asked for
          #   DELAYED_INIT +  start and stop the profiler explicitly, instead of
          #   MANUAL_LIFETIME at dlopen time when CFFI happens to load us
          #   NO_CRASH_HANDLER Tracy installs SIGSEGV/SIGBUS/SIGILL handlers on
          #                   Linux, and SBCL needs those signals for its GC
          #                   write barrier and guard pages
          #   NO_SYSTEM_TRACING keep perf_event sampling out of the Lisp image;
          #                   macOS has no system tracing to lose anyway
          tracyClient = pkgs.stdenv.mkDerivation {
            pname = "tracy-client";
            version = "0.13.1";
            src = tracy;
            dontConfigure = true;
            buildPhase =
              let
                extension = pkgs.stdenv.hostPlatform.extensions.sharedLibrary;
              in ''
                runHook preBuild
                $CXX -std=c++17 -O2 -fPIC -fvisibility=hidden -shared \
                  -DTRACY_ENABLE \
                  -DTRACY_ON_DEMAND \
                  -DTRACY_DELAYED_INIT \
                  -DTRACY_MANUAL_LIFETIME \
                  -DTRACY_NO_CRASH_HANDLER \
                  -DTRACY_NO_SYSTEM_TRACING \
                  -Ipublic \
                  public/TracyClient.cpp \
                  ${nixpkgs.lib.optionalString pkgs.stdenv.isDarwin
                      "-install_name $out/lib/libTracyClient${extension}"} \
                  ${nixpkgs.lib.optionalString pkgs.stdenv.isLinux
                      "-pthread -ldl"} \
                  -o libTracyClient${extension}
                runHook postBuild
              '';
            installPhase =
              let
                extension = pkgs.stdenv.hostPlatform.extensions.sharedLibrary;
              in ''
                runHook preInstall
                install -Dm555 libTracyClient${extension} \
                  $out/lib/libTracyClient${extension}
                mkdir -p $out/include
                cp -r public/tracy $out/include/
                runHook postInstall
              '';
            meta = {
              description = "Tracy profiler client, built for a live SBCL image";
              inherit (pkgs.stdenv.hostPlatform) system;
            };
          };
          tracyClientLibrary =
            "${tracyClient}/lib/libTracyClient${pkgs.stdenv.hostPlatform.extensions.sharedLibrary}";
          # FFmpeg is here for its libraries, not its command line: libavcodec
          # and friends are the one decoder front door that reaches hardware
          # video decode on both of our platforms, VideoToolbox on Darwin and
          # VAAPI or Vulkan Video on Linux, and hands back a frame that is
          # already on the GPU.  The default "small" variant already enables
          # Vulkan and VAAPI off-Darwin, and FFmpeg's configure turns
          # VideoToolbox on by itself when it sees the frameworks.
          #
          # CFFI finds these by soname: it searches LD_LIBRARY_PATH itself on
          # both platforms, before falling back to the system loader, so the
          # entry in NATIVE-LIBRARY-PATH below is what makes an unqualified
          # "libavcodec.dylib" resolve to this build rather than to whatever
          # Homebrew happens to have left in /usr/local/lib.
          ffmpeg = pkgs.ffmpeg;
          ffmpegLibraryDirectory = "${ffmpeg.lib}/lib";
          # MuPDF reads PDF.  Its shared library is found by soname the same
          # way the libav ones are, so it needs both a place on the loader
          # path and a pinned directory the binding can prefer.
          # `bin` is this package's default output and holds the viewers; the
          # shared library is in `out`, so both the loader path and the pinned
          # directory have to name that one explicitly.
          mupdf = pkgs.mupdf;
          mupdfLibraryDirectory = "${mupdf.out}/lib";
          nativeLibraryPath = nixpkgs.lib.makeLibraryPath (
            [
              ffmpeg
              mupdf.out
              pkgs.libffi
              libghosttyVt
              pkgs.harfbuzz
              tracyClient
              pkgs.mesa
              pkgs.sdl3
              pkgs.sdl3-image
              pkgs.sdl3-mixer
              pkgs.sdl3-ttf
              pkgs.vulkan-loader
            ] ++ nixpkgs.lib.optionals pkgs.stdenv.isDarwin [
              pkgs.moltenvk
            ]
          );
          # Keep the owned CFFI binding and development tools available to SBCL.
          # McCLIM itself comes from the pinned source above.  Pull only the
          # packaged McCLIM dependency closure into SBCL, since registering the
          # packaged McCLIM systems would make ASDF select them before our pin.
          lisp = sbcl.withPackages (lispPackages:
            let
              packageNode = package: {
                key = package.outPath;
                inherit package;
              };
              mcclimClosure = nixpkgs.lib.genericClosure {
                startSet = map packageNode lispPackages.mcclim.lispLibs;
                operator = node:
                  map packageNode (node.package.lispLibs or [ ]);
              };
              mcclimDependencies = map (node: node.package) (
                builtins.filter
                  (node: (node.package.src or null) != lispPackages.mcclim.src)
                  mcclimClosure
              );
            in [
              lispPackages.alexandria
              lispPackages.cffi
              lispPackages.cffi-libffi
              lispPackages.cl-base64
              lispPackages.cl-fad
              lispPackages.cl-who
              lispPackages.closer-mop
              lispPackages.defclass-std
              lispPackages.eclector
              lispPackages.flexi-streams
              lispPackages.lorem-ipsum
              lispPackages.named-readtables
              lispPackages.rove
              lispPackages.spinneret
              lispPackages.zpng
            ] ++ mcclimDependencies
              ++ nixpkgs.lib.optionals pkgs.stdenv.isDarwin [
                lispPackages.float-features
                lispPackages.trivial-main-thread
              ]);
          slyRoot =
            "${pkgs.emacsPackages.sly}/share/emacs/site-lisp/elpa/${pkgs.emacsPackages.sly.pname}-${pkgs.emacsPackages.sly.version}";
          # A second nixpkgs instance carrying the WPE overlay, so the main
          # `pkgs` above keeps its plain `legacyPackages` identity and nothing
          # else in the closure is rebuilt by the overlay.
          wpePkgs = import nixpkgs {
            inherit system;
            overlays = [ nix-wpe-webkit.overlays.default ];
          };
        in
        {
          inherit pkgs wpePkgs sbcl lisp nativeLibraryPath slyRoot;
          inherit ffmpeg ffmpegLibraryDirectory mupdf mupdfLibraryDirectory;
          inherit libghosttyVt libghosttyVtLibrary;
          inherit tracyClient tracyClientLibrary;
        };
    in
    {
      packages = forAllSystems (system:
        let
          env = environmentFor system;
        in {
          sbcl = env.sbcl;
          lisp = env.lisp;
          ffmpeg = env.ffmpeg;
          libghostty-vt = env.libghosttyVt;
          tracy-client = env.tracyClient;
          default = env.lisp;
        } // nixpkgs.lib.optionalAttrs env.pkgs.stdenv.isLinux {
          # Reachable only by name, `nix build .#wpewebkit` on Linux: not in
          # `default`, not in the dev shell, not in any library path.
          wpewebkit = env.wpePkgs.wpewebkit;
        });

      devShells = forAllSystems (system:
        let
          env = environmentFor system;
          lavapipeIcd =
            if system == "x86_64-linux" then "lvp_icd.x86_64.json"
            else if system == "aarch64-linux" then "lvp_icd.aarch64.json"
            else null;
        in {
          default = env.pkgs.mkShell ({
            packages = [
              env.lisp
              env.ffmpeg
              env.ffmpeg.dev
              env.pkgs.go
              env.mupdf
              env.libghosttyVt
              env.pkgs.libffi
              env.pkgs.harfbuzz
              env.pkgs.mesa
              env.pkgs.python3
              env.pkgs.pkg-config
              env.pkgs.sdl3
              env.pkgs.spirv-tools
              env.pkgs.urbit
              env.pkgs.vulkan-headers
              env.pkgs.vulkan-tools
              env.pkgs.vulkan-validation-layers
              env.pkgs.yt-dlp
            ];
            LD_LIBRARY_PATH = env.nativeLibraryPath;
            LUV_NIX_SHELL = "1";
            # The real urbit runtime (vere), which an urbit wall boots.
            LUV_URBIT = "${env.pkgs.urbit}/bin/urbit";
            LUV_GHOSTTY_LIBRARY = env.libghosttyVtLibrary;
            LUV_SLYNK_DIR = "${env.slyRoot}/slynk";
            LUV_TRACY_CLIENT = env.tracyClientLibrary;
            LUV_FFMPEG_LIBDIR = env.ffmpegLibraryDirectory;
            LUV_MUPDF_LIBDIR = env.mupdfLibraryDirectory;
            LUV_YT_DLP = "${env.pkgs.yt-dlp}/bin/yt-dlp";
            CL_SOURCE_REGISTRY = "${mcclim}//:${cl-sdl3}//";
            CPATH = "${env.pkgs.vulkan-headers}/include";
            VK_LAYER_PATH =
              "${env.pkgs.vulkan-validation-layers}/share/vulkan/explicit_layer.d";
            shellHook = ''
              if [ -z "''${SDL_VIDEODRIVER:-}" ] \
                && [ -z "''${DISPLAY:-}" ] \
                && [ -z "''${WAYLAND_DISPLAY:-}" ]; then
                export SDL_VIDEODRIVER=offscreen
              fi

              if [ -n "''${LUV_LAVAPIPE_ICD:-}" ] \
                && [ -f "$LUV_LAVAPIPE_ICD" ] \
                && [ -z "''${VK_DRIVER_FILES:-}" ] \
                && [ "''${SDL_VIDEODRIVER:-}" = offscreen ]; then
                export VK_DRIVER_FILES="$LUV_LAVAPIPE_ICD"
              fi
            '';
          } // nixpkgs.lib.optionalAttrs (lavapipeIcd != null) {
            LUV_LAVAPIPE_ICD =
              "${env.pkgs.mesa}/share/vulkan/icd.d/${lavapipeIcd}";
          } // nixpkgs.lib.optionalAttrs env.pkgs.stdenv.isDarwin {
            VK_DRIVER_FILES =
              "${env.pkgs.moltenvk}/share/vulkan/icd.d/MoltenVK_icd.json";
          });
        });
    };
}
