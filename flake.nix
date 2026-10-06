{
  description = "Luvcraft and Luft — Common Lisp GPU experiments";

  nixConfig = {
    extra-substituters = [ "https://luv.swa.sh/nix-cache?priority=30" ];
    extra-trusted-public-keys = [
      "luv.swa.sh-1:PpD45iCBkJ38ZkvlyZcLiGdIz6yVehXn3fm1JvG18Bw="
    ];
  };

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.ghostty.url = "github:ghostty-org/ghostty";
  inputs.mcclim = {
    url = "git+https://git.swa.sh/mcclim?ref=master";
    flake = false;
  };
  inputs.cl-sdl3 = {
    url = "git+https://github.com/aiffc/cl-sdl3.git?rev=47c90b54715aba23752b70d382a3eb310172cd34";
    flake = false;
  };
  inputs.parachute = {
    url = "github:Shinmera/parachute/9a6679e611925dfb59067393c5b7996f69501aa6";
    flake = false;
  };
  inputs.swash = {
    url = "github:lessrest/swash/43ba48386191947c3b1bbcc647db2df0ad376594";
    flake = false;
  };
  # Keep this tag equal to the Tracy profiler you actually run: the client and
  # the viewer negotiate an exact protocol version and refuse to talk across a
  # mismatch.
  inputs.tracy = {
    url = "github:wolfpld/tracy/v0.14.0";
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

  outputs = { self, nixpkgs, ghostty, mcclim, cl-sdl3, parachute, swash, tracy, nix-wpe-webkit, ... }:
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
          clSdlPatch = builtins.path {
            path = ./nix/cl-sdl3-no-mixer.patch;
            name = "cl-sdl3-no-mixer.patch";
          };
          tracyContext = builtins.path {
            path = ./nix/tracy-context.cpp;
            name = "luv-tracy-context.cpp";
          };
          libghosttyVt =
            ghostty.packages.${system}.libghostty-vt-releasesafe;
          libghosttyVtLibrary =
            "${libghosttyVt}/lib/libghostty-vt${pkgs.stdenv.hostPlatform.extensions.sharedLibrary}";
          sbcl = pkgs.wrapLisp {
            pkg = pkgs.sbcl;
            faslExt = "fasl";
            flags = [
              "--dynamic-space-size"
              "3000"
            ];
          };
          # Keep the next SBCL built and cached independently from the
          # nixpkgs release used by the development image.
          sbcl268Unwrapped = pkgs.sbcl.overrideAttrs (_finalAttrs: _previousAttrs: {
            version = "2.6.8";
            src = pkgs.fetchurl {
              url = "mirror://sourceforge/project/sbcl/sbcl/2.6.8/sbcl-2.6.8-source.tar.bz2";
              hash = "sha256-rVEm39+6XbJ+53vMJYkwIP5SLQt2U9RbTEeVrePdwj0=";
            };
          });
          sbcl268 = pkgs.wrapLisp {
            pkg = sbcl268Unwrapped;
            faslExt = "fasl";
            flags = [
              "--dynamic-space-size"
              "3000"
            ];
          };
          parachutePackage = sbcl.buildASDFSystem {
            pname = "parachute";
            version = "1.5.0";
            src = parachute;
            lispLibs = with sbcl.pkgs; [
              documentation-utils
              form-fiddle
              trivial-custom-debugger
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
            version = "0.14.0";
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
                  ${tracyContext} \
                  ${nixpkgs.lib.optionalString pkgs.stdenv.hostPlatform.isDarwin
                      "-install_name $out/lib/libTracyClient${extension}"} \
                  ${nixpkgs.lib.optionalString pkgs.stdenv.hostPlatform.isLinux
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
          tracyImGui = pkgs.fetchFromGitHub {
            name = "ImGui";
            owner = "ocornut";
            repo = "imgui";
            rev = "v1.92.9b-docking";
            hash = "sha256-PknWLxYuXQ73TCFN+eKOJDNLGbg/ZqKSF6mFxkJG6vI=";
          };
          tracyMd4c = pkgs.fetchFromGitHub {
            name = "md4c";
            owner = "mity";
            repo = "md4c";
            rev = "65c6c9d72cebd9a731aaa5597414ce04d9ea5de3";
            hash = "sha256-UMIebye8pQkiTjhbz3btTqPapzoqOnPHrtbPitc717A=";
          };
          tracyNfd = pkgs.fetchFromGitHub {
            name = "nfd";
            owner = "btzy";
            repo = "nativefiledialog-extended";
            rev = "3cd252a8f7ca32419b1ca235c2990ba6a0ecba7c";
            hash = "sha256-BV4FdH+AfNXAbLfipBPMGkJmggo59Kf4NIgKQ0hsB9g=";
            fetchSubmodules = true;
          };
          # nixpkgs may lag the protocol release pinned above.  Build its GUI
          # and capture tools from the same source as the embedded client: a
          # Tracy viewer and client from different releases cannot connect.
          tracyTools = pkgs.tracy.overrideAttrs (_final: previous: {
            version = "0.14.0";
            src = tracy;
            # The development viewer does not need whole-program LTO, whose
            # opaque final link otherwise spends tens of seconds silent.
            cmakeFlags = previous.cmakeFlags ++ [ "-DNO_LTO=ON" ];
            # fetchFromGitHub unpacked the old source into ./tracy; a flake
            # input is named ./source.  Keep nixpkgs' vendored CPM setup, but
            # point its one source-tree copy at the input's actual directory.
            postUnpack = builtins.replaceStrings
              [ "./tracy/" ] [ "./source/" ] previous.postUnpack + ''
                rm -rf ImGui
                cp -R ${tracyImGui} ImGui
                chmod -R u+w ImGui
                cp -R ${tracyMd4c} md4c
                chmod -R u+w md4c
                appendToVar cmakeFlags -DCPM_md4c_SOURCE=$(pwd)/md4c
                rm -rf nfd
                cp -R ${tracyNfd} nfd
                chmod -R u+w nfd
              '';
            postInstall = builtins.replaceStrings
              [ "--replace-fail Exec=/usr/bin/tracy Exec=tracy" ]
              [ ''--replace-fail "Exec=tracy-profiler %f" "Exec=tracy %f"'' ]
              previous.postInstall;
          });
          # FFmpeg is here for its libraries, not its command line: libavcodec
          # and friends are the one decoder front door that reaches hardware
          # video decode on both of our platforms, VideoToolbox on Darwin and
          # VAAPI or Vulkan Video on Linux, and hands back a frame that is
          # already on the GPU.  The default "small" variant already enables
          # Vulkan and VAAPI off-Darwin, and FFmpeg's configure turns
          # VideoToolbox on by itself when it sees the frameworks.
          #
          # CFFI finds these by soname through the process loader path.  Luv's
          # launchers scope NATIVE-LIBRARY-PATH below to their process trees,
          # while the exact FFmpeg directory remains the preferred binding
          # path.  Entering the checkout itself must not affect host tools.
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
          nativeLibraryPackages =
            [
              ffmpeg
              mupdf.out
              pkgs.libev
              pkgs.libffi
              pkgs.harfbuzz
              pkgs.openssl
              pkgs.sdl3
              pkgs.sdl3-image
              pkgs.sdl3-ttf
              pkgs.vulkan-loader
            ] ++ nixpkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [
              # Luvland serves Wayland clients through libwayland-server and
              # hands them an XKB keymap compiled by libxkbcommon.
              pkgs.libxkbcommon
              pkgs.wayland
            ] ++ nixpkgs.lib.optionals pkgs.stdenv.hostPlatform.isDarwin [
              pkgs.moltenvk
            ];
          nativeLibraryPath = nixpkgs.lib.makeLibraryPath (
            nativeLibraryPackages ++ [ libghosttyVt ]
          );
          slimNativeLibraryPackages =
            [
              ffmpeg
              pkgs.libev
              pkgs.libffi
              pkgs.harfbuzz
              pkgs.openssl
              pkgs.sdl3
              pkgs.sdl3-image
              pkgs.sdl3-ttf
              pkgs.vulkan-loader
            ] ++ nixpkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [
              # Luvland serves Wayland clients through libwayland-server and
              # hands them an XKB keymap compiled by libxkbcommon.
              pkgs.libxkbcommon
              pkgs.wayland
            ] ++ nixpkgs.lib.optionals pkgs.stdenv.hostPlatform.isDarwin [
              pkgs.moltenvk
            ];
          slimNativeLibraryPath = nixpkgs.lib.makeLibraryPath slimNativeLibraryPackages;
          applicationNativeLibraryPath = nixpkgs.lib.makeLibraryPath (
            slimNativeLibraryPackages ++ [ libghosttyVt ]
          );
          mesaLibraryPath = nixpkgs.lib.makeLibraryPath [ pkgs.mesa ];
          # Keep the owned CFFI binding and development tools available to SBCL.
          # McCLIM itself comes from the pinned source above.  Pull only the
          # packaged McCLIM dependency closure into SBCL, since registering the
          # packaged McCLIM systems would make ASDF select them before our pin.
          lispLibrariesFor = lispPackages:
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
              lispPackages.cl-json
              lispPackages.clack
              lispPackages.clack-handler-woo
              lispPackages.cl-fad
              lispPackages.cl-who
              lispPackages.closer-mop
              lispPackages.defclass-std
              lispPackages.eclector
              lispPackages.flexi-streams
              lispPackages.lorem-ipsum
              lispPackages.named-readtables
              lispPackages.parenscript
              parachutePackage
              lispPackages.spinneret
              lispPackages.zpng
              # websocket-driver-client is the blocking, TLS-capable client
              # wrapper around fast-websocket.  OPENAI owns one connection per
              # agent; it does not need an event-loop framework.
              lispPackages.websocket-driver-client
            ] ++ mcclimDependencies
              ++ nixpkgs.lib.optionals pkgs.stdenv.hostPlatform.isDarwin [
                lispPackages.float-features
                lispPackages.trivial-main-thread
              ];
          lisp = sbcl.withPackages lispLibrariesFor;
          # The shipped programs record the source directory of every Lisp
          # library they load.  nixpkgs' CFFI-LIBFFI keeps its groveller's
          # object file, whose debug information names the C compiler and
          # SDK.  ASDF counts the file as a grovel output, so it stays, but
          # its store hashes are blanked: nothing ever links it again.
          applicationSbcl = pkgs.wrapLisp {
            pkg = pkgs.sbcl;
            faslExt = "fasl";
            flags = [
              "--dynamic-space-size"
              "3000"
            ];
            packageOverrides = _self: super: {
              cffi-libffi = super.cffi-libffi.overrideLispAttrs (previous: {
                postInstall = (previous.postInstall or "") + ''
                  find "$out" -type f -name '*__grovel.o' -exec \
                    env LC_ALL=C sed -i -E \
                      's/[0-9a-df-np-sv-z]{32}-/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-/g' {} +
                '';
              });
            };
          };
          applicationLisp = applicationSbcl.withPackages lispLibrariesFor;
          # Upstream's one ASDF system bundles otherwise independent SDL_image,
          # SDL_ttf, and SDL_mixer bindings.  Luv uses the first two but not the
          # mixer, so keep its eagerly loaded foreign library out of the normal
          # development path.  The opt-in mixer shell below uses the untouched
          # source and supplies SDL3_mixer.
          clSdl3WithoutMixer = pkgs.applyPatches {
            name = "cl-sdl3-without-mixer";
            src = cl-sdl3;
            patches = [ clSdlPatch ];
          };
          slyRoot =
            "${pkgs.emacsPackages.sly}/share/emacs/site-lisp/elpa/${pkgs.emacsPackages.sly.pname}-${pkgs.emacsPackages.sly.version}";
          # The Lisp half of SLY without the Emacs package around it.  A
          # program that embeds Slynk would otherwise keep Emacs, and with it
          # a native-compilation toolchain, in its runtime closure.  Slynk
          # loads its contribs from ../contrib, so keep that layout.
          slynkSource = pkgs.runCommand
            "slynk-${pkgs.emacsPackages.sly.version}" { } ''
              mkdir -p "$out/contrib"
              cp -R ${slyRoot}/slynk "$out/slynk"
              cp ${slyRoot}/contrib/*.lisp "$out/contrib/"
            '';
          swashPackage = pkgs.buildGoModule {
            pname = "swash";
            version = "0-unstable-2026-09-04";
            src = swash;
            vendorHash = "sha256-4l7ASCxpMCM92DwSuhi4hphdwnoPNbBLDXxQRdOEKDo=";
            proxyVendor = true;
            subPackages = [ "cmd/swash" ];
            CGO_CFLAGS = "-I${swash}/cvendor";
            env.GOWORK = "off";
            nativeBuildInputs = nixpkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [
              pkgs.makeWrapper
            ];
            postFixup = nixpkgs.lib.optionalString pkgs.stdenv.hostPlatform.isLinux ''
              wrapProgram "$out/bin/swash" \
                --prefix LD_LIBRARY_PATH : ${nixpkgs.lib.makeLibraryPath [ pkgs.systemd ]}
            '';
          };
          # A second nixpkgs instance carrying the WPE overlay, so the main
          # `pkgs` above keeps its plain `legacyPackages` identity and nothing
          # else in the closure is rebuilt by the overlay.
          wpePkgs = import nixpkgs {
            inherit system;
            overlays = [ nix-wpe-webkit.overlays.default ];
          };
          developmentPackages = [
            pkgs.bashInteractive
            lisp
            ffmpeg
            ffmpeg.dev
            pkgs.go
            mupdf
            libghosttyVt
            pkgs.libffi
            pkgs.harfbuzz
            pkgs.mesa
            pkgs.python3
            pkgs.nodejs # Execute ParenScript browser parity tests.
            pkgs.pkg-config
            pkgs.qrencode
            pkgs.sdl3
            pkgs.spirv-tools
            # DXC validates the HLSL lowering, as spirv-tools does SPIR-V.
            pkgs.directx-shader-compiler
            swashPackage
            pkgs.typst
            pkgs.vulkan-headers
            pkgs.vulkan-tools
            pkgs.vulkan-validation-layers
            pkgs.yt-dlp
            pkgs.zig
          ] ++ nixpkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [
            # The first Luvland client: a terminal that draws through wl_shm.
            pkgs.foot
          ];
          # Remote agents need the complete Lisp closure and the native/build
          # tools exercised by ordinary builds, but not optional workstation
          # programs or Ghostty's large Zig closure.  The Ghostty binding can
          # still be compiled and inspected; explicitly loading libghostty-vt
          # requires the full environment.
          slimDevelopmentPackages = [
            pkgs.bashInteractive
            lisp
            ffmpeg
            ffmpeg.dev
            pkgs.libffi
            pkgs.harfbuzz
            pkgs.mesa
            pkgs.python3
            pkgs.nodejs # Execute ParenScript browser parity tests.
            pkgs.pkg-config
            pkgs.sdl3
            pkgs.spirv-tools
            # DXC validates the HLSL lowering, as spirv-tools does SPIR-V.
            pkgs.directx-shader-compiler
            swashPackage
            pkgs.typst
            pkgs.vulkan-headers
            pkgs.vulkan-tools
            pkgs.vulkan-validation-layers
            pkgs.zig
          ];
          # The `nix run` programs build from what they actually load, not
          # from the workstation shell: no Go, Zig, Node, Typst, MuPDF,
          # yt-dlp, Mesa, or Vulkan tooling.  Ghostty's terminal library is
          # the one heavy native dependency the game really opens.
          applicationPackages = [
            pkgs.bashInteractive
            applicationLisp
            ffmpeg
            ffmpeg.dev
            libghosttyVt
            pkgs.libffi
            pkgs.harfbuzz
            pkgs.pkg-config
            pkgs.sdl3
            pkgs.vulkan-headers
          ];
          lavapipeIcd =
            if system == "x86_64-linux" then "lvp_icd.x86_64.json"
            else if system == "aarch64-linux" then "lvp_icd.aarch64.json"
            else null;
          developmentEnvironment = {
            LUV_DEV_ENVIRONMENT = "1";
            # Entering the checkout must not alter the ELF loader for unrelated
            # host tools (notably an orb's own git).  Luv launchers promote this
            # path to LD_LIBRARY_PATH only for their process trees.
            LUV_NATIVE_LIBRARY_PATH = nativeLibraryPath;
            LUV_MESA_LIBRARY_PATH = mesaLibraryPath;
            LUV_GHOSTTY_LIBRARY = libghosttyVtLibrary;
            LUV_BASH = "${pkgs.bashInteractive}/bin/bash";
            LUV_SLYNK_DIR = "${slyRoot}/slynk";
            LUV_SWASH = "${swashPackage}/bin/swash";
            LUV_FFMPEG_LIBDIR = ffmpegLibraryDirectory;
            LUV_MUPDF_LIBDIR = mupdfLibraryDirectory;
            LUV_YT_DLP = "${pkgs.yt-dlp}/bin/yt-dlp";
            CL_SOURCE_REGISTRY = "${mcclim}//:${clSdl3WithoutMixer}//";
            # ASDF normally keys outputs by Lisp implementation and source
            # path, but CFFI grovel outputs also depend on native headers.
            # Keep each dependency closure's compiled ABI and FASLs together.
            LUV_ASDF_CACHE_KEY = builtins.baseNameOf developmentClosure;
            PKG_CONFIG_PATH = "${ffmpeg.dev}/lib/pkgconfig";
            CPATH = "${pkgs.vulkan-headers}/include";
            VK_LAYER_PATH =
              "${pkgs.vulkan-validation-layers}/share/vulkan/explicit_layer.d";
          } // nixpkgs.lib.optionalAttrs (lavapipeIcd != null) {
            LUV_LAVAPIPE_ICD =
              "${pkgs.mesa}/share/vulkan/icd.d/${lavapipeIcd}";
          } // nixpkgs.lib.optionalAttrs pkgs.stdenv.hostPlatform.isDarwin {
            VK_DRIVER_FILES =
              "${pkgs.moltenvk}/share/vulkan/icd.d/MoltenVK_icd.json";
          };
          slimDevelopmentEnvironment = {
            LUV_DEV_ENVIRONMENT = "1";
            LUV_NATIVE_LIBRARY_PATH = slimNativeLibraryPath;
            LUV_MESA_LIBRARY_PATH = mesaLibraryPath;
            LUV_BASH = "${pkgs.bashInteractive}/bin/bash";
            LUV_SLYNK_DIR = "${slyRoot}/slynk";
            LUV_SWASH = "${swashPackage}/bin/swash";
            LUV_FFMPEG_LIBDIR = ffmpegLibraryDirectory;
            CL_SOURCE_REGISTRY = "${mcclim}//:${clSdl3WithoutMixer}//";
            LUV_ASDF_CACHE_KEY = builtins.baseNameOf slimDevelopmentClosure;
            PKG_CONFIG_PATH = "${ffmpeg.dev}/lib/pkgconfig";
            CPATH = "${pkgs.vulkan-headers}/include";
            VK_LAYER_PATH =
              "${pkgs.vulkan-validation-layers}/share/vulkan/explicit_layer.d";
          } // nixpkgs.lib.optionalAttrs (lavapipeIcd != null) {
            LUV_LAVAPIPE_ICD =
              "${pkgs.mesa}/share/vulkan/icd.d/${lavapipeIcd}";
          } // nixpkgs.lib.optionalAttrs pkgs.stdenv.hostPlatform.isDarwin {
            VK_DRIVER_FILES =
              "${pkgs.moltenvk}/share/vulkan/icd.d/MoltenVK_icd.json";
          };
          applicationEnvironment = {
            LUV_DEV_ENVIRONMENT = "1";
            LUV_NATIVE_LIBRARY_PATH = applicationNativeLibraryPath;
            LUV_GHOSTTY_LIBRARY = libghosttyVtLibrary;
            LUV_BASH = "${pkgs.bashInteractive}/bin/bash";
            LUV_SLYNK_DIR = "${slynkSource}/slynk";
            LUV_FFMPEG_LIBDIR = ffmpegLibraryDirectory;
            CL_SOURCE_REGISTRY = "${mcclim}//:${clSdl3WithoutMixer}//";
            # Only a name for the FASL cache: without the string context, the
            # hash printed into the launcher does not make the build closure
            # a runtime dependency of the installed program.
            LUV_ASDF_CACHE_KEY = builtins.unsafeDiscardStringContext
              (builtins.baseNameOf applicationClosure);
            PKG_CONFIG_PATH = "${ffmpeg.dev}/lib/pkgconfig";
            CPATH = "${pkgs.vulkan-headers}/include";
          } // nixpkgs.lib.optionalAttrs pkgs.stdenv.hostPlatform.isDarwin {
            VK_DRIVER_FILES =
              "${pkgs.moltenvk}/share/vulkan/icd.d/MoltenVK_icd.json";
          };
          developmentEnvironmentHook = ''
            unset LUV_SLY_SYSTEM

            # Nix's SBCL wrapper prepends colon-separated mappings for its
            # precompiled packages, so use that syntax here too. The closure
            # includes the Lisp compiler; source paths remain distinct below it.
            export ASDF_OUTPUT_TRANSLATIONS="/:''${XDG_CACHE_HOME:-$HOME/.cache}/common-lisp/luv/$LUV_ASDF_CACHE_KEY/:"

            if [ -n "''${LUV_PROJECT_ROOT:-}" ]; then
              case ":''${CL_SOURCE_REGISTRY:-}:" in
                *:"$LUV_PROJECT_ROOT/":*) ;;
                *) export CL_SOURCE_REGISTRY="$LUV_PROJECT_ROOT/:''${CL_SOURCE_REGISTRY:-}" ;;
              esac
            fi

            luv_activate_native_environment() {
              if [ "''${LUV_USE_NIX_MESA:-}" = 1 ]; then
                export LD_LIBRARY_PATH="$LUV_MESA_LIBRARY_PATH:$LUV_NATIVE_LIBRARY_PATH''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
              else
                export LD_LIBRARY_PATH="$LUV_NATIVE_LIBRARY_PATH''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
              fi
              export LUV_NATIVE_ENVIRONMENT_ACTIVE=1
            }

            ${nixpkgs.lib.optionalString pkgs.stdenv.hostPlatform.isLinux ''
              if [ -z "''${SDL_VIDEODRIVER:-}" ] \
                && [ -z "''${DISPLAY:-}" ] \
                && [ -z "''${WAYLAND_DISPLAY:-}" ]; then
                case "$(${pkgs.coreutils}/bin/tty 2>/dev/null || :)" in
                  /dev/tty[0-9]*) export SDL_VIDEODRIVER=kmsdrm ;;
                  *) export SDL_VIDEODRIVER=offscreen ;;
                esac
              fi
            ''}

            if [ "''${SDL_VIDEODRIVER:-}" = kmsdrm ] \
              && [ -z "''${LUV_KEYBOARD_LAYOUT+x}" ] \
              && command -v localectl >/dev/null 2>&1 \
              && [ "$(localectl status 2>/dev/null \
                       | ${pkgs.gnused}/bin/sed -n \
                           's/^[[:space:]]*X11 Variant:[[:space:]]*//p')" = dvorak ]; then
              export LUV_KEYBOARD_LAYOUT=dvorak
            fi

            if [ "''${SDL_VIDEODRIVER:-}" = kmsdrm ] \
              && [ -z "''${LUV_KEYBOARD_SWAP_CAPS_CONTROL+x}" ]; then
              export LUV_KEYBOARD_SWAP_CAPS_CONTROL=1
            fi

            if [ -n "''${LUV_LAVAPIPE_ICD:-}" ] \
              && [ -f "$LUV_LAVAPIPE_ICD" ] \
              && [ -z "''${VK_DRIVER_FILES:-}" ] \
              && [ "''${SDL_VIDEODRIVER:-}" = offscreen ]; then
              export VK_DRIVER_FILES="$LUV_LAVAPIPE_ICD"
            fi
          '';
          slimDevelopmentEnvironmentHook = developmentEnvironmentHook + ''
            unset LUV_GHOSTTY_LIBRARY
            unset LUV_MUPDF_LIBDIR LUV_YT_DLP
          '';
          developmentClosure = pkgs.buildEnv {
            name = "luv-development-closure";
            paths = developmentPackages;
          };
          slimDevelopmentClosure = pkgs.buildEnv {
            name = "luv-slim-development-closure";
            paths = slimDevelopmentPackages;
          };
          applicationClosure = pkgs.buildEnv {
            name = "luv-application-closure";
            paths = applicationPackages;
          };
        in
        {
          inherit pkgs wpePkgs sbcl sbcl268 lisp clSdl3WithoutMixer;
          inherit developmentClosure developmentPackages developmentEnvironment;
          inherit slimDevelopmentClosure slimDevelopmentPackages slimDevelopmentEnvironment;
          inherit applicationClosure applicationPackages applicationEnvironment;
          inherit developmentEnvironmentHook slimDevelopmentEnvironmentHook;
          inherit nativeLibraryPath mesaLibraryPath;
          inherit slyRoot slynkSource swashPackage;
          inherit ffmpeg ffmpegLibraryDirectory mupdf mupdfLibraryDirectory;
          inherit libghosttyVt libghosttyVtLibrary;
          inherit tracyClient tracyClientLibrary tracyTools;
        };
      # The `nix run` programs: a dumped SBCL image per game, built from the
      # application environment above rather than the development shell.
      applicationPackage = system: { name, buildScript, executable }:
        let
          environment = environmentFor system;
          inherit (environment) pkgs;
          inherit (pkgs) lib;
          output = builtins.placeholder "out";
          runtimeEnvironment = lib.filterAttrs
            (variable: _value: builtins.elem variable [
              "LUV_ASDF_CACHE_KEY"
              "LUV_BASH"
              "LUV_FFMPEG_LIBDIR"
              "LUV_GHOSTTY_LIBRARY"
              "LUV_NATIVE_LIBRARY_PATH"
              "VK_DRIVER_FILES"
            ])
            environment.applicationEnvironment;
          exportRuntimeEnvironment = lib.concatStringsSep "\n"
            (lib.mapAttrsToList
              (variable: value:
                "export ${variable}=${lib.escapeShellArg (toString value)}")
              runtimeEnvironment);
          runtimePath = lib.makeBinPath [
            pkgs.bashInteractive
            pkgs.coreutils
            environment.ffmpeg
          ];
        in
        pkgs.stdenv.mkDerivation (environment.applicationEnvironment // {
          pname = name;
          version = "0-unstable";
          src = self;

          nativeBuildInputs = environment.applicationPackages;
          dontConfigure = true;
          dontPatchELF = true;
          dontStrip = true;

          buildPhase = ''
            runHook preBuild

            mkdir -p "$out/share"
            cp -R . "$out/share/luv"
            chmod -R u+w "$out/share/luv"
            cd "$out/share/luv"

            export HOME="$TMPDIR/home"
            export LUV_PROJECT_ROOT="$PWD"
            mkdir -p "$HOME"
            ${environment.developmentEnvironmentHook}
            luv_activate_native_environment

            sbcl --script ${buildScript}
            runHook postBuild
          '';

          installPhase = ''
            runHook preInstall
            cd "$out/share/luv"
            mkdir -p "$out/bin" "$out/lib/fasl" "$out/libexec"
            mv "build/${executable}" "$out/libexec/${name}"
            # CFFI restores foreign libraries from FASLs when a dumped image
            # starts. Shipping the build cache avoids recompiling them into a
            # new user's home while the first canvas is already starting.
            cp -R "$HOME/.cache/common-lisp/luv/$LUV_ASDF_CACHE_KEY/." "$out/lib/fasl/"
            # CFFI's groveller leaves its C program, object, and executable
            # beside the FASL.  None is loaded at runtime, and their debug
            # information would keep the whole C toolchain and SDK alive.
            find "$out/lib/fasl" -type f \( -name '*__grovel' \
              -o -name '*__grovel.c' -o -name '*__grovel.o' \) -delete
            ${lib.optionalString pkgs.stdenv.hostPlatform.isDarwin ''
              # SBCL reopens loaded shared objects by their original absolute
              # path before MAIN runs. Preserve the generated Objective-C
              # bridge and the source directory in its .../objective-c/../build
              # pathname exactly as the dumped image loaded them.
              find build -mindepth 1 -maxdepth 1 \
                ! -name 'objective-c-exception-bridge-*.dylib' \
                -exec rm -rf {} +
            ''}
            # The dumped image contains the Lisp program. Keep only the files
            # it opens at runtime, not a second copy of the whole repository.
            find . -mindepth 1 -maxdepth 1 ! -name fonts ${lib.optionalString pkgs.stdenv.hostPlatform.isDarwin "! -name build ! -name objective-c"} -exec rm -rf {} +
            # These private wiki fonts are git-annex links, not game resources.
            rm -rf fonts/equity

            cat > "$out/bin/${name}" <<'LUV_LAUNCHER'
            #!${pkgs.runtimeShell}
            set -eu

            ${exportRuntimeEnvironment}
            export LUV_PROJECT_ROOT=${lib.escapeShellArg "${output}/share/luv"}
            export PATH=${lib.escapeShellArg runtimePath}:"''${PATH:-}"
            asdf_output_translations="''${ASDF_OUTPUT_TRANSLATIONS:-}"
            ${environment.developmentEnvironmentHook}
            luv_activate_native_environment
            # The hook points ASDF at the developer cache. A launched program
            # reads the FASLs shipped beside it unless the caller says otherwise.
            export ASDF_OUTPUT_TRANSLATIONS="''${asdf_output_translations:-${lib.escapeShellArg "/:${output}/lib/fasl//"}}"
            unset asdf_output_translations

            ${lib.optionalString (name == "luvcraft") ''
              if [ -z "''${LUVCRAFT_SLYNK_ENDPOINT:-}" ]; then
                runtime_directory="''${XDG_RUNTIME_DIR:-''${TMPDIR:-/tmp}/luvcraft-$(${pkgs.coreutils}/bin/id -u)}"
                ${pkgs.coreutils}/bin/mkdir -p "$runtime_directory"
                export LUVCRAFT_SLYNK_ENDPOINT="$runtime_directory/luvcraft.slynk"
              fi
            ''}
            exec ${lib.escapeShellArg "${output}/libexec/${name}"} "$@"
            LUV_LAUNCHER
            chmod 0555 "$out/bin/${name}"
            runHook postInstall
          '';

          meta = {
            description = if name == "luvcraft"
              then "The original procedural block world"
              else "The second-generation cubical atelier";
            mainProgram = name;
            platforms = systems;
          };
        });
      # luv-shaderc compiles shader programs ahead of time for a native
      # renderer.  It needs the shader language and its lowerings alone, so
      # it is built from those sources by an SBCL holding only Closer-MOP:
      # no SDL, Vulkan, Metal, McCLIM, or workbench.
      shadercPackage = system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          inherit (nixpkgs) lib;
          closerMop = pkgs.sbcl.pkgs.closer-mop;
          shadercLisp = pkgs.sbcl.withPackages (_lispPackages: [ closerMop ]);
        in
        pkgs.stdenv.mkDerivation {
          pname = "luv-shaderc";
          version = "0-unstable";
          src = lib.fileset.toSource {
            root = ./.;
            fileset = lib.fileset.unions [
              ./luv.asd
              ./arithmetic
              ./domains
              ./hal/shader
              ./hal/metal/msl
              ./hal/d3d12
              ./hal/vulkan/spir-v
              ./hal/shaderc
              ./scripts/build-luv-shaderc.lisp
            ];
          };
          nativeBuildInputs = [ shadercLisp ];
          dontConfigure = true;
          # The executable is an SBCL image; stripping would remove its core.
          dontStrip = true;
          dontPatchELF = true;
          buildPhase = ''
            runHook preBuild
            export HOME="$TMPDIR/home"
            mkdir -p "$HOME" "$out/share"
            # Build in place under $out so the image's source locations name
            # its own output rather than a separate store path.
            cp -R . "$out/share/luv-shaderc"
            chmod -R u+w "$out/share/luv-shaderc"
            cd "$out/share/luv-shaderc"
            sbcl --non-interactive --load scripts/build-luv-shaderc.lisp
            runHook postBuild
          '';
          installPhase = ''
            runHook preInstall
            mkdir -p "$out/bin"
            mv build/luv-shaderc "$out/bin/luv-shaderc"
            rm -rf build
            # The image records where ASDF, Closer-MOP, and the unpacked source
            # were loaded from, though it never reads them again; on Darwin
            # the SBCL package alone would drag in a 400 MiB bootstrap
            # closure.  Blank those store hashes in the core, which lies past
            # the runtime's code signature (so remove-references-to's
            # re-signing fails).  The runtime's own libraries stay referenced.
            for reference in ${pkgs.sbcl} ${closerMop} ${closerMop.src} \
                "$src"; do
              hash=$(basename "$reference" | cut -c1-32)
              env LC_ALL=C sed -i "s/$hash/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee/g" \
                "$out/bin/luv-shaderc"
            done
            runHook postInstall
          '';
          meta = {
            description = "Compile Luv shader programs to MSL, HLSL, and reflection";
            mainProgram = "luv-shaderc";
            platforms = systems;
          };
        };
    in
    {
      lib = {
        inherit environmentFor;
      };

      packages = forAllSystems (system:
        let
          env = environmentFor system;
          luvcraft = applicationPackage system {
            name = "luvcraft";
            buildScript = "luvcraft/build.lisp";
            executable = "luvcraft";
          };
          luft = applicationPackage system {
            name = "luft";
            buildScript = "luft/build.lisp";
            executable = "luft-atelier";
          };
          luv-shaderc = shadercPackage system;
        in {
          inherit luvcraft luft luv-shaderc;
          sbcl = env.sbcl;
          sbcl-2_6_8 = env.sbcl268;
          lisp = env.lisp;
          environment = env.developmentClosure;
          slim-environment = env.slimDevelopmentClosure;
          ffmpeg = env.ffmpeg;
          libghostty-vt = env.libghosttyVt;
          tracy-client = env.tracyClient;
          tracy = env.tracyTools;
          swash = env.swashPackage;
          default = luvcraft;
        } // nixpkgs.lib.optionalAttrs env.pkgs.stdenv.hostPlatform.isLinux {
          # Reachable only by name, `nix build .#wpewebkit` on Linux: not in
          # `default`, not in the dev shell, not in any library path.
          wpewebkit = env.wpePkgs.wpewebkit;
        });

      devShells = forAllSystems (system:
        let
          env = environmentFor system;
          shellEnvironment = env.developmentEnvironment // {
            LUV_DEV_ENVIRONMENT_MODE = "nix-develop";
            LUV_DEV_SHELL = "default";
            packages = env.developmentPackages;
            shellHook = env.developmentEnvironmentHook + ''
              luv_activate_native_environment
            '';
          };
        in {
          default = env.pkgs.mkShell shellEnvironment;
          # Remote and non-terminal work should not have to realize Ghostty's
          # Zig toolchain and generated application-wide dependency cache.
          slim = env.pkgs.mkShell (env.slimDevelopmentEnvironment // {
            LUV_DEV_ENVIRONMENT_MODE = "nix-develop";
            LUV_DEV_SHELL = "slim";
            packages = env.slimDevelopmentPackages;
            shellHook = env.slimDevelopmentEnvironmentHook + ''
              luv_activate_native_environment
            '';
          });
          # Showcase publication alone needs git-annex.  It brings GHC and
          # its closure, so keep ordinary Lisp and capture development lean.
          # `scripts/showcase` enters this shell on Chapel and SWA itself.
          # Enter with `nix develop .#annex` for direct annex work.
          annex = env.pkgs.mkShell (shellEnvironment // {
            packages = shellEnvironment.packages ++ [ env.pkgs.git-annex ];
          });
          # SDL_mixer is not used by luvcraft's audio path, and its Darwin
          # build need not hold up the ordinary development shell.  Keep it
          # available for experiments without making it a default dependency.
          # Enter with `nix develop .#mixer`.
          mixer = env.pkgs.mkShell (shellEnvironment // {
            packages = shellEnvironment.packages ++ [ env.pkgs.sdl3-mixer ];
            LUV_NATIVE_LIBRARY_PATH =
              nixpkgs.lib.makeLibraryPath [ env.pkgs.sdl3-mixer ]
              + ":${shellEnvironment.LUV_NATIVE_LIBRARY_PATH}";
            CL_SOURCE_REGISTRY = "${mcclim}//:${cl-sdl3}//";
          });
          # Tracy is intentionally opt-in: its viewer is a large C++ build
          # that should not hold up the ordinary Lisp development shell.
          # Enter with `nix develop .#tracy`.
          tracy = env.pkgs.mkShell (shellEnvironment // {
            packages = shellEnvironment.packages ++ [
              env.tracyClient
              env.tracyTools
            ];
            LUV_NATIVE_LIBRARY_PATH =
              nixpkgs.lib.makeLibraryPath [ env.tracyClient ]
              + ":${shellEnvironment.LUV_NATIVE_LIBRARY_PATH}";
            LUV_TRACY_CLIENT = env.tracyClientLibrary;
            LUV_TRACY_CAPTURE = "${env.tracyTools}/bin/tracy-capture";
            LUV_TRACY_PROFILER = "${env.tracyTools}/bin/tracy";
          });
        });

      apps = forAllSystems (system: {
        default = {
          type = "app";
          program = "${self.packages.${system}.luvcraft}/bin/luvcraft";
        };
        luvcraft = {
          type = "app";
          program = "${self.packages.${system}.luvcraft}/bin/luvcraft";
        };
        luft = {
          type = "app";
          program = "${self.packages.${system}.luft}/bin/luft";
        };
        luv-shaderc = {
          type = "app";
          program = "${self.packages.${system}.luv-shaderc}/bin/luv-shaderc";
        };
      });
    };
}
