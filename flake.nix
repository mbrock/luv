{
  description = "luv — a Common Lisp Vulkan atelier";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.mcclim = {
    url = "git+https://codeberg.org/McCLIM/McCLIM.git?ref=master";
    flake = false;
  };
  inputs.cl-sdl3 = {
    url = "git+https://github.com/aiffc/cl-sdl3.git?rev=47c90b54715aba23752b70d382a3eb310172cd34";
    flake = false;
  };

  outputs = { nixpkgs, mcclim, cl-sdl3, ... }:
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
          nativeLibraryPath = nixpkgs.lib.makeLibraryPath (
            [
              pkgs.libffi
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
              lispPackages.cl-fad
              lispPackages.closer-mop
              lispPackages.defclass-std
              lispPackages.eclector
              lispPackages.named-readtables
              lispPackages.rove
              lispPackages.spinneret
            ] ++ mcclimDependencies
              ++ nixpkgs.lib.optionals pkgs.stdenv.isDarwin [
                lispPackages.float-features
                lispPackages.trivial-main-thread
              ]);
          slyRoot =
            "${pkgs.emacsPackages.sly}/share/emacs/site-lisp/elpa/${pkgs.emacsPackages.sly.pname}-${pkgs.emacsPackages.sly.version}";
          dev = pkgs.writeShellApplication {
            name = "luv-env";
            runtimeInputs = [
              lisp
              pkgs.libffi
              pkgs.mesa
              pkgs.python3
              pkgs.pkg-config
              pkgs.sdl3
              pkgs.spirv-tools
              pkgs.vulkan-tools
            ];
            text = ''
              export LUV_NIX_SHELL=1
              export LUV_SLYNK_DIR=${slyRoot}/slynk
              export CL_SOURCE_REGISTRY=${mcclim}//:${cl-sdl3}//
              export LD_LIBRARY_PATH=${nativeLibraryPath}''${LD_LIBRARY_PATH:+:}''${LD_LIBRARY_PATH:-}

              ${nixpkgs.lib.optionalString (system == "x86_64-linux") ''
                export LUV_LAVAPIPE_ICD=${pkgs.mesa}/share/vulkan/icd.d/lvp_icd.x86_64.json
              ''}
              ${nixpkgs.lib.optionalString (system == "aarch64-linux") ''
                export LUV_LAVAPIPE_ICD=${pkgs.mesa}/share/vulkan/icd.d/lvp_icd.aarch64.json
              ''}
              ${nixpkgs.lib.optionalString pkgs.stdenv.isDarwin ''
                export VK_DRIVER_FILES=${pkgs.moltenvk}/share/vulkan/icd.d/MoltenVK_icd.json
              ''}

              if [[ -z "''${SDL_VIDEODRIVER:-}" \
                    && -z "''${DISPLAY:-}" \
                    && -z "''${WAYLAND_DISPLAY:-}" ]]; then
                export SDL_VIDEODRIVER=offscreen
              fi

              if [[ -n "''${LUV_LAVAPIPE_ICD:-}" \
                    && -f "$LUV_LAVAPIPE_ICD" \
                    && -z "''${VK_DRIVER_FILES:-}" \
                    && "''${SDL_VIDEODRIVER:-}" == offscreen ]]; then
                export VK_DRIVER_FILES="$LUV_LAVAPIPE_ICD"
              fi

              if (( $# == 0 )); then
                exec sbcl
              fi
              exec "$@"
            '';
          };
        in
        { inherit pkgs sbcl lisp nativeLibraryPath slyRoot dev; };
    in
    {
      packages = forAllSystems (system:
        let
          env = environmentFor system;
        in {
          sbcl = env.sbcl;
          lisp = env.lisp;
          dev = env.dev;
          default = env.lisp;
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
              env.pkgs.libffi
              env.pkgs.mesa
              env.pkgs.python3
              env.pkgs.pkg-config
              env.pkgs.sdl3
              env.pkgs.spirv-tools
              env.pkgs.vulkan-tools
            ];
            LD_LIBRARY_PATH = env.nativeLibraryPath;
            LUV_NIX_SHELL = "1";
            LUV_SLYNK_DIR = "${env.slyRoot}/slynk";
            CL_SOURCE_REGISTRY = "${mcclim}//:${cl-sdl3}//";
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
