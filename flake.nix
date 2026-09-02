{
  description = "Luvcraft and Luft — Common Lisp GPU experiments";

  nixConfig = {
    extra-substituters = [ "https://luv.swa.sh/nix-cache?priority=30" ];
    extra-trusted-public-keys = [
      "luv.swa.sh-1:PpD45iCBkJ38ZkvlyZcLiGdIz6yVehXn3fm1JvG18Bw="
    ];
  };

  inputs.toolchain.url = "path:./nix";

  outputs = { self, toolchain }:
    let
      systems = builtins.attrNames toolchain.packages;
      forAllSystems = function:
        builtins.listToAttrs (map (system: {
          name = system;
          value = function system;
        }) systems);
      applicationPackage = system: { name, buildScript, executable }:
        let
          environment = toolchain.lib.environmentFor system;
          inherit (environment) pkgs;
          inherit (pkgs) lib;
          output = builtins.placeholder "out";
          runtimeEnvironment = lib.filterAttrs
            (variable: _value: builtins.elem variable [
              "LUV_BASH"
              "LUV_FFMPEG_LIBDIR"
              "LUV_GHOSTTY_LIBRARY"
              "LUV_LAVAPIPE_ICD"
              "LUV_MESA_LIBRARY_PATH"
              "LUV_MUPDF_LIBDIR"
              "LUV_NATIVE_LIBRARY_PATH"
              "LUV_URBIT"
              "LUV_YT_DLP"
              "VK_DRIVER_FILES"
              "VK_LAYER_PATH"
            ])
            environment.developmentEnvironment;
          exportRuntimeEnvironment = lib.concatStringsSep "\n"
            (lib.mapAttrsToList
              (variable: value:
                "export ${variable}=${lib.escapeShellArg (toString value)}")
              runtimeEnvironment);
          runtimePath = lib.makeBinPath [
            pkgs.bashInteractive
            pkgs.coreutils
            environment.ffmpeg
            pkgs.urbit
            pkgs.yt-dlp
          ];
        in
        pkgs.stdenv.mkDerivation (environment.developmentEnvironment // {
          pname = name;
          version = "0-unstable";
          src = self;

          nativeBuildInputs = environment.developmentPackages;
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
            cp -R "$HOME/.cache/common-lisp/"*/. "$out/lib/fasl/"
            # The dumped image contains the Lisp program. Keep only the fonts
            # it opens at runtime, not a second copy of the whole repository.
            find . -mindepth 1 -maxdepth 1 ! -name fonts -exec rm -rf {} +
            # These private wiki fonts are git-annex links, not game resources.
            rm -rf fonts/equity

            cat > "$out/bin/${name}" <<'LUV_LAUNCHER'
            #!${pkgs.runtimeShell}
            set -eu

            ${exportRuntimeEnvironment}
            export LUV_PROJECT_ROOT=${lib.escapeShellArg "${output}/share/luv"}
            export PATH=${lib.escapeShellArg runtimePath}:"''${PATH:-}"
            if [ -z "''${ASDF_OUTPUT_TRANSLATIONS:-}" ]; then
              export ASDF_OUTPUT_TRANSLATIONS=${lib.escapeShellArg "/:${output}/lib/fasl//"}
            fi
            ${environment.developmentEnvironmentHook}
            luv_activate_native_environment

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
    in {
      packages = forAllSystems (system:
        let
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
        in {
          inherit luvcraft luft;
          default = luvcraft;
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
      });
    };
}
