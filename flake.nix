{
  description = "luv — a Common Lisp Vulkan atelier";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { nixpkgs, ... }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
      environmentFor = system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          nativeLibraryPath = nixpkgs.lib.makeLibraryPath [
            pkgs.libffi
            pkgs.openssl
            pkgs.sdl3
            pkgs.sdl3-image
            pkgs.sdl3-mixer
            pkgs.sdl3-ttf
            pkgs.vulkan-loader
          ];
          # Nixpkgs' vk package handles its unusually large generated binding;
          # ordinary Lisp dependencies, including cl-mcp, stay in Quicklisp.
          lisp = pkgs.sbcl.withPackages (lispPackages: [ lispPackages.vk ]);
          mcp = pkgs.writeShellApplication {
            name = "luv-mcp";
            runtimeInputs = [ lisp ];
            text = ''
              export MCP_PROJECT_ROOT="''${MCP_PROJECT_ROOT:-$PWD}"
              export LD_LIBRARY_PATH="${nativeLibraryPath}:''${LD_LIBRARY_PATH:-}"
              exec sbcl --dynamic-space-size 4096 --noinform --non-interactive \
                --load /home/mbrock/quicklisp/setup.lisp \
                --eval '(let ((*standard-output* *error-output*) (*trace-output* *error-output*)) (ql:quickload :cl-mcp :silent t))' \
                --eval '(cl-mcp:run :transport :stdio)'
            '';
          };
        in
        { inherit pkgs lisp mcp nativeLibraryPath; };
    in
    {
      devShells = forAllSystems (system:
        let env = environmentFor system;
        in {
          default = env.pkgs.mkShell {
            packages = [
              env.lisp
              env.pkgs.libffi
              env.pkgs.pkg-config
              env.pkgs.sdl3
              env.pkgs.vulkan-tools
            ];
            LD_LIBRARY_PATH = env.nativeLibraryPath;
          };
        });

      packages = forAllSystems (system:
        let env = environmentFor system;
        in {
          mcp = env.mcp;
          default = env.mcp;
        });

      apps = forAllSystems (system:
        let env = environmentFor system;
        in {
          mcp = {
            type = "app";
            program = "${env.mcp}/bin/luv-mcp";
          };
          default = {
            type = "app";
            program = "${env.mcp}/bin/luv-mcp";
          };
        });
    };
}
