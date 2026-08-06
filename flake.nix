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
          mcpStdio = pkgs.writeShellApplication {
            name = "luv-mcp-stdio";
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
          mcpBridge = pkgs.writeShellApplication {
            name = "luv-mcp-bridge";
            runtimeInputs = [ pkgs.python3 ];
            text = ''
              exec python3 \
                /home/mbrock/quicklisp/local-projects/cl-mcp/scripts/stdio_tcp_bridge.py \
                --host "''${LUV_MCP_HOST:-127.0.0.1}" \
                --port "''${LUV_MCP_PORT:-12345}"
            '';
          };
        in
        { inherit pkgs lisp mcpBridge mcpStdio nativeLibraryPath; };
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
          mcp = env.mcpBridge;
          mcp-stdio = env.mcpStdio;
          default = env.mcpBridge;
        });

      apps = forAllSystems (system:
        let env = environmentFor system;
        in {
          mcp = {
            type = "app";
            program = "${env.mcpBridge}/bin/luv-mcp-bridge";
          };
          mcp-stdio = {
            type = "app";
            program = "${env.mcpStdio}/bin/luv-mcp-stdio";
          };
          default = {
            type = "app";
            program = "${env.mcpBridge}/bin/luv-mcp-bridge";
          };
        });
    };
}
