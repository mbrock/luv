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
            pkgs.sdl3
            pkgs.sdl3-image
            pkgs.sdl3-mixer
            pkgs.sdl3-ttf
            pkgs.vulkan-loader
          ];
          # The generated vk binding is vendored in this repository. Keep its
          # ordinary dependencies available without installing Nixpkgs' vk.
          lisp = pkgs.sbcl.withPackages (lispPackages: [
            lispPackages.alexandria
            lispPackages.cffi
          ]);
        in
        { inherit pkgs lisp nativeLibraryPath; };
    in
    {
      devShells = forAllSystems (system:
        let env = environmentFor system;
        in {
          default = env.pkgs.mkShell {
            packages = [
              env.lisp
              env.pkgs.libffi
              env.pkgs.python3
              env.pkgs.pkg-config
              env.pkgs.sdl3
              env.pkgs.vulkan-tools
            ];
            LD_LIBRARY_PATH = env.nativeLibraryPath;
          };
        });
    };
}
