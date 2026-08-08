{
  description = "luv — a Common Lisp Vulkan atelier";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { nixpkgs, ... }:
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
          lisp = pkgs.sbcl.withPackages (lispPackages: [
            lispPackages.alexandria
            lispPackages.cffi
            lispPackages.rove
          ]);
          slyRoot =
            "${pkgs.emacsPackages.sly}/share/emacs/site-lisp/elpa/${pkgs.emacsPackages.sly.pname}-${pkgs.emacsPackages.sly.version}";
        in
        { inherit pkgs lisp nativeLibraryPath slyRoot; };
    in
    {
      devShells = forAllSystems (system:
        let env = environmentFor system;
        in {
          default = env.pkgs.mkShell ({
            packages = [
              env.lisp
              env.pkgs.libffi
              env.pkgs.mesa
              env.pkgs.python3
              env.pkgs.pkg-config
              env.pkgs.sdl3
              env.pkgs.vulkan-tools
            ];
            LD_LIBRARY_PATH = env.nativeLibraryPath;
            LUV_SLYNK_DIR = "${env.slyRoot}/slynk";
          } // nixpkgs.lib.optionalAttrs env.pkgs.stdenv.isDarwin {
            VK_DRIVER_FILES =
              "${env.pkgs.moltenvk}/share/vulkan/icd.d/MoltenVK_icd.json";
          });
        });
    };
}
