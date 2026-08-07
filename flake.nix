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
          lavapipeIcd =
            {
              x86_64-linux = "lvp_icd.x86_64.json";
              aarch64-linux = "lvp_icd.aarch64.json";
            }.${system} or null;
          # The generated vk binding is vendored in this repository. Keep its
          # ordinary dependencies available without installing Nixpkgs' vk.
          lisp = pkgs.sbcl.withPackages (lispPackages: [
            lispPackages.alexandria
            lispPackages.cffi
            lispPackages.rove
          ]);
          slyRoot =
            "${pkgs.emacsPackages.sly}/share/emacs/site-lisp/elpa/${pkgs.emacsPackages.sly.pname}-${pkgs.emacsPackages.sly.version}";
        in
        { inherit pkgs lisp nativeLibraryPath lavapipeIcd slyRoot; };
    in
    {
      devShells = forAllSystems (system:
        let env = environmentFor system;
        in {
          default = env.pkgs.mkShell {
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
            VK_DRIVER_FILES =
              if env.pkgs.stdenv.isDarwin then
                "${env.pkgs.moltenvk}/share/vulkan/icd.d/MoltenVK_icd.json"
              else if env.lavapipeIcd != null then
                "${env.pkgs.mesa}/share/vulkan/icd.d/${env.lavapipeIcd}"
              else
                "";
            LUV_SLYNK_DIR = "${env.slyRoot}/slynk";
          };
        });
    };
}
