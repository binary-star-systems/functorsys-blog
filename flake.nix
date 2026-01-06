{
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit.url = "github:cachix/git-hooks.nix";
    pre-commit.inputs.nixpkgs.follows = "nixpkgs";

    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      haskellNix,
      pre-commit,
      treefmt-nix,
    }:
    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
    in
    flake-utils.lib.eachSystem supportedSystems (
      system:
      let
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };

        typst = pkgs.typst.withPackages (
          p: with p; [
            fletcher_0_5_8
            cetz_0_4_2
            oxifmt_0_2_1
            cmarker_0_1_5
            showybox_2_0_4
            self.packages.${system}.html-shim
            bullseye
          ]
        );

        runtimeDeps = [
          typst
        ]
        ++ (with pkgs; [
          minhtml
          self.packages.${system}.typst-html-wrapper
          tailwindcss_4
        ]);

        overlays = [
          haskellNix.overlay
          (final: _prev: {
            haskell-nix = _prev.haskell-nix // {
              extraPkgconfigMappings = _prev.haskell-nix.extraPkgconfigMappings // {
                # String pkgconfig-depends names are mapped to lists of Nixpkgs
                # package names
                "z3" = [ "z3" ];
              };
            };
            websiteProject = final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc9122";
              # This is used by `nix develop .` to open a shell for use with
              # `cabal`, `hlint` and `haskell-language-server`
              shell.tools = {
                cabal = { };
                hlint = { };
                haskell-language-server = { };
                cabal-gild = { };
                fourmolu = { };
              };
              # Non-Haskell shell tools go here
              shell.buildInputs =
                (with pkgs; [
                  just
                  rsync
                ])
                ++ runtimeDeps;
            };
          })
        ];

        flake = pkgs.websiteProject.flake { };

        treefmtEval = treefmt-nix.lib.evalModule pkgs ((import ./nix/treefmt.nix) { inherit pkgs; });

      in
      flake
      // {
        formatter = treefmtEval.config.build.wrapper;

        checks = {

          pre-commit-check = pre-commit.lib.${system}.run {
            src = ./.;
            hooks = {
              treefmt.enable = true;
              treefmt.package = treefmtEval.config.build.wrapper;
              check-merge-conflicts.enable = true;
              hlint.enable = true;
              cabal-gild.enable = true;
              trim-trailing-whitespace.enable = true;
              end-of-file-fixer.enable = true;
              mixed-line-endings.enable = true;
            };
          };
        };

        packages = flake.packages // {
          website-unwrapped = flake.packages."website:exe:website";
          website = pkgs.stdenvNoCC.mkDerivation {
            name = "website";
            src = self.packages.${system}.website-unwrapped;
            nativeBuildInputs = [ pkgs.makeWrapper ];
            installPhase = ''
              install -Dm755 ./bin/website $out/bin/website
              wrapProgram $out/bin/website \
                --prefix PATH : ${
                  pkgs.lib.makeBinPath runtimeDeps

                }
            '';
          };

          site = pkgs.stdenvNoCC.mkDerivation {
            name = "site";
            src = ./.;
            nativeBuildInputs = [ self.packages.${system}.website ];

            LANG = "en_US.UTF-8";
            LOCALE_ARCHIVE = pkgs.lib.optionalString (
              pkgs.stdenv.buildPlatform.libc == "glibc"
            ) "${pkgs.glibcLocales}/lib/locale/locale-archive";

            TZ = "UTC";
            GIT_COMMIT_HASH = builtins.toString (if (self ? rev) then self.rev else "unstable");
            LAST_COMMIT_TIMESTAMP = builtins.toString (self.lastModified);

            buildPhase = ''
              website build
            '';
            installPhase = ''
              mkdir -p $out
              mv ./_site/* $out
            '';
          };

          default = self.packages.${system}.website;

          html-shim = pkgs.buildTypstPackage {
            pname = "html-shim";
            version = "0.1.0";
            src = ./typst/pkgs/html-shim/0.1.0;
          };

          typst-html-wrapper = pkgs.writeShellScriptBin "typst-html-wrapper" ''
            ${pkgs.lib.getExe typst} compile "$@" --features html --format html - - | head -n -2 | tail -n +8
          '';
        };

        apps =
          let
            preview-drv =
              let
                siteFiles = self.packages.${system}.site;
                caddyfile = pkgs.writeText "Caddyfile" ''
                  :8000 {
                      root * ${siteFiles}
                      file_server
                      try_files {path} {path}.html {path}/ =404
                      header Cache-Control max-age=0
                  }
                '';

                formattedCaddyfile = pkgs.runCommand "Caddyfile" {
                  nativeBuildInputs = [ pkgs.caddy ];
                } ''(caddy fmt ${caddyfile} || :) > "$out"'';

                script = pkgs.writeShellApplication {
                  name = "preview";

                  runtimeInputs = [ pkgs.caddy ];

                  text = "caddy run --config ${formattedCaddyfile} --adapter caddyfile";
                };

              in
              script;
          in
          {
            default = flake-utils.lib.mkApp {
              drv = preview-drv;
            };
          };
      }
    );

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://luminite.cachix.org"
      "https://nix-community.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "luminite.cachix.org-1:+VgO/GJMmqsp4U79+QFle7TtEwT8LrJXPiImA8a3a3o="
    ];
    allow-import-from-derivation = "true";
  };
}
