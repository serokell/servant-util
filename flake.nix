{
  nixConfig = {
    flake-registry = "https://github.com/serokell/flake-registry/raw/master/flake-registry.json";
  };

  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.url = "github:serokell/nixpkgs";
    hackage.flake = false;
    stackage.flake = false;
  };

  outputs = { self, nixpkgs, flake-utils, haskell-nix, hackage, stackage, ... }@inputs:
  flake-utils.lib.eachSystem ["x86_64-linux"] (system: let
    pkgs = import inputs.nixpkgs {
      inherit system;
      overlays = [ inputs.haskell-nix.overlay ];
    };

    # run ci with resolver defined in stack.yaml and a newer resolver for servant 0.19
    extraDeps = {
      default = null;
      "servant-0.19" = ''
        - servant-client-core-0.19
        - servant-client-0.19
        - servant-0.19
      '';
    };

    local-packages = [
      { name = "servant-util";         subdirectory = "servant-util"; }
      { name = "servant-util-beam-pg"; subdirectory = "servant-util-beam-pg"; }
    ];

    # names of all local packages
    local-packages-names = map (p: p.name) local-packages;

    make-hs-pkgs = extra_dep: let
      # source with gitignored files filtered out
      projectSrc = pkgs.haskell-nix.haskellLib.cleanGit { name = "servant-util"; src = ./.; };

      # haskell.nix does not support 'include' in package.yaml, we have to generate .cabal ourselves
      cabalFiles = pkgs.runCommand "servant-util-cabal-files" {} ''
        mkdir -p $out
        ${pkgs.lib.concatMapStrings ({ name, subdirectory }: ''
          ${pkgs.haskellPackages.hpack}/bin/hpack ${projectSrc}/${subdirectory} - > $out/${name}.cabal
        '') local-packages}
      '';

      cabalsrc = pkgs.runCommand "src-with-cabal" {} ''
        cp -r --no-preserve=mode ${projectSrc} $out
        ${pkgs.lib.concatMapStrings ({ name, subdirectory }: ''
          cp ${cabalFiles}/${name}.cabal $out/${subdirectory}
        '') local-packages}
      '';

      # inject the extra dependencies at the top of the extra-deps list to create a build with them included
      src = (if extra_dep == null then cabalsrc else pkgs.runCommand "extradep-src" { } ''
        mkdir -p $out
        cp -r ${cabalsrc}/* .
        substituteInPlace stack.yaml --replace "extra-deps:" "extra-deps:
        ${extra_dep}"
        cp -r ./* $out
      '');
    in pkgs.haskell-nix.stackProject {
      # project src with .cabal files added
      inherit src;
      ignorePackageYaml = true;

      modules = [{
        # configuration for local packages
        packages = pkgs.lib.genAttrs local-packages-names (packageName: {
          ghcOptions = [ "-O0" "-Werror" ];
          # run haddock for local packages
          doHaddock = true;
        });

        # don't haddock dependencies
        doHaddock = false;
      }];
    };

    make-tests = depname: extra_dep: let
      hs-pkgs = make-hs-pkgs extra_dep;
      comps = map (pkg: hs-pkgs.${pkg}.components) local-packages-names;
      component-tests = pkgs.lib.fold pkgs.lib.mergeAttrs {} (builtins.catAttrs "tests" comps);

      mkTest = name: drv: {
        name = "${name}-${depname}";
        value = pkgs.runCommandLocal "${name}-run" {} ''
          ${drv}/bin/${name} && touch $out
        '';
      };
    in pkgs.lib.mapAttrs' mkTest component-tests;

  in with pkgs.lib; {
    checks = fold mergeAttrs {} (mapAttrsToList make-tests extraDeps);
  });
}
