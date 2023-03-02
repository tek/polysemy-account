{ config, lib, ... }:
with builtins;
with lib;
let

  mergeAttr = a: b:
  if isAttrs a
  then merge a b
  else if isList a
  then a ++ b
  else b;

  merge = l: r:
  let
    f = name:
    if hasAttr name l && hasAttr name r
    then mergeAttr l.${name} r.${name}
    else l.${name} or r.${name};
  in genAttrs (concatMap attrNames [l r]) f;

  paths = name: {
    when = {
      condition = false;
      generated-other-modules = ["Paths_${replaceStrings ["-"] ["_"] name}"];
    };
  };

  meta = {
    version = import ./version.nix;
    license = "BSD-2-Clause-Patent";
    license-file = "LICENSE";
    author = "Torsten Schmits";
    maintainer = "hackage@tryp.io";
    copyright = "2023 Torsten Schmits";
    category = "Web";
    build-type = "Simple";
  };

  options.ghc-options = [
    "-Wall"
    "-Wredundant-constraints"
    "-Wincomplete-uni-patterns"
    "-Wmissing-deriving-strategies"
    "-Widentities"
    "-Wunused-packages"
    "-fplugin=Polysemy.Plugin"
  ];

  dependencies = [
    { name = "base"; version = ">= 4.12 && < 5"; mixin = "hiding (Prelude)"; }
    { name = "prelate"; version = ">= 0.5"; mixin = ["(Prelate as Prelude)" "hiding (Prelate)"]; }
    "polysemy"
    "polysemy-plugin"
  ];

  project = name: doc: merge (meta // { library = paths name; } // options) {
    inherit name;
    description = "See https://hackage.haskell.org/package/${name}/docs/${doc}.html";
    library = {
      source-dirs = "lib";
      inherit dependencies;
    };
    default-extensions = config.ghci.extensions;
    extra-source-files = ["changelog.md" "readme.md"];
  };

  exe = pkg: dir: merge (paths pkg // {
    main = "Main.hs";
    source-dirs = dir;
    dependencies = dependencies ++ [pkg];
    ghc-options = [
      "-threaded"
      "-rtsopts"
      "-with-rtsopts=-N"
    ];
  });

in {

  polysemy-account = merge (project "polysemy-account" "Polysemy-Account") {
    synopsis = "Account management with Servant and Polysemy";
    library.dependencies = [
      "chronos"
      "elocrypt"
      "password ^>=3.0"
      "polysemy-db"
      "random"
      "sqel"
    ];
  };

  polysemy-account-api = merge (project "polysemy-account-api" "Polysemy-Account-Api") {
    synopsis = "Account management with Servant and Polysemy";
    library.dependencies = [
      "aeson"
      "exon"
      "fast-logger"
      "jose"
      "polysemy-account"
      "polysemy-conc"
      "polysemy-db"
      "polysemy-hasql"
      "polysemy-log"
      "servant"
      "servant-auth"
      "servant-auth-server"
      "servant-server"
      "sqel"
      "transformers"
      "uuid"
      "wai"
      "wai-extra"
      "warp"
    ];
    tests = {
      polysemy-account-api-unit = exe "polysemy-account-api" "test" {
        dependencies = [
          "aeson"
          "case-insensitive"
          "exon"
          "http-types"
          "polysemy-account"
          "polysemy-db"
          "servant-auth-server"
          "servant-server"
          "sqel"
          "wai"
          "wai-extra"
          "zeugma"
        ];
      };
      polysemy-account-api-integration = exe "polysemy-account-api" "integration" {
        dependencies = [
          "polysemy-account"
          "polysemy-db"
          "polysemy-hasql"
          "polysemy-hasql-test"
          "polysemy-test"
          "servant-auth"
          "servant-auth-server"
          "sqel"
          "tasty"
          "uuid"
        ];
      };
    };
  };

}