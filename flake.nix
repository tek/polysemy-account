{
  description = "Account management with Servant and Polysemy";

  inputs = {
    hix.url = "git+https://git.tryp.io/tek/hix";
    polysemy-hasql.url = "git+https://git.tryp.io/tek/polysemy-hasql";
  };

  outputs = { hix, polysemy-hasql, ...  }: hix.lib.pro ({ config, ... }: {
    hackage.versionFile = "ops/version.nix";
    depsFull = [polysemy-hasql];
    main = "polysemy-account-api-test";
    compiler = "ghc94";

    overrides = { hackage, notest, ... }: {
      elocrypt = notest (hackage "2.1.0" "0dm2k528bs4zwriyrrqs7j44pmpwpxzivaa6n8iwliwd2sh19s78");
    };

    cabal = {
      license = "BSD-2-Clause-Patent";
      license-file = "LICENSE";
      author = "Torsten Schmits";
      prelude = {
        enable = true;
        package = {
          name = "prelate";
          version = "^>= 0.6";
        };
        module = "Prelate";
      };
      paths = false;
      meta = {
        maintainer = "hackage@tryp.io";
        category = "Web";
        git = "https://git.tryp.io/tek/polysemy-account";
        homepage = "https://git.tryp.io/tek/polysemy-account";
        bug-reports = "https://github.com/tek/polysemy-account/issues";
        extra-source-files = ["changelog.md" "readme.md"];
      };
      dependencies = ["polysemy" "polysemy-plugin"];
      ghc-options = ["-fplugin=Polysemy.Plugin" "-Wno-partial-type-signatures"];
    };

    packages = {
      polysemy-account = {
        src = ./packages/account;
        cabal.meta.synopsis = "Account management with Servant and Polysemy";
        library = {
          enable = true;
          dependencies = [
            "chronos ^>= 1.1"
            "elocrypt ^>= 2.1"
            "password ^>=3.0"
            "polysemy-db"
            "random ^>= 1.2"
            "servant ^>= 0.19"
            "servant-auth ^>= 0.4"
            "sqel"
          ];
        };
      };

      polysemy-account-api = {
        src = ./packages/api;
        cabal.meta.synopsis = "Account management with Servant and Polysemy";
        library = {
          enable = true;
          dependencies = [
            "aeson >= 2.0 && < 2.2"
            "chronos ^>= 1.1"
            "exon ^>= 1.4"
            "fast-logger ^>= 3.1"
            "jose ^>= 0.9"
            "polysemy-conc ^>= 0.12"
            "polysemy-db"
            "polysemy-hasql"
            "servant ^>= 0.19"
            "servant-auth ^>= 0.4"
            "servant-auth-server ^>= 0.4"
            "servant-server ^>= 0.19"
            "sqel"
            "transformers"
            "uuid ^>= 1.3"
            "wai ^>= 3.2"
            "wai-extra ^>= 3.1"
            "warp ^>= 3.3"
            config.packages.polysemy-account.dep.minor
          ];
          reexported-modules = ["Polysemy.Account"];
        };
        test = {
          enable = true;
          env = "polysemy-account-integration";
          source-dirs = "integration";
          dependencies = [
            "polysemy-account"
            "polysemy-db"
            "polysemy-hasql"
            "polysemy-hasql-test"
            "polysemy-test"
            "servant-auth"
            "servant-auth-server"
            "sqel"
            "sqel-core"
            "tasty"
            "uuid"
          ];
        };
      };

      polysemy-account-api-test = {
        src = ./packages/api-test;
        cabal.meta.synopsis = "Testing tools for polysemy-account-api";
        library = {
          enable = true;
          dependencies = [
            "case-insensitive"
            "exon"
            "http-types"
            "polysemy-db"
            "servant-auth-server"
            "servant-server"
            "sqel"
            "wai"
            "wai-extra"
            "zeugma"
            config.packages.polysemy-account.dep.minor
            config.packages.polysemy-account-api.dep.minor
          ];
        };
        test = {
          enable = true;
          dependencies = [
            "aeson"
            "exon"
            "polysemy-account"
            "polysemy-account-api"
            "servant-auth-server"
            "servant-server"
            "sqel"
            "zeugma"
          ];
        };
      };
    };

    envs.polysemy-account-integration = {
      basePort = 14000;
      services.postgres = {
        enable = true;
        config = {
          name = "polysemy_account";
          log = true;
          creds = {
            user = "polysemy_account";
            password = "polysemy_account";
          };
        };
      };

      env = {
        polysemy_account_test_host = "localhost";
        polysemy_account_test_port = config.envs.polysemy-account-integration.hostPorts.postgres;
      };
    };
  });
}
