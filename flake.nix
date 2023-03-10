{
  description = "Account management with Servant and Polysemy";

  inputs = {
    hix.url = "git+https://git.tryp.io/tek/hix";
    hls.url = "github:haskell/haskell-language-server?ref=1.9.0.0";
    polysemy-hasql.url = "git+https://git.tryp.io/tek/polysemy-hasql";
  };

  outputs = { hix, hls, polysemy-hasql, ...  }:
  let

    all = { hackage, source, ... }: {
      elocrypt = hackage "2.1.0" "0dm2k528bs4zwriyrrqs7j44pmpwpxzivaa6n8iwliwd2sh19s78";
    };

    vm = {
      enable = true;
      name = "polysemy_account";
      port = 14000;
      postgres = {
        enable = true;
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
      polysemy_account_test_port = vm.port;
    };

  in hix.lib.pro ({ config, lib, ... }: {
    main = "polysemy-account-api-test";
    overrides = { inherit all; };
    depsFull = [polysemy-hasql];
    packages = {
      polysemy-account = ./packages/account;
      polysemy-account-api = ./packages/api;
      polysemy-account-api-test = ./packages/api-test;
    };
    devGhc.compiler = "ghc925";
    hpack.packages = import ./ops/hpack.nix { inherit config lib; };
    hackage.versionFile = "ops/version.nix";
    ghci = {
      preludePackage = "prelate";
      preludeModule = "Prelate";
      args = ["-fplugin=Polysemy.Plugin"];
      extensions = ["StandaloneKindSignatures" "OverloadedRecordDot" "NoFieldSelectors"];
    };
    ghcid = {
      shellConfig = { vm = lib.mkForce vm; };
      testConfig = conf: { inherit env; vm.enable = lib.mkForce (conf.type == "integration"); };
    };
    compat.enable = false;
    shell.hls.package = hls.packages.${config.system}.haskell-language-server-925;
  });
}
