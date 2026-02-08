{
  inputs = rec {
    ihp.url = "path:///Users/liuzichao/quant/ihp";
    nixpkgs.follows = "ihp/nixpkgs";
    flake-parts.follows = "ihp/flake-parts";
    devenv.follows = "ihp/devenv";
    systems.follows = "ihp/systems";
    devenv-root = {
      url = "file+file:///dev/null";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, ihp, flake-parts, systems, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {

      systems = import systems;
      imports = [ ihp.flakeModules.default ];

      perSystem = { pkgs, ... }: let
        # Extend haskellPackages to include purescript-bridge (not in upstream set)
        hspkgs = pkgs.haskellPackages.override {
          overrides = self: super: {
            purescript-bridge =
              let
                drv = super.callHackageDirect {
                  pkg = "purescript-bridge";
                  ver = "0.15.0.0";
                  sha256 = "1y2xrcnifrfbchjp3q9zkrgpz5hhrfk6083pi824zmywa29dyigq";
                } {};
              in assert (drv.pname == "purescript-bridge"); drv;
          };
        };
      in {
        ihp = {
          appName = "app"; # Change this to your project name
          enable = true;
          projectPath = ./.;
          packages = with pkgs; [
            # Native dependencies, e.g. imagemagick
          ];
          haskellPackages = p: with p; [
            # Haskell dependencies go here
            async
            p.ihp
            p.ihp-ide
            p.ihp-job-dashboard
            hspkgs.purescript-bridge
            cabal-install
            base
            wai
            text

            # Uncomment on local development for testing
            hspec
            ihp-hspec
          ];
        };

        # Custom configuration that will start with `devenv up`
        devenv.shells.default = {
          # Start Mailhog on local development to catch outgoing emails
          # services.mailhog.enable = true;
          packages = [
            pkgs.python3
            pkgs.gcc
            pkgs.esbuild
            pkgs.nodejs
            pkgs.playwright-driver
          ];
          enterShell = ''
            # python
            echo "enterShell"
            export PLAYWRIGHT_BROWSERS_PATH="${pkgs.playwright-driver.browsers}"
            export PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD=1

            ensure_js_deps() {
              if [ ! -d "node_modules/@playwright/test" ]; then
                echo "[devenv] installing js deps (including @playwright/test) ..."
                npm install --no-fund --no-audit
              fi
            }

            ensure_py_deps() {
              if ! "$VENV_PY" -c "import tushare, akshare" >/dev/null 2>&1; then
                echo "[devenv] missing akshare/tushare, installing ..."
                "$VENV_PY" -m pip install --upgrade pip wheel setuptools
                "$VENV_PY" -m pip install akshare tushare
              fi
            }
            VENV_PY=".venv/bin/python"
            if [ ! -x "$VENV_PY" ]; then
              echo "[devenv] creating .venv and installing akshare/tushare ..."
              rm -rf .venv
              python3 -m venv --without-pip .venv
              if [ ! -x "$VENV_PY" ]; then
                echo "[devenv] venv create failed: $VENV_PY missing"
                exit 1
              fi
              "$VENV_PY" -m ensurepip --upgrade
              "$VENV_PY" -m pip install --upgrade pip wheel setuptools
              source .venv/bin/activate
              ensure_py_deps
            else
              echo "[devenv] using existing .venv"
              source .venv/bin/activate
              ensure_py_deps
            fi
            ensure_js_deps
          '';

          # Custom processes that don't appear in https://devenv.sh/reference/options/
          processes = {
            # Uncomment if you use tailwindcss.
            # tailwind.exec = "tailwindcss -c tailwind/tailwind.config.js -i ./tailwind/app.css -o static/app.css --watch=always";
          };
        };
      };

      # Adding the new NixOS configuration for "production"
      # See https://ihp.digitallyinduced.com/Guide/deployment.html#deploying-with-deploytonixos for more info
      # Used to deploy the IHP application
      flake.nixosConfigurations."production" = import ./Config/nix/hosts/production/host.nix { inherit inputs; };
    };

  # The following configuration speeds up build times by using the devenv, cachix and digitallyinduced binary caches
  # You can add your own cachix cache here to speed up builds. For that uncomment the following lines and replace `CHANGE-ME` with your cachix cache name
  nixConfig = {
    extra-substituters = [
      "https://devenv.cachix.org"
      "https://cachix.cachix.org"
      "https://digitallyinduced.cachix.org"
      # "https://CHANGE-ME.cachix.org"
    ];
    extra-trusted-public-keys = [
      "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
      "cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM="
      "digitallyinduced.cachix.org-1:y+wQvrnxQ+PdEsCt91rmvv39qRCYzEgGQaldK26hCKE="
      # "CHANGE-ME.cachix.org-1:CHANGE-ME-PUBLIC-KEY"
    ];
  };
}
