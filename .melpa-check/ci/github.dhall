let Actions =
      https://raw.githubusercontent.com/akirak/melpa-check/v3/dhall/github-actions.dhall

let packages = ../packages.dhall

let config = Actions.MultiFileCiConfig.default

in  Actions.buildMultiFileCiWorkflows config packages
