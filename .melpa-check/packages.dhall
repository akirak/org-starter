let Package = (./schema.dhall).Package

in  [ Package::{
      , pname = "org-starter-swiper"
      , version = "0.1.1"
      , emacsVersion = "25.1"
      , files = [ "org-starter-swiper.el" ]
      , dependencies = [ "swiper", "org-starter" ]
      , localDependencies = [ "org-starter" ]
      , buttercupTests = [] : List Text
      , recipe =
          ''
          (org-starter-swiper :fetcher github :repo "akirak/org-starter" :files ("org-starter-swiper.el"))
          ''
      }
    , Package::{
      , pname = "org-starter"
      , version = "0.2.8"
      , emacsVersion = "25.1"
      , files = [ "org-starter.el" ]
      , dependencies = [ "dash", "dash-functional" ]
      , recipe =
          ''
          (org-starter :fetcher github :repo "akirak/org-starter" :files ("org-starter.el"))
          ''
      }
    ]
