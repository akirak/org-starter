let PackageName
    : Type
    = Text

let File
    : Type
    = Text

let Recipe
    : Type
    = Text

let Version
    : Type
    = Text

let Pattern
    : Type
    = Text

let defaultTests
    : List Pattern
    = [ "test?(s).el", "test-*.el", "*-test?(s).el", "test?(s)/*.el" ]

in  { Package =
      { Type =
          { pname : PackageName
          , version : Version
          , files : List File
          , emacsVersion : Version
          , localDependencies : List PackageName
          , dependencies : List PackageName
          , buttercupTests : List Pattern
          , mainFile : Optional File
          , recipe : Recipe
          }
      , default =
        { localDependencies = [] : List File
        , mainFile = None File
        , buttercupTests = defaultTests
        }
      }
    , noTests = [] : List Pattern
    , defaultTests = defaultTests : List Pattern
    }
