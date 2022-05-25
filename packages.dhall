let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.0-20220523/packages.dhall
        sha256:985f90fa68fd8b43b14c777d6ec2c161c4dd9009563b6f51685a54e4a26bf8ff

in      upstream
    //  { linalg =
          { dependencies = [ "functions" ]
          , repo = "https://github.com/gbagan/purescript-linalg.git"
          , version = "main"
          }
        }
