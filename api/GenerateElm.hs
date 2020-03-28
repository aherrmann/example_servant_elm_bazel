module GenerateElm where

import Api
import Models
import Servant.Elm
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

parseArgs :: IO FilePath
parseArgs = do
  args <- getArgs
  case args of
    [outdir] -> pure outdir
    _ -> do
      hPutStrLn stderr "USAGE: generate-elm OUTDIR"
      exitFailure

main :: IO ()
main = do
  outdir <- parseArgs
  generateElmModuleWith
    defElmOptions
    [ "Api"
    ]
    defElmImports
    outdir
    [ DefineElm (Proxy :: Proxy Id),
      DefineElm (Proxy :: Proxy Status),
      DefineElm (Proxy :: Proxy Task),
      DefineElm (Proxy :: Proxy (Entry Task))
    ]
    (Proxy :: Proxy TodoApi)
