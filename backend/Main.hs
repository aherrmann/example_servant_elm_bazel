{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Codec.Archive.Tar as Tar
import Data.Function ((&))
import Data.Semigroup ((<>))
import Db
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Logger (withStdoutLogger)
import qualified Options.Applicative as Opt
import Server
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)

data Options = Options
  { optAssets :: FilePath
  }

optParser :: Opt.Parser Options
optParser =
  Options
    <$> Opt.strOption
      ( Opt.long "assets"
          <> Opt.metavar "FILE"
          <> Opt.help "Static assets tarball"
      )

parseOptions :: IO Options
parseOptions =
  Opt.execParser $
    Opt.info
      (optParser Opt.<**> Opt.helper)
      ( Opt.fullDesc
          <> Opt.progDesc "Serves the backend API and static assets."
          <> Opt.header "backend - Application backend server"
      )

main :: IO ()
main = do
  opts <- parseOptions
  db <- initDb
  addExampleTasks db
  withSystemTempDirectory "servant-elm" $ \dir ->
    withStdoutLogger $ \apLogger -> do
      Tar.extract dir (optAssets opts)
      let settings =
            Warp.defaultSettings
              & Warp.setLogger apLogger
              & Warp.setPort 8080
      Warp.runSettings settings $ app db (dir </> "frontend")
