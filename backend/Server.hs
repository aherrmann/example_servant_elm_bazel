{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Api
import Control.Monad.IO.Class
import Db
import Models
import Servant
import Servant.Server
import WaiAppStatic.Storage.Filesystem (defaultWebAppSettings)
import WaiAppStatic.Types (ssIndices, unsafeToPiece)

apiServer :: Db -> Server TodoApi
apiServer db =
  tasksGet
    :<|> tasksPost
    :<|> tasksIdGet
    :<|> tasksIdPut
    :<|> tasksIdDelete
  where
    tasksGet :: Handler [Entry Task]
    tasksGet =
      liftIO $ allTasks db
    tasksPost :: Task -> Handler (Entry Task)
    tasksPost task = do
      id' <- liftIO $ newTask db task
      pure (Entry id' task)
    tasksIdGet :: Id -> Handler (Entry Task)
    tasksIdGet id' = do
      mbTask <- liftIO $ getTask db id'
      case mbTask of
        Just task -> pure (Entry id' task)
        Nothing -> throwError $ err404 {errBody = "No such task"}
    tasksIdPut :: Id -> Task -> Handler (Entry Task)
    tasksIdPut id' task = do
      liftIO $ setTask db id' task
      pure (Entry id' task)
    tasksIdDelete :: Id -> Handler ()
    tasksIdDelete id' =
      liftIO $ deleteTask db id'

staticServer :: FilePath -> Server Raw
staticServer dir =
  serveDirectoryWith
    (defaultWebAppSettings dir)
      { ssIndices = [unsafeToPiece "index.html"]
      }

app :: Db -> FilePath -> Application
app db dir = serve (Proxy @(TodoApi :<|> Raw)) (apiServer db :<|> staticServer dir)
