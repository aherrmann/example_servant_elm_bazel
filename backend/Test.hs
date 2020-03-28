{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test where

import Api
import Control.Exception (displayException)
import Data.Proxy (Proxy (..))
import Db
import Models
import qualified Network.HTTP.Client as Http
import qualified Network.Wai.Handler.Warp as Warp
import Servant
import Servant.Client
import Servant.Server
import Server
import Test.Hspec
import Test.Hspec.Expectations.Contrib (isLeft)

main :: IO ()
main = hspec $ do
  around withApp $ do
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ Http.newManager Http.defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl {baseUrlPort = port})

    describe "todo app" $ do
      describe "GET /tasks" $ do
        it "should retrieve all tasks" $ \port -> do
          result <- runClientM getTasks (clientEnv port)
          result
            `shouldBe` Right
              [ Entry (Id 0) (Task "first task" Done),
                Entry (Id 1) (Task "second task" Open),
                Entry (Id 2) (Task "third task" Open)
              ]
      describe "GET /tasks/:id" $ do
        it "should retrieve a specific tasks" $ \port -> do
          result <- runClientM (getTasksById (Id 0)) (clientEnv port)
          result `shouldBe` Right (Entry (Id 0) (Task "first task" Done))
      describe "POST /tasks" $ do
        it "should create a task" $ \port -> do
          let newTask = Task {taskLabel = "new task", taskStatus = Open}
          result <- runClientM (postTasks newTask) (clientEnv port)
          result `shouldBe` Right (Entry (Id 3) newTask)
          result <- runClientM (getTasksById (Id 3)) (clientEnv port)
          result `shouldBe` Right (Entry (Id 3) newTask)
      describe "PUT /tasks/:id" $ do
        it "should update a specific task" $ \port -> do
          let newTask = Task {taskLabel = "new task", taskStatus = Open}
          result <- runClientM (putTasksById (Id 0) newTask) (clientEnv port)
          result `shouldBe` Right (Entry (Id 0) newTask)
          result <- runClientM (getTasksById (Id 0)) (clientEnv port)
          result `shouldBe` Right (Entry (Id 0) newTask)
      describe "DELETE /tasks/:id" $ do
        it "should delete a specific task" $ \port -> do
          result <- runClientM (deleteTasksById (Id 0)) (clientEnv port)
          result `shouldBe` Right ()
          result <- runClientM (getTasksById (Id 0)) (clientEnv port)
          result `shouldSatisfy` isLeft

getTasks
  :<|> postTasks
  :<|> getTasksById
  :<|> putTasksById
  :<|> deleteTasksById =
    client (Proxy @TodoApi)

getTasks :: ClientM [Entry Task]

postTasks :: Task -> ClientM (Entry Task)

getTasksById :: Id -> ClientM (Entry Task)

putTasksById :: Id -> Task -> ClientM (Entry Task)

deleteTasksById :: Id -> ClientM ()

withApp :: (Warp.Port -> IO ()) -> IO ()
withApp = Warp.testWithApplication $ do
  db <- initDb
  addExampleTasks db
  pure $ serve (Proxy @TodoApi) (apiServer db)
