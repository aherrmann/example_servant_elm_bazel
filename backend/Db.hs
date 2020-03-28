{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Db
  ( Db,
    initDb,
    allTasks,
    newTask,
    getTask,
    setTask,
    deleteTask,
    addExampleTasks,
  )
where

import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Models

data Db = Db
  { dbMap :: TVar (IntMap Task),
    dbCount :: TVar Int
  }

initDb :: IO Db
initDb = do
  dbMap <- newTVarIO IntMap.empty
  dbCount <- newTVarIO 0
  pure $! Db {dbMap, dbCount}

allTasks :: Db -> IO [Entry Task]
allTasks (Db {dbMap}) =
  atomically $
    map toEntry . IntMap.toList <$> readTVar dbMap
  where
    toEntry (id', task) = Entry (Id id') task

newTask :: Db -> Task -> IO Id
newTask (Db {dbMap, dbCount}) task = atomically $ do
  id' <- stateTVar dbCount (\n -> (n, succ n))
  modifyTVar' dbMap (IntMap.insert id' task)
  pure (Id id')

getTask :: Db -> Id -> IO (Maybe Task)
getTask (Db {dbMap}) (Id id') =
  atomically $
    IntMap.lookup id' <$> readTVar dbMap

setTask :: Db -> Id -> Task -> IO (Maybe ())
setTask (Db {dbMap}) (Id id') task = atomically
  $ stateTVar dbMap
  $ \map' ->
    let (mbPrev, map'') = IntMap.updateLookupWithKey (\_ _ -> Just task) id' map'
     in (() <$ mbPrev, map'')

deleteTask :: Db -> Id -> IO ()
deleteTask (Db {dbMap}) (Id id') =
  atomically $
    modifyTVar' dbMap (IntMap.delete id')

addExampleTasks :: Db -> IO ()
addExampleTasks db = do
  _ <- newTask db (Task "first task" Done)
  _ <- newTask db (Task "second task" Open)
  _ <- newTask db (Task "third task" Open)
  pure ()
