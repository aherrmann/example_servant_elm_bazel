{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Models
import Servant.API

type TodoApi =
  "tasks" :> Get '[JSON] [Entry Task]
    :<|> "tasks" :> ReqBody '[JSON] Task :> Post '[JSON] (Entry Task)
    :<|> "tasks" :> Capture "id" Id :> Get '[JSON] (Entry Task)
    :<|> "tasks" :> Capture "id" Id :> ReqBody '[JSON] Task :> Put '[JSON] (Entry Task)
    :<|> "tasks" :> Capture "id" Id :> Delete '[JSON] ()
