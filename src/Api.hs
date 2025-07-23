{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}


module Api where
import Data.Text (Text)
import GHC.Generics (Generic)
import Lucid ( Html )
import Servant ( Get, ReqBody, Post, (:>), Capture, Delete )
import Servant.HTML.Lucid (HTML)
import Servant.API (FormUrlEncoded, (:<|>)(..))
import Model (Todo, Key)
import Web.FormUrlEncoded ( FromForm )



newtype TodoForm = TodoForm {
    title :: Text
} deriving (Generic, Show, Eq)
instance FromForm TodoForm


type API = 
    Get '[HTML] ( Html ())
    :<|> "todos" :> ReqBody '[FormUrlEncoded] TodoForm :> Post '[HTML]  (Html ())
    :<|> "toggle" :> Capture "id" (Key Todo) :> Post '[HTML] (Html ())
    :<|> "delete" :> Capture "id" (Key Todo) :> Delete '[HTML]  (Html ())

