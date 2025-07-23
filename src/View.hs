{-# LANGUAGE OverloadedStrings #-}
module View where
import Database.Persist (Entity (Entity))
import Model (Todo (todoCompleted, todoDescription))
import Lucid (Html, html_, head_, lang_, title_, doctype_, link_, rel_, href_, body_, h1_, class_, form_, input_, name_, placeholder_, button_, type_, id_, ul_, li_, span_, ToHtml (toHtml), Attribute, script_, src_, integrity_, crossorigin_, meta_, div_, content_, a_, target_)
import Data.Text (Text)
import qualified Data.Text as T
import Lucid.Base (makeAttribute)
import Web.PathPieces (toPathPiece)

renderHomePage :: [Entity Todo] -> Html ()
renderHomePage todos = do
    doctype_
    html_ [lang_ "en"] $ do
        head_ $ do
            title_ "Todo List"
            meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
            link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/tailwindcss@2.2.19/dist/tailwind.min.css"]
            script_ 
              [ src_ "https://cdn.jsdelivr.net/npm/htmx.org@2.0.6/dist/htmx.min.js"
              , integrity_ "sha384-Akqfrbj/HpNVo8k11SXBb6TlBWmXXlYQrCSqEWmyKJe+hDm3Z/B2WVG4smwBkRVm"
              , crossorigin_ "anonymous"
              ] (" "::Text)

        body_ [class_ "bg-gray-50 min-h-screen flex items-center justify-center p-4"] $ do
            div_ [class_ "w-full max-w-xl bg-white shadow-md rounded-md p-6"] $ do
                h1_ [class_ "text-3xl font-semibold mb-6 text-center text-gray-800"] "📝 Todo List"
                

                form_ [hxPost_ "/todos", hxTarget_ "#todo-list", class_ "flex flex-col sm:flex-row gap-2 mb-6"] $ do
                    input_ 
                      [ name_ "title"
                      , placeholder_ "What needs to be done today?"
                      , class_ "flex-1 px-4 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-400"
                      ]
                    button_ 
                      [ type_ "submit"
                      , class_ "px-4 py-2 bg-blue-600 text-white rounded-md hover:bg-blue-700 transition"
                      ] "Add"

                ul_ [id_ "todo-list", class_ "space-y-4"] $ renderTodoItems todos

                div_ [class_ "mt-8 text-center text-sm text-gray-500"] $ do
                    "Built with "
                    techLink "Haskell" "https://www.haskell.org/"
                    ", "
                    techLink "Servant" "https://hackage.haskell.org/package/servant"
                    ", "
                    techLink "Lucid" "https://hackage.haskell.org/package/lucid"
                    ", "
                    techLink "HTMX" "https://htmx.org/"
                    ", "
                    techLink "Tailwind CSS" "https://tailwindcss.com/"
                    ", and "
                    techLink "PostgreSQL" "https://www.postgresql.org/"


renderTodoItems :: [Entity Todo] -> Html ()
renderTodoItems = mapM_ renderTodoItem

renderTodoItem :: Entity Todo -> Html ()
renderTodoItem (Entity tid todo) = do
    li_ [class_ "flex flex-col sm:flex-row items-start sm:items-center justify-between bg-gray-100 px-4 py-3 rounded-md shadow-sm"] $ do
        span_ 
          [ class_ $ "text-gray-800 mb-2 sm:mb-0 " <> if todoCompleted todo then "line-through text-gray-400" else ""
          ] 
          (toHtml $ todoDescription todo)

        div_ [class_ "flex gap-2"] $ do
            button_ 
              [ hxPost_ ("/toggle/" <> toPathPiece tid)
              , hxTarget_ "#todo-list"
              , class_ "text-sm text-blue-600 hover:underline"
              ] "Toggle"

            button_ 
              [ hxDelete_ ("/delete/" <> toPathPiece tid)
              , hxTarget_ "#todo-list"
              , class_ "text-sm text-red-600 hover:underline"
              ] "Delete"


tshow :: Show a => a -> Text
tshow = T.pack . show

hxPost_, hxTarget_, hxDelete_:: Text -> Attribute
hxPost_ = makeAttribute "hx-post"
hxTarget_ = makeAttribute "hx-target"
hxDelete_ = makeAttribute "hx-delete"
techLink :: Text -> Text -> Html ()
techLink name url = a_ [href_ url, target_ "_blank", class_ "hover:underline text-blue-500"] (toHtml name)
