{-# LANGUAGE OverloadedStrings #-}
module View where
import Database.Persist (Entity (Entity))
import Model (Todo (todoCompleted, todoDescription))
import Lucid (Html, html_, head_, lang_, title_, doctype_, link_, rel_, href_, body_, h1_, class_, form_, input_, name_, placeholder_, button_, type_, id_, ul_, li_, span_, ToHtml (toHtml), Attribute, script_, src_, integrity_, crossorigin_)
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
            link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/tailwindcss@2.2.19/dist/tailwind.min.css"]
            script_ [src_ "https://cdn.jsdelivr.net/npm/htmx.org@2.0.6/dist/htmx.min.js",  integrity_ "sha384-Akqfrbj/HpNVo8k11SXBb6TlBWmXXlYQrCSqEWmyKJe+hDm3Z/B2WVG4smwBkRVm", crossorigin_  "anonymous"] (" "::Text)
 
        body_ [class_ "px-6"] $ do
            h1_ [class_ "text-2xl mb-4" ] "Todo List"
            form_ [hxPost_ "/todos", hxTarget_ "#todo-list", class_ "mb-4"] $ do
                input_ [class_ "border px-2 py-1", name_ "title", placeholder_ "What needs to be done today?"]
                button_ [type_ "submit", class_ "ml-2 bg-blue-500 text-white px-2 py-1"] "Add"
            ul_ [id_ "todo-list"] $ renderTodoItems todos


renderTodoItems :: [Entity Todo] -> Html ()
renderTodoItems = mapM_ renderTodoItem


renderTodoItem :: Entity Todo -> Html ()
renderTodoItem (Entity tid todo) = do
    li_ [class_ "flex items-center justify-between mb-2"]   $ do
        span_ [class_ (if todoCompleted todo then "line-through" else "")] $ toHtml $ todoDescription todo
        button_ [hxPost_ ("/toggle/" <> toPathPiece tid), hxTarget_ "#todo-list", class_ "text-sm mr-2"] "Toggle"
        button_ [hxDelete_ ("/delete/" <> toPathPiece tid), hxTarget_ "#todo-list", class_ "text-sm text-red-500"] "Delete"


tshow :: Show a => a -> Text
tshow = T.pack . show

hxPost_, hxTarget_, hxDelete_:: Text -> Attribute
hxPost_ = makeAttribute "hx-post"
hxTarget_ = makeAttribute "hx-target"
hxDelete_ = makeAttribute "hx-delete"