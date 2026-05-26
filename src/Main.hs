----------------------------------------------------------------------------
{-# LANGUAGE CPP                #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso
-- Copyright   :  (C) 2016-2025 David M. Johnson (@dmjio)
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Main where
----------------------------------------------------------------------------
import           Control.Category ((.))
import           Control.Monad
import           Control.Monad.State.Class (get)
import qualified Data.IntMap as IM
import           Data.IntMap (IntMap)
import           Data.Bool
import           Data.Char (isSpace)
import           Data.List (dropWhileEnd)
import           GHC.Generics
import           Prelude hiding ((.))
----------------------------------------------------------------------------
import           Miso hiding (at)
import           Miso.Html
import           Miso.JSON (ToJSON(..), FromJSON(..), encode, decode)
import           Miso.Lens
import           Miso.Lens.TH
import           Miso.Html.Property hiding (label_)
import qualified Miso.String as S
----------------------------------------------------------------------------
default (MisoString)
----------------------------------------------------------------------------
instance ToJSON v => ToJSON (IM.IntMap v) where
  toJSON = toJSON . IM.toAscList

instance FromJSON v => FromJSON (IM.IntMap v) where
  parseJSON v = IM.fromList <$> parseJSON v
----------------------------------------------------------------------------
data Model
  = Model
  { _entries :: IntMap Entry
  , _field :: MisoString
  , _uid :: Int
  , _visibility :: MisoString
  , _step :: Bool
  } deriving stock (Show, Generic, Eq)
    deriving anyclass (ToJSON, FromJSON)
----------------------------------------------------------------------------
data Entry
  = Entry
  { _description :: MisoString
  , _completed :: Bool
  , _editing :: Bool
  , _focussed :: Bool
  , _original :: MisoString
  } deriving stock (Show, Generic, Eq)
    deriving anyclass (ToJSON, FromJSON)
----------------------------------------------------------------------------
$(makeLenses ''Entry)
$(makeLenses ''Model)
----------------------------------------------------------------------------
emptyModel :: Model
emptyModel
  = Model
  { _entries = mempty
  , _visibility = "All"
  , _field = mempty
  , _uid = 0
  , _step = False
  }
----------------------------------------------------------------------------
trimMS :: MisoString -> MisoString
trimMS = S.pack . dropWhileEnd isSpace . dropWhile isSpace . S.unpack

newEntry :: MisoString -> Entry
newEntry desc
  = Entry
  { _description = desc
  , _completed = False
  , _editing = False
  , _focussed = False
  , _original = desc
  }
----------------------------------------------------------------------------
data Msg
  = NoOp
  | CurrentTime Int
  | UpdateField MisoString
  | EditingEntry Int Bool
  | UpdateEntry Int MisoString
  | SaveEdit Int
  | CancelEdit Int
  | Add
  | Delete Int
  | DeleteComplete
  | Check Int Bool
  | CheckAll Bool
  | ChangeVisibility MisoString
  | FocusOnInput
  deriving (Show)
----------------------------------------------------------------------------
#ifdef WASM
#ifndef INTERACTIVE
foreign export javascript "hs_start" main :: IO ()
#endif
#endif
----------------------------------------------------------------------------
main :: IO ()
main = do
    stored <- getLocalStorage "todos-miso"
    let initialModel = maybe emptyModel id (stored >>= decode)
    startApp (defaultEvents <> keyboardEvents) (app initialModel)
----------------------------------------------------------------------------
app :: Model -> App Model Msg
app initialModel = (component initialModel updateModel viewModel)
  { mount = Just FocusOnInput
#ifdef INTERACTIVE
  -- dmj: when using WASM repl mode append styles to <head> in dev mode
  , styles =
      [ Href "https://cdn.jsdelivr.net/npm/todomvc-common@1.0.5/base.min.css" False
      , Href "https://cdn.jsdelivr.net/npm/todomvc-app-css@2.4.3/index.min.css" False
      ]
#endif
  }
----------------------------------------------------------------------------
save :: Effect parent props Model Msg
save = get >>= \m -> io_ (setLocalStorage "todos-miso" (encode m))
----------------------------------------------------------------------------
updateModel :: Msg -> Effect parent props Model Msg
updateModel = \case
  NoOp ->
    pure ()
  FocusOnInput ->
    io_ (focus "input-box")
  CurrentTime time ->
    io_ $ consoleLog (S.ms time)
  Add -> do
    value <- trimMS <$> use field
    unless (S.null value) $ do
      field .= mempty
      uid += 1
      nextId <- use uid
      entries %= IM.insert nextId (newEntry value)
      save
  UpdateField str -> do
    field .= str
  EditingEntry idx isEditing -> do
    entries . at idx %?= (\e ->
      e & editing .~ isEditing
        & focussed .~ isEditing
        & (if isEditing then original .~ (e ^. description) else id))
    when isEditing $ io_ (focus ("todo-" <> S.ms idx))
  UpdateEntry idx task ->
    entries . at idx %?= do
      description .~ task
  SaveEdit idx -> do
    entry <- use (entries . at idx)
    case entry of
      Nothing -> pure ()
      Just e -> do
        let desc = trimMS (e ^. description)
        if S.null desc
          then entries . at idx .= Nothing
          else entries . at idx %?= (\e -> e & description .~ desc & editing .~ False & focussed .~ False)
        save
  CancelEdit idx ->
    entries . at idx %?= (\e ->
      e & description .~ (e ^. original)
        & editing .~ False
        & focussed .~ False)
  Delete idx -> do
    entries . at idx .= Nothing
    save
  DeleteComplete -> do
    entries %= IM.filter (\entry -> not (entry ^. completed))
    save
  Check idx isCompleted -> do
    entries . at idx %?= do completed .~ isCompleted
    save
  CheckAll isCompleted -> do
    entries %= IM.map (\entry -> entry & completed .~ isCompleted)
    save
  ChangeVisibility v -> do
    visibility .= v
    save
----------------------------------------------------------------------------
viewModel :: props -> Model -> View model Msg
viewModel _ m =
    div_
        [ class_ "todomvc-wrapper"
        ]
        [ section_
            [class_ "todoapp"]
            [ viewInput m (m ^. field)
            , viewEntries (m ^. visibility) (IM.toList (m ^. entries))
            , viewControls m (m ^. visibility) (IM.toList (m ^. entries))
            ]
        , infoFooter
        ]
----------------------------------------------------------------------------
viewEntries :: MisoString -> [(Int, Entry)] -> View model Msg
viewEntries visibility entries =
    section_
        [ class_ "main"
        , hidden_ (null entries)
        ]
        [ input_
            [ class_ "toggle-all"
            , type_ "checkbox"
            , name_ "toggle"
            , id_ "toggle-all"
            , checked_ allCompleted
            , onClick $ CheckAll (not allCompleted)
            ]
        , label_
            [for_ "toggle-all"]
            [text $ S.pack "Mark all as complete"]
        , ul_ [class_ "todo-list"] $
            filter isVisible entries <&> viewEntry
        ]
  where
    allCompleted = all _completed (snd <$> entries)
    isVisible (_, Entry {..}) =
        case visibility of
            "Completed" -> _completed
            "Active" -> not _completed
            _ -> True
----------------------------------------------------------------------------
viewEntry :: (Int, Entry) -> View model Msg
viewEntry (eid, Entry{..}) =
    li_
        [ class_ $
            S.intercalate " " $
                ["completed" | _completed] <> ["editing" | _editing]
        , key_ eid
        ]
        [ div_
            [class_ "view"]
            [ input_
                [ class_ "toggle"
                , type_ "checkbox"
                , checked_ _completed
                , onClick $ Check eid (not _completed)
                ]
            , label_
                [onDoubleClick (EditingEntry eid True) ]
                [text _description]
            , button_
                [ class_ "destroy"
                , onClick $ Delete eid
                ]
                []
            ]
        , input_
            [ class_ "edit"
            , value_ _description
            , name_ "title"
            , id_ ("todo-" <> S.ms eid)
            , onInput (UpdateEntry eid)
            , onBlur (SaveEdit eid)
            , onKeyDown $ \code -> case code of
                13 -> SaveEdit eid
                27 -> CancelEdit eid
                _  -> NoOp
            ]
        ]
----------------------------------------------------------------------------
viewControls :: Model -> MisoString -> [(Int,Entry)] -> View model Msg
viewControls model visibility entries =
    footer_
        [ class_ "footer"
        , hidden_ (null entries)
        ]
        [ viewControlsCount entriesLeft
        , viewControlsFilters visibility
        , viewControlsClear model entriesCompleted
        ]
  where
    entriesCompleted = length . filter (_completed . snd) $ entries
    entriesLeft = length entries - entriesCompleted
----------------------------------------------------------------------------
viewControlsCount :: Int -> View model Msg
viewControlsCount entriesLeft =
    span_
        [class_ "todo-count"]
        [ strong_ [] [text $ S.ms entriesLeft]
        , text (item_ <> " left")
        ]
  where
    item_ = S.pack $ bool " items" " item" (entriesLeft == 1)
----------------------------------------------------------------------------
viewControlsFilters :: MisoString -> View model Msg
viewControlsFilters visibility =
    ul_
        [class_ "filters"]
        [ visibilitySwap "#/" "All" visibility
        , text " "
        , visibilitySwap "#/active" "Active" visibility
        , text " "
        , visibilitySwap "#/completed" "Completed" visibility
        ]
----------------------------------------------------------------------------
visibilitySwap :: MisoString -> MisoString -> MisoString -> View model Msg
visibilitySwap uri visibility actualVisibility =
    li_
        []
        [ a_
            [ href_ uri
            , class_ $ S.concat ["selected" | visibility == actualVisibility]
            , onClick (ChangeVisibility visibility)
            ]
            [text visibility]
        ]
----------------------------------------------------------------------------
viewControlsClear :: Model -> Int -> View model Msg
viewControlsClear _ entriesCompleted =
    button_
        [ class_ "clear-completed"
        , hidden_ (entriesCompleted == 0)
        , onClick DeleteComplete
        ]
        [text $ "Clear completed (" <> S.ms entriesCompleted <> ")"]
----------------------------------------------------------------------------
viewInput :: Model -> MisoString -> View model Msg
viewInput _ task =
    header_
        [class_ "header"]
        [ h1_ [] [text "todos"]
        , input_
            [ class_ "new-todo"
            , id_ "input-box"
            , placeholder_ "What needs to be done?"
            , autofocus_ True
            , value_ task
            , name_ "newTodo"
            , onInput UpdateField
            , onEnter NoOp Add
            ]
        ]
----------------------------------------------------------------------------
infoFooter :: View model Msg
infoFooter =
    footer_
        [class_ "info"]
        [ p_ [] [text "Double-click to edit a todo"]
        , p_
            []
            [ text "Written by "
            , a_ [href_ "https://github.com/dmjio"] [text "@dmjio"]
            ]
        , p_
            []
            [ text "Part of "
            , a_ [href_ "http://todomvc.com"] [text "TodoMVC"]
            ]
        ]
----------------------------------------------------------------------------
