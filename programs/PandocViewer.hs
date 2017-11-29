{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Pandoc
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Lens.Micro
import Text.Pandoc.Builder as B

data VpPandoc = VpPandoc
  deriving (Eq, Ord, Show)

pandocScroll :: ViewportScroll VpPandoc
pandocScroll = viewportScroll VpPandoc

pandocDoc :: Pandoc
pandocDoc =
    setTitle "My title" $ doc $
        header 1 "This is my document" <>
        para ("This is the first paragraph with a "
              <> singleQuoted "single quoted text"
              <> " and a "
              <> doubleQuoted "double quoted text"
              <> ".") <>
        para ("And " <> emph "another" <> " paragraph.") <>
        horizontalRule <>
        header 2 "Unordered list" <>
        bulletList [ para "item one" <> para "continuation"
                   , plain "item two"
                   ] <>
        header 2 "Ordered list" <>
        orderedList [ para "item one" <> para "continuation"
                    , plain "item two"
                    ] <>
        header 2 "Definition list" <>
        definitionList [ ("foo", [para "A foo thing"])
                       , ("bar", [para "A bar thing. This is much longer than the definition for foo to illustrate how long lines are broken."])
                       ] <>
        horizontalRule <>
        header 3 "Code block" <>
        codeBlock "main :: IO()\nmain = putStrLn \"Hello, world!\"" <>
        header 3 "Block quote" <>
        blockQuote (para "To be, or not to be: that is the question.") <>
        header 3 "Line block" <>
        lineBlock [ "The limerick packs laughs anatomical"
                  , "In space that is quite economical."
                  , "   But the good ones I've seen"
                  , "   So seldom are clean"
                  , "And the clean ones so seldom are comical"
                  ] <>
        horizontalRule <>
        header 4 "Links and images" <>
        para ("An " <> link "http://www.example.org/" "link" "example link" <> ".") <>
        para ("And an " <> B.image "https://rycee.net/images/haskell-logo.png" "example image" "image" <> ".") <>
        horizontalRule <>
        header 5 "Simple table" <>
        simpleTable [plain "col 1", plain "col 2"] [ [ plain "cell 1", plain "cell 2" ]
                                                   , [ plain "cell 3", plain "cell 4" ] ]

initState :: PandocView VpPandoc
initState = set pvDocL pandocDoc $ pandocView VpPandoc

ui :: PandocView VpPandoc -> Widget VpPandoc
ui = border . renderPandocView

handleEvent :: PandocView VpPandoc
            -> BrickEvent VpPandoc e
            -> EventM VpPandoc (Next (PandocView VpPandoc))
handleEvent s (VtyEvent (EvResize _ _)) = continue s
handleEvent s (VtyEvent (EvKey (KChar 'r') [])) = continue . (pvShowRawL %~ not) $ s
handleEvent s (VtyEvent (EvKey KUp [])) = vScrollBy pandocScroll (-1) >> continue s
handleEvent s (VtyEvent (EvKey KDown [])) = vScrollBy pandocScroll 1 >> continue s
handleEvent s (VtyEvent (EvKey KLeft [])) = hScrollBy pandocScroll (-1) >> continue s
handleEvent s (VtyEvent (EvKey KRight [])) = hScrollBy pandocScroll 1 >> continue s
handleEvent s (VtyEvent (EvKey KPageUp [])) = vScrollPage pandocScroll Up >> continue s
handleEvent s (VtyEvent (EvKey KPageDown [])) = vScrollPage pandocScroll Down >> continue s
handleEvent s (VtyEvent (EvKey KHome [])) = vScrollToBeginning pandocScroll >> continue s
handleEvent s (VtyEvent (EvKey KEnd [])) = vScrollToEnd pandocScroll >> continue s
handleEvent s _ = halt s

attributes :: AttrMap
attributes = attrMap defAttr
    [ (pandocStyleHeaderAttr,      fg brightRed)
    , (pandocStyleLinkAttr,        fg brightBlue)
    , (pandocStyleDefinitionListTermAttr, fg brightWhite)
    , (pandocStyleHorizRuleAttr,   fg black)
    ]

app :: App (PandocView VpPandoc) e VpPandoc
app =
    App { appDraw = \ state -> [ (ui state) ]
        , appChooseCursor = neverShowCursor
        , appHandleEvent = handleEvent
        , appStartEvent = return
        , appAttrMap = const attributes
        }

main :: IO ()
main = defaultMain app initState >> return ()
