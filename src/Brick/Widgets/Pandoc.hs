{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module implements a simple Brick widget for displaying
-- Pandoc documents.
module Brick.Widgets.Pandoc
  ( PandocView
  , pandocView
  , pvDocL
  , pvShowRawL
  , renderPandocView

  -- * Attributes
  , pandocStyleBlockQuoteAttr
  , pandocStyleCodeAttr
  , pandocStyleCodeBlockAttr
  , pandocStyleDefinitionListTermAttr
  , pandocStyleEmphAttr
  , pandocStyleHeader1Attr
  , pandocStyleHeader2Attr
  , pandocStyleHeader3Attr
  , pandocStyleHeader4Attr
  , pandocStyleHeader5Attr
  , pandocStyleHeader6Attr
  , pandocStyleHeaderAttr
  , pandocStyleHorizRuleAttr
  , pandocStyleLinkAttr
  ) where

import qualified Brick as B
import Data.List (intersperse)
import           Data.Monoid
import Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc ((<+>))
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Render.Util.SimpleDocTree as PP
import qualified Data.Text.Prettyprint.Doc.Symbols.Unicode as PP
import qualified Data.Text.Prettyprint.Doc.Util as PP
import qualified Graphics.Vty as V
import           Lens.Micro
import qualified Text.Pandoc.Builder as Pandoc

data PandocView n =
    PandocView {
               -- | Name of the Pandoc viewport.
                 pvViewportName :: n

               -- | The Pandoc document we are displaying.
               , pvDoc :: Pandoc.Pandoc

               -- | Whether to show the raw Pandoc structure.
               , pvShowRaw :: !Bool
               }

B.suffixLenses ''PandocView

instance Show (PandocView n) where
  show _ = "PandocView"

instance B.Named (PandocView n) n where
  getName = pvViewportName

pandocView :: n -> PandocView n
pandocView name =
    PandocView { pvDoc = mempty
               , pvViewportName = name
               , pvShowRaw = False
               }

pvRawDoc :: Getting r (PandocView n) String
pvRawDoc = to (show . pvDoc)

data Annot = AnnotLink Text | AnnotAttrName B.AttrName
  deriving (Eq, Show)

pandocStyleBlockQuoteAttr :: B.AttrName
pandocStyleBlockQuoteAttr = "pandoc" <> "style" <> "blockquote"

pandocStyleCodeAttr :: B.AttrName
pandocStyleCodeAttr = "pandoc" <> "style" <> "code"

pandocStyleCodeBlockAttr :: B.AttrName
pandocStyleCodeBlockAttr = "pandoc" <> "style" <> "codeBlock"

pandocStyleDefinitionListTermAttr :: B.AttrName
pandocStyleDefinitionListTermAttr = "pandoc" <> "style" <> "definitionList" <> "term"

pandocStyleEmphAttr :: B.AttrName
pandocStyleEmphAttr = "pandoc" <> "style" <> "em"

pandocStyleHeaderAttr :: B.AttrName
pandocStyleHeaderAttr = "pandoc" <> "style" <> "header"

pandocStyleHeader1Attr :: B.AttrName
pandocStyleHeader1Attr = pandocStyleHeaderAttr <> "1"

pandocStyleHeader2Attr :: B.AttrName
pandocStyleHeader2Attr = pandocStyleHeaderAttr <> "2"

pandocStyleHeader3Attr :: B.AttrName
pandocStyleHeader3Attr = pandocStyleHeaderAttr <> "3"

pandocStyleHeader4Attr :: B.AttrName
pandocStyleHeader4Attr = pandocStyleHeaderAttr <> "4"

pandocStyleHeader5Attr :: B.AttrName
pandocStyleHeader5Attr = pandocStyleHeaderAttr <> "5"

pandocStyleHeader6Attr :: B.AttrName
pandocStyleHeader6Attr = pandocStyleHeaderAttr <> "6"

pandocStyleHorizRuleAttr :: B.AttrName
pandocStyleHorizRuleAttr = "pandoc" <> "style" <> "hr"

pandocStyleLinkAttr :: B.AttrName
pandocStyleLinkAttr = "pandoc" <> "style" <> "link"

renderPandocView :: (Ord n, Show n) => PandocView n -> B.Widget n
renderPandocView pv =
    B.Widget B.Greedy B.Greedy
    $ do
        width <- fmap (^. B.availWidthL) B.getContext
        B.render (inner width)
  where
    -- The inner widget, this is a viewport with the actual document
    -- widget inside. Note, we cannot use the viewport directly as the
    -- outer widget because we need the true available width to do
    -- nice word wrapping.
    inner width =
      B.viewport (pv ^. pvViewportNameL) B.Both
      $ B.Widget B.Fixed B.Fixed
      $ do
          let doc =
                if pv ^. pvShowRawL
                then Pandoc.doc . Pandoc.plain . Pandoc.text $ pv ^. pvRawDoc
                else pv ^. pvDocL
          ppDoc <- renderPandoc doc
          img <- renderDoc width ppDoc
          return $ set B.imageL img B.emptyResult

renderDoc :: Int -> PP.Doc Annot -> B.RenderM n V.Image
renderDoc width doc =
  do
    context <- B.getContext
    let opts = PP.defaultLayoutOptions {
          PP.layoutPageWidth = PP.AvailablePerLine width 1.0
        }
        simpleDoc = PP.layoutPretty opts doc
    return
        . (renderImage V.defAttr)
        . PP.treeForm
        . fmap (convertAttr (context ^. B.ctxAttrMapL))
        $ simpleDoc
  where
    convertAttr attrMap (AnnotAttrName n) = B.attrMapLookup n attrMap
    convertAttr attrMap (AnnotLink _) = B.attrMapLookup pandocStyleLinkAttr attrMap

-- | A monoid for recursively building images.
data ImageBuilder =
      ImgLine V.Image
    | ImgBlock V.Image V.Image V.Image
  deriving (Eq, Show)

instance Monoid ImageBuilder where
  mempty = ImgLine V.emptyImage
  (ImgLine a) `mappend` (ImgLine b) = ImgLine (a V.<|> b)
  (ImgLine a) `mappend` (ImgBlock b1 b2 b3) = ImgBlock (a V.<|> b1) b2 b3
  (ImgBlock a1 a2 a3) `mappend` (ImgLine b) = ImgBlock a1 a2 (a3 V.<|> b)
  (ImgBlock a1 a2 a3) `mappend` (ImgBlock b1 b2 b3) =
      ImgBlock a1 (a2 <-?> (a3 V.<|> b1) <-?> b2) b3

(<-?>) :: V.Image -> V.Image -> V.Image
a <-?> b
  | a == V.emptyImage = b
  | b == V.emptyImage = a
  | otherwise = a V.<-> b

buildImage :: ImageBuilder -> V.Image
buildImage (ImgLine a) = a
buildImage (ImgBlock a b c) = a <-?> b <-?> c

renderImage :: V.Attr -> PP.SimpleDocTree V.Attr -> V.Image
renderImage attr t = buildImage . renderImage' attr $ t

renderImage' :: V.Attr -> PP.SimpleDocTree V.Attr -> ImageBuilder
renderImage' attr = go
  where
    spaces n = V.charFill mempty ' ' n 1

    go (PP.STEmpty) = ImgLine V.emptyImage
    go (PP.STChar ch) = ImgLine (V.char attr ch)
    go (PP.STText _ t) = ImgLine (V.text' attr t)
    go (PP.STLine indent) = ImgBlock V.emptyImage V.emptyImage (spaces indent)
    go (PP.STAnn attr' tree) = renderImage' (attr <> attr') tree
    go (PP.STConcat ts) = foldMap go ts

renderPandoc :: Pandoc.Pandoc -> B.RenderM n (PP.Doc Annot)
renderPandoc (Pandoc.Pandoc _ blocks)= renderBlocks blocks

renderBlocks :: [Pandoc.Block] -> B.RenderM n (PP.Doc Annot)
renderBlocks = fmap (PP.vcat . intersperse (PP.pretty ' ')) . mapM renderBlock

-- | Helper to /roughly/ pretty print an instance of 'Show'.
dumpRaw :: Show a => a -> PP.Doc b
dumpRaw = PP.reflow . T.pack . show

renderBlock :: Pandoc.Block -> B.RenderM n (PP.Doc Annot)
renderBlock (Pandoc.BlockQuote blocks) = renderBlockquote blocks
renderBlock (Pandoc.BulletList blockss) = fmap PP.vcat . mapM (renderLi "•") $ blockss
renderBlock (Pandoc.CodeBlock _ code) = return $ renderCodeBlock code
renderBlock (Pandoc.DefinitionList entries)= fmap PP.vcat . mapM renderDefinition $ entries
renderBlock (Pandoc.Div _ blocks) = renderBlocks blocks
renderBlock (Pandoc.Header 1 _ inlines) = renderHeader "==" 1 inlines
renderBlock (Pandoc.Header 2 _ inlines) = renderHeader "--" 2 inlines
renderBlock (Pandoc.Header 3 _ inlines) = renderHeader "-" 3 inlines
renderBlock (Pandoc.Header n _ inlines) = renderHeader "" n inlines
renderBlock (Pandoc.HorizontalRule) =
  do
    context <- B.getContext
    let w = context ^. B.availWidthL
    return
        . PP.annotate (AnnotAttrName pandocStyleHorizRuleAttr)
        . PP.pretty
        $ T.replicate w "═"
renderBlock (Pandoc.LineBlock inlines) = fmap (PP.vcat . map (PP.nest 2)) . mapM renderInlines $ inlines
renderBlock (Pandoc.Null) = return mempty
renderBlock (Pandoc.OrderedList _ blockss) =
    fmap PP.vcat
    . mapM (\(n, blocks) -> renderLi (show n <> ".") blocks)
    $ zip [(1 :: Int) ..] blockss
renderBlock (Pandoc.Para inlines) = renderInlines inlines
renderBlock (Pandoc.Plain inlines) = renderInlines inlines
renderBlock (Pandoc.RawBlock _ _) = return mempty
renderBlock t@(Pandoc.Table _ _ _ _ _) = return $ "[Tables are unsupported:" PP.<+> dumpRaw t PP.<> "]"

renderInlines :: [Pandoc.Inline] -> B.RenderM n (PP.Doc Annot)
renderInlines = fmap (foldr (PP.<>) mempty) . mapM renderInline

renderInline :: Pandoc.Inline -> B.RenderM n (PP.Doc Annot)
renderInline (Pandoc.Cite _ inlines) = renderInlines inlines
renderInline (Pandoc.Code _ code) = return . renderCode $ code
renderInline (Pandoc.Emph inlines) = renderEm inlines
renderInline (Pandoc.Image _ inlines target) = renderLink (const $ renderInlines inlines) target
renderInline (Pandoc.LineBreak) = return $ PP.line
renderInline (Pandoc.Link _ inlines target) = renderLink (const $ renderInlines inlines) target
renderInline (Pandoc.Math Pandoc.DisplayMath math) = return . PP.enclose PP.line PP.line . PP.pretty $ math
renderInline (Pandoc.Math Pandoc.InlineMath math) = return . PP.pretty $ math
renderInline (Pandoc.Note blocks) = fmap (PP.enclose "[^" "]") . renderBlocks $ blocks
renderInline (Pandoc.Quoted Pandoc.DoubleQuote inlines) = fmap PP.d6699quotes . renderInlines $ inlines
renderInline (Pandoc.Quoted Pandoc.SingleQuote inlines) = fmap PP.s69quotes . renderInlines $ inlines
renderInline (Pandoc.RawInline _ _) = return mempty
renderInline (Pandoc.SmallCaps inlines) = renderInlines inlines
renderInline (Pandoc.SoftBreak) = return $ PP.softline
renderInline (Pandoc.Space) = return $ PP.softline
renderInline (Pandoc.Span _ inlines) = renderInlines inlines
renderInline (Pandoc.Str str) = return $ PP.pretty str
renderInline (Pandoc.Strikeout inlines) = renderStrike inlines
renderInline (Pandoc.Strong inlines) = renderEm inlines
renderInline (Pandoc.Subscript inlines) = renderInlines inlines
renderInline (Pandoc.Superscript inlines) = renderInlines inlines

renderStrike :: [Pandoc.Inline] -> B.RenderM n (PP.Doc Annot)
renderStrike ts =
  do
    inner <- renderInlines ts
    return $ strike <+> inner <+> strike
  where
    strike = "-"

renderHeader :: PP.Doc Annot -> Int -> [Pandoc.Inline] -> B.RenderM n (PP.Doc Annot)
renderHeader mark level inlines =
  do
    inner <- renderInlines inlines
    return
        . PP.annotate (AnnotAttrName attrName)
        $ mark <+> inner <+> mark
  where
    attrName = pandocStyleHeaderAttr <> (fromString . show $ level)

renderEm :: [Pandoc.Inline] -> B.RenderM n (PP.Doc Annot)
renderEm = fmap (PP.annotate (AnnotAttrName pandocStyleEmphAttr)) . renderInlines

renderCode :: String -> PP.Doc Annot
renderCode =
    PP.annotate (AnnotAttrName pandocStyleCodeAttr)
    . PP.pretty

renderCodeBlock :: String -> PP.Doc Annot
renderCodeBlock =
    PP.annotate (AnnotAttrName pandocStyleCodeBlockAttr)
    . PP.indent 4
    . PP.concatWith (PP.surround PP.hardline)
    . map PP.pretty
    . T.lines
    . T.filter ('\r' /=)
    . T.pack

renderBlockquote :: [Pandoc.Block] -> B.RenderM n (PP.Doc Annot)
renderBlockquote =
    fmap (PP.annotate (AnnotAttrName pandocStyleBlockQuoteAttr) . PP.indent 4)
    . renderBlocks

annotAttrName :: B.AttrName -> B.RenderM n (PP.Doc Annot) -> B.RenderM n (PP.Doc Annot)
annotAttrName n = fmap (PP.annotate (AnnotAttrName n))

renderDefinition :: ([Pandoc.Inline], [[Pandoc.Block]])
                 -> B.RenderM n (PP.Doc Annot)
renderDefinition (term, definition) =
  do
    rTerm <- annotAttrName pandocStyleDefinitionListTermAttr . renderInlines $ term
    rDefinition <- fmap PP.vcat . mapM renderBlocks $ definition
    return $ rTerm PP.<> ":" PP.<+> PP.nest 4 rDefinition

renderLi :: String -> [Pandoc.Block] -> B.RenderM m (PP.Doc Annot)
renderLi ch = fmap ((PP.pretty ch <+>) . PP.align) . renderBlocks

renderLink :: Monad m
           => (Pandoc.Target -> m (PP.Doc Annot))
           -> Pandoc.Target
           -> m (PP.Doc Annot)
renderLink titleRender target@(url, _) =
  do
    title <- titleRender target
    return . PP.annotate (AnnotLink (T.pack url)) $ title
