{-# LANGUAGE TypeOperators #-}

module Agora.Discourse.Html
       ( parseHtmlParts
       , defaultDescription
       , defaultHtmlParts
       , HtmlParts (..)
       , isDefaultShort
       , isDefaultLong
       , isDefaultUrl
       , toHtmlPartsMaybe
       ) where

import Data.Char (isControl)
import Data.List (span)
import qualified Data.Text as T
import qualified Text.HTML.Parser as H
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

data HtmlParts a = HtmlParts
  { hpShort    :: a
  , hpLong     :: a
  , hpFileLink :: Maybe Text
  } deriving (Eq, Show, Generic)

isDefaultShort :: Text -> HtmlParts Text -> Bool
isDefaultShort protocol hp = hpShort hp == hpShort (defaultHtmlParts protocol)

isDefaultLong :: Text -> HtmlParts Text -> Bool
isDefaultLong protocol hp = hpLong hp == hpLong (defaultHtmlParts protocol)

isDefaultUrl :: Text -> HtmlParts Text -> Bool
isDefaultUrl protocol hp = hpFileLink hp == hpFileLink (defaultHtmlParts protocol)

defaultHtmlParts :: Text -> HtmlParts Text
defaultHtmlParts protocol = HtmlParts
  { hpShort = "<i>Description for " <> protocol <> " will be added by a moderator</i>"
  , hpLong = ""
  , hpFileLink  = Nothing
  }

defaultDescription :: Text -> Text
defaultDescription protocol =
  "<p>" <> hpShort <> "</p>" <>
  hpLong <>
  "<h2><a>Proposal archive</a></h2>"
  where
    HtmlParts{..} = defaultHtmlParts protocol

toHtmlPartsMaybe :: Text -> HtmlParts Text -> HtmlParts (Maybe Text)
toHtmlPartsMaybe title hp =
  HtmlParts
    (if isDefaultShort title hp then Nothing else Just $ hpShort hp)
    (if isDefaultLong title hp then Nothing else Just $ hpLong hp)
    (if isDefaultUrl title hp then Nothing else hpFileLink hp)

attrValue :: H.Attr -> Text
attrValue (H.Attr _ v) = v

type HtmlParser = P.Parsec Void [H.Token]

instance P.Stream [H.Token] where
  type Token [H.Token] = H.Token
  type Tokens [H.Token] = [H.Token]
  tokenToChunk Proxy = pure
  tokensToChunk Proxy = id
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null
  advance1 Proxy = defaultAdvance1
  advanceN Proxy w = foldl' (defaultAdvance1 w)
  take1_ []     = Nothing
  take1_ (t:ts) = Just (t, ts)
  takeN_ n s
    | n <= 0    = Just ([], s)
    | null s    = Nothing
    | otherwise = Just (splitAt n s)
  takeWhile_ = span

instance P.ShowToken H.Token where
  showTokens = toString . H.renderTokens . toList

openTag :: Text -> H.Token -> Bool
openTag t (H.TagOpen x _) = t == x
openTag _ _               = False

closeTag :: Text -> H.Token -> Bool
closeTag t (H.TagClose x) = t == x
closeTag _ _              = False

contentText :: Text -> H.Token -> Bool
contentText ex (H.ContentText x) = ex == x
contentText _ _                  = False

attrMaybe :: Text -> Text -> HtmlParser (Maybe Text)
attrMaybe tag atr = fmap extractAttr $ P.satisfy $
  \case
    H.TagOpen t _      -> tag == t
    H.TagSelfClose t _ -> tag == t
    _  -> False
  where
    extractAttr (H.TagOpen _ attrs)      = find' attrs
    extractAttr (H.TagSelfClose _ attrs) = find' attrs
    extractAttr _                        = error "unexpected tag"

    find' = fmap attrValue . find (\(H.Attr nm _) -> nm == atr)

-- | Parse short and full description, also link to proposal file.
-- Html has to correspond to pattern
-- <p>{short_desc}</p>
-- {long_description}
-- <h2><a href={proposal_file}>Proposal archive</a></h2>
-- Link to a proposal archive is optional
htmlPartsParser :: HtmlParser (HtmlParts [H.Token])
htmlPartsParser = do
  void $ P.satisfy (openTag "p")
  hpShort <- P.takeWhileP (Just "short_desc") (not . closeTag "p")
  void $ P.satisfy (closeTag "p")

  hpLong <- many $ P.notFollowedBy aHref *> P.anyChar
  hpFileLink <- P.try aHref <|> pure Nothing
  pure $ HtmlParts{..}
  where
    aHref = do
      void $ P.satisfy (openTag "h2")
      url <- attrMaybe "a" "href" <* P.satisfy (contentText "Proposal archive") <* P.satisfy (closeTag "a")
      void $ P.satisfy (closeTag "h2")
      pure url

parseHtmlParts :: Text -> Either String (HtmlParts Text)
parseHtmlParts html = do
  HtmlParts{..} <- first P.parseErrorPretty $ P.parse htmlPartsParser "" $ H.parseTokens $ preprocess html
  pure $ HtmlParts (cleanupString hpShort) (cleanupString hpLong) hpFileLink
  where
    preprocess = T.replace "\\\"" "\""
    cleanupString = T.strip . toStrict . H.renderTokens . removeBreaklines

    removeBreaklines xs = flip map xs $ \case
      H.ContentText x -> H.ContentText $ T.dropAround isControl $ dropBreaklineP $ dropBreaklineS $ x
      y               -> y

    dropBreaklineP xs =
      if "\\n" `T.isPrefixOf` xs then dropBreaklineP (T.drop 2 xs)
      else xs

    dropBreaklineS xs =
      if "\\n" `T.isSuffixOf` xs then dropBreaklineS (T.dropEnd 2 xs)
      else xs

----------------------------------------------------------------------------
-- Helpers
-- Copy paste from Text.Megaparsec.Stream

defaultAdvance1
  :: P.Pos           -- ^ Tab width
  -> P.SourcePos     -- ^ Current position
  -> t               -- ^ Current token
  -> P.SourcePos     -- ^ Incremented position
defaultAdvance1 _width (P.SourcePos n l c) _t = P.SourcePos n l (c <> P.pos1)
{-# INLINE defaultAdvance1 #-}
