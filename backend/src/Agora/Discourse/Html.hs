{-# LANGUAGE TypeOperators #-}

module Agora.Discourse.Html
       ( parseHtmlParts
       , defaultDescription
       , defaultHtmlParts
       , HtmlParts (..)
       , isDefaultShort
       , isDefaultLong
       , toHtmlPartsMaybe
       ) where

import Data.Char (isControl)
import Data.List (span)
import qualified Data.Text as T
import qualified Text.HTML.Parser as H
import qualified Text.Megaparsec as P

data HtmlParts a = HtmlParts
  { hpShort    :: a
  , hpLong     :: a
  } deriving (Eq, Show, Generic)

isDefaultShort :: Text -> HtmlParts Text -> Bool
isDefaultShort protocol hp = hpShort hp == hpShort (defaultHtmlParts protocol)

isDefaultLong :: Text -> HtmlParts Text -> Bool
isDefaultLong protocol hp = hpLong hp == hpLong (defaultHtmlParts protocol)

defaultHtmlParts :: Text -> HtmlParts Text
defaultHtmlParts protocol = HtmlParts
  { hpShort = protocol
  , hpLong = ""
  }

defaultDescription :: Text -> Text
defaultDescription protocol =
  "<p>" <> hpShort <> "</p>" <> hpLong
  where
    HtmlParts{..} = defaultHtmlParts protocol

toHtmlPartsMaybe :: Text -> HtmlParts Text -> HtmlParts (Maybe Text)
toHtmlPartsMaybe desc hp =
  HtmlParts
    (if isDefaultShort desc hp then Nothing else Just $ hpShort hp)
    (if isDefaultLong desc hp then Nothing else Just $ hpLong hp)

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
  take1_ []     = Nothing
  take1_ (t:ts) = Just (t, ts)
  takeN_ n s
    | n <= 0    = Just ([], s)
    | null s    = Nothing
    | otherwise = Just (splitAt n s)
  takeWhile_ = span
  showTokens Proxy = toString . H.renderTokens . toList
  reachOffset = reachOffsetTokenStream

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

-- | Parse short and full description
-- Html has to correspond to pattern
-- <p>{short_desc}</p>
-- {long_description}
htmlPartsParser :: HtmlParser (HtmlParts [H.Token])
htmlPartsParser = do
  void $ P.satisfy (openTag "p")
  hpShort <- P.takeWhileP (Just "short_desc") (not . closeTag "p")
  void $ P.satisfy (closeTag "p")

  hpLong <- many $ P.notFollowedBy aHref *> P.anySingle
  pure $ HtmlParts{..}
  where
    aHref = do
      void $ P.satisfy (openTag "h2")
      url <- attrMaybe "a" "href" <* P.satisfy (contentText "Proposal archive") <* P.satisfy (closeTag "a")
      void $ P.satisfy (closeTag "h2")
      pure url

parseHtmlParts :: Text -> Either String (HtmlParts Text)
parseHtmlParts html = do
  HtmlParts{..} <- first P.errorBundlePretty $ P.parse htmlPartsParser "" $ H.parseTokens $ preprocess html
  pure $ HtmlParts (cleanupString hpShort) (cleanupString hpLong)
  where
    preprocess = T.replace "\\\"" "\""
    cleanupString = T.strip . toStrict . H.renderTokens . removeBreaklines

    removeBreaklines xs = flip map xs $ \case
      H.ContentText x -> H.ContentText $ T.dropAround isControl $ dropBreaklineP $ dropBreaklineS x
      y               -> y

    dropBreaklineP xs =
      if "\\n" `T.isPrefixOf` xs then dropBreaklineP (T.drop 2 xs)
      else xs

    dropBreaklineS xs =
      if "\\n" `T.isSuffixOf` xs then dropBreaklineS (T.dropEnd 2 xs)
      else xs


----
-- Megaparsec utils
----

-- | This is an implementation of Megaparsec’s @reachOffset@ for token streams
-- where splitting the stream into lines is no longer possible (because
-- new lines are messed up by the lexer), so it just treats it all as
-- one large line.
--
-- It is far from an ideal solution, but at least it works.
--
-- TODO: Fix it.
-- It seems that there are essentially two options: do not use Megaparsec or
-- rewrite the HTML lexer using Megaparsec. The latter option will give better
-- parse error message.
reachOffsetTokenStream :: Int -> P.PosState [H.Token] -> (String, P.PosState [H.Token])
reachOffsetTokenStream o P.PosState{..}
  = (pstateLinePrefix ++ toString (H.renderTokens pstateInput), newState)
  where
    (pre, post) = splitAt (o - pstateOffset) pstateInput
    P.SourcePos sourceName sourceLine _ = pstateSourcePos
    spos = P.SourcePos sourceName sourceLine (P.mkPos $ max pstateOffset o)
    newState = P.PosState
      { pstateInput = post
      , pstateOffset = max pstateOffset o
      , pstateSourcePos = spos
      , pstateTabWidth
      , pstateLinePrefix = pstateLinePrefix ++ toString (H.renderTokens pre)
      }
