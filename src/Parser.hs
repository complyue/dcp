
module Parser where

import           Prelude
-- import           Debug.Trace

import           Control.Applicative     hiding ( many
                                                , some
                                                )
import           Control.Monad
import           Control.Monad.Identity

import           Data.Void
import           Data.Maybe
import           Data.Functor
import qualified Data.Char                     as Char
import           Data.Text                      ( Text )
import qualified Data.Text                     as T

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L


type Parser = ParsecT Void Text Identity


optionalComma :: Parser Bool
optionalComma = fromMaybe False <$> optional (True <$ symbol ",")

optionalSemicolon :: Parser Bool
optionalSemicolon = fromMaybe False <$> optional (True <$ symbol ";")


sc :: Parser ()
sc =
  L.space space1 (L.skipLineComment "#") (L.skipBlockCommentNested "{#" "#}")

-- | doc comment must start with "{##", this will return effective lines of
-- the comment or Nothing if none there.
--
-- note whitespaces and a semicolon will be consumed (i.e. no back tracking),
-- even when Nothing is returned.
docComments :: Parser (Maybe [Text])
docComments = findIt
 where
  -- consumer of whitespaces except block comment
  nbsc   = L.space space1 (L.skipLineComment "#") empty

  findIt = do
    -- ignore leading whitespaces and an optional semicolon in between
    nbsc >> optionalSemicolon >> nbsc
    -- try get a doc comment block
    getIt >>= \case
      Nothing -> optional (L.skipBlockCommentNested "{#" "#}") >>= \case
        -- there may be a block comment following
        Nothing -> return Nothing -- no, we are sure there's no doc cmt
        Just{}  -> findIt -- try again after a block comment consumed
      gotCmt@Just{} -> return gotCmt -- got doc comment

  getIt = optional (string "{##") >>= \case
    Nothing -> return Nothing
    Just{}  -> do
      -- treat the 1st line specially
      s          <- getInput
      o          <- getOffset
      (o', done) <- findEoL
      let line     = maybe "" fst $ takeN_ (o' - o) s
          cumLines = if T.null $ T.strip line then [] else [line]
      if done then return $ Just $! cumLines else Just <$> cmtLines cumLines

  cmtLines :: [Text] -> Parser [Text]
  cmtLines cumLines = do
    s          <- getInput
    o          <- getOffset
    (o', done) <- findEoL
    let line = maybe "" fst $ takeN_ (o' - o) s
    if done
      then return $! reverse
        (if T.null (T.strip line) then cumLines else cmtLine line : cumLines)
      else cmtLines $ cmtLine line : cumLines

  findEoL :: Parser (Int, Bool)
  findEoL = getOffset >>= \ !o -> choice
    [ string "#}" >> return (o, True)
    , eol >> return (o, False)
    , anySingle >> findEoL
    ]

  cmtLine :: Text -> Text
  cmtLine s0 =
    let s1 = T.strip s0
    in  case T.stripPrefix "#" s1 of
          Nothing -> s0 -- no leading #, return original content anyway
          -- with leading #, remove 1 single leading space if present
          Just s2 -> fromMaybe s2 $ T.stripPrefix " " s2

-- | get the doc comment immediately before next non-whitespace lexeme, return
-- the last one if multiple consecutive doc comment blocks present.
--
-- note whitespaces will be consumed without back tracking anyway.
immediateDocComments :: Parser (Maybe [Text])
immediateDocComments = docComments >>= moreAfter
 where
  moreAfter Nothing = return Nothing
  moreAfter !gotCmt = docComments >>= \case
    Nothing        -> return gotCmt
    gotMore@Just{} -> moreAfter gotMore


lexeme :: Parser a -> Parser a
lexeme = (sc *>)

symbol :: Text -> Parser Text
symbol = lexeme . string

keyword :: Text -> Parser Text
keyword !kw = try $ lexeme (string kw <* notFollowedBy (satisfy isIdentChar))


isIdentStart :: Char -> Bool
isIdentStart !c = c == '_' || Char.isAlpha c

isIdentChar :: Char -> Bool
isIdentChar c = c == '_' || c == '\'' || Char.isAlphaNum c


type ModuleDecl = (Maybe ModuleCmt, [ArtDecl])
type ModuleCmt = [Text]
type ArtDecl = (Maybe ArtCmt, Text)
type ArtCmt = [Text]


moduleDecl :: Parser ModuleDecl
moduleDecl = do
  moduCmt <- docComments
  arts    <- many artifactDecl
  return (moduCmt, arts)

artifactDecl :: Parser ArtDecl
artifactDecl = do
  artCmt  <- immediateDocComments
  artBody <- takeWhileP (Just "artifact body") (/= '{')
  return (artCmt, artBody)


parseModule :: Text -> ModuleDecl
parseModule !moduSrc = case parse moduleDecl "" moduSrc of
  Left  e -> error $ errorBundlePretty e
  Right r -> r

