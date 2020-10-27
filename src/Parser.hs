
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


type DocComment = [Text]


type Parser = ParsecT Void Text Identity


sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") edhSkipBlockCommentNested

edhSkipBlockCommentNested :: Parser ()
edhSkipBlockCommentNested =
  try (string "{#" >> notFollowedBy (char '#')) >> void
    (manyTill (L.skipBlockCommentNested "{#" "#}" <|> void anySingle)
              (string "#}")
    )

-- | doc comment must start with "{##", this will return effective lines of
-- the comment
docComment :: Parser DocComment
docComment = do
  void $ string "{##"
  -- treat the 1st line specially
  s          <- getInput
  o          <- getOffset
  (o', done) <- findEoL
  let line     = maybe "" fst $ takeN_ (o' - o) s
      cumLines = [ line | not $ T.null $ T.strip line ]
  if done
    then do
      void sc
      return $! cumLines
    else cmtLines cumLines
 where
  cmtLines :: [Text] -> Parser [Text]
  cmtLines cumLines = do
    s          <- getInput
    o          <- getOffset
    (o', done) <- findEoL
    let line = maybe "" fst $ takeN_ (o' - o) s
    if done
      then do
        void sc
        return
          $! reverse
               (if T.null (T.strip line)
                 then cumLines
                 else cmtLine line : cumLines
               )
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
immediateDocComment :: Parser DocComment
immediateDocComment = docComment >>= moreAfter
 where
  moreAfter !gotCmt =
    (try (scWithSemiColons >> docComment) >>= moreAfter) <|> return gotCmt


scWithSemiColons :: Parser ()
scWithSemiColons = L.space (space1 <|> void (char ';'))
                           (L.skipLineComment "#")
                           edhSkipBlockCommentNested


symbol :: Text -> Parser Text
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

keyword :: Text -> Parser Text
keyword kw = try $ lexeme (string kw <* notFollowedBy (satisfy isIdentChar))

optionalComma :: Parser Bool
optionalComma = fromMaybe False <$> optional (True <$ symbol ",")

optionalSemicolon :: Parser Bool
optionalSemicolon = fromMaybe False <$> optional (True <$ symbol ";")


isIdentStart :: Char -> Bool
isIdentStart !c = c == '_' || Char.isAlpha c

isIdentChar :: Char -> Bool
isIdentChar c = c == '_' || c == '\'' || Char.isAlphaNum c


type ModuleDecl = (Maybe DocComment, [ArtDecl])
type ArtDecl = (Maybe DocComment, Text)


moduleDecl :: Parser ModuleDecl
moduleDecl = lexeme $ do
  sc
  moduCmt <- optional docComment
  arts    <- untilNothing (scWithSemiColons >> artifactDecl)
  eof
  return (moduCmt, arts)

untilNothing :: Parser (Maybe a) -> Parser [a]
untilNothing p = do
  ox <- p
  case ox of
    Nothing -> pure []
    Just x -> (x :) <$> untilNothing p

artifactDecl :: Parser (Maybe ArtDecl)
artifactDecl = lexeme $ do
  artCmt <- optional immediateDocComment
  optional $ do
    artBody <- takeWhileP (Just "artifact body")
                          (not . flip elem (";{" :: [Char]))
    if T.null $ T.strip artBody
      then empty -- this is not possible in real cases
      else return (artCmt, artBody)

parseModule :: Text -> ModuleDecl
parseModule !moduSrc = case parse moduleDecl "" moduSrc of
  Left  e -> error $ errorBundlePretty e
  Right r -> r

