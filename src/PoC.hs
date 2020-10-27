
module PoC where

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


data Module = Module [Art]
data Art = Art Text

modu :: Parser Module
modu = Module <$> many art

art :: Parser Art
art = do
  void $ many space1
  choice [eof >> empty, char ';' >> empty, Art <$> artBody]
 where
  artBody = do
    body <- takeWhileP (Just "artifact body") (/= ';')
    void $ optional $ char ';'
    return $ T.stripEnd body
