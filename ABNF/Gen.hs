{-# LANGUAGE QuasiQuotes #-}
{- |Create random documents which conform to a supplied grammar.

This module is experimental and incomplete.

-}
module ABNF.Gen where

import ABNF.RFC5234              (abnf_ruleMap)
import ABNF.Types                ( Alternation(..), CharVal(..), Concatenation(..)
                                 , Digit(..), Element(..), Elements, HexDigit(..), NumSpec(..)
                                 , NumVal(..), Repeat(..), Repetition(..), Rule(..)
                                 , RuleName(..))
import Control.Monad             (replicateM)
import Data.Char                 (chr)
import Data.Maybe                (fromMaybe)
import Data.Text                 (unpack, pack)
import Text.PrettyPrint.HughesPJ (Doc, char, empty, hcat, text)
import           Data.Map        (Map)
import qualified Data.Map        as Map
import Numeric                   (readDec, readHex)
import Text.Parsec               (parse)
import Test.QuickCheck           (Gen, arbitrary, choose, elements, oneof)

data GenConf = GenConf
    { theRules :: Map RuleName Elements
    , intToDoc :: Int -> Doc
    }

asciiGenConf :: GenConf
asciiGenConf =
    GenConf { theRules = Map.empty
            , intToDoc = char . chr
            }

abnfGenConf :: GenConf
abnfGenConf =
    asciiGenConf { theRules = abnf_ruleMap }


genRule :: GenConf -> RuleName -> Gen Doc
genRule genConf theRule =
    case Map.lookup theRule (theRules genConf) of
      Nothing -> error $ "rule not found: " ++ show theRule
      (Just e) ->
          genElements genConf e

genRuleName :: Gen RuleName
genRuleName =
    do str <- arbitrary
       return $ RuleName $ pack str

genElements :: GenConf -> Elements -> Gen Doc
genElements genConf = genAlternation genConf

genAlternation :: GenConf -> Alternation -> Gen Doc
genAlternation genConf (Alternation concatenations) =
    oneof $ map (genConcatenation genConf) concatenations

genConcatenation :: GenConf -> Concatenation -> Gen Doc
genConcatenation genConf (Concatenation repetitions) =
    do pieces <- mapM (genRepetition genConf) repetitions
       return $ hcat pieces

genRepetition :: GenConf -> Repetition -> Gen Doc
genRepetition genConf (Repetition Nothing element) =
    genElement genConf element
genRepetition genConf (Repetition (Just (Specific n)) element) =
    do ds <- replicateM (fromIntegral n) (genElement genConf element)
       return $ hcat ds
genRepetition genConf (Repetition (Just (Variable mn mx)) element) =
    do n <- choose (fromMaybe 0 mn, fromMaybe (1 + (fromMaybe 0 mn)) mx)
       ds <- replicateM (fromIntegral n) (genElement genConf element)
       return $ hcat ds

genElement :: GenConf -> Element -> Gen Doc
genElement genConf (RN ruleName) =
    genRule genConf ruleName
genElement genConf (Group alternation) =
    genAlternation genConf alternation
genElement genConf (Option alternation) =
    do b <- choose (True, False)
       if b
          then genAlternation genConf alternation
          else return empty
genElement _genConf (CV (CharVal txt)) =
    do return $ text $ unpack txt
genElement genConf (NV numVal) =
    genNumVal genConf numVal
genElement genConf (PV pv) = return empty -- error $ show pv

genNumVal :: GenConf -> NumVal -> Gen Doc
genNumVal genConf (DecVal ns) = genNumSpec genConf decToInt ns
genNumVal genConf (HexVal ns) = genNumSpec genConf hexToInt ns

hexToInt :: [HexDigit] -> Int
hexToInt hxs =
    let str = map hexDigit hxs in
    case readHex str of
      [(n,"")] -> n
      _        -> error $ "hexToInt: not a valid hex number: " ++ str

decToInt :: [Digit] -> Int
decToInt ds =
    let str = map decDigit ds in
    case readDec str of
      [(n,"")] -> n
      _        -> error $ "decToInt: not a valid decimal number: " ++ str

-- | FIXME: need to be able to specify, Int -> Doc
genNumSpec :: GenConf -> ([n] -> Int) -> NumSpec n -> Gen Doc
genNumSpec genConf readVal (NumSpec n Nothing) =
    return $ intToDoc genConf $ readVal n
genNumSpec genConf readVal (NumSpec n (Just (Left ns))) =
    return $ hcat $ map (intToDoc genConf . readVal) (n : ns)
genNumSpec genConf readVal (NumSpec m (Just (Right n))) =
    do i <- elements [readVal m .. readVal n]
       return $ (intToDoc genConf) $ i
