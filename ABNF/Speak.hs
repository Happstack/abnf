{-# LANGUAGE QuasiQuotes #-}
module ABNF.Speak where

import ABNF.Printer
import ABNF.Parser
import ABNF.Types
import Control.Applicative ((<$>))
import Control.Monad.State
import Data.Char  (chr)
import Data.Maybe (fromMaybe)
import Data.Text (unpack)
import Text.PrettyPrint.HughesPJ
import Data.Map (Map)
import qualified Data.Map as Map
import Numeric (readDec, readHex)
import ABNF.Verbatim
import Text.Parsec (parse)

data Speak
    = Speak { theRules   :: Map RuleName Elements
            , showNum    :: Int -> Doc
--            , showHexVal :: [HexDigit] -> Doc
--            , showDecVal :: [Digit] -> Doc
            }

data Tree
    = Leaf Doc
    | Empty
    | Alternatives [Tree]
    | Optional Tree
    | Concat [Tree]
      deriving Show

type Gibberish = State Speak Tree

speak :: Speak -> Gibberish -> IO ()
speak st gibberish =
    let forest = evalState gibberish st
    in print $ vcat (sentences forest)

sentences :: Tree -> [Doc]
sentences (Leaf doc)     =
    [doc]
sentences Empty          =
    []
sentences (Alternatives trees) =
    concatMap sentences trees
sentences (Concat trees) =
    let ss = map sentences trees
    in map hcat $ cartesianProduct ss
sentences (Optional tree) =
    empty : sentences tree

cartesianProduct :: [[a]] -> [[a]]
-- ^ cartesianProduct [[1,2,3], [4,5],[6]] -> [[1,4,6],[1,5,6],[2,4,6],[2,5,6],[3,4,6],[3,5,6]]
cartesianProduct [] = []
cartesianProduct [xs] = map (: []) xs
cartesianProduct (xs : yss) =
    distribute xs (cartesianProduct yss)
    where distribute xs yss = concat (map (\ x -> map (x :) yss) xs)

genRule :: RuleName -> Gibberish
genRule ruleName =
    do rules <- theRules <$> get
       case Map.lookup ruleName rules of
         Nothing         -> error $ "Missing rule: " ++ show (ruleNameText ruleName)
         (Just elements) -> genElements elements

genElements :: Elements -> Gibberish
genElements = genAlternation

genAlternation :: Alternation -> Gibberish
genAlternation (Alternation concatenations) =
    do t <- mapM genConcatenation concatenations
       return $ Alternatives t

genConcatenation :: Concatenation -> Gibberish
genConcatenation (Concatenation repetitions) =
    do parts <- mapM genRepetition repetitions
       return $ Concat parts

genRepetition :: Repetition -> Gibberish
genRepetition (Repetition Nothing element) =
    genElement element
genRepetition (Repetition (Just (Specific n)) element) =
    do ts <- replicateM 2 $ genElement element
       return $ Concat ts
genRepetition (Repetition (Just (Variable mMin mMax)) element) =
    do t1 <- genN (fromMaybe 0 mMin)
       t2 <- genN (fromMaybe (succ $ fromMaybe 0 mMin) mMax)
       return $ Alternatives [t1, t2]
    where
      genN 0 = return Empty
      genN 1 = genElement element
      genN n =
          do ts <- replicateM (fromIntegral n) $ genElement element
             return $ Concat ts

genElement :: Element -> Gibberish
genElement (RN ruleName) =
    genRule ruleName
genElement (Group alternation) =
    do t <- genAlternation alternation
       return $ Concat [t]
genElement (Option alternation) =
    do gibberish <- genAlternation alternation
       return (Optional gibberish )
genElement (CV charVal) =
    return $ Leaf (text $ unpack $ charText charVal)
genElement (NV numVal) =
    genNumVal numVal
genElement (PV proseVal) =
    return $ Empty -- FIXME

genNumVal :: NumVal -> Gibberish
genNumVal (HexVal ns) =
    genNumSpec hexToInt ns
genNumVal (DecVal ns) =
    genNumSpec decToInt ns
{-
genNumVal (BinVal ns) =
    do sdv <- showBinVal <$> get
       genNumSpec sbv ns
-}

genNumSpec :: ([n] -> Int) -> NumSpec n -> Gibberish
genNumSpec readVal (NumSpec n Nothing) =
    do sn <- showNum <$> get
       return $ Leaf $ sn (readVal n)
genNumSpec readVal (NumSpec n (Just (Left ns))) =
    do sn <- showNum <$> get
       return $ Leaf $ hcat $ map (sn . readVal) (n : ns)
genNumSpec readVal (NumSpec m (Just (Right n))) =
    do sn <- showNum <$> get
       return $ Alternatives $ map (Leaf . sn) [readVal m, readVal n]

{-
    case readHex $ show $ hcat (map ppN n) of
      [(n,[])] -> return $ Leaf $ text (show n)
-}
-- 
           
--    return $ Leaf $ hcat (map ppN n)


-- ppHexDigit :: HexDigit -> Doc
-- ppHexDigit (HexDigit x) = (char x)
--    Alternatives $ map (Leaf . char)  [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F' ]


------------------------------------------------------------------------------
-- tests
------------------------------------------------------------------------------

grammar1 =
        let g = [verbatim|
hello-world = 2*(hello space) [world]
hello = "hello"
space = " "
world = "world" / "earth"
|]
            gABNF = case parse pRuleList g g of
                      (Left e) -> error (show e)
                      (Right x) -> x
        in gABNF

speak1 = asciiSpeak { theRules =  ruleMap grammar1 }

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
      _        -> error $ "hexToInt: not a valid decimal number: " ++ str

asciiSpeak :: Speak
asciiSpeak =
    Speak { theRules   = Map.empty
          , showNum    = char . chr
          }

speakABNF = asciiSpeak { theRules = ruleMap (parseABNF abnf_raw) }

