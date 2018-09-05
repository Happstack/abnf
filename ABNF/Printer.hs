{- | Render an ABNF Grammar
-}
module ABNF.Printer where

import ABNF.Types
import Data.List (intersperse)
import Data.Text (unpack)
import Text.PrettyPrint.HughesPJ
import Prelude hiding ((<>))

-- | render a list of 'Rule'
ppRuleList :: RuleList -> Doc
ppRuleList (RuleList rules) =
    vcat $ map ppRule rules

-- | render a single 'Rule'
ppRule :: Rule -> Doc
ppRule (Rule rn elements) =
    ppRuleName rn <+> equals <+> ppElements elements

ppRuleName :: RuleName -> Doc
ppRuleName (RuleName n) =
    text (unpack n)

ppElements :: Elements -> Doc
ppElements =
    ppAlternation

ppAlternation :: Alternation -> Doc
ppAlternation (Alternation cs) =
    hcat $ punctuate (text " / ") $ map ppConcatenation cs

ppConcatenation :: Concatenation -> Doc
ppConcatenation (Concatenation reps) =
    hsep $ map ppRepetition reps

ppRepetition :: Repetition -> Doc
ppRepetition (Repetition Nothing element) =
    ppElement element
ppRepetition (Repetition (Just repeat) element) =
    ppRepeat repeat <> ppElement element

ppRepeat :: Repeat -> Doc
ppRepeat (Specific reps) =
    integer reps
ppRepeat (Variable mMin mMax) =
    (maybe empty integer mMin) <> char '*' <> (maybe empty integer mMax)
ppRepeat (CommaList mMin mMax) =
    (maybe empty integer mMin) <> char '#' <> (maybe empty integer mMax)



ppElement :: Element -> Doc
ppElement (RN rn) = ppRuleName rn
ppElement (Group  alternation) = parens   $ ppAlternation alternation
ppElement (Option alternation) = brackets $ ppAlternation alternation
ppElement (CV cv)              = ppCharVal cv
ppElement (NV nv)              = ppNumVal nv
ppElement (PV pv)              = ppProseVal pv

ppCharVal :: CharVal -> Doc
ppCharVal (CharVal txt) = doubleQuotes $ text (unpack txt)

ppProseVal :: ProseVal -> Doc
ppProseVal (ProseVal txt) = char '<' <> text (unpack txt) <> char '>'

ppNumVal :: NumVal -> Doc
ppNumVal (BinVal ns) = text "%b" <> ppNumSpec ppBit ns
ppNumVal (DecVal ns) = text "%d" <> ppNumSpec ppDigit ns
ppNumVal (HexVal ns) = text "%x" <> ppNumSpec ppHexDigit ns

ppNumSpec :: (n -> Doc) -> NumSpec n -> Doc
ppNumSpec ppN (NumSpec n mRange) =
    hcat (map ppN n) <> maybe empty ppRange mRange
    where
      ppRange (Left ns) = char '.' <> (hcat $ punctuate (char '.') $ map (\n -> hcat $ map ppN n) ns)
      ppRange (Right n) = char '-' <> (hcat $ map ppN n)

ppBit :: Bit -> Doc
ppBit Zero = char '0'
ppBit One  = char '1'

ppDigit :: Digit -> Doc
ppDigit (Digit d) = char d

ppHexDigit :: HexDigit -> Doc
ppHexDigit (HexDigit h) = char h
