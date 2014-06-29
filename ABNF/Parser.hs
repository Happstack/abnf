{-# LANGUAGE FlexibleContexts, QuasiQuotes, TemplateHaskell #-}
{- | Parse an ABNF grammar specification.
-}
module ABNF.Parser where

import ABNF.Printer
import ABNF.Types
import Control.Applicative ((<$>), (*>), (<*), pure)
import Control.Monad.Identity (Identity)
import Data.Char   (isSpace, ord)
import Data.Maybe (catMaybes)
import Data.String (fromString)
import Text.Parsec as Parsec (ParsecT, SourceName, Stream, (<|>), (<?>), anyChar, between, char, choice, many, many1, manyTill, satisfy, sepBy1, skipMany, skipMany1, space, letter, digit, parse, parseTest, string, optional, notFollowedBy, try, label, hexDigit)
import Language.Haskell.TH
import Language.Haskell.TH.Quote

-- | @rulelist       =  1*( rule / (*c-wsp c-nl) )@
pRuleList :: (Stream s m Char) => ParsecT s u m RuleList
pRuleList =
    (RuleList . catMaybes <$> (many1 $ (Just <$> pRule) <|> (try $ do skipMany c_wsp >> c_nl >> pure Nothing))) `label` "pRuleList"

-- | @rule           =  rulename defined-as elements c-nl
--                                ; continues if next line starts
--                                ;  with white space@
--
-- @defined-as     =  *c-wsp ("=" \/ "=\/") *c-wsp@
pRule :: (Stream s m Char) => ParsecT s u m Rule
pRule =
    (do rn    <- pRuleName
        definedAs
        elems <- pElements
--        pCRLF
--        c_nl
        return (Rule rn elems)) `label` "rule"

-- | @rulename =  ALPHA *(ALPHA \/ DIGIT \/ "-")@
pRuleName :: (Stream s m Char) => ParsecT s u m RuleName
pRuleName =
    (do c  <- pALPHA
        cs <- many (pALPHA <|> digit <|> char '-')
        return $ RuleName $ fromString (c:cs)) `label` "rulename"

-- | @defined-as     =  *c-wsp ("=" \/ "=\/") *c-wsp@
--
-- FIXME: handle =\/ in a sensible way
definedAs :: (Stream s m Char) => ParsecT s u m ()
definedAs =
   (do skipMany c_wsp
       (try $ string "=/") <|> string "="
       skipMany c_wsp) `label` "defined-as"

-- |@elements       =  alternation *c-wsp@
pElements :: (Stream s m Char) => ParsecT s u m Elements
pElements =
    (pAlternation <* skipMany c_wsp) `label` "pElements"

-- |@alternation    =  concatenation *(*c-wsp "\/" *c-wsp concatenation)@
--
-- NOTE: we also allow | instead of /
pAlternation :: (Stream s m Char) => ParsecT s u m Alternation
pAlternation =
   (do c  <- pConcatenation
       cs <- many $ try $ do skipMany c_wsp
                             char '/' <|> char '|'
                             skipMany c_wsp
                             pConcatenation
       return (Alternation (c:cs))) `label` "pAlternation"

-- |@concatenation  =  repetition *(1*c-wsp repetition)@
pConcatenation :: (Stream s m Char) => ParsecT s u m Concatenation
pConcatenation =
   (do r  <- pRepetition
       rs <- many (try $ skipMany1 c_wsp *> pRepetition)
       return (Concatenation (r:rs))) `label` "pConcatenation"

-- |@repetition     =  [repeat] element@
pRepetition :: (Stream s m Char) => ParsecT s u m Repetition
pRepetition =
   (do reps <- (Just <$> pRepeat) <|> pure Nothing
       element <- pElement
       return $ Repetition reps element) `label` "pRepetition"

-- |@element = rulename \/ group \/ option \/ char-val \/ num-val \/ prose-val@
pElement :: (Stream s m Char) => ParsecT s u m Element
pElement =
    (choice [ RN <$> pRuleName
            , pGroup
            , pOption
            , CV <$> pCharVal
            , NV <$> pNumVal
            , PV <$> pProseVal
           ]) `label` "pElement"

-- |@group          =  "(" *c-wsp alternation *c-wsp ")"@
pGroup :: (Stream s m Char) => ParsecT s u m Element
pGroup =
   (do char '('
       skipMany c_wsp
       a <- pAlternation
       skipMany c_wsp
       char ')'
       return (Group a)) `label` "pGroup"

-- |@option         =  "[" *c-wsp alternation *c-wsp "]"@
pOption :: (Stream s m Char) => ParsecT s u m Element
pOption =
   (do char '['
       skipMany c_wsp
       a <- pAlternation
       skipMany c_wsp
       char ']'
       return (Option a)) `label` "pOption"

-- |@char-val       =  DQUOTE *(%x20-21 / %x23-7E) DQUOTE
--                  ; quoted string of SP and VCHAR
--                  ;  without DQUOTE@
pCharVal :: (Stream s m Char) => ParsecT s u m CharVal
pCharVal =
    (do char '"'
        cvs <- many $ satisfy $ \c -> (ord c) == 0x20 || (ord c) == 0x21 || ((ord c) >= 0x23 && (ord c) <= 0x7E)
        char '"'
        return $ CharVal $ fromString cvs) `label` "pCharVal"

-- | @num-val        =  "%" (bin-val \/ dec-val \/ hex-val)@
pNumVal :: (Stream s m Char) => ParsecT s u m NumVal
pNumVal =
    do char '%'
       choice [ BinVal <$> pBinVal
              , DecVal <$> pDecVal
              , HexVal <$> pHexVal
              ]

-- | @bin-val        =  "b" 1*BIT [ 1*("." 1*BIT) \/ ("-" 1*BIT) ]@
pBinVal :: (Stream s m Char) => ParsecT s u m (NumSpec Bit)
pBinVal =
    do char 'b'
       pNumSpec pBit

-- | @dec-val        =  "d" 1*DIGIT [ 1*("." 1*DIGIT) \/ ("-" 1*DIGIT) ]@
pDecVal :: (Stream s m Char) => ParsecT s u m (NumSpec Digit)
pDecVal =
    do char 'd'
       pNumSpec pDigit

-- | @hex-val        =  "x" 1*HEXDIG [ 1*("." 1*HEXDIG) \/ ("-" 1*HEXDIG) ]@
pHexVal :: (Stream s m Char) => ParsecT s u m (NumSpec HexDigit)
pHexVal =
    do char 'x'
       pNumSpec pHexDigit



-- |@num-spec        =  1*num [ 1*("." 1*num) / ("-" 1*num) ]@
--
-- This rule does not really exist but is abstracted from the
-- similarities in 'bin-val', 'dec-val', and 'hex-val'.
pNumSpec :: (Stream s m Char) => ParsecT s u m n -> ParsecT s u m (NumSpec n)
pNumSpec p =
    do n <- many1 p
       r <- do ns <- many1 (char '.' *> many1 p)
               return $ Just $ Left ns
            <|>
            do char '-'
               m <- many1 p
               return $ Just $ Right m
            <|>
               return Nothing
       return $ NumSpec n r

-- |@BIT            =  "0" / "1"@
pBit :: (Stream s m Char) => ParsecT s u m Bit
pBit = (char '0' *> pure Zero) <|> (char '1' *> pure One)

-- |@DIGIT          =  %x30-39@
--                  ; 0-9@
pDigit  :: (Stream s m Char) => ParsecT s u m Digit
pDigit = Digit <$> digit

-- | @HEXDIG         =  DIGIT \/ \"A\" \/ \"B\" \/ \"C\" \/ \"D\" \/ \"E\" \/ \"F\"@
pHexDigit :: (Stream s m Char) => ParsecT s u m HexDigit
pHexDigit = HexDigit <$> Parsec.hexDigit

-- | @prose-val      =  "\<" *(%x20-3D \/ %x3F-7E) "\>"@
pProseVal :: (Stream s m Char) => ParsecT s u m ProseVal
pProseVal =
    do char '<'
       txt <- many $ satisfy $ \c ->
              ((ord c) >= 0x20 && (ord c) <= 0x3D) ||
              ((ord c) >= 0x3F && (ord c) <= 0x7E)

       char '>'
       return $ ProseVal (fromString txt)

-- |@repeat         =  1*DIGIT \/ (*DIGIT \"*\" *DIGIT) \/ (*DIGIT \"#\" *DIGIT)@
pRepeat :: (Stream s m Char) => ParsecT s u m Repeat
pRepeat = (pVariable <|> pSpecific) `label` "pRepeat"
    where
      pSpecific = Specific . read <$> try (many1 digit <* notFollowedBy (char '*' <|> char '#'))
      pVariable = try $
          do m <- mRead <$> many digit
             con <- (char '*' >> return Variable) <|> (char '#' >> return CommaList)
             n <- mRead <$> many digit
             return $ con m n
      mRead [] = Nothing
      mRead cs = Just $ read cs

-- |@ALPHA          =  %x41-5A \/ %x61-7A   ; A-Z \/ a-z@
pALPHA :: (Stream s m Char) => ParsecT s u m Char
pALPHA =
    (satisfy $ \c ->
        ((c >= 'A') && (c <= 'Z')) ||
        ((c >= 'a') && (c <= 'z'))) <?> "pALPHA"

-- |@c-wsp          =  WSP \/ (c-nl WSP)@
c_wsp :: (Stream s m Char) => ParsecT s u m ()
c_wsp = ((pWSP <|> (try $ c_nl >> pWSP)) >> return ()) `label` "c_wsp"

-- |@c-nl           =  comment \/ CRLF@
c_nl :: (Stream s m Char) => ParsecT s u m ()
c_nl = ((pComment >> pure ()) <|> (pCRLF >> pure ())) `label` "c-nl"

-- |@comment        =  ";" *(WSP \/ VCHAR) CRLF@
pComment :: (Stream s m Char) => ParsecT s u m Comment
pComment =
   (do char ';'
       c <- Comment . fromString <$> many (pWSP <|> pVCHAR)
       pCRLF
       return c) `label` "pComment"

-- |@CRLF           =  CR LF@
--
-- NOTE: we also allow just LF
pCRLF :: (Stream s m Char) => ParsecT s u m String
pCRLF = (string "\r\n" <|> string "\n") `label` "pCRLF"

-- |@VCHAR          =  %x21-7E@
pVCHAR :: (Stream s m Char) => ParsecT s u m Char
pVCHAR =
    (satisfy $ \c ->
        ((ord c) >= 0x21) && ((ord c) <= 0x7E)) `label` "pVCHAR"

-- |@WSP            =  SP \/ HTAB@
pWSP :: (Stream s m Char) => ParsecT s u m Char
pWSP = (char ' ' <|> char '\t') `label` "pWSP"


-- | parse a list of ABNF rules
parseRuleList :: (Stream s Identity Char) =>
             SourceName
          -> s
          -> RuleList
parseRuleList srcName str =
    case parse pRuleList srcName str of
      (Left e) -> error (show e)
      (Right x) -> x

-- | parse a single ABNF rules
parseRule :: (Stream s Identity Char) =>
             SourceName
          -> s
          -> Rule
parseRule srcName str =
    case parse pRule srcName str of
      (Left e) -> error (show e)
      (Right x) -> x

-- | QuasiQuote for a ABNF 'RuleList'
abnf :: QuasiQuoter
abnf = QuasiQuoter
    { quoteExp = \str -> [| parseRuleList "rule list" (dropWhile isSpace str) |]
    , quoteType = error "abnf does not implement quoteType"
    , quotePat  = error "abnf does not implement quotePat"
    , quoteDec  = error "abnf does not implement quoteDec"
    }

-- | QuasiQuote for a single ABNF 'Rule'
abnfRule :: QuasiQuoter
abnfRule = QuasiQuoter
    { quoteExp = \str -> [| parseRule "rule" (str :: String) |]
    , quoteType = error "abnf does not implement quoteType"
    , quotePat  = error "abnf does not implement quotePat"
    , quoteDec  = error "abnf does not implement quoteDec"
    }
