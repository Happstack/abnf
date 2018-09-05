{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, GeneralizedNewtypeDeriving, DataKinds #-}
{- |A set of types for represeting ABNF grammars.

These types do not capture comments or layout information. Additionally, many of the types permit values that are more general than the specification allows. For example:

@
newtype CharVal
    = CharVal { charText :: Text }
@

A 'CharVal' should actually be restricted to a much smaller set than 'Text'. The assumption here is that the values are going to be constructed by parsing an ABNF, not created by hand.

If a user does want to construct ABNF specifications in code, they should create a suitable eDSL and submit a patch!

-}
module ABNF.Types
    ( RuleList(..)
    , RuleName(..)
    , Rule(..)
    , Elements
    , Alternation(..)
    , Concatenation(..)
    , Element(..)
    , ProseVal(..)
    , Bit(..)
    , Digit(..)
    , HexDigit(..)
    , NumVal(..)
    , NumSpec(..)
    , Comment(..)
    , Repeat(..)
    , Repetition(..)
    , CharVal(..)
    , RuleMap
    , ruleMap
    ) where

import Data.Data      (Data, Typeable)
import Data.Monoid    (Monoid)
import Data.Semigroup (Semigroup)
import Data.Text      (Text, unpack, toCaseFold)
import Data.String    (fromString)
import Data.Map       (Map, fromList)
import GHC.Generics   (Generic)

-- | @rulelist       =  1*( rule / (*c-wsp c-nl) )@
newtype RuleList
    = RuleList { rules :: [Rule] }
      deriving (Eq, Ord, Read, Show, Data, Typeable, Generic, Semigroup, Monoid)

-- |NOTE: 'RuleName's are case insensitive. The 'Eq' and 'Ord' instances obey this.
--
-- @rulename =  ALPHA *(ALPHA \/ DIGIT \/ "-")@
newtype RuleName
    = RuleName { ruleNameText :: Text }
      deriving (Read, Show, Data, Typeable, Generic)

instance Eq RuleName where
    (RuleName r1) == (RuleName r2) = (toCaseFold r1) == (toCaseFold r2)
    (RuleName r1) /= (RuleName r2) = (toCaseFold r1) /= (toCaseFold r2)

instance Ord RuleName where
    compare (RuleName r1) (RuleName r2) = compare (toCaseFold r1) (toCaseFold r2)

-- | @rule           =  rulename defined-as elements c-nl@
--
-- @defined-as     =  *c-wsp ("=" \/ "=\/") *c-wsp@
--
-- NOTE: we do not really handle the =\/ operator well.
data Rule
    = Rule RuleName Elements
      deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

-- | @elements       =  alternation *c-wsp@
type Elements = Alternation

-- | @alternation    =  concatenation *(*c-wsp "\/" *c-wsp concatenation)@
newtype Alternation
    = Alternation { concatenations :: [Concatenation] }
      deriving (Eq, Ord, Read, Show, Data, Typeable, Generic, Semigroup, Monoid)

-- | @concatenation  =  repetition *(1*c-wsp repetition)@
newtype Concatenation
    = Concatenation { repetitions :: [Repetition] }
      deriving (Eq, Ord, Read, Show, Data, Typeable, Generic, Semigroup, Monoid)


-- | @element =  rulename \/ group \/ option \/ char-val \/ num-val \/ prose-val@
--
-- @group  =  "(" *c-wsp alternation *c-wsp ")"@
--
-- @option =  "[" *c-wsp alternation *c-wsp "]"@
data Element
    = RN RuleName
    | Group Alternation
    | Option Alternation
    | CV CharVal
    | NV NumVal
    | PV ProseVal
      deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

-- | @prose-val      =  "\<" *(%x20-3D \/ %x3F-7E) "\>"@
newtype ProseVal
    = ProseVal { proseText :: Text }
      deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

-- | @BIT            =  "0" \/ "1"@
data Bit
    = Zero
    | One
      deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

-- | @DIGIT          =  %x30-39@
newtype Digit
    = Digit { decDigit :: Char }
      deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

-- | @HEXDIG         =  DIGIT \/ \"A\" \/ \"B\" \/ \"C\" \/ \"D\" \/ \"E\" \/ \"F\"@
newtype HexDigit
    = HexDigit { hexDigit :: Char }
      deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

-- | @num-val        =  "%" (bin-val \/ dec-val \/ hex-val)@
--
-- @bin-val        =  "b" 1*BIT [ 1*("." 1*BIT) \/ ("-" 1*BIT) ]@
--
-- @dec-val        =  "d" 1*DIGIT [ 1*("." 1*DIGIT) \/ ("-" 1*DIGIT) ]@
--
-- @hex-val        =  "x" 1*HEXDIG [ 1*("." 1*HEXDIG) \/ ("-" 1*HEXDIG) ]@
data NumVal
    = BinVal (NumSpec Bit)
    | DecVal (NumSpec Digit)
    | HexVal (NumSpec HexDigit)
      deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

data NumSpec n
    = NumSpec [n] (Maybe (Either [[n]] [n]))
      deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

-- | @comment        =  ";" *(WSP \/ VCHAR) CRLF@
newtype Comment
    = Comment { commentText :: Text }
      deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

-- | @repeat         =  1*DIGIT \/ (*DIGIT "*" *DIGIT)@
data Repeat
    = Specific  { reps    :: Integer
                }
    | Variable  { minReps :: Maybe Integer
                , maxReps :: Maybe Integer
                }
    | CommaList { minReps :: Maybe Integer
                , maxReps :: Maybe Integer
                }

    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

-- | @repetition     =  [repeat] element@
data Repetition = Repetition
    { repeat  :: Maybe Repeat
    , element :: Element
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

-- | @char-val       =  DQUOTE *(%x20-21 \/ %x23-7E) DQUOTE@
newtype CharVal
    = CharVal { charText :: Text }
      deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

type RuleMap = Map RuleName Elements

-- | convert a 'RuleList' into a 'Map'
ruleMap :: RuleList
        -> RuleMap
ruleMap (RuleList rules) = fromList $ map (\(Rule rn elem) -> (rn, elem)) rules
