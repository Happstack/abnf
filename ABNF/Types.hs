{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module ABNF.Types where

import Data.Data (Data, Typeable)
import Data.Monoid (Monoid)
import Data.Text (Text, unpack, toCaseFold)
import Data.String (fromString)
import Data.Map    (Map, fromList)

newtype RuleList
    = RuleList { rules :: [Rule] }
      deriving (Eq, Ord, Read, Show, Data, Typeable, Monoid)

-- |NOTE: RuleName's are case insensitive. The Eq and Ord instances obey this.
newtype RuleName
    = RuleName { ruleNameText :: Text }
      deriving (Read, Show, Data, Typeable)

instance Eq RuleName where
    (RuleName r1) == (RuleName r2) = (toCaseFold r1) == (toCaseFold r2)
    (RuleName r1) /= (RuleName r2) = (toCaseFold r1) /= (toCaseFold r2)

instance Ord RuleName where
    compare (RuleName r1) (RuleName r2) = compare (toCaseFold r1) (toCaseFold r2)

data Rule
    = Rule RuleName Elements
      deriving (Eq, Ord, Read, Show, Data, Typeable)

type Elements = Alternation

newtype Alternation
    = Alternation { concatenations :: [Concatenation] }
      deriving (Eq, Ord, Read, Show, Data, Typeable, Monoid)

newtype Concatenation
    = Concatenation { repetitions :: [Repetition] }
      deriving (Eq, Ord, Read, Show, Data, Typeable, Monoid)

data Element
    = RN RuleName
    | Group Alternation
    | Option Alternation
    | CV CharVal
    | NV NumVal
    | PV ProseVal
      deriving (Eq, Ord, Read, Show, Data, Typeable)

newtype ProseVal
    = ProseVal { proseText :: Text }
      deriving (Eq, Ord, Read, Show, Data, Typeable)

data Bit
    = Zero
    | One
      deriving (Eq, Ord, Read, Show, Data, Typeable)

newtype Digit
    = Digit { decDigit :: Char }
      deriving (Eq, Ord, Read, Show, Data, Typeable)

newtype HexDigit
    = HexDigit { hexDigit :: Char }
      deriving (Eq, Ord, Read, Show, Data, Typeable)

data NumVal
    = BinVal (NumSpec Bit)
    | DecVal (NumSpec Digit)
    | HexVal (NumSpec HexDigit)
      deriving (Eq, Ord, Read, Show, Data, Typeable)

data NumSpec n
    = NumSpec [n] (Maybe (Either [[n]] [n]))
      deriving (Eq, Ord, Read, Show, Data, Typeable)

newtype Comment
    = Comment { commentText :: Text }
      deriving (Eq, Ord, Read, Show, Data, Typeable)

data Repeat
    = Specific { reps    :: Integer
               }
    | Variable { minReps :: Maybe Integer
               , maxReps :: Maybe Integer
               }
    | CommaList { minReps :: Maybe Integer
                , maxReps :: Maybe Integer
                }

    deriving (Eq, Ord, Read, Show, Data, Typeable)

data Repetition = Repetition
    { repeat  :: Maybe Repeat
    , element :: Element
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

newtype CharVal
    = CharVal { charText :: Text }
      deriving (Eq, Ord, Read, Show, Data, Typeable)

type RuleMap = Map RuleName Elements

-- | convert a 'RuleList' into a 'Map'
ruleMap :: RuleList
        -> RuleMap
ruleMap (RuleList rules) = fromList $ map (\(Rule rn elem) -> (rn, elem)) rules


------------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------------


sampleRuleList1 :: RuleList
sampleRuleList1 =
    RuleList [ Rule (RuleName (fromString "rulelist"))
               ( Alternation
                 [ Concatenation
                   [ Repetition (Just (Variable (Just 1) Nothing))
                     ( Group
                       ( Alternation
                         [ Concatenation
                           [ Repetition Nothing (RN (RuleName $ fromString "rule"))
                           ]
                         , Concatenation
                           [ Repetition Nothing
                             ( Group
                               ( Alternation
                                 [ Concatenation
                                   [ Repetition (Just (Variable Nothing Nothing)) (RN (RuleName (fromString "c-wsp")))
                                   , Repetition Nothing (RN (RuleName (fromString "c-nl")))
                                   ]
                                 ]
                               )
                             )
                           ]
                         ]
                       )
                     )
                   ]
                 ]
               )
             ]



