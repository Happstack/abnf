{-# LANGUAGE QuasiQuotes #-}
{-

The ABNF specification itself, as found in RFC5234.

-}
module ABNF.RFC5234 where

import ABNF.Parser
import ABNF.Types
import Data.String.QQ (s)
import Data.Text (Text)
import Text.Parsec.Text ()

-- | The ABNF spec as a 'RuleList'
abnf_ruleList :: RuleList
abnf_ruleList = parseRuleList "ABNF Spec" abnf_raw

-- | The ABNF spec as a 'RuleMap'
abnf_ruleMap :: RuleMap
abnf_ruleMap = ruleMap abnf_ruleList

-- | The ABNF spec as 'Text'
abnf_raw :: Text
abnf_raw = [s|
rulelist       =  1*( rule / (*c-wsp c-nl) )

rule           =  rulename defined-as elements c-nl
                                ; continues if next line starts
                                ;  with white space

rulename       =  ALPHA *(ALPHA / DIGIT / "-")

defined-as     =  *c-wsp ("=" / "=/") *c-wsp
                       ; basic rules definition and
                       ;  incremental alternatives

elements       =  alternation *c-wsp

c-wsp          =  WSP / (c-nl WSP)

c-nl           =  comment / CRLF
                       ; comment or newline

comment        =  ";" *(WSP / VCHAR) CRLF

alternation    =  concatenation
                  *(*c-wsp "/" *c-wsp concatenation)

concatenation  =  repetition *(1*c-wsp repetition)

repetition     =  [repeat] element

repeat         =  1*DIGIT / (*DIGIT "*" *DIGIT)

element        =  rulename / group / option /
                  char-val / num-val / prose-val

group          =  "(" *c-wsp alternation *c-wsp ")"

option         =  "[" *c-wsp alternation *c-wsp "]"

char-val       =  DQUOTE *(%x20-21 / %x23-7E) DQUOTE
                       ; quoted string of SP and VCHAR
                       ;  without DQUOTE

num-val        =  "%" (bin-val / dec-val / hex-val)

bin-val        =  "b" 1*BIT
                  [ 1*("." 1*BIT) / ("-" 1*BIT) ]
                       ; series of concatenated bit values
                       ;  or single ONEOF range

dec-val        =  "d" 1*DIGIT
                  [ 1*("." 1*DIGIT) / ("-" 1*DIGIT) ]

hex-val        =  "x" 1*HEXDIG
                  [ 1*("." 1*HEXDIG) / ("-" 1*HEXDIG) ]

defined-as     =  *c-wsp ("=" / "=/") *c-wsp
                       ; basic rules definition and
                       ;  incremental alternatives

elements       =  alternation *c-wsp

c-wsp          =  WSP / (c-nl WSP)

c-nl           =  comment / CRLF
                       ; comment or newline

comment        =  ";" *(WSP / VCHAR) CRLF

alternation    =  concatenation
                  *(*c-wsp "/" *c-wsp concatenation)

concatenation  =  repetition *(1*c-wsp repetition)

repetition     =  [repeat] element

repeat         =  1*DIGIT / (*DIGIT "*" *DIGIT)

element        =  rulename / group / option /
                  char-val / num-val / prose-val

group          =  "(" *c-wsp alternation *c-wsp ")"

option         =  "[" *c-wsp alternation *c-wsp "]"

char-val       =  DQUOTE *(%x20-21 / %x23-7E) DQUOTE
                       ; quoted string of SP and VCHAR
                       ;  without DQUOTE

num-val        =  "%" (bin-val / dec-val / hex-val)

bin-val        =  "b" 1*BIT
                  [ 1*("." 1*BIT) / ("-" 1*BIT) ]
                       ; series of concatenated bit values
                       ;  or single ONEOF range

dec-val        =  "d" 1*DIGIT
                  [ 1*("." 1*DIGIT) / ("-" 1*DIGIT) ]

hex-val        =  "x" 1*HEXDIG
                  [ 1*("." 1*HEXDIG) / ("-" 1*HEXDIG) ]


prose-val      =  "<" *(%x20-3D / %x3F-7E) ">"
                         ; bracketed string of SP and VCHAR
                         ;  without angles
                         ; prose description, to be used as
                         ;  last resort

ALPHA          =  %x41-5A / %x61-7A   ; A-Z / a-z

BIT            =  "0" / "1"

CHAR           =  %x01-7F
                       ; any 7-bit US-ASCII character,
                       ;  excluding NUL

CR             =  %x0D
                       ; carriage return

CRLF           =  CR LF
                       ; Internet standard newline

CTL            =  %x00-1F / %x7F
                       ; controls

DIGIT          =  %x30-39
                       ; 0-9

DQUOTE         =  %x22
                       ; " (Double Quote)

HEXDIG         =  DIGIT / "A" / "B" / "C" / "D" / "E" / "F"

HTAB           =  %x09
                       ; horizontal tab

LF             =  %x0A
                       ; linefeed

LWSP           =  *(WSP / CRLF WSP)
                       ; Use of this linear-white-space rule
                       ;  permits lines containing only white
                       ;  space that are no longer legal in
                       ;  mail headers and have caused
                       ;  interoperability problems in other
                       ;  contexts.
                       ; Do not use when defining mail
                       ;  headers and use with caution in
                       ;  other contexts.

OCTET          =  %x00-FF
                       ; 8 bits of data

SP             =  %x20

VCHAR          =  %x21-7E
                       ; visible (printing) characters

WSP            =  SP / HTAB
                       ; white space


|]
