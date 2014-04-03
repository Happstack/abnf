{-# LANGUAGE QuasiQuotes #-}
module ABNF.CoreRules where

import ABNF.Parser
import ABNF.Types

------------------------------------------------------------------------------
-- RFC5234 B.1. Core Rules
------------------------------------------------------------------------------

-- NOTE: we could generate these all at once using [abnf| |] but it
-- might be useful to have individual Haskell bindings to each rule,
-- so we'll just do that.

alpha_rule :: Rule
alpha_rule =
    [abnfRule|ALPHA          = %x41-5A / %x61-7A   ; A-Z / a-z|]

bit_rule :: Rule
bit_rule =
    [abnfRule|BIT            =  "0" / "1"|]

char_rule :: Rule
char_rule =
    [abnfRule|CHAR           =  %x01-7F
                                ; any 7-bit US-ASCII character,
                                ;  excluding NUL|]

cr_rule :: Rule
cr_rule =
    [abnfRule|CR             =  %x0D
                                ; carriage return|]

crlf_rule :: Rule
crlf_rule =
    [abnfRule|CRLF           =  CR LF
                                ; Internet standard newline|]

ctl_rule :: Rule
ctl_rule =
    [abnfRule|CTL            =  %x00-1F / %x7F
                                ; controls |]

digit_rule :: Rule
digit_rule =
    [abnfRule|DIGIT          = %x30-39
                                ; 0 - 9|]

dquote_rule :: Rule
dquote_rule =
    [abnfRule|DQUOTE         =  %x22
                                ; " (Double Quote)|]

hexdig_rule :: Rule
hexdig_rule =
    [abnfRule|HEXDIG         =  DIGIT / "A" / "B" / "C" / "D" / "E" / "F"|]

htab_rule :: Rule
htab_rule =
    [abnfRule|HTAB           =  %x09
                                ; horizontal tab|]

lf_rule :: Rule
lf_rule =
    [abnfRule|LF             =  %x0A
                                ; linefeed|]

lwsp_rule :: Rule
lwsp_rule =
    [abnfRule|LWSP           =  *(WSP / CRLF WSP)
                                ; Use of this linear-white-space rule
                                ;  permits lines containing only white
                                ;  space that are no longer legal in
                                ;  mail headers and have caused
                                ;  interoperability problems in other
                                ;  contexts.
                                ; Do not use when defining mail
                                ;  headers and use with caution in
                                ;  other contexts.|]

octet_rule :: Rule
octet_rule =
    [abnfRule|OCTET          =  %x00-FF
                                ; 8 bits of data|]

sp_rule :: Rule
sp_rule =
    [abnfRule|SP             =  %x20|]

vchar_rule :: Rule
vchar_rule =
    [abnfRule|VCHAR          =  %x21-7E
                                ; visible (printing) characters|]

wsp_rule :: Rule
wsp_rule =
    [abnfRule|WSP            =  SP / HTAB
                                ; white space|]

core_ruleList :: RuleList
core_ruleList = RuleList
    [ alpha_rule
    , bit_rule
    , char_rule
    , cr_rule
    , crlf_rule
    , ctl_rule
    , digit_rule
    , dquote_rule
    , hexdig_rule
    , htab_rule
    , lf_rule
    , lwsp_rule
    , octet_rule
    , sp_rule
    , vchar_rule
    , wsp_rule
    ]
