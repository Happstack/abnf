{-# LANGUAGE TemplateHaskell #-}
module ABNF.Verbatim where

import Language.Haskell.TH.Quote

verbatim :: QuasiQuoter
verbatim = QuasiQuoter { quoteExp  = \s -> [| s |]
                       , quotePat  = error "verbatim.quotePat not implemented."
                       , quoteDec  = error "verbatim.quoteDec not implemented."
                       , quoteType = error "verbatim.quoteType not implemented."
                       }
