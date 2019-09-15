module Ideograph ( isIdeograph, genIdeoName ) where

import Unitility

isIdeograph :: Integer -> Bool
isIdeograph x = (inUniRange x "3400"  "4DB5")  ||
                (inUniRange x "4E00"  "9FEA")  ||
                (inUniRange x "20000" "2A6D6") ||
                (inUniRange x "2A700" "2B734") ||
                (inUniRange x "2B740" "2B81D") ||
                (inUniRange x "2B820" "2CEA1") ||
                (inUniRange x "2CEB0" "2EBE0") ||
                (inUniRange x "17000" "187EC") ||
                (inUniRange x "1B170" "1B2FB") ||
                (inUniRange x "F900"  "FA6D")  ||
                (inUniRange x "FA70"  "FAD9")  ||
                (inUniRange x "2F800" "2FA1D")

scriptPrefix :: Integer -> String
scriptPrefix x = if (inUniRange x "3400"  "4DB5")  ||
                    (inUniRange x "4E00"  "9FEA")  ||
                    (inUniRange x "20000" "2A6D6") ||
                    (inUniRange x "2A700" "2B734") ||
                    (inUniRange x "2B740" "2B81D") ||
                    (inUniRange x "2B820" "2CEA1") ||
                    (inUniRange x "2CEB0" "2EBE0") then "CJK UNIFIED IDEOGRAPH-"
                else if inUniRange x "17000" "187EC" then "TANGUT IDEOGRAPH-"
                else if inUniRange x "1B170" "1B2FB" then "NUSHU CHARACTER-"
                else if (inUniRange x "F900"  "FA6D")  ||
                        (inUniRange x "FA70"  "FAD9")  ||
                        (inUniRange x "2F800" "2FA1D") then "CJK COMPATIBILITY IDEOGRAPH-"
                else error "Not script-prefixable"

genIdeoName :: Integer -> String
genIdeoName x = (scriptPrefix x) ++ (unihex x)