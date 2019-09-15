module Noname ( isNoname, getNoname ) where

import Unitility

isPrivateUse :: Integer -> Bool
isPrivateUse n = (inUniRange n "E000" "F8FF") ||
                  (inUniRange n "F0000" "FFFFD") ||
                  (inUniRange n "100000" "10FFFD")

isHighSurr :: Integer -> Bool
isHighSurr n = inUniRange n "D800" "DBFF"
isLowSurr :: Integer -> Bool
isLowSurr n = inUniRange n "DC00" "DFFF"
isSurrogate :: Integer -> Bool
isSurrogate n = (isHighSurr n) || (isLowSurr n)

isNoncharacter :: Integer -> Bool
isNoncharacter n = (inUniRange n "FDD0" "FDEF") ||
                    (inUniRange n "FFFE" "FFFF") ||
                    (inUniRange n "1FFFE" "1FFFF") ||
                    (inUniRange n "2FFFE" "2FFFF") ||
                    (inUniRange n "3FFFE" "3FFFF") ||
                    (inUniRange n "4FFFE" "4FFFF") ||
                    (inUniRange n "5FFFE" "5FFFF") ||
                    (inUniRange n "6FFFE" "6FFFF") ||
                    (inUniRange n "7FFFE" "7FFFF") ||
                    (inUniRange n "8FFFE" "8FFFF") ||
                    (inUniRange n "9FFFE" "9FFFF") ||
                    (inUniRange n "AFFFE" "AFFFF") ||
                    (inUniRange n "BFFFE" "BFFFF") ||
                    (inUniRange n "CFFFE" "CFFFF") ||
                    (inUniRange n "DFFFE" "DFFFF") ||
                    (inUniRange n "EFFFE" "EFFFF") ||
                    (inUniRange n "FFFFE" "FFFFF") ||
                    (inUniRange n "10FFFE" "10FFFF")

isNoname :: Integer -> Bool
isNoname n = (isPrivateUse n) || (isSurrogate n) || (isNoncharacter n)

getNoname :: Integer -> String
getNoname n = if isPrivateUse n then "<private-use>"
              else if isSurrogate n then "<surrogate>"
              else if isNoncharacter n then "<noncharacter>"
              else error "Not a noname"
