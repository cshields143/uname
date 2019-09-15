module Hangul ( isHangul, decompose, genHangulName ) where

import Hex
import Data.List

sBase = hex2dec "AC00"
lBase = hex2dec "1100"
vBase = hex2dec "1161"
tBase = hex2dec "11A7"
sentinel = hex2dec "D7A4"

lCount = 19
vCount = 21
tCount = 28
nCount = vCount * tCount
sCount = lCount * nCount

getShortName :: Integer -> String
getShortName 4352 = "G"
getShortName 4353 = "GG"
getShortName 4354 = "N"
getShortName 4355 = "D"
getShortName 4356 = "DD"
getShortName 4357 = "R"
getShortName 4358 = "M"
getShortName 4359 = "B"
getShortName 4360 = "BB"
getShortName 4361 = "S"
getShortName 4362 = "SS"
getShortName 4363 = ""
getShortName 4364 = "J"
getShortName 4365 = "JJ"
getShortName 4366 = "C"
getShortName 4367 = "K"
getShortName 4368 = "T"
getShortName 4369 = "P"
getShortName 4370 = "H"
getShortName 4449 = "A"
getShortName 4450 = "AE"
getShortName 4451 = "YA"
getShortName 4452 = "YAE"
getShortName 4453 = "EO"
getShortName 4454 = "E"
getShortName 4455 = "YEO"
getShortName 4456 = "YE"
getShortName 4457 = "O"
getShortName 4458 = "WA"
getShortName 4459 = "WAE"
getShortName 4460 = "OE"
getShortName 4461 = "YO"
getShortName 4462 = "U"
getShortName 4463 = "WEO"
getShortName 4464 = "WE"
getShortName 4465 = "WI"
getShortName 4466 = "YU"
getShortName 4467 = "EU"
getShortName 4468 = "YI"
getShortName 4469 = "I"
getShortName 4520 = "G"
getShortName 4521 = "GG"
getShortName 4522 = "GS"
getShortName 4523 = "N"
getShortName 4524 = "NJ"
getShortName 4525 = "NH"
getShortName 4526 = "D"
getShortName 4527 = "L"
getShortName 4528 = "LG"
getShortName 4529 = "LM"
getShortName 4530 = "LB"
getShortName 4531 = "LS"
getShortName 4532 = "LT"
getShortName 4533 = "LP"
getShortName 4534 = "LH"
getShortName 4535 = "M"
getShortName 4536 = "B"
getShortName 4537 = "BS"
getShortName 4538 = "S"
getShortName 4539 = "SS"
getShortName 4540 = "NG"
getShortName 4541 = "J"
getShortName 4542 = "C"
getShortName 4543 = "K"
getShortName 4544 = "T"
getShortName 4545 = "P"
getShortName 4546 = "H"
getShortName _ = error "Not hangul"

isHangul :: Integer -> Bool
isHangul n = n >= sBase && n < sentinel

decompose :: Integer -> [Integer]
decompose s = if tIndex > 0 then [lPart, vPart, tPart] else [lPart, vPart]
  where sIndex = s - sBase
        lIndex = div sIndex nCount
        vIndex = div (mod sIndex nCount) tCount
        tIndex = mod sIndex tCount
        lPart  = lBase + lIndex
        vPart  = vBase + vIndex
        tPart  = tBase + tIndex

genHangulName :: Integer -> String
genHangulName n = "HANGUL SYLLABLE " ++ (concat (map getShortName (decompose n)))