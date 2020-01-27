# saslprep

WARNING - EARLY ALPHA QUALITY AND SHOULD NOT BE CONSIDERED USABLE AT THIS MOMENT

This is a fork of a subset of work done by Takeru Ohta in 2010 and is intended to be a common lisp implementation of the unicode normalization functions set forth in

  * https://tools.ietf.org/html/rfc3454 (2002) Stringprep
  * https://tools.ietf.org/html/rfc4013 (2005) SASLprep

with future work to provide support for:

  * https://tools.ietf.org/html/rfc7564 (2015)
  * https://tools.ietf.org/html/rfc8264 (2017)

It has only one exported function - (normalize (str method))

The currently supported methods are :nfc :nfkc :nfd :nfkd

Example with reference to relevant xkcd https://www.xkcd.com/936/

`(normalize "正しい馬バッテリーステープル" :nfkc)
"正しい馬バッテリーステープル"

(normalize "الحصان الصحيح البطارية التيلة" :nfkc)
"الحصان الصحيح البطارية التيلة"

(normalize "اstáplacha ceart ceallraí capall" :nfkc)
"اstáplacha ceart ceallraí capall"
`

# To Do list
## Characters that should be mapped to nothing

   00AD; SOFT HYPHEN
   1806; MONGOLIAN TODO SOFT HYPHEN
   200B; ZERO WIDTH SPACE
   2060; WORD JOINER
   FEFF; ZERO WIDTH NO-BREAK SPACE
   034F; COMBINING GRAPHEME JOINER
   180B; MONGOLIAN FREE VARIATION SELECTOR ONE
   180C; MONGOLIAN FREE VARIATION SELECTOR TWO
   180D; MONGOLIAN FREE VARIATION SELECTOR THREE
   200C; ZERO WIDTH NON-JOINER
   200D; ZERO WIDTH JOINER
   FE00; VARIATION SELECTOR-1
   FE01; VARIATION SELECTOR-2
   FE02; VARIATION SELECTOR-3
   FE03; VARIATION SELECTOR-4
   FE04; VARIATION SELECTOR-5
   FE05; VARIATION SELECTOR-6
   FE06; VARIATION SELECTOR-7
   FE07; VARIATION SELECTOR-8
   FE08; VARIATION SELECTOR-9
   FE09; VARIATION SELECTOR-10
   FE0A; VARIATION SELECTOR-11
   FE0B; VARIATION SELECTOR-12
   FE0C; VARIATION SELECTOR-13
   FE0D; VARIATION SELECTOR-14
   FE0E; VARIATION SELECTOR-15
   FE0F; VARIATION SELECTOR-16

## Printable ASCII checked first before going to the bigger hash tables

## Validate against RFC 3454 and 4013 and test against the official test file
And document that validation

## All the things I do not understand yet

# Data Files
UnicodeData.txt was downloaded from ftp://ftp.unicode.org/Public/UNIDATA/UnicodeData.txt
The test file NormalizationTest.txt was downloaded from http://www.unicode.org/Public/UNIDATA/NormalizationTest.txt
The file CompositionExclusions.txt was downloaded from http://www.unicode.org/Public/UNIDATA/CompositionExclusions.txt
The file DerivedNormalizationProps.txt was downloaded from http://www.unicode.org/Public/UNIDATA/DerivedNormalizationProps.txt
