# saslprep

This package provides a common lisp unicode normalization function using nfc, nfd, nfkc and nfkd as per Unicode Standard Annex #15 found at http://www.unicode.org/reports/tr15/tr15-22.html as well as saslprep and stringprep functions required by  https://tools.ietf.org/html/rfc5802as and
found in:

  * https://tools.ietf.org/html/rfc4013 (2005) SASLprep
  * https://tools.ietf.org/html/rfc3454 (2002) Stringprep

This is a fork of a subset of work done by Takeru Ohta in 2010. Future work is intended to provide support for:

  * https://tools.ietf.org/html/rfc7564 (2015)
  * https://tools.ietf.org/html/rfc8264 (2017)

It has two major exported functions

  * (normalize (str method))
  * (salsprep-normalize (str)

The currently supported normalization methods are :nfc :nfkc :nfd :nfkd

Example with reference to relevant xkcd https://www.xkcd.com/936/

```common-lisp
    (normalize "正しい馬バッテリーステープル" :nfkc)
    "正しい馬バッテリーステープル"

    (normalize "الحصان الصحيح البطارية التيلة" :nfkc)
    "الحصان الصحيح البطارية التيلة"

    (normalize "اstáplacha ceart ceallraí capall" :nfkc)
    "اstáplacha ceart ceallraí capall"
```

# Saslprep Process
## Map non-ascii space characters to space #x0020
## Map commonly mapped to nothing
## KC Normalization as described in Stringprep 4
## check for prohibited input characters
### ascii control characters (stringprep c.2.1)
### non-ascii control characters (stringprep c.2.2)
### private use characters (stringprep c.3)

### non-character code points (stringprep c.4)
### surrogate code points (stringprep c.5)
### inappropriate for plain text characters (stringprep c.6)

### Inappropriate for canonical representation characters (StringPrep, C.7)

### Change display properties or deprecated characters (StringPrep, C.8)

### Tagging characters (StringPrep, C.9)

## Check for bidirectional strings (stringprep Section 6)

## Unassigned code points


# To Do list
## Case folding is not the same as normalization.
## All the things I do not understand yet

# Data Files
UnicodeData.txt was downloaded from http://www.unicode.org/Public/UNIDATA/UnicodeData.txt
The test file NormalizationTest.txt was downloaded from http://www.unicode.org/Public/UNIDATA/NormalizationTest.txt
The file CompositionExclusions.txt was downloaded from http://www.unicode.org/Public/UNIDATA/CompositionExclusions.txt
The file DerivedNormalizationProps.txt was downloaded from http://www.unicode.org/Public/UNIDATA/DerivedNormalizationProps.txt

# Other References
https://tools.ietf.org/html/rfc5802
https://tools.ietf.org/html/rfc4013
https://tools.ietf.org/html/rfc3454#section-3.1
http://www.unicode.org/reports/tr15/#References
https://www.unicode.org/reports/tr41/tr41-24.html
https://www.unicode.org/charts/normalization/
https://towardsdatascience.com/difference-between-nfd-nfc-nfkd-and-nfkc-explained-with-python-code-e2631f96ae6c
https://towardsdatascience.com/difference-between-nfd-nfc-nfkd-and-nfkc-explained-with-python-code-e2631f96ae6c
https://en.wikipedia.org/wiki/Unicode_equivalence
http://www.unicode.org/faq/normalization.html
https://github.com/edicl/cl-unicode/blob/master/specials.lisp
https://docs.python.org/3/library/unicodedata.html
https://perldoc.perl.org/Unicode/Normalize.html
https://www.mkssoftware.com/docs/perl/lib/Unicode/Normalize.asp
https://github.com/Wisdom/Awesome-Unicode
http://kunststube.net/encoding/
http://www.lispworks.com/documentation/HyperSpec/Front/index.htm
http://quickdocs.org/babel/api
https://www.postgresql.org/docs/current/multibyte.html
