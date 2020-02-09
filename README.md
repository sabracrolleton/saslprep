# saslprep

This package provides a common lisp unicode normalization function using nfc, nfd, nfkc and nfkd as per Unicode Standard Annex #15 found at [http://www.unicode.org/reports/tr15/tr15-22.html](http://www.unicode.org/reports/tr15/tr15-22.html) as well as saslprep and stringprep functions required by  [https://tools.ietf.org/html/rfc5802](https://tools.ietf.org/html/rfc5802) and
found in:

  * [https://tools.ietf.org/html/rfc4013](https://tools.ietf.org/html/rfc4013) (2005) SASLprep
  * [https://tools.ietf.org/html/rfc3454](https://tools.ietf.org/html/rfc3454) (2002) Stringprep

This is a fork of a subset of work done by Takeru Ohta in 2010. Future work is intended to provide support for:

# Implementation Notes
This has been tested on sbcl, ccl, ecl against the unicode test file found at [http://www.unicode.org/Public/UNIDATA/NormalizationTest.txt](http://www.unicode.org/Public/UNIDATA/NormalizationTest.txt)

Testing currently shows about a 8% failure rate on ABCL and Allegro. Further work needs to be done to determine the cause.

# Usage
It has two major exported functions:

  * (normalize (str unicode-normalization-method))
  * (saslprep-normalize (str)

The currently supported normalization methods are :nfc :nfkc :nfd :nfkd

Normalization example with reference to relevant xkcd [https://www.xkcd.com/936/](https://www.xkcd.com/936/)

```common-lisp
    (normalize "正しい馬バッテリーステープル" :nfkc)
    "正しい馬バッテリーステープル"

    (normalize "الحصان الصحيح البطارية التيلة" :nfkc)
    "الحصان الصحيح البطارية التيلة"

    (normalize "اstáplacha ceart ceallraí capall" :nfkc)
    "اstáplacha ceart ceallraí capall"
```

Saslprep-normalize example
```common-lisp
    (saslprep-normalize "some-string-here")

```

Explanation of what saslprep-normalize adds to the process RFC 4013 and RFC 3454:

1. First map non-ascii space characters to space #x0020
2. Then delete characters that are commonly mapped to nothing
3. KC Normalization as described in RFC 3454 Table 4
4. Generate an error if there are any prohibited input characters

    * Non-ascii space characters (RFC 3454 Table c.1.2)
    * Ascii control characters (RFC 3454 Table c.2.1)
    * Non-ascii control characters (RFC 3454 Table c.2.2)
    * Private use characters (RFC 3454 Table c.3)
    * Non-character code points (RFC 3454 Table c.4)
    * Surrogate code points (RFC 3454 Table c.5)
    * Inappropriate for plain text characters (RFC 3454 Table c.6)
    * Inappropriate for canonical representation characters (RFC 3454 Table, C.7)
    * Change display properties or deprecated characters (RFC 3454 Table, C.8)
    * Tagging characters (RFC 3454 Table, C.9)

5. Check for bidirectional strings (RFC 3454 Table Section 6) and ensure that there are no conflicting directions.

# To Do list
  * Support for RFC 8264 [https://tools.ietf.org/html/rfc8264](https://tools.ietf.org/html/rfc8264) (2017)
  * Support for RFC 7564 [https://tools.ietf.org/html/rfc7564](https://tools.ietf.org/html/rfc7564) (2015)
  * Review test failures in ABCL and Allegro
  * All the things I do not understand yet
  * Optimization?

More relevant xkcd [https://xkcd.com/1726/](https://xkcd.com/1726/), [https://xkcd.com/1953/](https://xkcd.com/1953/), [https://www.xkcd.com/1209/](https://www.xkcd.com/1209/), [https://xkcd.com/1137/](https://xkcd.com/1137/)

# Data Files
  * UnicodeData.txt was downloaded from [http://www.unicode.org/Public/UNIDATA/UnicodeData.txt](http://www.unicode.org/Public/UNIDATA/UnicodeData.txt)
  * The file CompositionExclusions.txt was downloaded from [http://www.unicode.org/Public/UNIDATA/CompositionExclusions.txt](http://www.unicode.org/Public/UNIDATA/CompositionExclusions.txt)
  * The file DerivedNormalizationProps.txt was downloaded from [http://www.unicode.org/Public/UNIDATA/DerivedNormalizationProps.txt](http://www.unicode.org/Public/UNIDATA/DerivedNormalizationProps.txt)
  * The test file NormalizationTest.txt was downloaded from [http://www.unicode.org/Public/UNIDATA/NormalizationTest.txt](http://www.unicode.org/Public/UNIDATA/NormalizationTest.txt)

# Other References
  * [https://tools.ietf.org/html/rfc5802](https://tools.ietf.org/html/rfc5802)
  * [https://tools.ietf.org/html/rfc4013](https://tools.ietf.org/html/rfc4013)
  * [https://tools.ietf.org/html/rfc3454#section-3.1](https://tools.ietf.org/html/rfc3454#section-3.1)
  * [http://www.unicode.org/reports/tr15/#References](http://www.unicode.org/reports/tr15/#References)
  * [https://www.unicode.org/reports/tr41/tr41-24.html](https://www.unicode.org/reports/tr41/tr41-24.html)
  * [https://www.unicode.org/charts/normalization/](https://www.unicode.org/charts/normalization/)
  * [https://towardsdatascience.com/difference-between-nfd-nfc-nfkd-and-nfkc-explained-with-python-code-e2631f96ae6c](https://towardsdatascience.com/difference-between-nfd-nfc-nfkd-and-nfkc-explained-with-python-code-e2631f96ae6c)
  * [https://en.wikipedia.org/wiki/Unicode_equivalence](https://en.wikipedia.org/wiki/Unicode_equivalence)
  * [http://www.unicode.org/faq/normalization.html](http://www.unicode.org/faq/normalization.html)
  * [https://github.com/edicl/cl-unicode/blob/master/specials.lisp](https://github.com/edicl/cl-unicode/blob/master/specials.lisp)
  * [https://perldoc.perl.org/Unicode/Normalize.html](https://perldoc.perl.org/Unicode/Normalize.html)
  * [https://www.mkssoftware.com/docs/perl/lib/Unicode/Normalize.asp](https://www.mkssoftware.com/docs/perl/lib/Unicode/Normalize.asp)
  * [https://github.com/Wisdom/Awesome-Unicode](https://github.com/Wisdom/Awesome-Unicode)
  * [https://www.postgresql.org/docs/current/multibyte.html](https://www.postgresql.org/docs/current/multibyte.html)
