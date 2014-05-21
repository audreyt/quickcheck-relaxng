quickcheck-relaxng
==================

This module exports the `matchingRNG` function that turns a RelaxNG pattern
into a QuickCheck generator for XML documents matching that pattern.

For example, given a `book.rng` schema file:

    <grammar xmlns="http://relaxng.org/ns/structure/1.0"
             datatypeLibrary="http://www.w3.org/2001/XMLSchema-datatypes">
      <start><element name="book" xmlns="http://relaxng.org/ns/structure/1.0">
        <oneOrMore><element name="author"><data type="string">
          <param name="pattern">[-a-z0-9._%]+@[-a-z0-9.]+\.[a-z]{3,18}\.(asia|eu|today)</param>
        </data></element></oneOrMore>
      </element></start>
    </grammar>

We can generate a random `XmlTree` conforming to it:

    >>> import Test.QuickCheck.RelaxNG (loadRNG, matchingRNG, showXmlTree)
    >>> import Test.QuickCheck (generate)
    >>> putStr . showXmlTree =<< generate . matchingRNG =<< loadRNG "book.rng"
    <?xml version="1.0" encoding="UTF-8"?>
    <book>
      <author>3qd9g02xlu@swbbnr.kmmlxdy.asia</author>
      <author>o09a-_6w@tnj.qom.eu</author>
      <author>z6ckxi8@jtm.cbozhxpinfbweossd.asia</author>
    </book>

# CC0 1.0 Universal

To the extent possible under law, 唐鳳 has waived all copyright
and related or neighboring rights to quickcheck-relaxng.

This work is published from Taiwan.

http://creativecommons.org/publicdomain/zero/1.0
