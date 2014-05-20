{-|

This module exports the 'matchingRNG' function that turns a RelaxNG pattern
into a QuickCheck generator for XML documents matching that pattern.

For example, given a 'book.rng' schema file:

@
<grammar xmlns="http://relaxng.org/ns/structure/1.0"
         datatypeLibrary="http://www.w3.org/2001/XMLSchema-datatypes">
  <start><element name="book" xmlns="http://relaxng.org/ns/structure/1.0">
    <oneOrMore><element name="author"><data type="string">
      <param name="pattern">[-a-z0-9._%]+@[-a-z0-9.]+\.[a-z]{3,18}\.(asia|eu|today)</param>
    </data></element></oneOrMore>
  </element></start>
</grammar>
@

We can generate a random `XmlTree` conforming to it:

>>> import Test.QuickCheck.RelaxNG (loadRNG, matchingRNG)
>>> import Test.QuickCheck (generate)
>>> print =<< generate . matchingRNG =<< loadRNG "book.rng"
<?xml version="1.0" encoding="UTF-8"?>
<book>
  <author>3qd9g02xlu@swbbnr.kmmlxdy.asia</author>
  <author>o09a-_6w@tnj.qom.eu</author>
  <author>z6ckxi8@jtm.cbozhxpinfbweossd.asia</author>
</book>

-}
module Test.QuickCheck.RelaxNG (loadRNG, matchingRNG) where
import Test.QuickCheck
import Text.XML.HXT.RelaxNG
import Text.XML.HXT.Arrow.XmlState
import Text.XML.HXT.DOM.TypeDefs
import Control.Arrow.ListArrow
import System.IO

loadRNG :: String -> IO Pattern
loadRNG file = do
    rngs <- runX $ validateSchemaWithRelax file
    case rngs of
        []    -> fail $ "Invalid RelaxNG file: " ++ file
        (r:_) -> case runLA createPatternFromXmlTree r of
            []    -> fail $ "Invalid RelaxNG pattern: " ++ file
            (p:_) -> return p

matchingRNG :: Pattern -> Gen XmlTree
matchingRNG p = undefined

genElements :: Pattern -> Gen XmlTrees
genElements p = undefined

genQName :: NameClass -> Gen QName
genQName nc = undefined

genString :: Datatype -> ParamList -> Gen String
genString (ns, ty) pl = undefined

main = print =<< generate . matchingRNG =<< loadRNG "book.rng"
