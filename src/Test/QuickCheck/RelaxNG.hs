{-|

This module exports a 'matchingRNG' function that turns a RelaxNG pattern
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
>>> putStr . showXmlTree =<< generate . matchingRNG =<< loadRNG "book.rng"
<?xml version="1.0" encoding="UTF-8"?>
<book>
  <author>3qd9g02xlu@swbbnr.kmmlxdy.asia</author>
  <author>o09a-_6w@tnj.qom.eu</author>
  <author>z6ckxi8@jtm.cbozhxpinfbweossd.asia</author>
</book>

-}
module Test.QuickCheck.RelaxNG (loadRNG, matchingRNG, showXmlTree) where
import Test.QuickCheck
import Test.QuickCheck.Regex
import Text.XML.HXT.RelaxNG
import Text.XML.HXT.Arrow.WriteDocument
import Text.XML.HXT.Arrow.XmlState
import Text.XML.HXT.DOM.TypeDefs
import Text.XML.HXT.DOM.XmlNode
import Control.Arrow ((>>>), arr)
import Control.Arrow.ListArrow
import Control.Monad (liftM2)
import System.IO
import Data.List ((\\), intersperse, partition)

loadRNG :: String -> IO Pattern
loadRNG file = do
    rngs <- runX $ validateSchemaWithRelax file
    case rngs of
        []    -> fail $ "Invalid RelaxNG file: " ++ file
        (r:_) -> case runLA createPatternFromXmlTree r of
            []    -> fail $ "Invalid RelaxNG pattern: " ++ file
            (p:_) -> return p

matchingRNG :: Pattern -> Gen XmlTree
matchingRNG p = do
    nodes <- genNodes p
    return . mkRoot [] $ case nodes of
        ((NTree (XTag qn attrs) body):rest)
            | uri@(_:_) <- namespaceUri qn ->
                ( NTree (XTag qn (mkAttr (mkName "xmlns") [mkText uri]:attrs)) body
                : rest )
        _ -> nodes

showXmlTree :: XmlTree -> String
showXmlTree tree = concat $ runLA (arr (const tree) >>> writeDocumentToString [withIndent True]) undefined

genNodes :: Pattern -> Gen XmlTrees
genNodes p = case p of
    Empty -> return []
    Text -> do
        text <- oneof (map return sampleStrings)
        return [mkText text]
    Element nc pat -> do
        qname <- genTagName nc
        nodes <- genNodes pat
        let (attrs, elems) = partition isAttr nodes
        return [mkElement qname attrs elems]
    Data typ params -> do
        text <- genText typ params
        return [mkText text]
    Attribute nc pat -> do
        qname <- genAttrName nc
        nodes <- genNodes pat
        return [mkAttr qname nodes]
    List p' -> do
        nodes <- genNodes p'
        return $ intersperse (mkText " ") nodes
    Interleave p1 p2 -> oneof [genNodes (Group p1 p2), genNodes (Group p2 p1)]
    Choice Empty p' -> do
        n <- choose (1, 3)
        if n == (1 :: Int) then return [] else genNodes p'
    Choice p1 p2 -> oneof [genNodes p1, genNodes p2]
    Group p1 p2 -> liftM2 (++) (genNodes p1) (genNodes p2)
    Value _ text _ -> return [mkText text]
    OneOrMore p' -> concat `fmap` listOf1 (genNodes p')
    _ -> fail $ "[genNodes] Not yet implemented: " ++ concat (runLA patternToStringTree p)

genTagName :: NameClass -> Gen QName
genTagName nc = case nc of
    AnyName         -> mkName `fmap` oneof (map return sampleTags)
    AnyNameExcept{} -> mkName `fmap` oneof (map return sampleTags) -- not yet implemented
    Name uri ln -> return $ mkNsName ln uri
    NameClassChoice l r -> oneof [genTagName l, genTagName r]
    _ -> fail $ "[genTagName] Not yet implemented: " ++ show nc

genAttrName :: NameClass -> Gen QName
genAttrName AnyName = mkName `fmap` oneof (map return sampleAttributes)
genAttrName AnyNameExcept{} = mkName `fmap` oneof (map return sampleAttributes) -- not yet implemented
genAttrName nc = genTagName nc

sampleTags = words "font-face animate animateColor animateMotion animateTransform feFuncA feFuncB feFuncG feFuncR glyph set feDistantLight feTurbulence svg feConvolveMatrix a altGlyph circle clipPath defs desc ellipse feBlend feColorMatrix feComponentTransfer feComposite feDiffuseLighting feDisplacementMap feFlood feGaussianBlur feImage feMerge feMorphology feOffset feSpecularLighting feTile filter font foreignObject g glyphRef image line linearGradient marker mask missing-glyph path pattern polygon polyline radialGradient rect stop switch symbol text textPath title tref tspan use cursor mpath script view hkern vkern altGlyphDef altGlyphItem color-profile feMergeNode fePointLight feSpotLight font-face-format font-face-name font-face-src font-face-uri metadata style"

sampleAttributes = words "accent-height accumulate additive alphabetic amplitude arabic-form ascent attributeName attributeType azimuth baseFrequency baseProfile bbox begin bias by calcMode cap-height class clipPathUnits contentScriptType contentStyleType cx cy d descent diffuseConstant divisor dur dx dy edgeMode elevation end exponent externalResourcesRequired fill filterRes filterUnits font-family font-size font-stretch font-style font-variant font-weight format from fx fy g1 g2 glyph-name glyphRef gradientTransform gradientUnits hanging height horiz-adv-x horiz-origin-x horiz-origin-y id ideographic in in2 intercept k k1 k2 k3 k4 kernelMatrix kernelUnitLength keyPoints keySplines keyTimes lang lengthAdjust limitingConeAngle local markerHeight markerUnits markerWidth maskContentUnits maskUnits mathematical max media method min mode name numOctaves offset onabort onactivate onbegin onclick onend onerror onfocusin onfocusout onload onmousedown onmousemove onmouseout onmouseover onmouseup onrepeat onresize onscroll onunload onzoom operator order orient orientation origin overline-position overline-thickness panose-1 path pathLength patternContentUnits patternTransform patternUnits points pointsAtX pointsAtY pointsAtZ preserveAlpha preserveAspectRatio primitiveUnits r radius refX refY rendering-intent repeatCount repeatDur requiredExtensions requiredFeatures restart result rotate rx ry scale seed slope spacing specularConstant specularExponent spreadMethod startOffset stdDeviation stemh stemv stitchTiles strikethrough-position strikethrough-thickness string style surfaceScale systemLanguage tableValues target targetX targetY textLength title to transform type u1 u2 underline-position underline-thickness unicode unicode-range units-per-em v-alphabetic v-hanging v-ideographic v-mathematical values version vert-adv-y vert-origin-x vert-origin-y viewBox viewTarget width widths x x-height x1 x2 xChannelSelector y y1 y2 yChannelSelector z zoomAndPan alignment-baseline baseline-shift clip-path clip-rule clip color-interpolation-filters color-interpolation color-profile color-rendering color cursor direction display dominant-baseline enable-background fill-opacity fill-rule filter flood-color flood-opacity font-size-adjust glyph-orientation-horizontal glyph-orientation-vertical image-rendering kerning letter-spacing lighting-color marker-end marker-mid marker-start mask opacity overflow pointer-events shape-rendering stop-color stop-opacity stroke-dasharray stroke-dashoffset stroke-linecap stroke-linejoin stroke-miterlimit stroke-opacity stroke-width stroke text-anchor text-decoration text-rendering unicode-bidi visibility word-spacing writing-mode"

sampleStrings = words "Bool Char String IO IOError Maybe Either Ordering Integer Int Ratio Float Double Complex Eq Show Read Ord Num Bounded Enum Real Fractional Integral RealFrac Floating RealFloat Monad Functor"

genText :: Datatype -> ParamList -> Gen String
genText (ns, ty) pl
    | ns /= "http://www.w3.org/2001/XMLSchema-datatypes" = fail $ "[genText] Unknown Namespace: " ++ ns
    | otherwise = case ty of
        "NMTOKENS"           -> list
        "IDREFS"             -> list
        "ENTITIES"           -> list
        "decimal"            -> show `fmap` int
        "number"             -> show `fmap` int
        "integer"            -> show `fmap` int
        "real"               -> show `fmap` int
        "positiveInteger"    -> show `fmap` between 1 intMax
        "nonNegativeInteger" -> show `fmap` between 0 intMax
        "negativeInteger"    -> show `fmap` between intMin (-1)
        "nonPositiveInteger" -> show `fmap` between intMin 0
        _ -> str
    where
    str | Just regex <- attr "pattern" = matching regex
        | Just len   <- attr "length"  = matching $ "[a-z]{" ++ len ++ "}"
        | otherwise = case (attr "minLength", attr "maxLength") of
            (Just x, Just y)
                | x > y      -> fail $ "[genText] minLen(" ++ show x ++ ") > maxLen(" ++ show y ++ ")"
                | otherwise  -> matching $ "[a-z]{" ++ x ++ "," ++ y ++ "}"
            (_     , Just y) -> matching $ "[a-z]{0," ++ y ++ "}"
            (Just x, _)      -> matching $ "[a-z]{" ++ x ++ ",}"
            _                -> oneString
    int = between intMin intMax
    between :: Int -> Int -> Gen Int
    between x y
        | x > y = fail $ "[genText] min(" ++ show x ++ ") > max(" ++ show y ++ ")"
        | otherwise = choose (x, y)
    intMin = maybe (maybe (-9999) succ (from "minExclusive")) id (from "minInclusive")
    intMax = maybe (maybe (10000) pred (from "maxExclusive")) id (from "maxInclusive")
    attr = (`lookup` pl)
    from = fmap read . attr
    list = concat `fmap` listOf1 oneString
    oneString = oneof $ map return sampleStrings

-- main = putStr . showXmlTree =<< generate . matchingRNG =<< loadRNG "book.rng"
