module Parser
  ( parseClue, parseTrees, parseClueUnfiltered
  ) where

import Control.Monad
import Data.Char
import Data.Functor
import Data.List
import Debug.Trace

import Indicators
import Lists
import Types
import Wordlists
import Constraints
import Memoize

parseFitsLength :: Length -> ParsedClue -> Bool
parseFitsLength l pc@(ParsedClue (c, def, zs, pt)) =
  minLength pt <= l && maxLength pt >= l

parseClue :: Clue -> [ParsedClue]
parseClue c@(Clue (phr, l))
  = (filter (parseFitsLength l) parses)
    where
        parses = parseClueUnfiltered c

parseClueUnfiltered :: Clue -> [ParsedClue]
parseClueUnfiltered c@(Clue (phr, l))
  = parseWithDefIndicator c ws l ++ parseWithoutDefIndicator c ws l
    where
        normalisePhrase = filter (not . isPunctuation) . map toLower
        ws              = words (normalisePhrase phr)

parseWithDefIndicator :: Clue -> Words -> Length -> [ParsedClue]
parseWithDefIndicator c ws l
  = do
      (ys, zs, ws) <- mirroredThreeSplitsOf ws
      let def = unwords ys
      guard (isInWordlist def && isDefIndicator zs)
      pt <- parseTrees ws
      return (ParsedClue (c, def, zs, pt))

parseWithoutDefIndicator :: Clue -> Words -> Length -> [ParsedClue]
parseWithoutDefIndicator c ws l
  = do
      (ys, zs) <- mirroredTwoSplitsOf ws
      let def = unwords ys
      guard (isInWordlist def)
      pt <- parseTrees zs
      return (ParsedClue (c, def, [], pt))



parseTrees = memoize parseTreesUnMemoized

parseTreesUnMemoized :: Words -> [ParseTree]
parseTreesUnMemoized ws@[w]
  = parseWithoutConcat ws

parseTreesUnMemoized ws
  = parseWithoutConcat ws ++ parseWithConcat ws

parseWithConcat :: Words -> [ParseTree]
parseWithConcat ws
  = do
      ps <- partitions ws
      guard (atLeastTwo ps)
      let ptss = sequence (map parseWithoutConcat ps)
      map ConcatC ptss

parseWithoutConcat :: Words -> [ParseTree]
parseWithoutConcat ws
  = parseSyn ws :
      -- parseJuxts wts ++
      parseAnags wps ++
      parseInserts wts ++
      parseSubs wts ++
      parseHiddens wps ++
      parseRevs wps ++
      parseFirsts wps ++
      parseLasts wps ++
      parseParts wps

  where
    wps = mirroredTwoSplitsOf ws
    wts = threeSplitsOf ws


parseJuxts :: TriplesOf Words -> [ParseTree]
parseJuxts wts
  = do
      (ys, zs, ws) <- wts
      let ysJuxtWs = isJuxtIndicator zs
          wsJuxtYs = isReverseJuxtIndicator zs

      guard (ysJuxtWs || wsJuxtYs)
      pt1 <- parseTrees ys
      pt2 <- parseTrees ws

      let pts
            | ysJuxtWs && wsJuxtYs  = [JuxtC zs pt1 pt2, JuxtC zs pt2 pt1]
            | ysJuxtWs              = [JuxtC zs pt1 pt2]
            | wsJuxtYs              = [JuxtC zs pt2 pt1]

      pts

parseSyn :: Words -> ParseTree
parseSyn
  = SynC . unwords

parseAnags :: PairsOf Words -> [ParseTree]
parseAnags wps
  = do
      (ys, zs) <- wps
      guard (isAnagIndicator ys)
      guard (zs /= [])
      return (AnagC ys zs)

parseInserts :: TriplesOf Words -> [ParseTree]
parseInserts wts
  = do
      (ys, zs, ws) <- wts
      let ysIntoWs = isInsertIndicator zs
          wsIntoYs = isReverseInsertIndicator zs

      guard (ysIntoWs || wsIntoYs)
      pt1 <- parseTrees ys
      pt2 <- parseTrees ws

      let pts
            | ysIntoWs && wsIntoYs  = [InsertC zs pt1 pt2, InsertC zs pt2 pt1]
            | ysIntoWs              = [InsertC zs pt1 pt2]
            | wsIntoYs              = [InsertC zs pt2 pt1]

      pts

parseSubs :: TriplesOf Words -> [ParseTree]
parseSubs wts
  = do
      (ys, zs, ws) <- wts
      let ysFromWs = isReverseSubIndicator zs
          wsFromYs = isSubIndicator zs

      guard (ysFromWs || wsFromYs)
      pt1 <- parseTrees ys
      pt2 <- parseTrees ws

      let pts
            | ysFromWs && wsFromYs  = [SubC zs pt1 pt2, SubC zs pt2 pt1]
            | ysFromWs              = [SubC zs pt1 pt2]
            | wsFromYs              = [SubC zs pt2 pt1]

      pts

parseHiddens :: PairsOf Words -> [ParseTree]
parseHiddens wps
  = do
      (ys, zs) <- wps
      guard (isHiddenIndicator ys)
      return (HiddenC ys zs)

parseRevs :: PairsOf Words -> [ParseTree]
parseRevs wps
  = do
      (ys, zs) <- wps
      guard (isReverseIndicator ys)
      pt <- parseTrees zs
      return (RevC zs pt)

parseFirsts :: PairsOf Words -> [ParseTree]
parseFirsts wps
  = do
      (ys, zs) <- wps
      guard (isFirstsIndicator ys)
      return (FirstsC ys zs)

parseLasts :: PairsOf Words -> [ParseTree]
parseLasts wps
  = do
      (ys, zs) <- wps
      guard (isLastsIndicator ys)
      return (LastsC ys zs)

parseParts :: PairsOf Words -> [ParseTree]
parseParts wps
  = do
      (ys, zs) <- wps
      guard (isPartIndicator ys)
      pt <- simplifyPartsClue <$> parseTrees zs
      return (PartC zs pt)

simplifyPartsClue :: ParseTree -> ParseTree
simplifyPartsClue (ConcatC pts)
  = maybe NullC ConcatC (foldr f (Just []) pts)
  where
    f pt mpts
      = do
          pts <- mpts
          case simplifyPartsClue pt of
            NullC  -> Nothing
            spt   -> Just (spt : pts)

simplifyPartsClue (SynC phr)
  = IdentC phr

simplifyPartsClue _
  = NullC
