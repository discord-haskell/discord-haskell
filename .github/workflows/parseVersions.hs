-- Small script to use the Cabal library to parse the Tested-With stanza
-- from discord-haskell.cabal, and output to stdout in a JSON friendly list.
module Main where

import Prelude hiding (readFile)
import Data.ByteString (readFile)
import Data.List (intersperse)
import Data.Maybe (maybeToList)
import Distribution.Compiler (CompilerFlavor(GHC))
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import Distribution.Pretty (pretty)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription(packageDescription))
import Distribution.Types.PackageDescription (PackageDescription(testedWith))
import Distribution.Types.Version (versionNumbers)
import Distribution.Types.VersionRange.Internal (VersionRange(ThisVersion))
import Text.PrettyPrint (render, brackets, comma, doubleQuotes)

main = do
    bs <- readFile "discord-haskell.cabal"
    let mbVersions = testedWith . packageDescription <$> parseGenericPackageDescriptionMaybe bs
    -- e.g. mbVersions = Just [(GHC,ThisVersion (mkVersion [8,10,7])),(GHC,ThisVersion (mkVersion [9,2])),(GHC,ThisVersion (mkVersion [9,4,1]))]
    let versions = concat $ maybeToList mbVersions
    -- e.g. versions = [(GHC,ThisVersion (mkVersion [8,10,7])),(GHC,ThisVersion (mkVersion [9,2])),(GHC,ThisVersion (mkVersion [9,4,1]))]
    let ghcVersions =
            [ ghcVersion
            | (flavor, versionRange) <- versions
            -- Filter only GHC
            , GHC == flavor
            -- Filter only to exact matches: "== VERSION"
            , let (ThisVersion ghcVersion) = versionRange
            ]
    -- e.g. ghcVersions = [mkVersion [8,10,7],mkVersion [9,2],mkVersion [9,4,1]]
    let prettyVersions = brackets $ mconcat $ intersperse comma $ map (doubleQuotes . pretty) ghcVersions
    putStrLn $ render prettyVersions
