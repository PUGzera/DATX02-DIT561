
module Frontend.Util (
    cd,
    readExtension
) where

import Frontend.Base

import qualified Frontend.GHCInterface as GHC

import System.Environment

import System.Directory

import Data.List.Split

import Data.List

winToUnix :: String -> String
winToUnix s = intercalate "/" (splitOn "\\" s)

unixToWin :: String -> String
unixToWin s = intercalate "\\" (splitOn "/" s)

cd' :: String -> String -> String
cd' ".." s = reverse $ dropWhile (\c -> c /= '/') (reverse s)
cd' d s    = s ++ "/" ++ d


-- ToDo: Check if OS before calling winToUnix or unixToWin
cd :: String -> DaisonI ()
cd s = do
    st <- getState
    let ud = cd' s $ winToUnix $ currentDirectory st
    id <- GHC.liftIO $ doesDirectoryExist ud
    case id of
        True -> do
            modifyState (\st -> st { currentDirectory = ud } )
            GHC.liftIO $ print ud
            return ()
        False -> return ()

readExtension :: String -> Maybe GHC.Extension
readExtension "Cpp" = Just GHC.Cpp
readExtension "OverlappingInstances" = Just GHC.OverlappingInstances
readExtension "UndecidableInstances" = Just GHC.UndecidableInstances
readExtension "IncoherentInstances" = Just GHC.IncoherentInstances
readExtension "UndecidableSuperClasses" = Just GHC.UndecidableSuperClasses
readExtension "MonomorphismRestriction" = Just GHC.MonomorphismRestriction
readExtension "MonoPatBinds" = Just GHC.MonoPatBinds
readExtension "MonoLocalBinds" = Just GHC.MonoLocalBinds
readExtension "RelaxedPolyRec" = Just GHC.RelaxedPolyRec
readExtension "ExtendedDefaultRules" = Just GHC.ExtendedDefaultRules
readExtension "ForeignFunctionInterface" = Just GHC.ForeignFunctionInterface
readExtension "UnliftedFFITypes" = Just GHC.UnliftedFFITypes
readExtension "InterruptibleFFI" = Just GHC.InterruptibleFFI
readExtension "CApiFFI" = Just GHC.CApiFFI
readExtension "GHCForeignImportPrim" = Just GHC.GHCForeignImportPrim
readExtension "JavaScriptFFI" = Just GHC.JavaScriptFFI
readExtension "ParallelArrays" = Just GHC.ParallelArrays
readExtension "Arrows" = Just GHC.Arrows
readExtension "TemplateHaskell" = Just GHC.TemplateHaskell
readExtension "TemplateHaskellQuotes" = Just GHC.TemplateHaskellQuotes
readExtension "QuasiQuotes" = Just GHC.QuasiQuotes
readExtension "ImplicitParams" = Just GHC.ImplicitParams
readExtension "ImplicitPrelude" = Just GHC.ImplicitPrelude
readExtension "ScopedTypeVariables" = Just GHC.ScopedTypeVariables
readExtension "AllowAmbiguousTypes" = Just GHC.AllowAmbiguousTypes
readExtension "UnboxedTuples" = Just GHC.UnboxedTuples
readExtension "UnboxedSums" = Just GHC.UnboxedSums
readExtension "UnliftedNewtypes" = Just GHC.UnliftedNewtypes
readExtension "BangPatterns" = Just GHC.BangPatterns
readExtension "TypeFamilies" = Just GHC.TypeFamilies
readExtension "TypeFamilyDependencies" = Just GHC.TypeFamilyDependencies
readExtension "TypeInType" = Just GHC.TypeInType
readExtension "OverloadedStrings" = Just GHC.OverloadedStrings
readExtension "OverloadedLists" = Just GHC.OverloadedLists
readExtension "NumDecimals" = Just GHC.NumDecimals
readExtension "DisambiguateRecordFields" = Just GHC.DisambiguateRecordFields
readExtension "RecordWildCards" = Just GHC.RecordWildCards
readExtension "RecordPuns" = Just GHC.RecordPuns
readExtension "ViewPatterns" = Just GHC.ViewPatterns
readExtension "GADTs" = Just GHC.GADTs
readExtension "GADTSyntax" = Just GHC.GADTSyntax
readExtension "NPlusKPatterns" = Just GHC.NPlusKPatterns
readExtension "DoAndIfThenElse" = Just GHC.DoAndIfThenElse
readExtension "BlockArguments" = Just GHC.BlockArguments
readExtension "RebindableSyntax" = Just GHC.RebindableSyntax
readExtension "ConstraintKinds" = Just GHC.ConstraintKinds
readExtension "PolyKinds" = Just GHC.PolyKinds
readExtension "DataKinds" = Just GHC.DataKinds
readExtension "InstanceSigs" = Just GHC.InstanceSigs
readExtension "ApplicativeDo" = Just GHC.ApplicativeDo
readExtension "StandaloneDeriving" = Just GHC.StandaloneDeriving
readExtension "DeriveDataTypeable" = Just GHC.DeriveDataTypeable
readExtension "AutoDeriveTypeable" = Just GHC.AutoDeriveTypeable
readExtension "DeriveFunctor" = Just GHC.DeriveFunctor
readExtension "DeriveTraversable" = Just GHC.DeriveTraversable
readExtension "DeriveFoldable" = Just GHC.DeriveFoldable
readExtension "DeriveGeneric" = Just GHC.DeriveGeneric
readExtension "DefaultSignatures" = Just GHC.DefaultSignatures
readExtension "DeriveAnyClass" = Just GHC.DeriveAnyClass
readExtension "DeriveLift" = Just GHC.DeriveLift
readExtension "DerivingStrategies" = Just GHC.DerivingStrategies
readExtension "DerivingVia" = Just GHC.DerivingVia
readExtension "TypeSynonymInstances" = Just GHC.TypeSynonymInstances
readExtension "FlexibleContexts" = Just GHC.FlexibleContexts
readExtension "FlexibleInstances" = Just GHC.FlexibleInstances
readExtension "ConstrainedClassMethods" = Just GHC.ConstrainedClassMethods
readExtension "MultiParamTypeClasses" = Just GHC.MultiParamTypeClasses
readExtension "NullaryTypeClasses" = Just GHC.NullaryTypeClasses
readExtension "FunctionalDependencies" = Just GHC.FunctionalDependencies
readExtension "UnicodeSyntax" = Just GHC.UnicodeSyntax
readExtension "ExistentialQuantification" = Just GHC.ExistentialQuantification
readExtension "MagicHash" = Just GHC.MagicHash
readExtension "EmptyDataDecls" = Just GHC.EmptyDataDecls
readExtension "KindSignatures" = Just GHC.KindSignatures
readExtension "RoleAnnotations" = Just GHC.RoleAnnotations
readExtension "ParallelListComp" = Just GHC.ParallelListComp
readExtension "TransformListComp" = Just GHC.TransformListComp
readExtension "MonadComprehensions" = Just GHC.MonadComprehensions
readExtension "GeneralizedNewtypeDeriving" = Just GHC.GeneralizedNewtypeDeriving
readExtension "RecursiveDo" = Just GHC.RecursiveDo
readExtension "PostfixOperators" = Just GHC.PostfixOperators
readExtension "TupleSections" = Just GHC.TupleSections
readExtension "PatternGuards" = Just GHC.PatternGuards
readExtension "LiberalTypeSynonyms" = Just GHC.LiberalTypeSynonyms
readExtension "RankNTypes" = Just GHC.RankNTypes
readExtension "ImpredicativeTypes" = Just GHC.ImpredicativeTypes
readExtension "TypeOperators" = Just GHC.TypeOperators
readExtension "ExplicitNamespaces" = Just GHC.ExplicitNamespaces
readExtension "PackageImports" = Just GHC.PackageImports
readExtension "ExplicitForAll" = Just GHC.ExplicitForAll
readExtension "AlternativeLayoutRule" = Just GHC.AlternativeLayoutRule
readExtension "AlternativeLayoutRuleTransitional" = Just GHC.AlternativeLayoutRuleTransitional
readExtension "DatatypeContexts" = Just GHC.DatatypeContexts
readExtension "NondecreasingIndentation" = Just GHC.RelaxedLayout
readExtension "RelaxedLayout" = Just GHC.RelaxedLayout
readExtension "TraditionalRecordSyntax" = Just GHC.TraditionalRecordSyntax
readExtension "LambdaCase" = Just GHC.LambdaCase
readExtension "MultiWayIf" = Just GHC.MultiWayIf
readExtension "BinaryLiterals" = Just GHC.BinaryLiterals
readExtension "NegativeLiterals" = Just GHC.NegativeLiterals
readExtension "HexFloatLiterals" = Just GHC.HexFloatLiterals
readExtension "DuplicateRecordFields" = Just GHC.DuplicateRecordFields
readExtension "OverloadedLabels" = Just GHC.OverloadedLabels
readExtension "EmptyCase" = Just GHC.EmptyCase
readExtension "PatternSynonyms" = Just GHC.PatternSynonyms
readExtension "PartialTypeSignatures" = Just GHC.PartialTypeSignatures
readExtension "NamedWildCards" = Just GHC.NamedWildCards
readExtension "StaticPointers" = Just GHC.StaticPointers
readExtension "TypeApplications" = Just GHC.TypeApplications
readExtension "Strict" = Just GHC.Strict
readExtension "StrictData" = Just GHC.StrictData
readExtension "MonadFailDesugaring" = Just GHC.MonadFailDesugaring
readExtension "EmptyDataDeriving" = Just GHC.EmptyDataDeriving
readExtension "NumericUnderscores" = Just GHC.NumericUnderscores
readExtension "QuantifiedConstraints" = Just GHC.QuantifiedConstraints
readExtension "StarIsType" = Just GHC.StarIsType
readExtension "ImportQualifiedPost" = Just GHC.ImportQualifiedPost
readExtension "CUSKs" = Just GHC.CUSKs
readExtension "StandaloneKindSignatures" = Just GHC.StandaloneKindSignatures
readExtension _ = Nothing