
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
        True -> modifyState (\st -> st { currentDirectory = ud } )
        False -> return ()

readExtension :: String -> GHC.Extension
readExtension "Cpp" = GHC.Cpp
readExtension "OverlappingInstances" = GHC.OverlappingInstances
readExtension "UndecidableInstances" = GHC.UndecidableInstances
readExtension "IncoherentInstances" = GHC.IncoherentInstances
readExtension "UndecidableSuperClasses" = GHC.UndecidableSuperClasses
readExtension "MonomorphismRestriction" = GHC.MonomorphismRestriction
readExtension "MonoPatBinds" = GHC.MonoPatBinds
readExtension "MonoLocalBinds" = GHC.MonoLocalBinds
readExtension "RelaxedPolyRec" = GHC.RelaxedPolyRec
readExtension "ExtendedDefaultRules" = GHC.ExtendedDefaultRules
readExtension "ForeignFunctionInterface" = GHC.ForeignFunctionInterface
readExtension "UnliftedFFITypes" = GHC.UnliftedFFITypes
readExtension "InterruptibleFFI" = GHC.InterruptibleFFI
readExtension "CApiFFI" = GHC.CApiFFI
readExtension "GHCForeignImportPrim" = GHC.GHCForeignImportPrim
readExtension "JavaScriptFFI" = GHC.JavaScriptFFI
readExtension "ParallelArrays" = GHC.ParallelArrays
readExtension "Arrows" = GHC.Arrows
readExtension "TemplateHaskell" = GHC.TemplateHaskell
readExtension "TemplateHaskellQuotes" = GHC.TemplateHaskellQuotes
readExtension "QuasiQuotes" = GHC.QuasiQuotes
readExtension "ImplicitParams" = GHC.ImplicitParams
readExtension "ImplicitPrelude" = GHC.ImplicitPrelude
readExtension "ScopedTypeVariables" = GHC.ScopedTypeVariables
readExtension "AllowAmbiguousTypes" = GHC.AllowAmbiguousTypes
readExtension "UnboxedTuples" = GHC.UnboxedTuples
readExtension "UnboxedSums" = GHC.UnboxedSums
readExtension "UnliftedNewtypes" = GHC.UnliftedNewtypes
readExtension "BangPatterns" = GHC.BangPatterns
readExtension "TypeFamilies" = GHC.TypeFamilies
readExtension "TypeFamilyDependencies" = GHC.TypeFamilyDependencies
readExtension "TypeInType" = GHC.TypeInType
readExtension "OverloadedStrings" = GHC.OverloadedStrings
readExtension "OverloadedLists" = GHC.OverloadedLists
readExtension "NumDecimals" = GHC.NumDecimals
readExtension "DisambiguateRecordFields" = GHC.DisambiguateRecordFields
readExtension "RecordWildCards" = GHC.RecordWildCards
readExtension "RecordPuns" = GHC.RecordPuns
readExtension "ViewPatterns" = GHC.ViewPatterns
readExtension "GADTs" = GHC.GADTs
readExtension "GADTSyntax" = GHC.GADTSyntax
readExtension "NPlusKPatterns" = GHC.NPlusKPatterns
readExtension "DoAndIfThenElse" = GHC.DoAndIfThenElse
readExtension "BlockArguments" = GHC.BlockArguments
readExtension "RebindableSyntax" = GHC.RebindableSyntax
readExtension "ConstraintKinds" = GHC.ConstraintKinds
readExtension "PolyKinds" = GHC.PolyKinds
readExtension "DataKinds" = GHC.DataKinds
readExtension "InstanceSigs" = GHC.InstanceSigs
readExtension "ApplicativeDo" = GHC.ApplicativeDo
readExtension "StandaloneDeriving" = GHC.StandaloneDeriving
readExtension "DeriveDataTypeable" = GHC.DeriveDataTypeable
readExtension "AutoDeriveTypeable" = GHC.AutoDeriveTypeable
readExtension "DeriveFunctor" = GHC.DeriveFunctor
readExtension "DeriveTraversable" = GHC.DeriveTraversable
readExtension "DeriveFoldable" = GHC.DeriveFoldable
readExtension "DeriveGeneric" = GHC.DeriveGeneric
readExtension "DefaultSignatures" = GHC.DefaultSignatures
readExtension "DeriveAnyClass" = GHC.DeriveAnyClass
readExtension "DeriveLift" = GHC.DeriveLift
readExtension "DerivingStrategies" = GHC.DerivingStrategies
readExtension "DerivingVia" = GHC.DerivingVia
readExtension "TypeSynonymInstances" = GHC.TypeSynonymInstances
readExtension "FlexibleContexts" = GHC.FlexibleContexts
readExtension "FlexibleInstances" = GHC.FlexibleInstances
readExtension "ConstrainedClassMethods" = GHC.ConstrainedClassMethods
readExtension "MultiParamTypeClasses" = GHC.MultiParamTypeClasses
readExtension "NullaryTypeClasses" = GHC.NullaryTypeClasses
readExtension "FunctionalDependencies" = GHC.FunctionalDependencies
readExtension "UnicodeSyntax" = GHC.UnicodeSyntax
readExtension "ExistentialQuantification" = GHC.ExistentialQuantification
readExtension "MagicHash" = GHC.MagicHash
readExtension "EmptyDataDecls" = GHC.EmptyDataDecls
readExtension "KindSignatures" = GHC.KindSignatures
readExtension "RoleAnnotations" = GHC.RoleAnnotations
readExtension "ParallelListComp" = GHC.ParallelListComp
readExtension "TransformListComp" = GHC.TransformListComp
readExtension "MonadComprehensions" = GHC.MonadComprehensions
readExtension "GeneralizedNewtypeDeriving" = GHC.GeneralizedNewtypeDeriving
readExtension "RecursiveDo" = GHC.RecursiveDo
readExtension "PostfixOperators" = GHC.PostfixOperators
readExtension "TupleSections" = GHC.TupleSections
readExtension "PatternGuards" = GHC.PatternGuards
readExtension "LiberalTypeSynonyms" = GHC.LiberalTypeSynonyms
readExtension "RankNTypes" = GHC.RankNTypes
readExtension "ImpredicativeTypes" = GHC.ImpredicativeTypes
readExtension "TypeOperators" = GHC.TypeOperators
readExtension "ExplicitNamespaces" = GHC.ExplicitNamespaces
readExtension "PackageImports" = GHC.PackageImports
readExtension "ExplicitForAll" = GHC.ExplicitForAll
readExtension "AlternativeLayoutRule" = GHC.AlternativeLayoutRule
readExtension "AlternativeLayoutRuleTransitional" = GHC.AlternativeLayoutRuleTransitional
readExtension "DatatypeContexts" = GHC.DatatypeContexts
readExtension "NondecreasingIndentation" = GHC.RelaxedLayout
readExtension "RelaxedLayout" = GHC.RelaxedLayout
readExtension "TraditionalRecordSyntax" = GHC.TraditionalRecordSyntax
readExtension "LambdaCase" = GHC.LambdaCase
readExtension "MultiWayIf" = GHC.MultiWayIf
readExtension "BinaryLiterals" = GHC.BinaryLiterals
readExtension "NegativeLiterals" = GHC.NegativeLiterals
readExtension "HexFloatLiterals" = GHC.HexFloatLiterals
readExtension "DuplicateRecordFields" = GHC.DuplicateRecordFields
readExtension "OverloadedLabels" = GHC.OverloadedLabels
readExtension "EmptyCase" = GHC.EmptyCase
readExtension "PatternSynonyms" = GHC.PatternSynonyms
readExtension "PartialTypeSignatures" = GHC.PartialTypeSignatures
readExtension "NamedWildCards" = GHC.NamedWildCards
readExtension "StaticPointers" = GHC.StaticPointers
readExtension "TypeApplications" = GHC.TypeApplications
readExtension "Strict" = GHC.Strict
readExtension "StrictData" = GHC.StrictData
readExtension "MonadFailDesugaring" = GHC.MonadFailDesugaring
readExtension "EmptyDataDeriving" = GHC.EmptyDataDeriving
readExtension "NumericUnderscores" = GHC.NumericUnderscores
readExtension "QuantifiedConstraints" = GHC.QuantifiedConstraints
readExtension "StarIsType" = GHC.StarIsType
readExtension "ImportQualifiedPost" = GHC.ImportQualifiedPost
readExtension "CUSKs" = GHC.CUSKs
readExtension "StandaloneKindSignatures" = GHC.StandaloneKindSignatures