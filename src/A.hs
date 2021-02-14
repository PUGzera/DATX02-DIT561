--A.hs
--invoke: ghci -package ghc A.hs

{-# LANGUAGE CPP #-}
import qualified GHCInterface as GHC
import Outputable

import Prelude
import Database.Daison

targetFile = "B.hs"

runExpr :: String -> IO ()
runExpr expr = do
   res <- example expr
#if __GLASGOW_HASKELL__ > 704
   str <- GHC.runGhc (Just GHC.libdir) $ do
      dflags <- GHC.getSessionDynFlags
      return $ showSDoc dflags $ ppr res
   putStrLn str
#else
   putStrLn $ showSDoc ( ppr res )
#endif

example expr =
#if __GLASGOW_HASKELL__ > 704
    GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $ do
#else
    GHC.defaultErrorHandler defaultLogAction $ do
#endif
      GHC.runGhc (Just GHC.libdir) $ do
        dflags <- GHC.getSessionDynFlags
        let dflags' = dflags -- foldl xopt_set dflags
                             -- [Opt_Cpp, Opt_ImplicitPrelude, Opt_MagicHash]
        GHC.setSessionDynFlags dflags'
        target <- GHC.guessTarget targetFile Nothing
        GHC.setTargets [target]
        GHC.load GHC.LoadAllTargets
        modSum <- GHC.getModSummary $ GHC.mkModuleName "B"
        p <- GHC.parseModule modSum
        t <- GHC.typecheckModule p
        d <- GHC.desugarModule t
        l <- GHC.loadModule d
        n <- GHC.getNamesInScope
        c <- return $ GHC.coreModule d

        g <- GHC.getModuleGraph


        --
        res <- run expr
        return res
        --
        --  mapM showModule g
        return $ (GHC.parsedSource d,"/n-----/n",  GHC.typecheckedSource d)





-- run :: GhcMonad m => ModSummary -> String -> m ()
run expr = do
#if __GLASGOW_HASKELL__ < 704
  setContext [ms_mod modSum] []
#else
#if __GLASGOW_HASKELL__ < 706
  GHC.setContext [GHC.IIModule $ ms_mod modSum]
#else
  GHC.setContext [ GHC.IIDecl $ GHC.simpleImportDecl (GHC.mkModuleName "Prelude") ] -- setContext [IIModule $ moduleName  $ ms_mod modSum]
  ctx <- GHC.getContext
  GHC.setContext ( (GHC.IIDecl $ GHC.simpleImportDecl (GHC.mkModuleName "Database.Daison")) : ctx )
  ctx <- GHC.getContext
  GHC.setContext ( (GHC.IIDecl $ GHC.simpleImportDecl (GHC.mkModuleName "B")) : ctx )
#endif
#endif
  rr<- GHC.execStmt expr GHC.execOptions
  case rr of
    GHC.ExecComplete {} -> return ()
{-- do
        let q=(qualName &&& qualModule) defaultUserStyle
        mapM_ (\n -> do
                mty <- lookupName n
                case mty of
                  Just (AnId aid) -> do
                      df <- getSessionDynFlags
                      t <- gtry $ obtainTermFromId maxBound True aid
                      evalDoc <- case t of
                          Right term -> showTerm term
                          Left  exn  -> return (text "*** Exception:" <+>
                                                  text (show (exn :: SomeException)))
                      liftIO $ putStrLn $ showSDocForUser df q evalDoc
                      return ()
                  _ -> return ()
                ) ns --}
    GHC.ExecBreak {} -> return () --liftIO $ print e
    --  _ -> return ()
