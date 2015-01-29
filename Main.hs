module Main where

import Control.Applicative
import Control.Arrow ((&&&))
-- import Control.Exception
import Control.Monad
import Control.Monad.Catch (MonadMask(..))
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
-- import Control.Monad.Catch
-- import Control.Monad.IO.Class
-- import Data.Typeable
-- import Language.Haskell.Interpreter
import System.Console.Haskeline
-- import System.Environment

import Exception hiding (catch)
import GhcMonad
import HscTypes
import Module
import InteractiveEval hiding (obtainTermFromId)

import Debugger (showTerm)
-- import GHC (gcatch, getModSummary, lookupName, runGhc)
-- import GHC -- (gcatch, getModSummary, lookupName, runGhc)
import GHC -- (gcatch, getModSummary, lookupName, runGhc)
import GHC.Paths (libdir)
import DynFlags
import Outputable

{-
showError :: InterpreterError -> String
showError error = case error of
    WontCompile es -> "GhcError:\n" ++ unwords (map (("\t"++) . errMsg) es)
    other          -> show other

showResult :: Show a => Either InterpreterError a -> String
showResult r = case r of
    Left error -> showError error
    Right a    -> show a

evalExpr :: (Functor m, MonadMask m, MonadIO m) => String -> m String
evalExpr expr = fmap showResult . runInterpreter $ do
    setImports ["Prelude"]
    typeChecks stupidTest >>= liftIO . putStrLn . show
    typeOf stupidTest >>= liftIO . putStrLn
    eval expr
    -- interpret expr (as :: String)
    where stupidTest = "let x = 2"

main :: IO ()
main = concat <$> getArgs >>= evalExpr >>= putStrLn
-}

-- main :: IO ()
-- main = runInputT defaultSettings loop
--     where
--     loop :: InputT IO ()
--     loop = do
--         minput <- getInputLine "% "
--         case minput of
--             Nothing -> return ()
--             Just "quit" -> return ()
--             Just expr   -> do -- void $ runInterpreter $ do
--                 -- loadModules ["Prelude"]
--                 liftIO $ evalExpr expr >>= putStrLn . show
--                 loop


{-
main :: IO ()
main = do
    res <- example
    str <- runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        return $ showSDoc dflags $ ppr res
    putStrLn str

    where
    example = defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
        runGhc (Just libdir) $ do
            dflags <- getSessionDynFlags
            let dflags' = foldl xopt_set dflags [Opt_Cpp, Opt_ImplicitPrelude, Opt_MagicHash]
            setSessionDynFlags dflags'
            setContext [IIDecl $ simpleImportDecl (mkModuleName "Prelude")]
            run "let x = 2"
            run "x * x"
-}


run :: GhcMonad m => String -> m ()
run expr=do
    rr <- runStmt expr RunToCompletion
    case rr of
            RunOk ns->do
                    let q=(qualName &&& qualModule) defaultUserStyle
                    mapM_ (\n->do
                            mty<-lookupName n
                            case mty of
                                    Just (AnId aid)->do
                                            df <- getSessionDynFlags
                                            evalDoc <- gcatch (obtainTermFromId maxBound True aid >>= showTerm) catchError
                                            liftIO $ putStrLn $ showSDocForUser df q evalDoc
                                            return ()
                                    _ ->return ()
                            ) ns
            RunException e ->liftIO $ print e
            _->return ()

    where
    catchError exn = return (text "*** Exception:" <+> text (show (exn :: SomeException)))

-- instance MonadTrans GhcT where
--     lift = liftGhcT


initGhc :: GhcMonad m => m ()
initGhc = do
    -- necessary, otherwise GHC complains about missing package state:
    -- no package state yet: call GHC.setSessionDynFlags
    getSessionDynFlags >>= setSessionDynFlags
    -- dflags <- getSessionDynFlags
    -- the Opt_ImplicitPrelude, they do nothing!
    -- flip xopt_set Opt_ImplicitPrelude <$> getSessionDynFlags >>= setSessionDynFlags
    -- manually load Prelude
    setContext [IIDecl $ simpleImportDecl (mkModuleName "Prelude")]

runExpr :: GhcMonad m => String -> m ()
runExpr expr = runStmt expr RunToCompletion >>= \rr -> case rr of
    RunOk ns -> do
        let q = (qualName &&& qualModule) defaultUserStyle
        mapM_ (printIds q) ns
    RunException e -> liftIO $ print e
    _ -> return ()

    where
    printIds q n = lookupName n >>= \mty -> case mty of
        Just (AnId aid) -> do
            df <- getSessionDynFlags
            liftIO . putStrLn . showSDocForUser df q
                =<< (obtainTermFromId maxBound True aid >>= showTerm) `gcatch` catchError
        _ -> return ()

    catchError e = return $ text "*** Exception:" <+> text (show (e :: SomeException))

-- copied and modified from the ReaderT instance
-- 1) orphan
-- 2) calling runGhcT with (Just libdir) smells fishy;
--    can I somehow get libdir from a Session?
instance (Functor m, ExceptionMonad m, MonadException m)
    => MonadException (GhcT m) where
    controlIO f = GhcT $ \r -> controlIO $ \(RunIO run) ->
        let run' = RunIO (fmap (GhcT . const) . run . runGhcT (Just libdir))
        in fmap (runGhcT (Just libdir)) $ f run'

runDefaultGhcT :: (ExceptionMonad m, Functor m) => GhcT m a -> m a
runDefaultGhcT = defaultErrorHandler defaultFatalMessager defaultFlushOut
               . runGhcT (Just libdir)

hoistMaybeT :: Monad m => Maybe a -> MaybeT m a
hoistMaybeT = MaybeT . return

main :: IO ()
main = runDefaultGhcT . runInputT defaultSettings . void . runMaybeT $ do
    lift (lift $ initGhc) >> loop
    where
    loop :: MaybeT (InputT (GhcT IO)) ()
    loop = do
        input <- lift (getInputLine "% ") >>= hoistMaybeT
        liftIO $ putStrLn input
        if input == "quit"
            then return ()
            else lift (lift $ runExpr input) >> loop
