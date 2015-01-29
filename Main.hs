import Control.Arrow ((&&&))
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import System.Console.Haskeline

-- GHC
import Debugger (showTerm)
import DynFlags
import Exception hiding (catch)
import GHC
import GHC.Paths (libdir)
import GhcMonad
import Outputable

initGhc :: GhcMonad m => m ()
initGhc = do
    -- necessary, otherwise GHC complains about missing package state:
    -- no package state yet: call GHC.setSessionDynFlags
    _ <- getSessionDynFlags >>= setSessionDynFlags
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
-- 1) Orphan instance
-- 2) calling runGhcT with (Just libdir) smells fishy;
--    can I somehow get libdir from a Session?
instance (Functor m, ExceptionMonad m, MonadException m) => MonadException (GhcT m) where
    controlIO f = GhcT $ \_ -> controlIO $ \(RunIO run) ->
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
