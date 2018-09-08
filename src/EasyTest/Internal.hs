{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module EasyTest.Internal
  ( -- * Core
    crash
  , crashDiff
  , note
  , noteDiff
  , scope
  , -- * Internal
    Status(..)
  , Env(..)
  , Test(..)
  , actionAllowed
  , putResult
  , runWrap
  , combineStatus
  ) where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Data.List              (isPrefixOf)
#if !(MIN_VERSION_base(4,11,0))
import           Data.Semigroup
#endif
import           Data.String            (IsString (..))
import           Data.Text              (Text)
import qualified Data.Text              as T
import           GHC.Exts               (fromList, toList)
#if MIN_VERSION_base(4,9,0)
import           GHC.Stack
#else
import           Data.CallStack
#endif
import qualified System.Random          as Random

import           EasyTest.Diff

-- | Status of a test
data Status = Failed | Passed !Int | Skipped

combineStatus :: Status -> Status -> Status
combineStatus Skipped s             = s
combineStatus s Skipped             = s
combineStatus Failed _              = Failed
combineStatus _ Failed              = Failed
combineStatus (Passed n) (Passed m) = Passed (n + m)

instance Semigroup Status where
  (<>) = combineStatus

instance Monoid Status where
  mempty  = Passed 0
#if !MIN_VERSION_base(4,11,0)
  -- This is redudant starting with base-4.11 / GHC 8.4.
  mappend = combineStatus
#endif

data Env = Env
  { envRng      :: !(TVar Random.StdGen)
  , envScopes   :: ![Text]
  , envAllow    :: ![Text]
  , envResults  :: !(TBQueue (Maybe (TMVar ([Text], Status))))
  , envNote     :: !(Text -> IO ())
  , envNoteDiff :: !([Diff String] -> IO ())
  }

-- | Tests are values of type @Test a@, and 'Test' forms a monad with access to:
--
--     * repeatable randomness (the 'EasyTest.random' and 'EasyTest.random'' functions for random and bounded random values, or handy specialized 'EasyTest.int', 'EasyTest.int'', 'EasyTest.double', 'EasyTest.double'', etc)
--
--     * I/O (via 'liftIO' or 'EasyTest.io', which is an alias for 'liftIO')
--
--     * failure (via 'crash', which yields a stack trace, or 'fail', which does not)
--
--     * logging (via 'EasyTest.note', 'EasyTest.noteScoped', or 'EasyTest.note'')
--
--     * hierarchically-named subcomputations (under 'EasyTest.scope') which can be switched on and off via 'EasyTest.runOnly'
--
--     * parallelism (via 'EasyTest.fork')
--
--     * conjunction of tests via 'MonadPlus' (the '<|>' operation runs both tests, even if the first test fails, and the tests function used above is just 'msum').
--
-- Using any or all of these capabilities, you assemble 'Test' values into a "test suite" (just another 'Test' value) using ordinary Haskell code, not framework magic. Notice that to generate a list of random values, we just 'replicateM' and 'forM' as usual.
newtype Test a = Test (MaybeT (ReaderT Env IO) a)

#if !MIN_VERSION_base(4,9,0)
prettyCallStack :: CallStack -> String
prettyCallStack = show
#endif

-- | Record a failure at the current scope
crash :: HasCallStack => Text -> Test a
crash msg = do
  let trace = callStack
      trace' = fromList $ filter
        (\(_msg, loc) -> srcLocFile loc /= "src/EasyTest/Porcelain.hs")
        $ toList trace
      msg' = msg <> " " <> T.pack (prettyCallStack trace')
  putResult Failed
  noteScoped ("FAILURE " <> msg')
  empty

-- | Record a failure with a diff at the current scope
crashDiff :: HasCallStack => Text -> [Diff String] -> Test a
crashDiff msg chunks = do
  let trace = callStack
      trace' = fromList $ filter
        (\(_msg, loc) -> srcLocFile loc /= "src/EasyTest/Porcelain.hs")
        $ toList trace
      msg' = msg <> " " <> T.pack (prettyCallStack trace')
  putResult Failed
  noteScoped ("FAILURE " <> msg')
  noteDiff chunks
  empty

putResult :: (MonadReader Env m, MonadIO m) => Status -> m ()
putResult passed = do
  scopes <- asks envScopes
  allow  <- asks envAllow
  r <- liftIO . atomically $ newTMVar
    (scopes, if allow `isPrefixOf` scopes then passed else Skipped)
  q <- asks envResults
  liftIO . atomically $ writeTBQueue q (Just r)

-- | Label a test. Can be nested. A "." is placed between nested
-- scopes, so @scope "foo" . scope "bar"@ is equivalent to @scope "foo.bar"@
scope :: Text -> Test a -> Test a
scope msg t = do
  env <- ask
  let msg'      = T.splitOn "." msg
      scopes'   = envScopes env <> msg'
      env'      = env { envScopes = scopes' }
      passes    = actionAllowed env'

  if passes
    then local (const env') t
    else putResult Skipped >> empty

-- | Prepend the current scope to a logging message
noteScoped :: Text -> Test ()
noteScoped msg = do
  s <- currentScope
  note (T.intercalate "." s <> (if null s then "" else " ") <> msg)

-- | Log a diff
noteDiff :: [Diff String] -> Test ()
noteDiff chunks = do
  note_ <- asks envNoteDiff
  liftIO $ note_ chunks
  pure ()

-- | Log a message
note :: Text -> Test ()
note msg = do
  note_ <- asks envNote
  liftIO $ note_ msg
  pure ()

-- | The current scope
currentScope :: Test [Text]
currentScope = asks envScopes

-- | Catch all exceptions that could occur in the given `Test`
wrap :: Test a -> Test a
wrap (Test t) = Test $ MaybeT $ do
  env <- ask
  lift $ runWrap env t

runWrap :: Env -> MaybeT (ReaderT Env IO) a -> IO (Maybe a)
runWrap env t = do
  result <- try $ runReaderT (runMaybeT t) env
  case result of
    Left e -> do
      envNote env $
           T.intercalate "." (envScopes env)
        <> " EXCEPTION: "
        <> T.pack (show (e :: SomeException))
      runReaderT (putResult Failed) env
      pure Nothing
    Right a -> pure a

-- * @allow' `isPrefixOf` scopes'@: we're messaging within the allowed range
-- * @scopes' `isPrefixOf` allow'@: we're still building a prefix of the
--   allowed range but could go deeper
actionAllowed :: Env -> Bool
actionAllowed Env{envScopes = scopes, envAllow = allow}
  = allow `isPrefixOf` scopes || scopes `isPrefixOf` allow

instance MonadReader Env Test where
  ask = Test $ MaybeT $ do
    allowed <- asks actionAllowed
    if allowed
      then Just <$> ask
      else empty
  local f (Test t) = Test $ local f t
  reader f = Test $ reader f

instance Monad Test where
  fail = crash . T.pack
  return a = Test $ MaybeT $ do
    allowed <- asks actionAllowed
    pure $ if allowed
      then Just a
      else Nothing
  Test a >>= f = Test $ MaybeT $ do
    a' <- runMaybeT a
    case a' of
      Nothing  -> pure Nothing
      Just a'' -> let Test (MaybeT t) = f a'' in t

instance Functor Test where
  fmap = liftM

instance Applicative Test where
  pure = return
  (<*>) = ap

instance MonadIO Test where
  liftIO action = do
    allowed <- asks actionAllowed
    if allowed
      then wrap $ liftIO action
      else empty

instance Alternative Test where
  empty = Test $ MaybeT $ pure Nothing
  Test t1 <|> Test t2 = Test $ MaybeT $ do
    env <- ask
    (rng1, rng2) <- liftIO . atomically $ do
      currentRng <- readTVar (envRng env)
      let (rng1, rng2) = Random.split currentRng
      (,) <$> newTVar rng1 <*> newTVar rng2
    lift $ do
      _ <- runWrap (env { envRng = rng1 }) t1
      runWrap (env { envRng = rng2 }) t2

instance MonadPlus Test where
  mzero = empty
  mplus = (<|>)

instance IsString (Test a -> Test a) where
  fromString str = scope (T.pack str)
