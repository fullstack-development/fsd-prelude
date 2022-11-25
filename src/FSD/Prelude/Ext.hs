{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module FSD.Prelude.Ext
where

import Prelude hiding (undefined)
import qualified Prelude
import qualified Control.Concurrent
import qualified Control.Exception
import qualified Control.Exception.Safe
import qualified Control.Monad.Catch
import qualified Control.Monad.Reader
import qualified Control.Monad.State
import qualified Data.Aeson
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.Char
import qualified Data.Fixed
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.Encoding.Error
import qualified Data.Time
import qualified Data.Yaml.Pretty
import qualified GHC.Stack
import qualified Language.Haskell.TH
import qualified System.Console.ANSI
import qualified System.IO.Error
import qualified Text.Show.Pretty

assertM :: Applicative f => Bool -> f ()
assertM cond = Control.Exception.assert cond $ Prelude.pure ()

decodeJSON ::
    Data.Aeson.FromJSON a =>
    Data.ByteString.ByteString ->
    Either String a
decodeJSON =
    Data.Aeson.eitherDecodeStrict

encodeJSON ::
    Data.Aeson.ToJSON a =>
    a ->
    Data.ByteString.ByteString
encodeJSON x =
    Data.ByteString.Lazy.toStrict (Data.Aeson.encode x)

failText :: forall x m. (MonadFail m) => Data.Text.Text -> m x
failText msg =
    fail $ Data.Text.unpack msg

{-# DEPRECATED failUndefined "failUndefined" #-}
failUndefined :: forall x m. (GHC.Stack.HasCallStack, MonadFail m) => m x
failUndefined =
    fail $
        "undefined\n" <>
        GHC.Stack.prettyCallStack (GHC.Stack.callStack)

lowerInit :: Prelude.String -> Prelude.String
lowerInit (c : cs) | Data.Char.isUpper c = Data.Char.toLower c : lowerInit cs
lowerInit other = other

nominalDiffTimeThreadDelay ::
    Data.Time.NominalDiffTime ->
    IO ()
nominalDiffTimeThreadDelay ndt = do
    case Data.Time.nominalDiffTimeToSeconds ndt of
        Data.Fixed.MkFixed pico -> do
            delayLoop $ pico `Prelude.quot` 1000000
  where
    maxSingleDelay = 2000000000
    delayLoop msLeft
        | msLeft > maxSingleDelay = do
            Control.Concurrent.threadDelay (fromInteger maxSingleDelay)
            delayLoop (msLeft - maxSingleDelay)
        | otherwise = do
            Control.Concurrent.threadDelay (fromInteger msLeft)

{-| Run an action if an error is thrown in the main action, allowing it to
    observe the error thrown. This works similarly to
    `Control.Monad.Catch.onError`, but also allows to observe the exception
    object that was thrown (for example, to write in in a log).

    If the main action makes an early exit without an exception
    (such as by returning @`Control.Monad.Trans.MaybeT` (`pure` `Nothing`)@),
    we substitute @`System.IO.Error.userError` "aborted"@. -}
onErrorObserving ::
    (Control.Monad.Catch.MonadMask m) =>
    m a ->
    (Control.Monad.Catch.SomeException -> m b) ->
    m a
onErrorObserving act handler =
    fmap fst $ Control.Monad.Catch.generalBracket
        (pure ())
        (\() exitCase -> case exitCase of
            Control.Monad.Catch.ExitCaseSuccess _ ->
                pure ()
            Control.Monad.Catch.ExitCaseException ex ->
                handler ex >> pure ()
            Control.Monad.Catch.ExitCaseAbort -> do
                handler aborted >> pure ()
        )
        (\() -> act)
  where
    aborted = Control.Exception.toException $
        System.IO.Error.userError "aborted"

packText :: String -> Data.Text.Text
packText = Data.Text.pack

prettyText :: (Prelude.Show a) => a -> Data.Text.Text
prettyText = Data.Text.pack . Text.Show.Pretty.ppShow

showText :: (Show a) => a -> Data.Text.Text
showText = Data.Text.pack . show

thPutStrLn :: String -> Language.Haskell.TH.Q ()
thPutStrLn msg = do
    Language.Haskell.TH.runIO $ Prelude.putStrLn $
        System.Console.ANSI.setSGRCode
            [ System.Console.ANSI.SetColor
                System.Console.ANSI.Foreground
                System.Console.ANSI.Dull
                System.Console.ANSI.Cyan
            ] <>
        msg <>
        System.Console.ANSI.setSGRCode
            [ System.Console.ANSI.Reset
            ]

{-  This definition swaps the order of type variables, to allow constructs
    like `try @ExceptionType throwingAction`. -}
try ::
    (Control.Exception.Exception e, Control.Monad.Catch.MonadCatch m) =>
    m a ->
    m (Either e a)
try = Control.Exception.Safe.try

{-# DEPRECATED undefined "undefined" #-}
undefined :: forall x. (GHC.Stack.HasCallStack) => x
undefined = Prelude.undefined

withReaderT :: r -> Control.Monad.Reader.ReaderT r m a -> m a
withReaderT r act = Control.Monad.Reader.runReaderT act r

withStateT :: s -> Control.Monad.State.StateT s m a -> m (a, s)
withStateT s act = Control.Monad.State.runStateT act s

yamlText :: (Data.Aeson.ToJSON a) => a -> Data.Text.Text
yamlText =
    Data.Text.Encoding.decodeUtf8With Data.Text.Encoding.Error.lenientDecode .
    Data.Yaml.Pretty.encodePretty yamlConfig
  where
    yamlConfig =
        Data.Yaml.Pretty.setConfCompare compare $
        Data.Yaml.Pretty.setConfDropNull True $
        Data.Yaml.Pretty.defConfig
