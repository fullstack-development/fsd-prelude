{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}

module FSD.Prelude
    ( Prelude.Applicative (..)
    , Prelude.Bool (..)
    , Prelude.Bounded (..)
    , Prelude.Char
    , Prelude.Either (..)
    , Prelude.Enum (..)
    , Prelude.Eq (..)
    , Prelude.FilePath
    , Prelude.Functor (..)
    , Prelude.IO
    , Prelude.Int
    , Prelude.Integer
    , Prelude.Integral (..)
    , Prelude.Maybe (..)
    , Prelude.Monad (..)
    , Prelude.MonadFail (..)
    , Prelude.Monoid (..)
    , Prelude.Num (..)
    , Prelude.Ord (..)
    , Prelude.Ordering (..)
    , Prelude.Semigroup (..)
    , Prelude.String
    , Prelude.Show (..)
    , (Prelude.$)
    , (Prelude.$!)
    , (Prelude..)
    , (Prelude.<$>)
    , (Prelude.&&)
    , (Prelude.^)
    , (Prelude.||)
    , Prelude.either
    , Prelude.error
    , Prelude.flip
    , Prelude.fromIntegral
    , Prelude.fst
    , Prelude.id
    , Prelude.maybe
    , Prelude.not
    , Prelude.otherwise
    , Prelude.seq
    , Prelude.snd
    , Prelude.subtract

    , Control.Applicative.Alternative (..)

    , Control.Exception.Exception (..)
    , Control.Exception.SomeException
    , Control.Exception.assert

    , Control.Exception.Safe.Handler (..)
    , Control.Exception.Safe.catch
    , Control.Exception.Safe.catches

    , Control.Monad.MonadPlus (..)
    , (Control.Monad.=<<)
    , Control.Monad.ap
    , Control.Monad.filterM
    , Control.Monad.forM
    , Control.Monad.forM_
    , Control.Monad.forever
    , Control.Monad.guard
    , Control.Monad.join
    , Control.Monad.liftM
    , Control.Monad.mapM_
    , Control.Monad.msum
    , Control.Monad.replicateM
    , Control.Monad.replicateM_
    , Control.Monad.unless
    , Control.Monad.when
    , Control.Monad.void

    , Control.Monad.Catch.ExitCase (..)
    , Control.Monad.Catch.MonadThrow (throwM)
    , Control.Monad.Catch.MonadCatch
    , Control.Monad.Catch.MonadMask (..)
    , Control.Monad.Catch.bracket
    , Control.Monad.Catch.bracket_
    , Control.Monad.Catch.bracketOnError
    , Control.Monad.Catch.finally
    , Control.Monad.Catch.mask_
    , Control.Monad.Catch.onError
    , Control.Monad.Catch.uninterruptibleMask_

    , Control.Monad.Except.ExceptT (..)
    , Control.Monad.Except.MonadError (..)
    , Control.Monad.Except.mapExceptT
    , Control.Monad.Except.runExceptT

    , Control.Monad.Fix.MonadFix (..)

    , Control.Monad.Reader.MonadReader (..)
    , Control.Monad.Reader.ReaderT (..)
    , Control.Monad.Reader.mapReaderT

    , Control.Monad.State.MonadState (..)
    , Control.Monad.State.StateT (..)
    , Control.Monad.State.evalStateT
    , Control.Monad.State.execStateT
    , Control.Monad.State.mapStateT

    , Control.Monad.IO.Class.MonadIO (..)

    , Control.Monad.Trans.Class.MonadTrans (..)

    , (Control.Lens.^.)
    , (Control.Lens..~)

    , Data.Aeson.FromJSON
    , Data.Aeson.ToJSON
    , Data.Aeson.defaultOptions

    , Data.Aeson.TH.deriveJSON
    , Data.Aeson.TH.deriveFromJSON
    , Data.Aeson.TH.deriveToJSON

    , Data.ByteString.ByteString

    , Data.Foldable.Foldable (fold, foldMap, foldl', foldr)

    , (Data.Function.&)
    , Data.Function.const

    , Data.Functor.Const.Const (..)

    , Data.Functor.Identity.Identity (..)

    , Data.Kind.Constraint
    , Data.Kind.Type

    , Data.List.NonEmpty.NonEmpty (..)

    , Data.Map.Map

    , Data.Proxy.Proxy (..)

    , Data.Sequence.Seq

    , Data.String.IsString (..)

    , Data.Text.Text

    , Data.Time.NominalDiffTime
    , Data.Time.UTCTime

    , Data.Traversable.Traversable (..)

    , Data.Vector.Vector

    , Data.Void.Void
    , Data.Void.absurd
    , Data.Void.vacuous

    , GHC.Generics.Generic

    , FSD.Prelude.Base16.Base16
    , FSD.Prelude.Base16.Base16Tagged (.., Base16)
    , FSD.Prelude.Base16.encodeBase16
    , FSD.Prelude.Base16.decodeBase16

    , FSD.Prelude.Base64.Base64
    , FSD.Prelude.Base64.Base64Tagged (.., Base64)
    , FSD.Prelude.Base64.encodeBase64
    , FSD.Prelude.Base64.decodeBase64

    , FSD.Prelude.Ext.assertM
    , FSD.Prelude.Ext.decodeJSON
    , FSD.Prelude.Ext.encodeJSON
    , FSD.Prelude.Ext.failText
    , FSD.Prelude.Ext.failUndefined
    , FSD.Prelude.Ext.lowerInit
    , FSD.Prelude.Ext.nominalDiffTimeThreadDelay
    , FSD.Prelude.Ext.onErrorObserving
    , FSD.Prelude.Ext.packText
    , FSD.Prelude.Ext.prettyText
    , FSD.Prelude.Ext.showText
    , FSD.Prelude.Ext.thPutStrLn
    , FSD.Prelude.Ext.try
    , FSD.Prelude.Ext.undefined
    , FSD.Prelude.Ext.withReaderT
    , FSD.Prelude.Ext.withStateT
    , FSD.Prelude.Ext.yamlText
    )
where

import qualified Prelude
import qualified Control.Applicative
import qualified Control.Exception
import qualified Control.Exception.Safe
import qualified Control.Monad
import qualified Control.Monad.Catch
import qualified Control.Monad.Except
import qualified Control.Monad.Fix
import qualified Control.Monad.Reader
import qualified Control.Monad.State
import qualified Control.Monad.IO.Class
import qualified Control.Monad.Trans.Class
import qualified Control.Lens
import qualified Data.Aeson
import qualified Data.Aeson.TH
import qualified Data.ByteString
import qualified Data.Foldable
import qualified Data.Function
import qualified Data.Functor.Const
import qualified Data.Functor.Identity
import qualified Data.Kind
import qualified Data.List.NonEmpty
import qualified Data.Map
import qualified Data.Proxy
import qualified Data.Sequence
import qualified Data.String
import qualified Data.Text
import qualified Data.Time
import qualified Data.Traversable
import qualified Data.Vector
import qualified Data.Void
import qualified GHC.Generics
import qualified FSD.Prelude.Base16
import qualified FSD.Prelude.Base64
import qualified FSD.Prelude.Ext
import qualified Data.Generics.Labels () {- instances -}
