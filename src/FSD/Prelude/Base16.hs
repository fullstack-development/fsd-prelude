{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module FSD.Prelude.Base16
where

import Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson.Encoding
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS.Base16
import qualified Data.Proxy as Proxy
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified GHC.Generics
import qualified GHC.TypeLits

encodeBase16 :: BS.ByteString -> Text.Text
encodeBase16 = BS.Base16.encodeBase16

decodeBase16 :: Text.Text -> Either Text.Text BS.ByteString
decodeBase16 = BS.Base16.decodeBase16 . Text.Encoding.encodeUtf8

type Base16 = Base16Tagged "Base16"

newtype Base16Tagged (s :: GHC.TypeLits.Symbol)
    = Base16Tagged
        { base16Bytes :: BS.ByteString }
    deriving (GHC.Generics.Generic)
    deriving newtype
        (Eq, Ord, Semigroup, Monoid, BA.ByteArray, BA.ByteArrayAccess)

instance (GHC.TypeLits.KnownSymbol s) => Show (Base16Tagged s) where
    showsPrec d (Base16Tagged bs) =
        showParen (d > 0) $
            shows (BS.Base16.encodeBase16' bs) .
            showString (" :: " <> GHC.TypeLits.symbolVal (Proxy.Proxy @s))

instance String.IsString (Base16Tagged s) where
    fromString str =
        Base16Tagged (BS.Base16.decodeBase16Lenient (String.fromString str))

instance Aeson.ToJSON (Base16Tagged s) where
    toJSON (Base16Tagged bs) =
        Aeson.String (BS.Base16.encodeBase16 bs)
    toEncoding (Base16Tagged bs) =
        Aeson.Encoding.text (BS.Base16.encodeBase16 bs)

instance Aeson.FromJSON (Base16Tagged s) where
    parseJSON (Aeson.String text) =
        case BS.Base16.decodeBase16 (Text.Encoding.encodeUtf8 text) of
            Left err -> fail (Text.unpack err)
            Right bs -> pure (Base16Tagged bs)
    parseJSON _ = fail "Base16-encoded bytestring expected"
