{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module FSD.Prelude.Base64
where

import Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson.Encoding
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as BS.Base64
import qualified Data.Proxy as Proxy
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified GHC.Generics
import qualified GHC.TypeLits

encodeBase64 :: BS.ByteString -> Text.Text
encodeBase64 = BS.Base64.encodeBase64Unpadded

decodeBase64 :: Text.Text -> Either Text.Text BS.ByteString
decodeBase64 = BS.Base64.decodeBase64Unpadded . Text.Encoding.encodeUtf8

type Base64 = Base64Tagged "Base64"

pattern Base64 :: BS.ByteString -> Base64
pattern Base64 x = Base64Tagged x

newtype Base64Tagged (s :: GHC.TypeLits.Symbol)
    = Base64Tagged
        { base64Bytes :: BS.ByteString }
    deriving (GHC.Generics.Generic)
    deriving newtype
        (Eq, Ord, Semigroup, Monoid, BA.ByteArray, BA.ByteArrayAccess)

instance (GHC.TypeLits.KnownSymbol s) => Show (Base64Tagged s) where
    showsPrec d (Base64Tagged bs) =
        showParen (d > 0) $
            shows (BS.Base64.encodeBase64Unpadded' bs) .
            showString (" :: " <> GHC.TypeLits.symbolVal (Proxy.Proxy @s))

instance String.IsString (Base64Tagged s) where
    fromString str =
        Base64Tagged (BS.Base64.decodeBase64Lenient (String.fromString str))

instance Aeson.ToJSON (Base64Tagged s) where
    toJSON (Base64Tagged bs) =
        Aeson.String (BS.Base64.encodeBase64Unpadded bs)
    toEncoding (Base64Tagged bs) =
        Aeson.Encoding.text (BS.Base64.encodeBase64Unpadded bs)

instance Aeson.FromJSON (Base64Tagged s) where
    parseJSON (Aeson.String text) =
        case BS.Base64.decodeBase64Unpadded (Text.Encoding.encodeUtf8 text) of
            Left err -> fail (Text.unpack err)
            Right bs -> pure (Base64Tagged bs)
    parseJSON _ = fail "Base64-encoded bytestring expected"
