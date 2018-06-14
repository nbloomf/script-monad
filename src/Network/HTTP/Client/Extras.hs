{- |
Module      : Network.HTTP.Client.Extras
Description : Some stuff not included in Network.HTTP.Client
Copyright   : 2018, Automattic, Inc.
License     : BSD3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX

HTTP helpers
-}

{-# LANGUAGE RecordWildCards #-}
module Network.HTTP.Client.Extras (
    Url
  , HttpResponse(..)
  , readHttpResponse
  , jsonResponseHeaders

  -- * Responses
  , _200ok
  , _400badRequest
  , _404notFound
  , _405methodNotAllowed
  , _408requestTimeout
  , _500internalServerError
) where



import qualified Data.ByteString as SB
  ( ByteString, unpack )
import Data.ByteString.Lazy
  ( ByteString, unpack )
import Data.Vector
  ( fromList )
import Network.HTTP.Client
  ( HttpException(..), CookieJar, HttpExceptionContent(StatusCodeException)
  , Response, responseCookieJar, responseBody, createCookieJar
  , responseHeaders, responseVersion, responseStatus )
import Network.HTTP.Types
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Text as T (Text, pack)


-- | To make type signatures nicer
type Url = String

-- | Non-opaque HTTP response type.
data HttpResponse = HttpResponse
  { _responseStatus :: Status
  , _responseVersion :: HttpVersion
  , _responseHeaders :: ResponseHeaders
  , _responseBody :: ByteString
  , _responseCookieJar :: CookieJar
  } deriving (Eq, Show)

-- | Convert an opaque `Response ByteString` into an `HttpResponse`.
readHttpResponse :: Response ByteString -> HttpResponse
readHttpResponse r = HttpResponse
  { _responseStatus = responseStatus r
  , _responseVersion = responseVersion r
  , _responseHeaders = responseHeaders r
  , _responseBody = responseBody r
  , _responseCookieJar = responseCookieJar r
  }



-- | Convert response headers to a JSON value; specifically a list of objects, one for each header.
jsonResponseHeaders :: ResponseHeaders -> Value
jsonResponseHeaders =
  Array . fromList . map (\(k,v) -> object [ (key k) .= (val v) ])
  where
    key = T.pack . concatMap esc . show
    val = T.pack . concatMap esc . show

    esc c = case c of
      '\\' -> "\\"
      '"'  -> "\\\""
      _    -> [c]



-- | Status 200; no headers
_200ok :: ByteString -> HttpResponse
_200ok body = HttpResponse
  { _responseStatus = status200
  , _responseVersion = http11
  , _responseHeaders = []
  , _responseBody = body
  , _responseCookieJar = createCookieJar []
  }

-- | Status 400; no headers
_400badRequest :: ByteString -> HttpResponse
_400badRequest body = HttpResponse
  { _responseStatus = status400
  , _responseVersion = http11
  , _responseHeaders = []
  , _responseBody = body
  , _responseCookieJar = createCookieJar []
  }

-- | Status 404; no headers
_404notFound :: ByteString -> HttpResponse
_404notFound body = HttpResponse
  { _responseStatus = status404
  , _responseVersion = http11
  , _responseHeaders = []
  , _responseBody = body
  , _responseCookieJar = createCookieJar []
  }

-- | Status 405; no headers
_405methodNotAllowed :: ByteString -> HttpResponse
_405methodNotAllowed body = HttpResponse
  { _responseStatus = status405
  , _responseVersion = http11
  , _responseHeaders = []
  , _responseBody = body
  , _responseCookieJar = createCookieJar []
  }

-- | Status 408; no headers
_408requestTimeout :: ByteString -> HttpResponse
_408requestTimeout body = HttpResponse
  { _responseStatus = status408
  , _responseVersion = http11
  , _responseHeaders = []
  , _responseBody = body
  , _responseCookieJar = createCookieJar []
  }

-- | Status 500; no headers
_500internalServerError :: ByteString -> HttpResponse
_500internalServerError body = HttpResponse
  { _responseStatus = status500
  , _responseVersion = http11
  , _responseHeaders = []
  , _responseBody = body
  , _responseCookieJar = createCookieJar []
  }
