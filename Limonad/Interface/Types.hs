module Limonad.Interface.Types where

	import qualified Data.ByteString as B
	import qualified Data.Time as Time
	import qualified Codec.MIME.Type as MIME
	import qualified Limonad.Templates.Shortcuts as Tpl
	import Data.Functor
	import Limonad.Utils
	import Limonad.Types

	data Content = Content (IO B.ByteString)

	data View = View {
			getStatusCode :: HttpStatus,
			getMimeType :: MIME.Type,
			getDate :: Maybe Time.UTCTime,
			getExpire :: Maybe Time.UTCTime,
			getContent :: Content
		}

	data Request = Request {
			getMethod :: Maybe String,
			getParams :: [(String, String)],
			getRefer :: Maybe String,
			getHost :: Maybe String
		}

	data Route = Route {
			getPath :: String,
			getAllowedMethod :: [String]
		}

	type Routes = [Route]

	class Status a where
		code :: a -> Int 
		string :: a -> String 

	data HttpStatus = Continue
					| SwitchingProtocols
					| Ok
					| Created
					| Accepted
					| NonAuthoritativeInformation
					| NoContent
					| ResetContent
					| PartialContent
					| MultipleChoices
					| MovedPermanently
					| Found
					| SeeOther
					| NotModified
					| UseProxy
					| TemporaryRedirect
					| BadRequest
					| Unauthorized
					| PaymentRequired
					| Forbidden
					| NotFound
					| MethodNotAllowed
					| NotAcceptable
					| ProxyAuthenticationRequired
					| RequestTimeout
					| Conflict
					| Gone
					| LengthRequired
					| PreconditionFailed
					| RequestEntityTooLarge
					| RequestURITooLong
					| UnsupportedMediaType
					| RequestedRangeNotSatisfiable
					| ExpectationFailed
					| IMATeapot
					| InternalServerError
					| NotImplemented
					| BadGateway
					| ServiceUnavailable
					| GatewayTimeout
					| HTTPVersionNotSupported

	instance Status HttpStatus where
		code Continue = 100
		code SwitchingProtocols = 101
		code Ok = 200
		code Created = 201
		code Accepted = 202
		code NonAuthoritativeInformation = 203
		code NoContent = 204
		code ResetContent = 205
		code PartialContent = 206
		code MultipleChoices = 300
		code MovedPermanently = 301
		code Found = 302
		code SeeOther = 303
		code NotModified = 304
		code UseProxy = 305
		code TemporaryRedirect = 307
		code BadRequest = 400
		code Unauthorized = 401
		code PaymentRequired = 402
		code Forbidden = 403
		code NotFound = 404
		code MethodNotAllowed = 405
		code NotAcceptable = 406
		code ProxyAuthenticationRequired = 407
		code RequestTimeout = 408
		code Conflict = 409
		code Gone = 410
		code LengthRequired = 411
		code PreconditionFailed = 412
		code RequestEntityTooLarge = 413
		code RequestURITooLong = 414
		code UnsupportedMediaType = 415
		code RequestedRangeNotSatisfiable = 416
		code ExpectationFailed = 417
		code IMATeapot = 418
		code InternalServerError = 500
		code NotImplemented = 501
		code BadGateway = 502
		code ServiceUnavailable = 503
		code GatewayTimeout = 504
		code HTTPVersionNotSupported = 505
		string Continue = "Continue"
		string SwitchingProtocols = "Switching Protocols"
		string Ok = "OK"
		string Created = "Created"
		string Accepted = "Accepted"
		string NonAuthoritativeInformation = "Non-Authoritative Information"
		string NoContent = "No Content"
		string ResetContent = "Reset Content"
		string PartialContent = "Partial Content"
		string MultipleChoices = "Multiple Choices"
		string MovedPermanently = "Moved Permanently"
		string Found = "Found"
		string SeeOther = "See Other"
		string NotModified = "Not Modified"
		string UseProxy = "Use Proxy"
		string TemporaryRedirect = "Temporary Redirect"
		string BadRequest = "Bad Request"
		string Unauthorized = "Unauthorized"
		string PaymentRequired = "Payment Required"
		string Forbidden = "Forbidden"
		string NotFound = "Not Found"
		string MethodNotAllowed = "Method Not Allowed"
		string NotAcceptable = "Not Acceptable"
		string ProxyAuthenticationRequired = "Proxy Authentication Required"
		string RequestTimeout = "Request Timeout"
		string Conflict = "Conflict"
		string Gone = "Gone"
		string LengthRequired = "Length Required"
		string PreconditionFailed = "Precondition Failed"
		string RequestEntityTooLarge = "Request Entity Too Large"
		string ExpectationFailed = "Expectation Failed"
		string IMATeapot = "I'm a teapot"
		string InternalServerError = "Internal Server Error"
		string NotImplemented = "Not Implemented"
		string BadGateway = "Bad Gateway"
		string ServiceUnavailable = "Service Unavailable"
		string GatewayTimeout = "Gateway Timeout"
		string HTTPVersionNotSupported = "HTTP Version Not Supported"

	instance Show HttpStatus where
		show a = show $ code a ++ string a

	text :: IO String -> View
	text c = View {
			getMimeType = MIME.nullType,
			getDate = Nothing,
			getExpire = Nothing,
			getContent = Content (s2bs <$> c)
		}

	html :: IO B.ByteString -> View
	html c = View {
			getMimeType = MIME.Type { mimeType = MIME.Application "xhtml+xml", mimeParams = [] },
			getDate = Nothing,
			getExpire = Nothing,
			getContent = Content $ c
		}

	tpl :: FilePath -> Env -> View
	tpl f e = View {
			getMimeType = MIME.Type { mimeType = MIME.Application "xhtml+xml", mimeParams = [] },
			getDate = Nothing,
			getExpire = Nothing,
			getContent = Content (s2bs <$> Tpl.renderFile f e)
		}

	css :: IO B.ByteString -> View
	css c = View {
			getMimeType = MIME.Type { mimeType = MIME.Text "css", mimeParams = [] },
			getDate = Nothing,
			getExpire = Nothing,
			getContent = Content $ c
		}

	js :: IO B.ByteString -> View
	js c = View {
			getMimeType = MIME.Type { mimeType = MIME.Text "javascript", mimeParams = [] },
			getDate = Nothing,
			getExpire = Nothing,
			getContent = Content $ c
		}

	mkEnv :: Request -> Env
	mkEnv r = Env [Static a b |(a, b) <- getParams r]

	defaultRoute :: String -> Route 
	defaultRoute p = Route { getPath = p }

	(/+) :: Routes -> String -> Routes
	(/+) r n = r /: (defaultRoute n)

	(/:) :: Routes -> Route -> Routes
	(/:) r n = n:r
