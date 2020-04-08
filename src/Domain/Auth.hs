module Domain.Auth where

import ClassyPrelude
import Domain.Validation
import Text.Regex.PCRE.Heavy
import Control.Monad.Except

data RegistrationError
  = RegistrationErrorEmailTaken
  deriving (Show, Eq)

data EmailValidationErr = EmailValidationerrInvalidEmail

data PasswordValidationErr = PasswordValidationErrLength Int
  | PasswordValidationErrMustContainUpperCase
  | PasswordValidationErrMustContainLowercase
  | PasswordValidationErrMustContainNumber

newtype Email = Email { emailRaw ::Text } deriving (Show, Eq)

rawEmail :: Email -> Text
rawEmail = emailRaw

-- Only export the function and not the Email data constructor
-- So any Email is always valid in the domain
mkEmail :: Text -> Either [Text] Email
mkEmail = validate Email
  [ regexMatches
    [re|^[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,64}$|]
    "Not a valid email"
  ]

newtype Password = Password { passwordRaw :: Text } deriving (Show, Eq)

rawPassword :: Password -> Text
rawPassword = passwordRaw

mkPassword :: Text -> Either [Text] Password
mkPassword = validate Password
  [ lengthBetween 5 50 "Should be between 5 and 50"
  , regexMatches [re|\d|] "Should contain a number"
  , regexMatches [re|[A-Z]|] "Should contain an uppercase letter"
  , regexMatches [re|[a-z]|] "Should contain lowercase letter"
  ]

data Auth = Auth
  { authEmail :: Email
  , authPassword :: Password
  } deriving (Show, Eq)


-- Unfortunately the file structure is not explained very well, so unsure if
-- some of these declarations are supposed to be here or in a different file
type VerificationCode = Text

-- so our repo needs to implement this function that takes an Auth "object"
-- and returns either a registration error or the verification code
class Monad m => AuthRepo m where
  addAuth :: Auth -> m (Either RegistrationError VerificationCode)
  setEmailAsVerified :: VerificationCode -> m (Either EmailVerificationError ())
  findUserByAuth :: Auth -> m (Maybe (UserId, Bool))
  findEmailFromUserId :: UserId -> m (Maybe Email)

class Monad m => EmailVerificationNotif m where
  notifyEmailVerification :: Email -> VerificationCode -> m ()

{- 
  ExceptT is a monad transformer that adds exceptions to other monads.
  Add the auth and then send the notfication email. We return (Either Registration ())
  because ultimately sending a notification returns no error, so we just return the unit
  inside the monad.
  TODO: rewrite this using the bind syntax
-}
register :: (AuthRepo m, EmailVerificationNotif m)
         => Auth -> m (Either RegistrationError ())
register auth = runExceptT $ do
  vCode <- ExceptT $ addAuth auth
  let email = authEmail auth
  lift $ notifyEmailVerification email vCode

-- Temporary code for testing, to be removed
instance AuthRepo IO where
  addAuth (Auth email pass) = do
    putStrLn $ "adding auth: " <> rawEmail email
    return $ Right "fake verification code"

instance EmailVerificationNotif IO where
  notifyEmailVerification email vcode =
    putStrLn $ "Notify " <> rawEmail email <> " - " <> vcode

-- End temporary code

data EmailVerificationError = EmailVerificationErrorInvalidCode
  deriving (Eq, Show)

verifyEmail :: AuthRepo m 
            => VerificationCode -> m (Either EmailVerificationError ())
verifyEmail = setEmailAsVerified

type UserId = Int

type SessionId = Text

data LoginError = LoginErrorInvalidAuth
  | LoginErrorEmailNotVerified
  deriving (Eq, Show)

class Monad m => SessionRepo m where
  newSession :: UserId -> m SessionId
  findUserBySessionId :: SessionId -> m (Maybe UserId)

resolveSessionId :: SessionRepo m => SessionId -> m (Maybe UserId)
resolveSessionId = findUserBySessionId

login :: (AuthRepo m, SessionRepo m)
      => Auth -> m (Either LoginError SessionId)
login auth = runExceptT $ do
  result <- lift $ findUserByAuth auth
  case result of
    Nothing -> throwError LoginErrorInvalidAuth
    Just (_, False) -> throwError LoginErrorEmailNotVerified
    Just (uId, _) -> lift $ newSession uId

getUser :: AuthRepo m => UserId -> m (Maybe Email)
getUser = findEmailFromUserId
