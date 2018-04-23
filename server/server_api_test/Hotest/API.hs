module Hotest.API (
    Command (..),
    ErrorCode (..),
    toCmd,
    fromCmd,
    toErrCode,
    fromErrCode
)
where
    
import Data.Word

data Command = INVALID_COMMAND
    | GET_TEST_LIST_SIZE
    | GET_TEST
    | SEND_TEST_ANSWERS
    | OPEN_SESSION
    | CLOSE_SESSION
    | GET_RESULTS
    | CHANGE_CREDENTIALS
    | ADD_GROUP
    | ADD_USER
    | ERROR_DATAGRAM
    deriving (Enum, Show, Eq)

data ErrorCode = SUCCESS 
    | ACCESS_DENIED 
    deriving (Enum, Show, Eq)
    
toCmd :: Word8 -> Command
toCmd x = toEnum $ fromIntegral x :: Command

fromCmd :: Command -> Word8
fromCmd x = fromIntegral $ fromEnum x

toErrCode :: Word8 -> ErrorCode
toErrCode x = toEnum $ fromIntegral x :: ErrorCode

fromErrCode :: ErrorCode -> Word8
fromErrCode x = fromIntegral $ fromEnum x

main = do
    putStrLn $ "toCmd       :: [" ++ res ((toCmd 2) == GET_TEST) ++ "]"
    putStrLn $ "fromCmd     :: [" ++ res ((fromCmd OPEN_SESSION) == 4) ++ "]"
    putStrLn $ "toErrCode   :: [" ++ res ((toErrCode 1) == ACCESS_DENIED) ++ "]"
    putStrLn $ "fromErrCode :: [" ++ res ((fromErrCode SUCCESS) == 0) ++ "]"
    where
        res True = "OK"
        ret False = "FAILED"
