import Network.Simple.TCP
import System.Environment
import qualified Data.ByteString.Char8 as BSC 
import qualified Data.ByteString as BS
import Data.Word
import Data.Maybe

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
    deriving (Enum, Show)

data ErrorCode = SUCCESS | ACCESS_DENIED deriving (Enum, Show)

main = do
    args <- getArgs
    let creds = if length args /= 2
        then ["admin","admin"]
        else args

    connect "127.0.0.1" "6666" $ \(sock, addr) -> do
        putStrLn $ "Try log in as " ++ show creds
        let msg = datagram OPEN_SESSION $ extend $ map BSC.pack creds
        putStrLn $ show $ BS.unpack msg
        send sock msg
        Just ret <- recv sock 4
        putStrLn $ show $ BS.unpack ret
        let (cmd:size:err:cret:_) = BS.unpack ret
        let retDgram = [
                        show (toEnum $ fromIntegral cmd::Command),
                        show size,
                        show (toEnum $ fromIntegral err::ErrorCode),
                        show (toEnum $ fromIntegral cret::Command)
                       ]
        putStrLn $ "Recieved responce: " ++ show retDgram

datagram :: Command -> BS.ByteString -> BS.ByteString
datagram cmd msg = BS.pack (comByte : [dataSize]) `BS.append` msg where
    comByte = fromIntegral (fromEnum cmd) :: Word8
    dataSize = fromIntegral (BS.length msg) :: Word8
    
extend :: [BS.ByteString] -> BS.ByteString
extend (x:y:_) = (expand' x 20) `BS.append` (expand' y 30) where
    expand' x n = x `BS.append` (BS.pack $ replicate (n - (BS.length x)) (0::Word8))
