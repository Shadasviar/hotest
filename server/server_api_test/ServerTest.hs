import Network.Simple.TCP
import System.Environment
import qualified Data.ByteString.Char8 as BSC 
import Data.ByteString hiding (map, putStrLn, zipWith, length)
import qualified Data.ByteString as BS (length)
import Hotest.Datagram
import Hotest.API
import Prelude hiding (concat, head, last)

main = do
    args <- getArgs
    let creds = if length args /= 2
        then ["admin","admin"]
        else args

    connect "127.0.0.1" "6666" $ \(sock, addr) -> do
        putStrLn $ "Try log in as " ++ show creds
        
        let dmsg = concat $ zipWith completeMsg (map BSC.pack creds) [20,30]        
        let dtg = datagram OPEN_SESSION dmsg
        writeDatagram sock dtg
        
        ret <- readDatagram sock
        let rmsg = msg ret
        
        putStrLn $ "Recieved responce: [" 
                    ++ show (cmd ret) ++ ", "
                    ++ show (BS.length rmsg) ++ ", "
                    ++ show (toErrCode $ head rmsg) ++ "/"
                    ++ show (toCmd $ last rmsg)
                    ++ "]"
