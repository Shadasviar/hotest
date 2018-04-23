module TestCases (
    testCases,
    runTest
) 
where

import Hotest.Datagram
import Hotest.API
import qualified Data.ByteString.Char8 as BSC 
import Data.ByteString hiding (map, putStrLn, zipWith, length, putStr)
import Prelude hiding (concat, head, last)
import Network.Simple.TCP


testCases :: [(String, Datagram, Datagram)]
testCases = [
        ("OPEN_SESSION admin admin",
        datagram OPEN_SESSION $ opSesData ["admin", "admin"],
        datagram ERROR_DATAGRAM $ pack [fromErrCode SUCCESS, fromCmd OPEN_SESSION]
        ),
        ("OPEN_SESSION lol lol",
        datagram OPEN_SESSION $ opSesData ["lol", "lol"],
        datagram ERROR_DATAGRAM $ pack [fromErrCode ACCESS_DENIED, fromCmd OPEN_SESSION]
        )
    ]

runTest :: (String, Datagram, Datagram) -> IO ()
runTest (name, out, expected) = do
    connect "127.0.0.1" "6666" $ \(sock, addr) -> do
        putStr $ name ++ " ... "
        writeDatagram sock out
        res <- readDatagram sock
        putStrLn $ ret res $ res == expected
        where
            ret _ True  = "[  OK  ]"
            ret res False = "[FAILED] " ++ show res

opSesData :: [String] -> ByteString
opSesData x = concat $ zipWith completeMsg (map BSC.pack x) [20,30]
