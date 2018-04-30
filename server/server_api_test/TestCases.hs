module TestCases (
    sesTestCases,
    testCases,
    runTest,
    testSession
) 
where

import Hotest.Datagram
import Hotest.API
import qualified Data.ByteString.Char8 as BSC 
import Data.ByteString hiding (map, putStrLn, zipWith, length, putStr)
import Prelude hiding (concat, head, last)
import Network.Simple.TCP
import Text.Printf


sesTestCases :: [(String, Datagram, Datagram)]
sesTestCases = [
        ("OPEN_SESSION admin admin",
        datagram OPEN_SESSION $ opSesData ["admin", "admin"],
        datagram ERROR_DATAGRAM $ pack [fromErrCode SUCCESS, fromCmd OPEN_SESSION]
        ),
        ("OPEN_SESSION as unexisting user",
        datagram OPEN_SESSION $ opSesData ["lol", "lol"],
        datagram ERROR_DATAGRAM $ pack [fromErrCode ACCESS_DENIED, fromCmd OPEN_SESSION]
        ),
        ("OPEN_SESSION as test user",
        datagram OPEN_SESSION $ opSesData ["t.user", "test"],
        datagram ERROR_DATAGRAM $ pack [fromErrCode SUCCESS, fromCmd OPEN_SESSION]
        )
    ]

-- Test cases in format (Name, Datagram to be sent, expected answer datagram)
testCases :: [(String, Datagram, Datagram)]
testCases = [
        ("GET_TEST_LIST_SIZE",
        datagram GET_TEST_LIST_SIZE $ empty,
        datagram GET_TEST_LIST_SIZE $ singleton 0
        ),
        ("GET_TEST",
        datagram GET_TEST $ singleton 1,
        datagram GET_TEST $ BSC.pack "{'text':'TEST', 'variants':['OPT1','OPT2']}"
        ),
        ("SEND_TEST_ANSWERS",
        datagram SEND_TEST_ANSWERS $ BSC.pack "{'answers':['1':'1', '2':'0']}",
        datagram ERROR_DATAGRAM $ pack [fromErrCode SUCCESS, fromCmd SEND_TEST_ANSWERS]
        ),
        ("CHANGE_CREDENTIALS",
        datagram CHANGE_CREDENTIALS $ opSesData ["lol", "lol"],
        datagram ERROR_DATAGRAM $ pack [fromErrCode SUCCESS, fromCmd CHANGE_CREDENTIALS]
        ),
        ("GET_RESULTS",
        datagram GET_RESULTS $ empty,
        datagram GET_RESULTS $ BSC.pack "{'pass':'80', 'all':'100'}"
        ),
        ("ADD_GROUP",
        datagram ADD_GROUP $ BSC.pack "Group test",
        datagram ERROR_DATAGRAM $ pack [fromErrCode SUCCESS, fromCmd ADD_GROUP]
        ),
        ("ADD_USER",
        datagram ADD_USER $ opSesData ["User", "123"],
        datagram ERROR_DATAGRAM $ pack [fromErrCode SUCCESS, fromCmd ADD_USER]
        ),
        ("CLOSE_SESSION",
        datagram CLOSE_SESSION $ empty,
        datagram ERROR_DATAGRAM $ pack [fromErrCode SUCCESS, fromCmd CLOSE_SESSION]
        )
    ]

runTest :: Socket -> (String, Datagram, Datagram) -> IO ()
runTest s (name, out, expected) = do
    printf "%-40s" $ name ++ " ... "
    writeDatagram s out
    res <- readDatagram s
    putStrLn $ testRes res $ res == expected
            
testSession :: (String, Datagram, Datagram) -> IO ()
testSession (name, out, expected) = do
    connect "127.0.0.1" "6666" $ \(sock, addr) -> do
        printf "%-40s" $ name ++ " ... "
        writeDatagram sock out
        res <- readDatagram sock
        putStrLn $ testRes res $ res == expected
        closeSock sock

opSesData :: [String] -> ByteString
opSesData x = concat $ zipWith completeMsg (map BSC.pack x) [20,30]

testRes :: Datagram -> Bool -> String
testRes _ True = "[  OK  ]"
testRes res _ = "[FAILED] " ++ show res
