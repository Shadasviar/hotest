module TestCases (
    sesTestCases,
    testCasesTestUser,
    testCasesAdmin,
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
testCasesTestUser :: [(String, Datagram, Datagram)]
testCasesTestUser = [
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
        ("CHANGE_CREDENTIALS back",
        datagram CHANGE_CREDENTIALS $ opSesData ["t.user", "test"],
        datagram ERROR_DATAGRAM $ pack [fromErrCode SUCCESS, fromCmd CHANGE_CREDENTIALS]
        ),
        ("GET_RESULTS",
        datagram GET_RESULTS $ empty,
        datagram GET_RESULTS $ BSC.pack "{\"all\":100,\"pass\":80}"
        ),
        ("ADD_GROUP",
        datagram ADD_GROUP $ BSC.pack "Group test",
        datagram ERROR_DATAGRAM $ pack [fromErrCode ACCESS_DENIED, fromCmd ADD_GROUP]
        ),
        ("ADD_USER",
        datagram ADD_USER $ opSesData ["User", "123"],
        datagram ERROR_DATAGRAM $ pack [fromErrCode ACCESS_DENIED, fromCmd ADD_USER]
        ),
        ("DELETE_USER",
        datagram DELETE_USER $ opSesData ["admin"],
        datagram ERROR_DATAGRAM $ pack [fromErrCode ACCESS_DENIED, fromCmd DELETE_USER]
        ),
        ("CLOSE_SESSION",
        datagram CLOSE_SESSION $ empty,
        datagram ERROR_DATAGRAM $ pack [fromErrCode SUCCESS, fromCmd CLOSE_SESSION]
        )
    ]

-- Test cases in format (Name, Datagram to be sent, expected answer datagram)
testCasesAdmin :: [(String, Datagram, Datagram)]
testCasesAdmin = [
        ("Add new group",
        datagram ADD_GROUP $ BSC.pack "New group",
        datagram ERROR_DATAGRAM $ pack [fromErrCode SUCCESS, fromCmd ADD_GROUP]
        ),
        ("Add already existing group",
        datagram ADD_GROUP $ BSC.pack "New group",
        datagram ERROR_DATAGRAM $ pack [fromErrCode ALREADY_EXISTS, fromCmd ADD_GROUP]
        ),
        ("Add default group",
        datagram ADD_GROUP $ BSC.pack "Admins",
        datagram ERROR_DATAGRAM $ pack [fromErrCode ALREADY_EXISTS, fromCmd ADD_GROUP]
        ),
        ("Add new user",
        datagram ADD_USER $ opSesData ["{\"login\":\"User\",\"password\":\"12345sd\",\"name\":\"Test new\",\"surname\":\"SurnameXXX\"}"],
        datagram ERROR_DATAGRAM $ pack [fromErrCode SUCCESS, fromCmd ADD_USER]
        ),
        ("Delete user",
        datagram DELETE_USER $ BSC.pack "User",
        datagram ERROR_DATAGRAM $ pack [fromErrCode SUCCESS, fromCmd DELETE_USER]
        ),
        ("Add existing user",
        datagram ADD_USER $ opSesData ["{\"login\":\"t.user\",\"password\":\"12345sd\",\"name\":\"Test new\",\"surname\":\"SurnameXXX\"}"],
        datagram ERROR_DATAGRAM $ pack [fromErrCode ALREADY_EXISTS, fromCmd ADD_USER]
        ),
        ("Add user bad format",
        datagram ADD_USER $ opSesData ["{\"log\":\"df\"}"],
        datagram ERROR_DATAGRAM $ pack [fromErrCode BAD_COMMAND, fromCmd ADD_USER]
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
