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
        datagram GET_TEST_LIST_SIZE $ singleton 1
        ),
        ("GET_TEST",
        datagram GET_TEST $ singleton 0,
        datagram GET_TEST $ BSC.pack "{\"text\":\"Test test\",\"variants\":[{\"0\":\"test 0\"},{\"1\":\"test 1\"},{\"2\":\"test 2\"}]}"
        ),
        ("GET_TEST unexisting",
        datagram GET_TEST $ singleton 100,
        datagram ERROR_DATAGRAM $ pack [fromErrCode DOES_NOT_EXISTS, fromCmd GET_TEST]
        ),
        ("ADD_TEST",
        datagram ADD_TEST $ BSC.pack "{\"text\":\"Test test\",\"answers\":[\"test 0\",\"test 1\",\"test 2\"], \"right_answer\":\"proud\"}",
        datagram ERROR_DATAGRAM $ pack [fromErrCode ACCESS_DENIED, fromCmd ADD_TEST]
        ),
        ("SEND_TEST_ANSWERS",
        datagram SEND_TEST_ANSWERS $ BSC.pack "{\"answers\":[{\"0\":\"1\"}]}",
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
        datagram GET_RESULTS $ BSC.pack "{\"all\":1,\"pass\":0}"
        ),
        ("ADD_GROUP",
        datagram ADD_GROUP $ BSC.pack "Group test",
        datagram ERROR_DATAGRAM $ pack [fromErrCode ACCESS_DENIED, fromCmd ADD_GROUP]
        ),
        ("DELETE_GROUP",
        datagram DELETE_GROUP $ BSC.pack "Group test",
        datagram ERROR_DATAGRAM $ pack [fromErrCode ACCESS_DENIED, fromCmd DELETE_GROUP]
        ),
        ("ADD_USER",
        datagram ADD_USER $ opSesData ["User", "123"],
        datagram ERROR_DATAGRAM $ pack [fromErrCode ACCESS_DENIED, fromCmd ADD_USER]
        ),
        ("DELETE_USER",
        datagram DELETE_USER $ BSC.pack "admin",
        datagram ERROR_DATAGRAM $ pack [fromErrCode ACCESS_DENIED, fromCmd DELETE_USER]
        ),
        ("GET_USER_INFO",
        datagram GET_USER_INFO $ BSC.pack "t.user",
        datagram GET_USER_INFO $ BSC.pack "{\"groups\":[],\"login\":\"t.user\",\"name\":\"Test\\n\",\"surname\":\"User\"}" 
        ),
        ("GET_USER_INFO of admin",
        datagram GET_USER_INFO $ BSC.pack "admin",
        datagram GET_USER_INFO $ BSC.pack "{\"groups\":[\"Admins\",\"Examinators\"],\"login\":\"admin\",\"name\":\"admin\",\"surname\":\"\"}"
        ),
        ("GET_USER_INFO of unexisting user",
        datagram GET_USER_INFO $ BSC.pack "babuin",
        datagram ERROR_DATAGRAM $ pack [fromErrCode DOES_NOT_EXISTS, fromCmd GET_USER_INFO]
        ),
        ("SET_USER_INFO",
        datagram SET_USER_INFO $ BSC.pack "{\"login\":\"t.user\",\"surname\":\"User changed\",\"name\":\"Test changed\"}",
        datagram ERROR_DATAGRAM $ pack [fromErrCode ACCESS_DENIED, fromCmd SET_USER_INFO]
        ),
        ("ADD_TO_GROUP",
        datagram ADD_TO_GROUP $ BSC.pack "{\"login\":\"User\",\"group\":\"Admins\"}",
        datagram ERROR_DATAGRAM $ pack [fromErrCode ACCESS_DENIED, fromCmd ADD_TO_GROUP]
        ),
        ("REMOVE_FROM_GROUP",
        datagram REMOVE_FROM_GROUP $ BSC.pack "{\"login\":\"User\",\"group\":\"Admins\"}",
        datagram ERROR_DATAGRAM $ pack [fromErrCode ACCESS_DENIED, fromCmd REMOVE_FROM_GROUP]
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
        ("Delete group",
        datagram DELETE_GROUP $ BSC.pack "New group",
        datagram ERROR_DATAGRAM $ pack [fromErrCode SUCCESS, fromCmd DELETE_GROUP]
        ),
        ("Delete persistant group",
        datagram DELETE_GROUP $ BSC.pack "Admins",
        datagram ERROR_DATAGRAM $ pack [fromErrCode ACCESS_DENIED, fromCmd DELETE_GROUP]
        ),
        ("GET_TEST_LIST_SIZE",
        datagram GET_TEST_LIST_SIZE $ empty,
        datagram GET_TEST_LIST_SIZE $ singleton 1
        ),
        ("Add test",
        datagram ADD_TEST $ BSC.pack "{\"text\":\"Test test\",\"answers\":[\"test 0\",\"test 1\",\"test 2\"], \"right_answer\":\"proud\"}",
        datagram ERROR_DATAGRAM $ pack [fromErrCode SUCCESS, fromCmd ADD_TEST]
        ),
        ("Send 2/2",
        datagram SEND_TEST_ANSWERS $ BSC.pack "{\"answers\":[{\"0\":\"2\"}, {\"1\":\"3\"}]}",
        datagram ERROR_DATAGRAM $ pack [fromErrCode SUCCESS, fromCmd SEND_TEST_ANSWERS]
        ),
        ("Pass 2/2",
        datagram GET_RESULTS $ empty,
        datagram GET_RESULTS $ BSC.pack "{\"all\":2,\"pass\":2}"
        ),
        ("Send 0/2",
        datagram SEND_TEST_ANSWERS $ BSC.pack "{\"answers\":[{\"0\":\"0\"}, {\"1\":\"2\"}]}",
        datagram ERROR_DATAGRAM $ pack [fromErrCode SUCCESS, fromCmd SEND_TEST_ANSWERS]
        ),
        ("Pass 0/2",
        datagram GET_RESULTS $ empty,
        datagram GET_RESULTS $ BSC.pack "{\"all\":2,\"pass\":0}"
        ),
        ("Check tests count after adding test",
        datagram GET_TEST_LIST_SIZE $ empty,
        datagram GET_TEST_LIST_SIZE $ singleton 2
        ),
        ("Remove test",
        datagram REMOVE_TEST $ singleton 1,
        datagram ERROR_DATAGRAM $ pack [fromErrCode SUCCESS, fromCmd REMOVE_TEST]
        ),
        ("Check tests count after remove",
        datagram GET_TEST_LIST_SIZE $ empty,
        datagram GET_TEST_LIST_SIZE $ singleton 1
        ),    
        ("Add new user",
        datagram ADD_USER $ opSesData ["{\"login\":\"User\",\"password\":\"12345sd\",\"name\":\"Test new\",\"surname\":\"SurnameXXX\"}"],
        datagram ERROR_DATAGRAM $ pack [fromErrCode SUCCESS, fromCmd ADD_USER]
        ),
        ("Get info of new user",
        datagram GET_USER_INFO $ BSC.pack "User",
        datagram GET_USER_INFO $ BSC.pack "{\"groups\":[],\"login\":\"User\",\"name\":\"Test new\",\"surname\":\"SurnameXXX\"}" 
        ),
        ("Set info of new user",
        datagram SET_USER_INFO $ BSC.pack "{\"login\":\"User\",\"name\":\"Changed user\",\"surname\":\"Changed Surname\"}",
        datagram ERROR_DATAGRAM $ pack [fromErrCode SUCCESS, fromCmd SET_USER_INFO]
        ),
        ("Verify changed info",
        datagram GET_USER_INFO $ BSC.pack "User",
        datagram GET_USER_INFO $ BSC.pack "{\"groups\":[],\"login\":\"User\",\"name\":\"Changed user\",\"surname\":\"Changed Surname\"}" 
        ),
        ("Add user to group",
        datagram ADD_TO_GROUP $ BSC.pack "{\"login\":\"User\",\"group\":\"Admins\"}",
        datagram ERROR_DATAGRAM $ pack [fromErrCode SUCCESS, fromCmd ADD_TO_GROUP]
        ),
        ("Verify changed info",
        datagram GET_USER_INFO $ BSC.pack "User",
        datagram GET_USER_INFO $ BSC.pack "{\"groups\":[\"Admins\"],\"login\":\"User\",\"name\":\"Changed user\",\"surname\":\"Changed Surname\"}" 
        ),
        ("Remove user from group",
        datagram REMOVE_FROM_GROUP $ BSC.pack "{\"login\":\"User\",\"group\":\"Admins\"}",
        datagram ERROR_DATAGRAM $ pack [fromErrCode SUCCESS, fromCmd REMOVE_FROM_GROUP]
        ),
        ("Verify changed info",
        datagram GET_USER_INFO $ BSC.pack "User",
        datagram GET_USER_INFO $ BSC.pack "{\"groups\":[],\"login\":\"User\",\"name\":\"Changed user\",\"surname\":\"Changed Surname\"}" 
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
