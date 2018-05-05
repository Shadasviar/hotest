#ifndef SESSION_H
#define SESSION_H

#include <string>
#include <map>
#include <functional>
#include <hotest_protocol.h>

#define SHIFT(member_f) (std::bind(&Session::member_f, this, std::placeholders::_1))

/**
 * @brief The Session class - describes authorized connection with client.
 */
class Session
{

using Operation = std::function<void(HotestProtocol::Datagram&&)>;

public:
    /**
     * @brief Session constructor
     * @param fd - file descriptor of connected with client socket
     */
    Session(int fd);

    /**
     * @brief run - try authorize client and start communication
     *        with it in infinite loop if succeed.
     * @return false if error occured, otherwise run infinetely. If session was closed by
     *         CLOSE_SESSION command, returns true.
     */
    bool run();

private:
    int _clientFd = -1;
    std::string _login;
    bool _connected = false;

    void closeSession(HotestProtocol::Datagram&&);
    void getTestListSize(HotestProtocol::Datagram&&);
    void getTest(HotestProtocol::Datagram&&dtg);
    void sendTestAnswers(HotestProtocol::Datagram&&dtg);
    void getResult(HotestProtocol::Datagram&&);
    void invalidCommand(HotestProtocol::Datagram&&);
    void openSession(HotestProtocol::Datagram&&dtg);
    void changeCredentials(HotestProtocol::Datagram&&dtg);
    void addGroup(HotestProtocol::Datagram&&dtg);
    void addUser(HotestProtocol::Datagram&&dtg);
    void deleteUser(HotestProtocol::Datagram&&dtg);
    void deleteGroup(HotestProtocol::Datagram&&dtg);
    void getUserInfo(HotestProtocol::Datagram&&dtg);
    void setUserInfo(HotestProtocol::Datagram&&dtg);
    void addToGroup(HotestProtocol::Datagram&&dtg);
    void removeFromGroup(HotestProtocol::Datagram&&dtg);

    std::map<HotestProtocol::Command, Operation> _operations {
        {HotestProtocol::INVALID_COMMAND,   SHIFT(invalidCommand)},
        {HotestProtocol::OPEN_SESSION,      SHIFT(openSession)},
        {HotestProtocol::CLOSE_SESSION,     SHIFT(closeSession)},
        {HotestProtocol::GET_TEST_LIST_SIZE,SHIFT(getTestListSize)},
        {HotestProtocol::GET_TEST,          SHIFT(getTest)},
        {HotestProtocol::SEND_TEST_ANSWERS, SHIFT(sendTestAnswers)},
        {HotestProtocol::GET_RESULTS,       SHIFT(getResult)},
        {HotestProtocol::CHANGE_CREDENTIALS,SHIFT(changeCredentials)},
        {HotestProtocol::ADD_GROUP,         SHIFT(addGroup)},
        {HotestProtocol::ADD_USER,          SHIFT(addUser)},
        {HotestProtocol::DELETE_USER,       SHIFT(deleteUser)},
        {HotestProtocol::DELETE_GROUP,      SHIFT(deleteGroup)},
        {HotestProtocol::GET_USER_INFO,     SHIFT(getUserInfo)},
        {HotestProtocol::SET_USER_INFO,     SHIFT(setUserInfo)},
        {HotestProtocol::ADD_TO_GROUP,      SHIFT(addToGroup)},
        {HotestProtocol::REMOVE_FROM_GROUP, SHIFT(removeFromGroup)},
    };

    void cliendDeadErrorExit();
    std::string hash(std::string str);
};

#endif // SESSION_H
