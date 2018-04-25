#ifndef SESSION_H
#define SESSION_H

#include <string>
#include <map>
#include <functional>
#include <hotest_protocol.h>

#define SHIFT(member_f) (std::bind(&Session::member_f, this, std::placeholders::_1))

class Session
{

using Operation = std::function<void(HotestProtocol::Datagram&&)>;

public:
    Session(int fd);
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
    void openSession(HotestProtocol::Datagram&&);
    void changeCredentials(HotestProtocol::Datagram&&);
    void addGroup(HotestProtocol::Datagram&&);
    void addUser(HotestProtocol::Datagram&&);

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
    };

    void cliendDeadErrorExit();
};

#endif // SESSION_H
