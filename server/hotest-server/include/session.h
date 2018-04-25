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

    void closeSession(HotestProtocol::Datagram&&);
    void getTestListSize(HotestProtocol::Datagram&&);
    void getTest(HotestProtocol::Datagram&&);
    void sendTestAnswers(HotestProtocol::Datagram&&);
    void getResult(HotestProtocol::Datagram&&);
    void invalidCommand(HotestProtocol::Datagram&&);
    void openSession(HotestProtocol::Datagram&&);

    std::map<HotestProtocol::Command, Operation> _operations {
        {HotestProtocol::INVALID_COMMAND, SHIFT(invalidCommand)},
    };
};

#endif // SESSION_H
