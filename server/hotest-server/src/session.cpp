#include "session.h"
#include <hotest_protocol.h>
#include <cinttypes>
#include <posix_server.h>
#include <cstring>
#include <cerrno>
#include <string>

using namespace HotestProtocol;

#define CLIENT_DEAD_ERROR

Session::Session(int fd) :
    _clientFd(fd)
{

}

bool Session::run()
{
    Datagram cmd = recvDatagram(_clientFd);
    if (cmd.cmd != OPEN_SESSION) {
        slog(SLOG_INFO, "Client autentification failed\n");
        sendDatagram(_clientFd, ErrorDatagram(OPEN_SESSION, ACCESS_DENIED));
        return false;
    }
    if (cmd.dataSize < LOGIN_BYTE_SIZE + PASSWORD_BYTE_SIZE) {
        slog(SLOG_INFO, "Bad data size recieved from client\n");
        sendDatagram(_clientFd, ErrorDatagram(OPEN_SESSION, ACCESS_DENIED));
        return false;
    }
    char login[LOGIN_BYTE_SIZE] =  {0};
    strncpy(login, (char*)cmd.data.data(), LOGIN_BYTE_SIZE);
    if (!login) return false;
    _login = login;

    char password[PASSWORD_BYTE_SIZE] =  {0};
    strncpy(password, (char*)(cmd.data.data() + LOGIN_BYTE_SIZE), PASSWORD_BYTE_SIZE);
    if (!password) return false;
    std::string pas = password;

    if (pas != "admin" || _login != "admin") {
        sendDatagram(_clientFd, ErrorDatagram(OPEN_SESSION, ACCESS_DENIED));
        return false;
    }

    sendDatagram(_clientFd, ErrorDatagram(OPEN_SESSION, SUCCESS));
    _connected = true;

    while (_connected) {
        Datagram dtg = recvDatagram(_clientFd);
        _operations[dtg.cmd](std::move(dtg));
    }
}

void Session::closeSession(Datagram &&)
{
    bool ret = sendDatagram(_clientFd, ErrorDatagram(CLOSE_SESSION, SUCCESS));
    _connected = false;
}

void Session::getTestListSize(Datagram &&)
{
    bool ret = sendDatagram(_clientFd, Datagram(GET_TEST_LIST_SIZE, 1, {0}));
    if (!ret) cliendDeadErrorExit();
}

void Session::getTest(Datagram && dtg)
{
    Datagram response;
    response.cmd = GET_TEST;
    std::string resStr = "{'text':'TEST', 'variants':['OPT1','OPT2']}";
    response.data = std::vector<uint8_t>(resStr.begin(), resStr.end());
    response.dataSize = response.data.size();

    bool ret = sendDatagram(_clientFd, std::move(response));
    if (!ret) cliendDeadErrorExit();
}

void Session::sendTestAnswers(Datagram && dtg)
{
    bool ret = sendDatagram(_clientFd, ErrorDatagram(SEND_TEST_ANSWERS, SUCCESS));
    if (!ret) cliendDeadErrorExit();
}

void Session::getResult(Datagram &&)
{
    Datagram response;
    response.cmd = GET_RESULTS;
    std::string resStr = "{'pass':'80', 'all':'100'}";
    response.data = std::vector<uint8_t>(resStr.begin(), resStr.end());
    response.dataSize = response.data.size();

    bool ret = sendDatagram(_clientFd, std::move(response));
    if (!ret) cliendDeadErrorExit();
}

void Session::invalidCommand(Datagram &&)
{
    bool ret = false;
    ret = sendDatagram(_clientFd, ErrorDatagram(ERROR_DATAGRAM, BAD_COMMAND));
    if (!ret) cliendDeadErrorExit();
}

void Session::openSession(Datagram &&)
{
    bool ret = sendDatagram(_clientFd, ErrorDatagram(ERROR_DATAGRAM, BAD_COMMAND));
    if (!ret) cliendDeadErrorExit();
}

void Session::changeCredentials(Datagram &&)
{
    bool ret = sendDatagram(_clientFd, ErrorDatagram(CHANGE_CREDENTIALS, SUCCESS));
    if (!ret) cliendDeadErrorExit();
}

void Session::addGroup(Datagram &&)
{
    bool ret = sendDatagram(_clientFd, ErrorDatagram(ADD_GROUP, SUCCESS));
    if (!ret) cliendDeadErrorExit();
}

void Session::addUser(Datagram &&)
{
    bool ret = sendDatagram(_clientFd, ErrorDatagram(ADD_USER, SUCCESS));
    if (!ret) cliendDeadErrorExit();
}

void Session::cliendDeadErrorExit()
{
    slog(SLOG_ERROR, std::string("Client ["+
                                 _login +
                                 "] dead ... close connection\n").c_str());
    _connected = false;
}
