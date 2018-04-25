#include "session.h"
#include <hotest_protocol.h>
#include <cinttypes>
#include <posix_server.h>
#include <cstring>
#include <cerrno>
#include <string>

using namespace HotestProtocol;

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

    while (true) {
        Datagram dtg = recvDatagram(_clientFd);
        _operations[dtg.cmd](std::move(dtg));
    }
}

void Session::closeSession(Datagram &&)
{

}

void Session::getTestListSize(Datagram &&)
{

}

void Session::getTest(Datagram &&)
{

}

void Session::sendTestAnswers(Datagram &&)
{

}

void Session::getResult(Datagram &&)
{

}

void Session::invalidCommand(Datagram &&)
{

}

void Session::openSession(Datagram &&)
{

}
