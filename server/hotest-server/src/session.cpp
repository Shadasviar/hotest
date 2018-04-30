#include "session.h"
#include <hotest_protocol.h>
#include <cinttypes>
#include <posix_server.h>
#include <cstring>
#include <cerrno>
#include <string>
#include <database.h>
#include <openssl/sha.h>

using namespace HotestProtocol;

Session::Session(int fd) :
    _clientFd(fd)
{

}

bool Session::run()
{
    Datagram ses = recvDatagram(_clientFd);
    if (ses.cmd != OPEN_SESSION) {
        slog(SLOG_INFO, "No authorized user tried to connect");
        return false;
    }

    _operations[ses.cmd](std::move(ses));

    while (_connected) {
        Datagram dtg = recvDatagram(_clientFd);
        _operations[dtg.cmd](std::move(dtg));
    }

    return true;
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

void Session::openSession(Datagram &&dtg)
{
    using namespace FunctionalExtensions;

    char login[LOGIN_BYTE_SIZE];
    char passGot[PASSWORD_BYTE_SIZE];
    strncpy(login, (char*)dtg.data.data(), LOGIN_BYTE_SIZE);
    strncpy(passGot, (char*)dtg.data.data() + LOGIN_BYTE_SIZE, PASSWORD_BYTE_SIZE);

    SHA256_CTX sha256;
    SHA256_Init(&sha256);
    SHA256_Update(&sha256, passGot, PASSWORD_BYTE_SIZE);
    unsigned char hash[SHA256_DIGEST_LENGTH];
    SHA256_Final(hash, &sha256);
    auto pass = Database::getInstance().getPassword(login);

    if (!pass || (0 != memcmp(hash, (*pass).c_str(), SHA256_DIGEST_LENGTH))) {
        slog(SLOG_ERROR, "Bad login or password\n");
        sendDatagram(_clientFd, ErrorDatagram(OPEN_SESSION, ACCESS_DENIED));
        return;
    }
    sendDatagram(_clientFd, ErrorDatagram(OPEN_SESSION, SUCCESS));
    _connected = true;
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
