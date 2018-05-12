#include "session.h"
#include <hotest_protocol.h>
#include <cinttypes>
#include <posix_server.h>
#include <cstring>
#include <cerrno>
#include <string>
#include <database.h>
#include <openssl/sha.h>
#include <json.hpp>

using namespace HotestProtocol;
using json = nlohmann::json;

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

    slog(SLOG_INFO, "[%s]disconnected\n", _login.c_str());
    return true;
}

void Session::closeSession(Datagram &&)
{
    bool ret = sendDatagram(_clientFd, ErrorDatagram(CLOSE_SESSION, SUCCESS));
    ret = ret;
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
    json res = {
        {"all", 100},
        {"pass", 80},
    };
    std::string resStr = res.dump();
    response.data = std::vector<uint8_t>(resStr.begin(), resStr.end());
    response.dataSize = response.data.size();

    bool ret = sendDatagram(_clientFd, std::move(response));
    if (!ret) cliendDeadErrorExit();
}

void Session::invalidCommand(Datagram &&)
{
    bool ret = sendDatagram(_clientFd, ErrorDatagram(ERROR_DATAGRAM, BAD_COMMAND));
    if (!ret) cliendDeadErrorExit();
}

void Session::openSession(Datagram &&dtg)
{
    using namespace FunctionalExtensions;

    char login[LOGIN_BYTE_SIZE];
    char passGot[PASSWORD_BYTE_SIZE];
    strncpy(login, (char*)dtg.data.data(), LOGIN_BYTE_SIZE);
    strncpy(passGot, (char*)dtg.data.data() + LOGIN_BYTE_SIZE, PASSWORD_BYTE_SIZE);
    _login = login;
    std::string passGotStr(PASSWORD_BYTE_SIZE, 0);
    strncpy((char*)passGotStr.data(), passGot, PASSWORD_BYTE_SIZE);

    auto pass = Database::getInstance().getPassword(login);
    auto x = hash(passGotStr);

    if (!pass || *pass != Session::hash(passGotStr)) {
        slog(SLOG_INFO, "[%s]Bad login or password\n", _login.c_str());
        bool ret = sendDatagram(_clientFd, ErrorDatagram(OPEN_SESSION, ACCESS_DENIED));
        if (!ret) cliendDeadErrorExit();
        return;
    }
    sendDatagram(_clientFd, ErrorDatagram(OPEN_SESSION, SUCCESS));
    slog(SLOG_INFO, "[%s]Connected\n", _login.c_str());
    _connected = true;
}

void Session::changeCredentials(Datagram &&dtg)
{
    char newLogin[LOGIN_BYTE_SIZE];

    strncpy(newLogin, (char*)dtg.data.data(), LOGIN_BYTE_SIZE);

    std::string newPassword(PASSWORD_BYTE_SIZE, 0);
    strncpy((char*)newPassword.c_str(), (char*)dtg.data.data() + LOGIN_BYTE_SIZE, PASSWORD_BYTE_SIZE);

    bool ret = Database::getInstance().changeCredentials(_login, std::string(newLogin), hash(newPassword));
    if (!ret) {
        ret = sendDatagram(_clientFd, ErrorDatagram(CHANGE_CREDENTIALS, GENERIC_ERROR));
        if (!ret) cliendDeadErrorExit();
        return;
    }

    slog(SLOG_INFO, "[%s]login changed to %s\n", _login.c_str(), newLogin);
    _login = std::string(newLogin);
    ret = sendDatagram(_clientFd, ErrorDatagram(CHANGE_CREDENTIALS, SUCCESS));
    if (!ret) cliendDeadErrorExit();
}

void Session::addGroup(Datagram &&dtg)
{
    if (!Database::getInstance().hasAccess(_login, Database::defaultGroups[Database::ADMIN])) {
        bool ret = sendDatagram(_clientFd, ErrorDatagram(ADD_GROUP, ACCESS_DENIED));
        if (!ret) cliendDeadErrorExit();
        return;
    }

    std::string grpName(dtg.data.size(), 0);
    memcpy((char*)grpName.data(), dtg.data.data(), dtg.data.size());
    bool ret = Database::getInstance().addGroup(grpName);
    if (!ret) {
        ret = sendDatagram(_clientFd, ErrorDatagram(ADD_GROUP, ALREADY_EXISTS));
        if (!ret) cliendDeadErrorExit();
        return;
    }
    ret = sendDatagram(_clientFd, ErrorDatagram(ADD_GROUP, SUCCESS));
    slog(SLOG_INFO, "[%s]add group '%s'\n", _login.c_str(), grpName.c_str());
    if (!ret) cliendDeadErrorExit();
}

void Session::addUser(Datagram &&dtg)
{
    if (!Database::getInstance().hasAccess(_login, Database::defaultGroups[Database::ADMIN])) {
        bool ret = sendDatagram(_clientFd, ErrorDatagram(ADD_USER, ACCESS_DENIED));
        if (!ret) cliendDeadErrorExit();
        return;
    }

    std::string login, password, name, surname;
    try {
        json userdata = json::parse(dtg.data);
        login       = userdata["login"];
        password    = userdata["password"];
        name        = userdata["name"];
        surname     = userdata["surname"];
    } catch (std::exception &e) {
        slog(SLOG_ERROR, "[%s]user data parse error: %s\n", _login.c_str(), e.what());
        bool ret = sendDatagram(_clientFd, ErrorDatagram(ADD_USER, BAD_COMMAND));
        if (!ret) cliendDeadErrorExit();
        return;
    }

    bool ret = Database::getInstance().addUser(login, hash(password), name, surname);
    if (!ret) {
        bool ret = sendDatagram(_clientFd, ErrorDatagram(ADD_USER, ALREADY_EXISTS));
        if (!ret) cliendDeadErrorExit();
        return;
    }
    ret = sendDatagram(_clientFd, ErrorDatagram(ADD_USER, SUCCESS));
    slog(SLOG_INFO, "[%s]add user '%s'\n", _login.c_str(), login.c_str());
    if (!ret) cliendDeadErrorExit();
}

void Session::deleteUser(Datagram &&dtg)
{
    if (!Database::getInstance().hasAccess(_login, Database::defaultGroups[Database::ADMIN])) {
        bool ret = sendDatagram(_clientFd, ErrorDatagram(DELETE_USER, ACCESS_DENIED));
        if (!ret) cliendDeadErrorExit();
        return;
    }

    std::string login(dtg.data.size(), 0);
    memcpy((char*)login.data(), dtg.data.data(), dtg.data.size());

    bool ret = Database::getInstance().deleteUser(login);
    if (!ret) {
        ret = sendDatagram(_clientFd, ErrorDatagram(DELETE_USER, GENERIC_ERROR));
        if (!ret) cliendDeadErrorExit();
        return;
    }

    slog(SLOG_INFO, "[%s]delete user '%s'\n", _login.c_str(), login.c_str());
    ret = sendDatagram(_clientFd, ErrorDatagram(DELETE_USER, SUCCESS));
    if (!ret) cliendDeadErrorExit();
    return;
}

void Session::deleteGroup(Datagram &&dtg)
{
    if (!Database::getInstance().hasAccess(_login, Database::defaultGroups[Database::ADMIN])) {
        bool ret = sendDatagram(_clientFd, ErrorDatagram(DELETE_GROUP, ACCESS_DENIED));
        if (!ret) cliendDeadErrorExit();
        return;
    }

    std::string group(dtg.data.size(), 0);
    memcpy((char*)group.data(), dtg.data.data(), dtg.data.size());

    using namespace FunctionalExtensions;
    /* If admin tries delete persistant group deny it */
    auto ret = Database::getInstance().deleteGroup(group);
    if (!ret) {
        ret = just(sendDatagram(_clientFd, ErrorDatagram(DELETE_GROUP, ACCESS_DENIED)));
        if (!*ret) cliendDeadErrorExit();
        return;
    }

    if(!*ret) {
        ret = just(sendDatagram(_clientFd, ErrorDatagram(DELETE_GROUP, DOES_NOT_EXISTS)));
        if (!*ret) cliendDeadErrorExit();
        return;
    }

    slog(SLOG_INFO, "[%s]delete group '%s'\n", _login.c_str(), group.c_str());
    ret = just(sendDatagram(_clientFd, ErrorDatagram(DELETE_GROUP, SUCCESS)));
    if (!*ret) cliendDeadErrorExit();
    return;
}

void Session::getUserInfo(Datagram &&dtg)
{
    std::string login(dtg.data.size(), 0);
    memcpy((char*)login.data(), dtg.data.data(), dtg.data.size());

    Maybe<json> res = Database::getInstance().getUserInfo(login);
    if (!res) {
        bool ret = sendDatagram(_clientFd, ErrorDatagram(GET_USER_INFO, DOES_NOT_EXISTS));
        if (!ret) cliendDeadErrorExit();
        return;
    }

    Datagram responce;
    responce.cmd = GET_USER_INFO;
    std::string msg = (*res).dump();
    responce.data.resize(msg.size());
    memcpy(responce.data.data(), msg.data(), msg.size());
    responce.dataSize = responce.data.size();
    sendDatagram(_clientFd, std::move(responce));
}

void Session::setUserInfo(Datagram &&dtg)
{
    if (!Database::getInstance().hasAccess(_login, Database::defaultGroups[Database::ADMIN])) {
        bool ret = sendDatagram(_clientFd, ErrorDatagram(SET_USER_INFO, ACCESS_DENIED));
        if (!ret) cliendDeadErrorExit();
        return;
    }

    std::string data(dtg.data.size(), 0);
    memcpy((char*)data.data(), dtg.data.data(), dtg.data.size());

    bool ret(false);
    try {
        json request = json::parse(data);
        ret = Database::getInstance().updateUser(request["login"], request["name"], request["surname"]);
        if (!ret) {
            cliendDeadErrorExit();
            return;
        }
    } catch (std::exception &e) {
        slog(SLOG_INFO, "[%s]Set user info bad format: %s\n", _login.c_str(), e.what());
        sendDatagram(_clientFd, ErrorDatagram(SET_USER_INFO, BAD_COMMAND));
        if (!ret) cliendDeadErrorExit();
        return;
    }

    ret = sendDatagram(_clientFd, ErrorDatagram(SET_USER_INFO, SUCCESS));
    if (!ret) cliendDeadErrorExit();
}

void Session::addToGroup(Datagram &&dtg)
{
    if (!Database::getInstance().hasAccess(_login, Database::defaultGroups[Database::ADMIN])) {
        bool ret = sendDatagram(_clientFd, ErrorDatagram(ADD_TO_GROUP, ACCESS_DENIED));
        if (!ret) cliendDeadErrorExit();
        return;
    }

    std::string data(dtg.data.size(), 0);
    memcpy((char*)data.data(), dtg.data.data(), dtg.data.size());

    try {
        json request = json::parse(data);
        bool ret = Database::getInstance().addUserToGroup(request["login"], request["group"]);
        if (!ret) {
            ret = sendDatagram(_clientFd, ErrorDatagram(ADD_TO_GROUP, DOES_NOT_EXISTS));
            if (!ret) cliendDeadErrorExit();
            return;
        }
        slog(SLOG_INFO, "[%s]Add user '%s' to group '%s'\n",
             _login.c_str(),
             request["login"].get<std::string>().c_str(),
             request["group"].get<std::string>().c_str());

    } catch (std::exception &e) {
        slog(SLOG_INFO, "[%s]Add user to group bad format: %s\n", _login.c_str(), e.what());
        bool ret = sendDatagram(_clientFd, ErrorDatagram(ADD_TO_GROUP, BAD_COMMAND));
        if (!ret) cliendDeadErrorExit();
        return;
    }

    bool ret = sendDatagram(_clientFd, ErrorDatagram(ADD_TO_GROUP, SUCCESS));
    if (!ret) cliendDeadErrorExit();
}

void Session::removeFromGroup(Datagram &&dtg)
{
    if (!Database::getInstance().hasAccess(_login, Database::defaultGroups[Database::ADMIN])) {
        bool ret = sendDatagram(_clientFd, ErrorDatagram(REMOVE_FROM_GROUP, ACCESS_DENIED));
        if (!ret) cliendDeadErrorExit();
        return;
    }

    std::string data(dtg.data.size(), 0);
    memcpy((char*)data.data(), dtg.data.data(), dtg.data.size());

    try {
        json request = json::parse(data);
        bool ret = Database::getInstance().removeFromGroup(request["login"], request["group"]);
        if (!ret) {
            ret = sendDatagram(_clientFd, ErrorDatagram(REMOVE_FROM_GROUP, DOES_NOT_EXISTS));
            if (!ret) cliendDeadErrorExit();
            return;
        }
        slog(SLOG_INFO, "[%s]Delete user '%s' from group '%s'\n",
             _login.c_str(),
             request["login"].get<std::string>().c_str(),
             request["group"].get<std::string>().c_str());

    } catch (std::exception &e) {
        slog(SLOG_INFO, "[%s]Remove user from group bad format: %s\n", _login.c_str(), e.what());
        bool ret = sendDatagram(_clientFd, ErrorDatagram(REMOVE_FROM_GROUP, BAD_COMMAND));
        if (!ret) cliendDeadErrorExit();
        return;
    }

    bool ret = sendDatagram(_clientFd, ErrorDatagram(REMOVE_FROM_GROUP, SUCCESS));
    if (!ret) cliendDeadErrorExit();
}

void Session::cliendDeadErrorExit()
{
    _connected = false;
}

std::string Session::hash(std::string str)
{
    SHA256_CTX sha256;
    SHA256_Init(&sha256);
    SHA256_Update(&sha256, str.c_str(), str.size());
    unsigned char hash[SHA256_DIGEST_LENGTH];
    SHA256_Final(hash, &sha256);
    std::string res(SHA256_DIGEST_LENGTH, 0);
    memcpy((char*)res.data(), hash, SHA256_DIGEST_LENGTH);
    return res;
}
