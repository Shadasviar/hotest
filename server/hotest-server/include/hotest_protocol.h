#ifndef COMMUNICATION_PROTOCOL_H
#define COMMUNICATION_PROTOCOL_H

#include <cinttypes>
#include <vector>

namespace HotestProtocol {

/**
 * @brief Commands of the protocol described in the part 3.1.1.2 of the Konspekt
 */
enum Command : uint8_t {
    INVALID_COMMAND = 0,
    GET_TEST_LIST_SIZE = 1,
    GET_TEST,
    SEND_TEST_ANSWERS,
    OPEN_SESSION,
    CLOSE_SESSION,
    GET_RESULTS,
    CHANGE_CREDENTIALS,
    ADD_GROUP,
    ADD_USER,
    ERROR_DATAGRAM,
};

#define LOGIN_BYTE_SIZE 20
#define PASSWORD_BYTE_SIZE 30
#define CMD_BYTE_LEN 1

/**
 * @brief Error codes of the protocol described in the part 3.1.3 of the Konspekt
 */
enum ErrorCode : uint8_t {
    SUCCESS = 0,
    ACCESS_DENIED,
};

struct Datagram {
    Command cmd = INVALID_COMMAND;
    uint8_t dataSize = 0;
    std::vector<uint8_t> data;
};

struct ErrorDatagram : Datagram
{
    ErrorDatagram(Command c, ErrorCode e) {
        cmd = ERROR_DATAGRAM;
        dataSize = 2;
        data.resize(dataSize);
        data[0] = e;
        data[1] = c;
    }
};

Datagram recvDatagram(int fd);
bool sendDatagram(int fd, Datagram&& d);

}

#endif // COMMUNICATION_PROTOCOL_H
