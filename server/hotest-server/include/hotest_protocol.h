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
    DELETE_USER,
    DELETE_GROUP,
    GET_USER_INFO,
    SET_USER_INFO,
    ADD_TO_GROUP,
    REMOVE_FROM_GROUP,
    ADD_TEST,
    REMOVE_TEST
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
    BAD_COMMAND,
    ALREADY_EXISTS,
    GENERIC_ERROR,
    DOES_NOT_EXISTS,
};

struct Datagram {
    /**
     * @brief Datagram - constructor of datagram with parameters to fast writing
     * @param c Command
     * @param d Data as vector<uint8_t> abd size s
     */
    Datagram(Command c, std::vector<uint8_t> d):
        cmd(c), data(d){}

    Datagram() = default;

    Command cmd = INVALID_COMMAND;
    std::vector<uint8_t> data;
};

struct ErrorDatagram : Datagram
{
    /**
     * @brief ErrorDatagram - easy constructable response for client
     * @param c Command responce is send for
     * @param e Error code of responce
     */
    ErrorDatagram(Command c, ErrorCode e) {
        cmd = ERROR_DATAGRAM;
        data.resize(2);
        data[0] = e;
        data[1] = c;
    }
};

/**
 * @brief recvDatagram - recieve datagram from client given by connected socket file descriptor
 * @param fd File descriptor of connected to client socket
 * @return Datagram recieved from client. If error occured,
 *         returned datagram contains INVALID_COMMAND in cmd field.
 */
Datagram recvDatagram(int fd);

/**
 * @brief sendDatagram - send given datagram to given client
 * @param fd File descriptor of connected to client socket
 * @param d Filled datagram to send.
 * @return True if success, otherwise false.
 */
bool sendDatagram(int fd, Datagram&& d);

}

#endif // COMMUNICATION_PROTOCOL_H
