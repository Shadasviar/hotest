#ifndef POSIX_SERVER_H
#define POSIX_SERVER_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <netinet/in.h>

#define MAX_BACKLOG 5

extern uint8_t SLOG_ERROR;
extern uint8_t SLOG_INFO;

/** @brief Log function used by server application;
 *  @param level The level of log message, e.g. SLOG_ERROR, SLOG_INFO.
 *  @param format The format string for log message like in printf function.
 */
extern void (*slog) (int level, const char* format, ...);

/** @brief Set output of slog function to syslog
 */
void set_syslog();

/** Start stream socket server which will listen on given port and do given action for
 * each conection.
 *  @param port Port number which socket server will listen on.
 *  @param action Function which will be executed in infinity loop on each accepted connection.
 *  @param client Structure with information about client of accepted connection.
 *  @param fd File descriptor of the accepted connection.
 */
void start_server(
    int port,
    void(*action)(const struct sockaddr_in* client,
                  const int fd)
);

/** @brief Connect to server with adress and port.
 *  @param address Address of the server whitch connect to in format "xxx.xxx.xxx.xxx"
 *  @param port Port number for connection as string.
 *  @return On success returns file descriptor of connected socket, otherwise -1.
 */
int connect_server(const char* address, const char* port);

/**
 * @brief ignore_sigpipe signal from OS
 */
void ignore_sigpipe();

#ifdef __cplusplus
}
#endif

#endif // POSIX_SERVER_H
