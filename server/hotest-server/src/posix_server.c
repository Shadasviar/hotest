#include <posix_server.h>
#include <stdio.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <stdbool.h>
#include <stdlib.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <syslog.h>
#include <errno.h>
#include <string.h>
#include <stdarg.h>
#include <signal.h>

uint8_t SLOG_ERROR = STDERR_FILENO;
uint8_t SLOG_INFO = STDOUT_FILENO;

static void void_dprintf(int fd, const char* format, ...) {
    va_list args;
    va_start(args, format);
    vdprintf(fd, format, args);
    va_end(args);
}

void (*slog) (int level, const char* format, ...) = void_dprintf;

void set_syslog() {
    slog = syslog;
    SLOG_ERROR = LOG_ERR;
    SLOG_INFO = LOG_INFO;
}

void start_server(
    int port,
    void(*action)(const struct sockaddr_in* client,
                  const int fd))
{

    int fd = -1;
    int ret = -1;
    struct sockaddr_in server = {0};
    struct sockaddr_in client = {0};

    fd = socket(AF_INET, SOCK_STREAM, 0);
    if (fd < 0) {
        slog(SLOG_ERROR, "Socket error: %s\n", strerror(errno));
        exit(-1);
    }

    server.sin_family = AF_INET;
    server.sin_port = htons(port);
    server.sin_addr.s_addr = INADDR_ANY;

    ret = bind(fd, (struct sockaddr*)&server, sizeof(server));
    if (ret < 0) {
        slog(SLOG_ERROR, "Bind socket failed: %s\n", strerror(errno));
        exit(-1);
    }

    ret = listen(fd, MAX_BACKLOG);
    if (ret < 0) {
        slog(SLOG_ERROR, "Listen failed: %s\n", strerror(errno));
        exit(-1);
    }

    while (true) {
        socklen_t client_size = sizeof(client);
        int newfd = accept(fd, (struct sockaddr*)&client, &client_size);
        if (newfd < 0) {
            slog(SLOG_ERROR, "Accept connection failed: %s\n", strerror(errno));
            continue;
        }

        action(&client, newfd);
    }
}

int connect_server(const char* address, const char* portstr) {

    int fd = -1;
    int port = -1;
    in_addr_t host = inet_addr(address);
    struct sockaddr_in server = {0};

    char* endptr;
    port = strtol(portstr, &endptr, 10);
    if (endptr == portstr || port < 0) {
        slog(SLOG_ERROR, "Invalid port number: %s\n", portstr);
        return -1;
    }

    fd = socket(AF_INET, SOCK_STREAM, 0);
    if (fd < 0) {
        slog(SLOG_ERROR, "Open socket failed: %s\n", strerror(errno));
        return -1;
    }

    if (host == INADDR_NONE) {
        slog(SLOG_ERROR, "Bad hostname\n");
        return -1;
    }

    server.sin_family = AF_INET;
    server.sin_addr.s_addr = host;
    server.sin_port = htons(port);

    if (connect(fd, (struct sockaddr*)&server, sizeof(server)) < 0) {
        slog(SLOG_ERROR, "Connect to server failed: %s\n", strerror(errno));
        return -1;
    }

    return fd;
}

void ignore_sigpipe() {
    signal(SIGPIPE, SIG_IGN);
}
