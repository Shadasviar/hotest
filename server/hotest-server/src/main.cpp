#include <iostream>
#include <hotest_protocol.h>
#include <posix_server.h>
#include <stdio.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <stdbool.h>
#include <time.h>
#include <stdlib.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <errno.h>
#include <string.h>
#include <signal.h>

#define DEFAULT_PORT 37
#define UNIX_EPOCHE_TIME_OFFSET 2208988800

using namespace std;

static void print_client_ip(const struct sockaddr_in* addr) {
    slog(SLOG_INFO, "Time was requested by client with IP %s\n",
            inet_ntoa(addr->sin_addr));
}

static void send_time(const struct sockaddr_in* client, const int fd) {
    print_client_ip(client);

    uint32_t curr_time = time(NULL);
    curr_time += UNIX_EPOCHE_TIME_OFFSET;
    int n = write(fd, &curr_time, sizeof(curr_time));
    if (n < 0) {
        slog(SLOG_ERROR, "Write error: %s\n", strerror(errno));
    }
}

static void before_exit() {
    slog(SLOG_INFO, "Stop time server\n");
}

static void sigterm_handler(int sig) {
    if (sig == SIGTERM) {
        exit(0);
    }
}

int main(int argc, char** argv) {

    int port = DEFAULT_PORT;
    bool daemonize = false;

    int opt = 0;
    while ((opt = getopt(argc, argv, "dp:")) != -1) {
        switch(opt){
            case 'd':
                daemonize = true;
                break;
            case 'p':
                port = atoi(optarg);
                break;
            default:
                slog(SLOG_ERROR, "Usage: %s [-d] [p port]\n", argv[0]);
                slog(SLOG_ERROR, "\t-d: Run program as daemon\n");
                return -1;
        }
    }

    if (atexit(before_exit) < 0) {
        slog(SLOG_ERROR, "Atexit failed\n");
    }

    signal(SIGTERM, sigterm_handler);

    if (daemonize) {
        if (daemon(0,1) < 0) {
            slog(SLOG_ERROR, "%s\n", strerror(errno));
            return -1;
        }
        set_syslog();
    }

    slog(SLOG_INFO, "Start time server on port %u\n", port);

    start_server(port, send_time);

    return 0;
}
