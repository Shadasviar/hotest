#include <iostream>
#include <posix_server.h>
#include <unistd.h>
#include <cstring>
#include <session.h>

using namespace std;

void action(const struct sockaddr_in* client,
            const int fd) {
    Session session(fd);
    session.run();
}

int main(int argc, char** argv) {

    int port = 6666;
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

    if (daemonize) {
        if (daemon(0,1) < 0) {
            slog(SLOG_ERROR, "%s\n", strerror(errno));
            return -1;
        }
        set_syslog();
    }

    slog(SLOG_INFO, "Start time server on port %u\n", port);

    start_server(port, action);

    return 0;
}
