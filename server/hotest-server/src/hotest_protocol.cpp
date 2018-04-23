#include <hotest_protocol.h>
#include <posix_server.h>
#include <unistd.h>

using namespace HotestProtocol;

Datagram HotestProtocol::recvDatagram(int fd) {
    Datagram dgram;
    if (read(fd, &dgram.cmd, CMD_BYTE_LEN) < CMD_BYTE_LEN) {
        slog(SLOG_ERROR, "Read command from client failed\n");
        dgram.cmd = INVALID_COMMAND;
        return dgram;
    }
    if (read(fd, &dgram.dataSize, 1) < 1) {
        slog(SLOG_ERROR, "Read data size from client failed\n");
        dgram.cmd = INVALID_COMMAND;
        return dgram;
    }

    dgram.data.resize(dgram.dataSize);
    size_t wrote = 0;
    while ((wrote += read(fd, dgram.data.data()+wrote, dgram.dataSize-wrote)) < dgram.dataSize);

    return dgram;
}

bool HotestProtocol::sendDatagram(int fd, Datagram&& d) {
    if (write(fd, &d.cmd, CMD_BYTE_LEN) < CMD_BYTE_LEN) {
        slog(SLOG_ERROR, "Write command to client failed\n");
        return false;
    }

    if (write(fd, &d.dataSize, CMD_BYTE_LEN) < CMD_BYTE_LEN) {
        slog(SLOG_ERROR, "Write data size to client failed\n");
        return false;
    }

    size_t wrote = 0;
    while ((wrote += write(fd, d.data.data()+wrote, d.dataSize-wrote)) < d.dataSize);

    return true;
}
