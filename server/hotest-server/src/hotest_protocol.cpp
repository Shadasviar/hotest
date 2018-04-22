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
    if (read(fd, dgram.data.data(), dgram.dataSize) < dgram.dataSize) {
        slog(SLOG_ERROR, "Read data from client failed\n");
        dgram.cmd = INVALID_COMMAND;
        return dgram;
    }

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

    if (write(fd, d.data.data(), d.dataSize) < d.dataSize) {
        slog(SLOG_ERROR, "Write data to client failed\n");
        return false;
    }

    return true;
}
