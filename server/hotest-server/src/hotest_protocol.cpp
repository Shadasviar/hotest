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
    uint8_t dataSize = 0;
    if (read(fd, &dataSize, 1) < 1) {
        slog(SLOG_ERROR, "Read data size from client failed\n");
        dgram.cmd = INVALID_COMMAND;
        return dgram;
    }

    dgram.data.resize(dataSize);
    size_t wrote = 0;
    while ((wrote += read(fd, dgram.data.data()+wrote, dataSize-wrote)) < dataSize);

    return dgram;
}

bool HotestProtocol::sendDatagram(int fd, Datagram&& d) {
    if (write(fd, &d.cmd, CMD_BYTE_LEN) < CMD_BYTE_LEN) {
        slog(SLOG_ERROR, "Write command to client failed\n");
        return false;
    }

    uint8_t dataSize = d.data.size();
    if (write(fd, &dataSize, CMD_BYTE_LEN) < CMD_BYTE_LEN) {
        slog(SLOG_ERROR, "Write data size to client failed\n");
        return false;
    }

    size_t wrote = 0;
    while ((wrote += write(fd, d.data.data()+wrote, d.data.size()-wrote)) < d.data.size());

    return true;
}
