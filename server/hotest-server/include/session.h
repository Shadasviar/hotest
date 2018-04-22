#ifndef SESSION_H
#define SESSION_H

#include <string>

class Session
{
public:
    Session(int fd);
    bool run();

private:
    int _clientFd = -1;
    std::string _login;
};

#endif // SESSION_H
