#include "database.h"
#include <posix_server.h>

Database::Database(std::string l):
    _dbLocation(l),
    _db(nullptr, sqlite3_close)
{
    sqlite3* dbPtr;
    int ret = sqlite3_open(_dbLocation.c_str(), &dbPtr);
    if (ret != SQLITE_OK) {
        slog(SLOG_ERROR, "Can't open/create database file: %s\n", _dbLocation.c_str());
        return;
    }
    _db.reset(dbPtr);
}

Database &Database::getInstance()
{
    static Database instance;
    return instance;
}

Maybe<std::string> Database::getPassword(std::string login)
{
    using namespace FunctionalExtensions;
    Maybe<std::string> res;

    auto callback = [](void* data , int , char **argv, char **) -> int {
        auto *login = (std::string*)((uintptr_t*)data)[0];
        auto *res = (Maybe<std::string>*)((uintptr_t*)data)[1];
        if (*login == argv[0]) {
            *res = just(std::string(argv[1]));
        }
        return 0;
    };

    std::string query("SELECT login, password from Users");
    void* data[2] = {(void*)&login, (void*)&res};
    int ret = sqlite3_exec(_db.get(), query.c_str(), callback, (void*)data, nullptr);
    if (ret != SQLITE_OK) {
        slog(SLOG_ERROR, "Failed query to database\n");
        return nothing<std::string>();
    }

    return res;
}

