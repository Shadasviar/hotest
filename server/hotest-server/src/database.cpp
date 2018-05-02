#include "database.h"
#include <posix_server.h>
#include <mutex>

std::map<Database::ACCESS_GROUP, std::string> Database::defaultGroups {
    {Database::ADMIN, "Admins"},
    {Database::EXAMINATOR, "Examinators"},
    {Database::USER, "Students"},
};

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
    static std::mutex mtx;
    std::lock_guard<std::mutex> lck(mtx);

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

bool Database::changeCredentials(std::string oldLogin, std::string login, std::string password)
{
    static std::mutex mtx;
    std::lock_guard<std::mutex> lck(mtx);

    char* errmsg;
    std::string query("UPDATE Users SET Login='"+login+"', Password='"+password+"' WHERE Login='"+oldLogin+"';");
    int ret = sqlite3_exec(_db.get(), query.c_str(), nullptr, nullptr, &errmsg);
    std::unique_ptr<char, decltype(&sqlite3_free)> errmsgPtr(errmsg, sqlite3_free);
    if (ret != SQLITE_OK) {
        slog(SLOG_INFO, "Update credentials error for user %s: %s\n", oldLogin.c_str(), errmsg);
        return false;
    }

    return true;
}

bool Database::addGroup(std::string name)
{
    static std::mutex mtx;
    std::lock_guard<std::mutex> lck(mtx);

    char* errmsg;
    std::string query("INSERT INTO Groups (Name) VALUES ('"+name+"');");
    int ret = sqlite3_exec(_db.get(), query.c_str(), nullptr, nullptr, &errmsg);
    std::unique_ptr<char, decltype(&sqlite3_free)> errmsgPtr(errmsg, sqlite3_free);
    if (ret != SQLITE_OK) {
        slog(SLOG_INFO, "Add group failed: %s\n", errmsg);
        return false;
    }

    return true;
}

bool Database::addUser(std::string login, std::string password, std::string name, std::string surname)
{
    static std::mutex mtx;
    std::lock_guard<std::mutex> lck(mtx);

    char* errmsg;
    std::string query("INSERT INTO Users (login,password,name,surname) "
                      "VALUES ('"+login+"','"+password+"','"+name+"','"+surname+"');");
    int ret = sqlite3_exec(_db.get(), query.c_str(), nullptr, nullptr, &errmsg);
    std::unique_ptr<char, decltype(&sqlite3_free)> errmsgPtr(errmsg, sqlite3_free);
    if (ret != SQLITE_OK) {
        slog(SLOG_INFO, "Add user failed: %s\n", errmsg);
        return false;
    }

    return true;
}

bool Database::deleteUser(std::string login)
{
    static std::mutex mtx;
    std::lock_guard<std::mutex> lck(mtx);

    char* errmsg;
    std::string query("DELETE FROM Users WHERE login='"+login+"';");
    int ret = sqlite3_exec(_db.get(), query.c_str(), nullptr, nullptr, &errmsg);
    std::unique_ptr<char, decltype(&sqlite3_free)> errmsgPtr(errmsg, sqlite3_free);
    if (ret != SQLITE_OK) {
        slog(SLOG_INFO, "Delete user failed: %s\n", errmsg);
        return false;
    }

    return true;
}

bool Database::hasAccess(std::string login, std::string group)
{
    static std::mutex mtx;
    std::lock_guard<std::mutex> lck(mtx);

    auto callback = [](void* data , int , char **, char **) -> int {
        auto *res = (bool*)data;
        *res = true;
        return 0;
    };

    char* errmsg;
    bool res(false);
    std::string query("SELECT Groups.Name FROM Users "
                      "JOIN GroupsMembers ON GroupsMembers.id = Users.id "
                      "JOIN Groups ON Groups.groupeid = GroupsMembers.groupeId "
                      "WHERE Users.login = '"+login+"' AND Groups.Name = '"+group+"'; ");
    int ret = sqlite3_exec(_db.get(), query.c_str(), callback, (void*)&res, &errmsg);
    std::unique_ptr<char, decltype(&sqlite3_free)> errmsgPtr(errmsg, sqlite3_free);
    if (ret != SQLITE_OK) {
        slog(SLOG_INFO, "Check access failed: %s\n", errmsg);
        return false;
    }

    return res;
}

