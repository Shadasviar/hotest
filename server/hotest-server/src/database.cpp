#include "database.h"
#include <posix_server.h>
#include <mutex>
#include <list>

std::map<Database::ACCESS_GROUP, std::string> Database::defaultGroups {
    {Database::ADMIN, "Admins"},
    {Database::EXAMINATOR, "Examinators"},
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

bool Database::execQuery(std::string userQuery,
                         std::string userErrmsg,
                         int (*callback)(void*,int,char**,char**),
                         void *data)
{
    char* errmsg;
    std::string query(userQuery);
    int ret = sqlite3_exec(_db.get(), query.c_str(), callback, data, &errmsg);
    std::unique_ptr<char, decltype(&sqlite3_free)> errmsgPtr(errmsg, sqlite3_free);
    if (ret != SQLITE_OK) {
        slog(SLOG_INFO, std::string(userErrmsg + ": %s\n").c_str(), errmsg);
        return false;
    }
    return true;
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
    if (execQuery(query, "Failed connection to database", callback, (void*)data)) {
        return res;
    }

    return nothing<std::string>();
}

bool Database::changeCredentials(std::string oldLogin, std::string login, std::string password)
{
    static std::mutex mtx;
    std::lock_guard<std::mutex> lck(mtx);

    return execQuery("UPDATE Users SET Login='"+login+"', Password='"+password+"' WHERE Login='"+oldLogin+"';",
                     std::string("Update credentials error for user") + oldLogin.c_str());
}

bool Database::addGroup(std::string name)
{
    static std::mutex mtx;
    std::lock_guard<std::mutex> lck(mtx);

    return execQuery("INSERT INTO Groups (Name) VALUES ('"+name+"');",
                     "Add group failed");
}

bool Database::addUser(std::string login, std::string password, std::string name, std::string surname)
{
    static std::mutex mtx;
    std::lock_guard<std::mutex> lck(mtx);

    return execQuery("INSERT INTO Users (login,password,name,surname) "
                     "VALUES ('"+login+"','"+password+"','"+name+"','"+surname+"');",
                     "Add user failed");
}

bool Database::deleteUser(std::string login)
{
    static std::mutex mtx;
    std::lock_guard<std::mutex> lck(mtx);

    return execQuery("DELETE FROM Users WHERE login='"+login+"';",
                     "Delete user failed");
}

Maybe<bool> Database::deleteGroup(std::string name)
{
    static std::mutex mtx;
    std::lock_guard<std::mutex> lck(mtx);

    using namespace FunctionalExtensions;
    if(std::count_if(defaultGroups.begin(), defaultGroups.end(), [name](auto& item){return item.second == name;}) > 0) {
        return nothing<bool>();
    }

    return just(execQuery("DELETE FROM Groups WHERE Name='"+name+"';",
                     "Delete group failed"));
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

    bool res(false);
    bool ret = execQuery("SELECT Groups.Name FROM Users "
                     "JOIN GroupsMembers ON GroupsMembers.id = Users.id "
                     "JOIN Groups ON Groups.groupeid = GroupsMembers.groupeId "
                     "WHERE Users.login = '"+login+"' AND Groups.Name = '"+group+"'; ",
                     "Check access failed",
                     callback, (void*)&res);
    if (ret) return res;
    return false;
}

Maybe<nlohmann::json> Database::getUserInfo(std::string login)
{
    static std::mutex mtx;
    std::lock_guard<std::mutex> lck(mtx);

    using namespace FunctionalExtensions;

    json responce;

    auto callbackUser = [](void* data , int , char **argv, char **) -> int {
        json* responce = (json*)data;
        (*responce)["login"]    = argv[0];
        (*responce)["name"]     = argv[1];
        (*responce)["surname"]  = argv[2];
        return 0;
    };

    auto callbackGroups = [](void* data , int , char **argv, char **) -> int {
        auto* grps = (std::list<std::string>*)data;
        grps->push_back(argv[0]);
        return 0;
    };

    bool ret = execQuery("SELECT login,name,ifnull(surname, '') FROM Users WHERE login='"+login+"';",
                         "Getting user info failed",
                         callbackUser, (void*)&responce);
    if (!ret) return nothing<json>();

    std::list<std::string> grps;
    ret = execQuery("SELECT Groups.name FROM Groups "
                    "JOIN GroupsMembers ON GroupsMembers.groupeId=Groups.groupeId "
                    "JOIN Users ON Users.id = GroupsMembers.id "
                    "WHERE Users.login='"+login+"';",
                    "Getting groups failed",
                    callbackGroups, (void*)&grps);

    if (!ret) return nothing<json>();
    if (responce.empty()) return nothing<json>();

    responce["groups"] = grps;
    return just(responce);
}

bool Database::updateUser(std::string login, std::string name, std::string surname)
{
    static std::mutex mtx;
    std::lock_guard<std::mutex> lck(mtx);

    return execQuery("UPDATE Users SET name = '"+name+"', surname = '"+surname+"' "
                     "WHERE login ='"+login+"';",
                     "Update user data failed");
}

bool Database::addUserToGroup(std::string login, std::string group)
{
    static std::mutex mtx;
    std::lock_guard<std::mutex> lck(mtx);

    return execQuery("INSERT INTO GroupsMembers (id, groupeId) "
                     "SELECT Users.id,Groups.groupeId FROM Users, Groups "
                     "WHERE Users.Login = '"+login+"' AND Groups.Name = '"+group+"';",
                     "Add record failed");
}

bool Database::removeFromGroup(std::string login, std::string group)
{
    static std::mutex mtx;
    std::lock_guard<std::mutex> lck(mtx);

    return execQuery("DELETE FROM GroupsMembers "
                     "WHERE id=(SELECT id FROM Users WHERE Users.Login = '"+login+"') "
                     "AND groupeId = (SELECT groupeId FROM Groups WHERE Groups.name = '"+group+"'); ",
                     "Delete record failed");
}

int Database::getTestsNumber()
{
    static std::mutex mtx;
    std::lock_guard<std::mutex> lck(mtx);

    auto callback = [](void* data , int , char **argv, char **) -> int {
        auto* n = (int*)data;
        *n = atoi(argv[0]);
        return 0;
    };

    int res = 0;

    bool ret = execQuery("SELECT COUNT(testId) FROM Tests;",
                     "Select tests failed",
                     callback,
                     (void*)&res);

    if (!ret) return -1;
    return res;
}

