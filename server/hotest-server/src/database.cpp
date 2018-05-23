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

Maybe<std::map<int, std::string> > Database::getAnswers(int testId)
{
    static std::mutex mtx;
    std::lock_guard<std::mutex> lck(mtx);

    using namespace FunctionalExtensions;

    std::map<int, std::string> responce;

    auto callback = [](void* data , int , char **argv, char **) -> int {
        auto* responce = (std::map<int, std::string>*)data;
        (*responce)[atoi(argv[0])] = argv[1];
        return 0;
    };

    bool ret = execQuery("SELECT answerId, answer FROM Answers "
                         "JOIN Tests ON Tests.testId = Answers.testId "
                         "WHERE Answers.testId = '"+std::to_string(testId)+"';",
                         "Get answers failed",
                         callback, (void*)&responce);

    if (!ret || responce.size() == 0) return nothing<std::map<int, std::string>>();
    return just(responce);
}

Maybe<std::string> Database::getTestText(int testId)
{
    static std::mutex mtx;
    std::lock_guard<std::mutex> lck(mtx);

    using namespace FunctionalExtensions;

    std::string responce;

    auto callback = [](void* data , int , char **argv, char **) -> int {
        auto* responce = (std::string*)data;
        (*responce) = std::string(argv[0]);
        return 0;
    };

    bool ret = execQuery("SELECT questionText FROM Tests WHERE testId='"
                         +std::to_string(testId)+"';",
                         "get test text failed",
                         callback, (void*)&responce);

    if (!ret || responce == "") return nothing<std::string>();
    return just(responce);
}

bool Database::addAnswers(std::string username, std::map<int, int> answers)
{
    static std::mutex mtx;
    std::lock_guard<std::mutex> lck(mtx);

    bool ret(true);

    try {
        for (size_t i(0); i < answers.size(); ++i) {
            ret &= execQuery("REPLACE INTO UserAnswers (userId, testId, answerId) "
                             "SELECT Users.id, '"+std::to_string(i)+"', "
                             "'"+std::to_string(answers[i])+"' FROM Users "
                             "WHERE Users.Login = '"+username+"';",
                             "Cant update database");
        }
    } catch (std::exception& e) {
        slog(SLOG_ERROR, "Add answers: %s\n", e.what());
        return false;
    }

    return ret;
}

bool Database::addTest(std::string text, std::vector<std::string> answers, std::string ranswer)
{
    static std::mutex mtx;
    std::lock_guard<std::mutex> lck(mtx);

    bool ret(true);
    int id(-1);

    /* Insert test and get id by SELECT last_insert_rowid();*/
    ret = execQuery("INSERT INTO Tests (questionText, rightAnswerIndex) "
                    "VALUES ('" + text +"', -1);",
                    "Cant add text of question");

    if (!ret) return ret;

    ret = execQuery("SELECT last_insert_rowid();", "Cant get id of test",
                     [](void* data , int , char **argv, char **){
                            int *id = (int*)data;
                            *id = std::stoi(argv[0]);
                            return 0;
                        },
                    (void*)&id);
    if (!ret) return ret;

    /* Insert all answers*/
    int ansId(0);
    for (std::string&s : answers) {
        ret &= execQuery("INSERT INTO Answers (testId, answerId, answer) "
                         "VALUES ('"+std::to_string(id)+"', "
                         "'"+std::to_string(ansId++)+"', "
                         "'"+s+"');",
                         "Cant update database");
    }
    if (!ret) return ret;

    /* Insert right answer and get it id*/
    ret = execQuery("INSERT INTO Answers (testId, answerId, answer) "
                    "VALUES ('"+std::to_string(id)+"','"+std::to_string(ansId)+"' "
                    ", '"+ranswer+"');",
                     "Cant insert right answer");
    if (!ret) return ret;

    /* Update test with right answer id*/
    ret = execQuery("UPDATE Tests SET rightAnswerIndex = '"+std::to_string(ansId)+"' "
                     "WHERE testId = '"+std::to_string(id)+"';",
                     "Cant update right answer");
    return ret;
}

bool Database::removeTest(int id)
{
    static std::mutex mtx;
    std::lock_guard<std::mutex> lck(mtx);

    return execQuery("DELETE FROM Tests WHERE testId = '"+std::to_string(id)+"';",
                     "Cant remove test.");
}

