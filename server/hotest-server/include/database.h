#ifndef DATABASE_H
#define DATABASE_H

#include <sqlite3.h>
#include <string>
#include <memory>
#include <functional_extensions.h>
#include <map>
#include <json.hpp>
#include <map>
#include <set>

using FunctionalExtensions::Maybe;
using nlohmann::json;

/**
 * @brief The Database class incapsulates low-level operations on database
 */
class Database
{
public:
    /**
     * @brief get instance of Database singleton.
     * @return unique instance of connected database.
     */
    static Database& getInstance();

    /**
     * @brief getPassword - get hashed password of user from database by login
     * @param login - login of user whose password to find
     * @return if user specified by login was found returns his password fom database
     *         packed in Maybe type, otherwise returns empty Maybe object (as nothing).
     */
    Maybe<std::string> getPassword(std::string login);

    /**
     * @brief changeCredentials - change login and password of user
     * @param oldLogin - login of user to change data
     * @param login - new login
     * @param password - new password
     * @return true if succeed, otherwise false.
     */
    bool changeCredentials(std::string oldLogin, std::string login, std::string password);

    /**
     * @brief addGroup - add group of students to the database
     * @param name - name of the group
     * @return true if succeed, otherwise false
     */
    bool addGroup(std::string name);

    /**
     * @brief addUser - create new user
     * @param login - login of the new user
     * @param password - password of the new user
     * @param name - name of the new user
     * @param surname - surname of the new user
     * @return if user created successfully, returns true, otherwise false.
     */
    bool addUser(std::string login, std::string password, std::string name, std::string surname);

    /**
     * @brief deleteUser - delete given user from database
     * @param login - login of the user to be deleted
     * @return true if user was deleted successfully, otherwise false.
     */
    bool deleteUser(std::string login);

    /**
     * @brief deleteGroup - delete group from database. Predefined groups: Admins and Examinators
     * can not be deleted.
     * @param name - name of group to be deleted.
     * @return Return Nothing if try delete persistant group, otherwise return true on success, otherwise false.
     */
    Maybe<bool> deleteGroup(std::string name);

    enum ACCESS_GROUP {
        ADMIN,
        EXAMINATOR,
    };

    static std::map<ACCESS_GROUP, std::string> defaultGroups;

    /**
     * @brief hasAccess - check if given user is member of given group
     * @param login - login of the user
     * @param group - group to check user membership for
     * @return true if user is member of group, otherwise false.
     */
    bool hasAccess(std::string login, std::string group);

    /**
     * @brief getUserInfo - get complete information about user by login.
     * @param login - login of the user to get information about.
     * @return Nothing if user doesn't exists, otherwise json with information about user
     * as described in the GET_USER_INFO command of communication protocol
     */
    Maybe<json> getUserInfo(std::string login);

    /**
     * @brief updateUser - change information about given user.
     * @param login - login of the user to be updated.
     * @param name - new name of the user.
     * @param surname - new surname of the user.
     * @return true if data changed succeed, otherwise false.
     */
    bool updateUser(std::string login, std::string name, std::string surname);

    /**
     * @brief addUserToGroup - Add user to given group if both exists.
     * @param login - login of user to be added to group.
     * @param group - name of the group.
     * @return - If user with given login and group exist and adding user to group succeed return true
     * otherwise false.
     */
    bool addUserToGroup(std::string login, std::string group);

    /**
     * @brief removeFromGroup - remove given user from group
     * @param login - login of user to be removed
     * @param group - group to remove user from
     * @return true if succeed, otherwise false.
     */
    bool removeFromGroup(std::string login, std::string group);

    /**
     * @brief getTestsNumber - returns number of tests in database
     * @return number of tests in the Tests table. If error, returns -1.
     */
    int getTestsNumber();

    /**
     * @brief getAnswers return answers of test question given by its id.
     * @param testId - id of test to get answers for.
     * @return If test with given id doesn't exists return nothing. On success
     * returns map filled as [answer_id][answer_text]
     */
    Maybe<std::map<int, std::string>> getAnswers(int testId);

    /**
     * @brief getTestText returns text of question given by its id.
     * @param testId - id of test to get text of question for.
     * @return If test with given id doesn't exists return nothing,
     * otherwise returns text of the question.
     */
    Maybe<std::string> getTestText(int testId);

    /**
     * @brief addAnswers - add choosen by answers to the database.
     * @param username - login of user who answer the questions
     * @param answers - vector of int where indexes of vector are question id
     * and value is index of choosen answer.
     * @return true if success, otherwise false.
     */
    bool addAnswers(std::string username, std::map<int, int> answers);

    /**
     * @brief addTest - add new question to the list of tests
     * @param text - text of the question
     * @param answers - answers excepting right answer for the question
     * @param ranswer - right answer for the question
     * @return true if successfully added, otherwise false.
     */
    bool addTest(std::string text, std::vector<std::string> answers, std::string ranswer);

    /**
     * @brief removeTest remove test with given id from list of tests.
     * @param id - id of test to be removed.
     * @return true if removed successfully, otherwise false.
     */
    bool removeTest(int id);

private:
    Database(std::string l = "./users.db");
    std::string _dbLocation = "./users.db";
    std::unique_ptr<sqlite3, decltype(&sqlite3_close)> _db;
    std::set<int> _freeIndeces;

    bool execQuery(std::string userQuery, std::string userErrmsg,
                   int(*callback)(void *, int, char **, char **) = nullptr,
                   void* data = nullptr);

};

#endif // DATABASE_H
