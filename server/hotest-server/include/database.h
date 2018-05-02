#ifndef DATABASE_H
#define DATABASE_H

#include <sqlite3.h>
#include <string>
#include <memory>
#include <functional_extensions.h>
#include <map>

using FunctionalExtensions::Maybe;

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

    enum ACCESS_GROUP {
        ADMIN,
        EXAMINATOR,
        USER,
    };

    static std::map<ACCESS_GROUP, std::string> defaultGroups;

    /**
     * @brief hasAccess - check if given user is member of given group
     * @param login - login of the user
     * @param group - group to check user membership for
     * @return true if user is member of group, otherwise false.
     */
    bool hasAccess(std::string login, std::string group);

private:
    Database(std::string l = "./users.db");
    std::string _dbLocation = "./users.db";
    std::unique_ptr<sqlite3, decltype(&sqlite3_close)> _db;
};

#endif // DATABASE_H
