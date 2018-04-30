#ifndef DATABASE_H
#define DATABASE_H

#include <sqlite3.h>
#include <string>
#include <memory>
#include <functional_extensions.h>

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

private:
    Database(std::string l = "./users.db");
    std::string _dbLocation = "./users.db";
    std::unique_ptr<sqlite3, decltype(&sqlite3_close)> _db;
};

#endif // DATABASE_H
