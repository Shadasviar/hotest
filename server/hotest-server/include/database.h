#ifndef DATABASE_H
#define DATABASE_H

#include <sqlite3.h>
#include <string>
#include <memory>
#include <functional_extensions.h>

using FunctionalExtensions::Maybe;

class Database
{
public:
    static Database& getInstance();
    Maybe<std::string> getPassword(std::string login);

private:
    Database(std::string l = "./users.db");
    std::string _dbLocation = "./users.db";
    std::unique_ptr<sqlite3, decltype(&sqlite3_close)> _db;
};

#endif // DATABASE_H
