cmake_minimum_required(VERSION 3.1)

project(hotest-server)
INCLUDE_DIRECTORIES(${CMAKE_CURRENT_LIST_DIR}/include)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/include)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/src)
set(CMAKE_CXX_STANDARD 14)
set(DB_DIR ${CMAKE_CURRENT_LIST_DIR}/database)

add_custom_target(
    ${PROJECT_NAME}-database
    cp ${DB_DIR}/* ${CMAKE_BINARY_DIR}
)

add_executable(${PROJECT_NAME} ${SRCS} ${HEADERS})
add_dependencies(${PROJECT_NAME} ${PROJECT_NAME}-database)

target_link_libraries(${PROJECT_NAME} pthread sqlite3 crypto)
