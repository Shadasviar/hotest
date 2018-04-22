cmake_minimum_required(VERSION 2.8)

project(hotest-server)
INCLUDE_DIRECTORIES(${CMAKE_CURRENT_LIST_DIR}/include)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/include)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/src)

add_executable(${PROJECT_NAME} ${SRCS} ${HEADERS})
