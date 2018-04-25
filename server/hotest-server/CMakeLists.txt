cmake_minimum_required(VERSION 3.1)

project(hotest-server)
INCLUDE_DIRECTORIES(${CMAKE_CURRENT_LIST_DIR}/include)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/include)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/src)
set(CMAKE_CXX_STANDARD 11)

add_executable(${PROJECT_NAME} ${SRCS} ${HEADERS})
target_link_libraries(${PROJECT_NAME} pthread)
