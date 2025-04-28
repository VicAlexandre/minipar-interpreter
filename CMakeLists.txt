cmake_minimum_required(VERSION 3.10)

project(minipar)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

include_directories(src/ include/)

file(GLOB SOURCES "src/*.cpp" "src/core/*.cpp" "src/enum/*.cpp")

add_executable(${PROJECT_NAME} ${SOURCES})
