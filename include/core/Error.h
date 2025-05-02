#pragma once

#include <string>

class Error {
public:
  Error(std::string message, unsigned int line_column,
        unsigned int line_number) {
    _message = "[Linha " + std::to_string(line_number) + "] Erro: " + message +
               " na posição " + std::to_string(line_column);
  }

  std::string get_message() const { return _message; }

private:
  std::string _message;
};
