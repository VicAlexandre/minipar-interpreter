#pragma once

#include "enum/TokenType.h"

#include <string>
#include <variant>

/**
 * @brief All possible literal types for a Minipar token.
 */
using ValueType = std::variant<double, std::string, bool>;

/**
 * @brief Represents a single unit of a Minipar program
 */
class Token {
public:
  /* constructors for all token types */
  Token(TokenType type, std::string lexeme, unsigned int line,
        unsigned int column)
      : type(type), lexeme(std::move(lexeme)), line(line), column(column),
        literal() {}

  Token(TokenType type, std::string lexeme, unsigned int line,
        unsigned int column, double val)
      : type(type), lexeme(std::move(lexeme)), line(line), column(column),
        literal(val) {}

  Token(TokenType type, std::string lexeme, unsigned int line,
        unsigned int column, const char *val)
      : type(type), lexeme(std::move(lexeme)), line(line), column(column),
        literal(std::string(val)) {}

  Token(TokenType type, std::string lexeme, unsigned int line,
        unsigned int column, const std::string &val)
      : type(type), lexeme(std::move(lexeme)), line(line), literal(val) {}

  Token(TokenType type, std::string lexeme, unsigned int line,
        unsigned int column, std::string &&val)
      : type(type), lexeme(std::move(lexeme)), line(line), column(column),
        literal(std::move(val)) {}

  Token(TokenType type, std::string lexeme, unsigned int line, bool val)
      : type(type), lexeme(std::move(lexeme)), line(line), literal(val) {}

  ~Token() {}

  /* getters */
  double get_double() const { return std::get<double>(literal); }
  bool get_bool() const { return std::get<bool>(literal); }
  std::string get_string() const { return std::get<std::string>(literal); }
  TokenType get_type() const { return type; }
  unsigned int get_line() const { return line; }
  unsigned int get_column() const { return column; }

  std::string to_string();

private:
  TokenType type;
  std::string lexeme;
  unsigned int line;
  unsigned int column;
  ValueType literal;
};
