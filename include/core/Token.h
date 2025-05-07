#pragma once

#include "enum/TokenType.h"
#include <stdexcept>
#include <string>
#include <variant>
#include <vector>

/**
 * @brief All possible literal types for a Minipar token.
 */
using ValueType =
    std::variant<int, double, std::string, bool, std::vector<double>>;

/**
 * @brief Represents a single unit of a Minipar program
 */
class Token {
public:
  /* constructors for all token types */
  Token(TokenType type, std::string lexeme, unsigned int line,
        unsigned int column, ValueType literal = {})
      : type(type), lexeme(std::move(lexeme)), line(line), column(column),
        literal(std::move(literal)) {}

  bool is_number() const { return std::holds_alternative<double>(literal); }
  bool is_string() const {
    return std::holds_alternative<std::string>(literal);
  }
  bool is_array() const {
    return std::holds_alternative<std::vector<double>>(literal);
  }
  bool is_bool() const { return std::holds_alternative<bool>(literal); }
  bool is_valid() const {
    switch (type) {
    case TokenType::NUMBER:
      return is_number();
    case TokenType::STRING_LITERAL:
      return is_string();
    case TokenType::TYPE_ARRAY_NUMBER:
      return is_array();
    case TokenType::TRUE_LITERAL:
    case TokenType::FALSE_LITERAL:
      return is_bool();
    default:
      return true;
    }
  }

  // Getters
  const ValueType &get_literal() const { return literal; }
  int get_number() const { return std::get<int>(literal); }
  double get_double() const { return std::get<double>(literal); }
  std::vector<double> get_array() const {
    return std::get<std::vector<double>>(literal);
  }
  std::string get_string() const { return std::get<std::string>(literal); }
  bool get_bool() const { return std::get<bool>(literal); }

  // Outros getters
  TokenType get_type() const { return type; }
  unsigned int get_line() const { return line; }
  unsigned int get_column() const { return column; }
  const std::string &get_lexeme() const { return lexeme; }

  std::string to_string() const;

private:
  TokenType type;
  std::string lexeme;
  unsigned int line;
  unsigned int column;
  ValueType literal;
};