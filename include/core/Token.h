#ifndef TOKEN_H
#define TOKEN_H

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
  Token(TokenType type, std::string lexeme, unsigned int line)
      : type(type), lexeme(std::move(lexeme)), line(line), literal() {}

  Token(TokenType type, std::string lexeme, unsigned int line, double val)
      : type(type), lexeme(std::move(lexeme)), line(line), literal(val) {}

  Token(TokenType type, std::string lexeme, unsigned int line, const char *val)
      : type(type), lexeme(std::move(lexeme)), line(line),
        literal(std::string(val)) {}

  Token(TokenType type, std::string lexeme, unsigned int line,
        const std::string &val)
      : type(type), lexeme(std::move(lexeme)), line(line), literal(val) {}

  Token(TokenType type, std::string lexeme, unsigned int line,
        std::string &&val)
      : type(type), lexeme(std::move(lexeme)), line(line),
        literal(std::move(val)) {}

  Token(TokenType type, std::string lexeme, unsigned int line, bool val)
      : type(type), lexeme(std::move(lexeme)), line(line), literal(val) {}

  ~Token() {}

  /* getters */
  double get_double() const { return std::get<double>(literal); }
  bool get_bool() const { return std::get<bool>(literal); }
  std::string get_string() const { return std::get<std::string>(literal); }

  TokenType get_type() const { return type; }

  std::string to_string();

private:
  TokenType type;
  std::string lexeme;
  unsigned int line;
  ValueType literal;
};

#endif /* TOKEN_H */
