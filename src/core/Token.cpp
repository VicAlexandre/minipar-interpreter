#include "../include/core/Token.h"

std::string Token::to_string() {
  return "Token<" + to_str(type) + ", " + lexeme + ">";
}
