#ifndef SCANNER_H
#define SCANNER_H

#include "core/Token.h"

#include <string>
#include <vector>

enum LiteralType {
  STRING,
  NUMBER,
  BOOL,
  NONE,
};

/**
 * @brief The lexical analyzer class, a.k.a. scanner, for the Minipar
 * interpreter.
 */
class Scanner {
public:
  Scanner(std::string source) : source(source) {}
  ~Scanner() {}

  /**
   * @brief Scans the source code and returns a vector of tokens.
   *
   * @return std::vector<Token> The vector of processed tokens.
   */
  std::vector<Token> scan_tokens();

private:
  std::string source;
  std::vector<Token> tokens;
  unsigned int start = 0;
  unsigned int current = 0;
  unsigned int line = 1;

  void scan_token();

  void add_token(TokenType type);

  void add_token(TokenType type, ValueType literal, LiteralType lit);

  bool match_token(char expected);

  void scan_string();

  void scan_number();

  void scan_identifier();
};

#endif /* SCANNER_H */
