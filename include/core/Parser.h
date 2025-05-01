#pragma once

#include "core/Stmt.h"
#include "core/Token.h"

#include <memory>
#include <vector>

class Parser {
public:
  Parser(std::vector<Token> tokens) : tokens(tokens), current(0) {}

  std::vector<std::unique_ptr<Stmt>> parse();

private:
  std::vector<Token> tokens;
  unsigned int current;

  std::unique_ptr<Stmt> parse_statement();

  bool match(TokenType type);

  bool lookahead_is_declaration();

  bool check(TokenType type);

  inline bool is_at_end() const { return current >= tokens.size(); }

  inline bool is_at_end(unsigned int index) const {
    return index >= tokens.size();
  }

  /* private statement parsing functions */
  std::unique_ptr<Stmt> parse_function_stmt();
  std::unique_ptr<Stmt> parse_if_stmt();
  std::unique_ptr<Stmt> parse_while_stmt();
  std::unique_ptr<Stmt> parse_seq_stmt();
  std::unique_ptr<Stmt> parse_par_stmt();
  std::unique_ptr<Stmt> parse_s_channel_stmt();
  std::unique_ptr<Stmt> parse_c_channel_stmt();
  std::unique_ptr<Stmt> parse_declaration_stmt();
  std::unique_ptr<Stmt> parse_assignment_or_call();
  std::unique_ptr<Stmt> parse_return_stmt();
  std::unique_ptr<Stmt> parse_break_stmt();
  std::unique_ptr<Stmt> parse_continue_stmt();
  std::unique_ptr<Stmt> parse_expression_stmt();
};
