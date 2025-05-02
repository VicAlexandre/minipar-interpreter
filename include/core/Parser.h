#pragma once

#include "core/Error.h"
#include "core/Expr.h"
#include "core/Stmt.h"
#include "core/Token.h"

#include <memory>
#include <vector>

struct StepResult {
  std::unique_ptr<Stmt> statement;
  std::unique_ptr<Error> syntax_error;
};

struct ParseResult {
  std::vector<std::unique_ptr<Stmt>> statements;
  std::vector<std::unique_ptr<Error>> syntax_errors;
};

struct ExprResult {
  ExprPtr expression;
  std::unique_ptr<Error> syntax_error;
};

class Parser {
public:
  Parser(std::vector<Token> tokens) : tokens(tokens), current(0) {}

  ParseResult parse();

private:
  std::vector<Token> tokens;
  unsigned int current;

  StepResult parse_statement();

  bool match(TokenType type);

  bool lookahead_is_declaration();

  bool check(TokenType type);

  inline bool is_at_end() const { return current >= tokens.size(); }

  inline bool is_at_end(unsigned int index) const {
    return index >= tokens.size();
  }

  void sync();
  void rewind(int steps);
  Token consume();
  Token previous();

  TokenType peek();

  /* private statement parsing functions */
  StepResult parse_function_stmt();
  StepResult parse_if_stmt();
  StepResult parse_while_stmt();
  StepResult parse_seq_stmt();
  StepResult parse_par_stmt();
  StepResult parse_c_channel_stmt();
  StepResult parse_declaration_stmt();
  StepResult parse_assignment();
  StepResult parse_return_stmt();
  StepResult parse_break_stmt();
  StepResult parse_continue_stmt();

  /* private expression parsing functions */
  ExprResult parse_expression();
  ExprResult parse_disjunction();
  ExprResult parse_conjunction();
  ExprResult parse_equality();
  ExprResult parse_comparison();
  ExprResult parse_sum();
  ExprResult parse_term();
  ExprResult parse_unary();
  ExprResult parse_primary();
  ExprResult parse_local();

  StepResult parse_block();
};
