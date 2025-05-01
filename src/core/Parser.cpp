#include "core/Parser.h"

/**
 * @brief If a parse error occurs, this macro will create a new StepResult
 * with a nullptr statement and an Error object with the error message.
 */
#define PARSER_ERROR(_msg)                                                     \
  {nullptr, std::make_unique<Error>(_msg, tokens[current].get_column(),        \
                                    tokens[current].get_line())};

static inline void sync() {}

ParseResult Parser::parse() {
  std::vector<std::unique_ptr<Stmt>> statements = {};
  std::vector<std::unique_ptr<Error>> syntax_errors = {};

  while (!is_at_end()) {
    StepResult statement = parse_statement();
    if (statement.syntax_error) {
      syntax_errors.push_back(std::move(statement.syntax_error));
      sync();
      continue;
    }

    statements.push_back(std::move(statement.statement));
  }

  return {std::move(statements), std::move(syntax_errors)};
}

StepResult Parser::parse_statement() {
  if (match(TokenType::FUNC)) {
    return parse_function_stmt();
  }

  if (match(TokenType::IF)) {
    return parse_if_stmt();
  }

  if (match(TokenType::WHILE)) {
    return parse_while_stmt();
  }

  if (match(TokenType::SEQ)) {
    return parse_seq_stmt();
  }

  if (match(TokenType::PAR)) {
    return parse_par_stmt();
  }

  if (match(TokenType::C_CHANNEL)) {
    return parse_c_channel_stmt();
  }

  if (check(TokenType::IDENTIFIER)) {
    if (lookahead_is_declaration()) {
      return parse_declaration_stmt();
    }
    return parse_assignment_or_call();
  }

  if (match(TokenType::RETURN)) {
    return parse_return_stmt();
  }

  if (match(TokenType::BREAK)) {
    return parse_break_stmt();
  }

  if (match(TokenType::CONTINUE)) {
    return parse_continue_stmt();
  }

  return {nullptr, std::make_unique<Error>("Esperado um comando ou expressão",
                                           tokens[current].get_column(),
                                           tokens[current].get_line())};
}

bool Parser::match(TokenType type) {
  if (is_at_end()) {
    return false;
  }

  if (tokens[current].get_type() != type) {
    return false;
  }

  current++;
  return true;
}

bool Parser::lookahead_is_declaration() {
  if (is_at_end(current + 2)) {
    return false;
  }

  TokenType next_type = tokens[current + 1].get_type();
  TokenType after_type = tokens[current + 2].get_type();

  return (next_type == TokenType::COLON &&
          (after_type == TokenType::TYPE_NUMBER ||
           after_type == TokenType::TYPE_BOOL ||
           after_type == TokenType::TYPE_STRING));
}

bool Parser::check(TokenType type) {
  if (is_at_end()) {
    return false;
  }

  return tokens[current].get_type() == type;
}

StepResult Parser::parse_function_stmt() {
  if (is_at_end()) {
    return {nullptr, std::make_unique<Error>("Esperado o nome da função",
                                             tokens[current].get_column(),
                                             tokens[current].get_line())};
  }
}

StepResult Parser::parse_if_stmt() {};

StepResult Parser::parse_while_stmt() {};

StepResult Parser::parse_seq_stmt() {};

StepResult Parser::parse_par_stmt() {};

StepResult Parser::parse_c_channel_stmt() {};

StepResult Parser::parse_declaration_stmt() {};

StepResult Parser::parse_assignment_or_call() {};

StepResult Parser::parse_return_stmt() {};

StepResult Parser::parse_break_stmt() {};

StepResult Parser::parse_continue_stmt() {};

StepResult Parser::parse_expression_stmt() {};
