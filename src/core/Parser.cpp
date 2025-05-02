#include "core/Parser.h"
#include "core/Stmt.h"

#include <cassert>

/**
 * @brief If a parse error occurs, this macro will create a new StepResult
 * with a nullptr statement and an Error object with the error message.
 */
#define PARSER_ERROR(_msg)                                                     \
  {nullptr, std::make_unique<Error>(_msg, tokens[current].get_column(),        \
                                    tokens[current].get_line())};

static inline void sync() {}

Token Parser::consume() {
  assert(!is_at_end() && "Trying to consume a token when at end");
  return tokens[current++];
}

Token Parser::previous() {
  assert(current > 0 && "Trying to get previous token when at start");
  return tokens[current - 1];
}

TokenType Parser::peek() {
  assert(!is_at_end() && "Trying to peek a token when at end");
  return tokens[current].get_type();
}

ParseResult Parser::parse() {
  std::vector<std::unique_ptr<Stmt>> statements = {};
  std::vector<std::unique_ptr<Error>> syntax_errors = {};

  while (!is_at_end()) {
    if (match(TokenType::END_OF_FILE)) {
      break;
    }

    StepResult result = parse_statement();
    if (result.syntax_error) {
      syntax_errors.push_back(std::move(result.syntax_error));
      sync();
      continue;
    }

    statements.push_back(std::move(result.statement));
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

  return PARSER_ERROR("Instrução inválida");
}

bool Parser::match(TokenType type) {
  if (is_at_end()) {
    return false;
  }

  if (peek() != type) {
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

  return peek() == type;
}

StepResult Parser::parse_function_stmt() {
  if (is_at_end()) {
    return PARSER_ERROR("Esperado nome da função após 'func'");
  }

  Token function_name = consume();

  if (is_at_end()) {
    return PARSER_ERROR("Esperado '(' após nome da função");
  }
  consume();

  std::vector<Token> param_names = {};
  std::vector<Token> param_types = {};
  while (!is_at_end() && (peek() == TokenType::IDENTIFIER)) {
    param_names.push_back(consume());

    if (is_at_end() || peek() != TokenType::COLON) {
      return PARSER_ERROR("Esperado ':' após nome do parâmetro");
    }
    consume();

    param_types.push_back(consume());

    if (is_at_end()) {
      return PARSER_ERROR("Esperado ',' ou ')' após parâmetro");
    }

    if (peek() == TokenType::COMMA) {
      consume();
    }
  }

  if (is_at_end() || peek() != TokenType::RIGHT_PAREN) {
    return PARSER_ERROR("Esperado ')' após parâmetros");
  }
  consume();

  if (is_at_end() || peek() != TokenType::ARROW) {
    return PARSER_ERROR("Esperado '->' após ')'");
  }
  consume();

  if (is_at_end() ||
      (peek() != TokenType::TYPE_NUMBER && peek() != TokenType::TYPE_BOOL &&
       peek() != TokenType::TYPE_STRING)) {
    return PARSER_ERROR("Esperado tipo de retorno após '->'");
  }
  Token return_type = consume();

  if (is_at_end() || peek() != TokenType::LEFT_BRACE) {
    return PARSER_ERROR("Esperado '{' após tipo de retorno");
  }
  consume();

  std::vector<StmtPtr> body = {};
  while (!is_at_end() && (peek() != TokenType::RIGHT_BRACE)) {
    StepResult result = parse_statement();
    if (result.syntax_error) {
      return result;
    }

    body.push_back(std::move(result.statement));
  }

  if (is_at_end() || peek() != TokenType::RIGHT_BRACE) {
    return PARSER_ERROR("Esperado '}' após corpo da função");
  }
  consume();

  BlockStmt body_block{std::move(body)};
  Params params{std::move(param_names), std::move(param_types)};

  return {std::make_unique<Stmt>(FunctionStmt{
              function_name, params, return_type, std::move(body_block)}),
          nullptr};
}

StepResult Parser::parse_if_stmt() {};

StepResult Parser::parse_while_stmt() {};

StepResult Parser::parse_seq_stmt() {};

StepResult Parser::parse_par_stmt() {};

StepResult Parser::parse_c_channel_stmt() {};

StepResult Parser::parse_declaration_stmt() {
  Token identifier = consume();
  consume(); /* consume ':' */

  Token type = consume();
  consume(); /* consume '=' */

  if (is_at_end()) {
    return PARSER_ERROR("Esperado expressão após '='");
  }

  ExprResult expr = parse_expression();
  if (expr.syntax_error) {
    return {nullptr, std::move(expr.syntax_error)};
  }
  return {std::make_unique<Stmt>(
              DeclarationStmt{identifier, type, std::move(expr.expression)}),
          nullptr};
};

StepResult Parser::parse_assignment_or_call() {};

StepResult Parser::parse_return_stmt() {
  ExprResult expr = parse_expression();
  if (expr.syntax_error) {
    return {nullptr, std::move(expr.syntax_error)};
  }

  return {std::make_unique<Stmt>(ReturnStmt{std::move(expr.expression)}),
          nullptr};
};

StepResult Parser::parse_break_stmt() {
  return {std::make_unique<Stmt>(BreakStmt{}), nullptr};
};

StepResult Parser::parse_continue_stmt() {
  return {std::make_unique<Stmt>(ContinueStmt{}), nullptr};
};
ExprResult Parser::parse_expression() { return parse_disjunction(); }

ExprResult Parser::parse_disjunction() {
  ExprResult left = parse_conjunction();
  if (left.syntax_error)
    return left;

  while (match(TokenType::OR_OR)) {
    Token op = previous();

    ExprResult right = parse_conjunction();
    if (right.syntax_error)
      return right;

    left.expression = std::make_unique<BinaryExpr>(
        std::move(left.expression), op, std::move(right.expression));
  }

  return left;
}

ExprResult Parser::parse_conjunction() {
  ExprResult left = parse_equality();
  if (left.syntax_error)
    return left;

  while (match(TokenType::AND_AND)) {
    Token op = previous();

    ExprResult right = parse_equality();
    if (right.syntax_error)
      return right;

    left.expression = std::make_unique<BinaryExpr>(
        std::move(left.expression), op, std::move(right.expression));
  }

  return left;
}

ExprResult Parser::parse_equality() {
  ExprResult left = parse_comparison();
  if (left.syntax_error)
    return left;

  while (match(TokenType::BANG_EQUAL) || match(TokenType::EQUAL_COMPARE)) {
    Token op = previous();

    ExprResult right = parse_comparison();
    if (right.syntax_error)
      return right;

    left.expression = std::make_unique<BinaryExpr>(
        std::move(left.expression), op, std::move(right.expression));
  }

  return left;
}

ExprResult Parser::parse_comparison() {
  ExprResult left = parse_sum();
  if (left.syntax_error)
    return left;

  while (match(TokenType::GREATER) || match(TokenType::GREATER_EQUAL) ||
         match(TokenType::LESS) || match(TokenType::LESS_EQUAL)) {
    Token op = previous();

    ExprResult right = parse_sum();
    if (right.syntax_error)
      return right;

    left.expression = std::make_unique<BinaryExpr>(
        std::move(left.expression), op, std::move(right.expression));
  }

  return left;
}

ExprResult Parser::parse_sum() {
  ExprResult left = parse_term();
  if (left.syntax_error)
    return left;

  while (match(TokenType::PLUS) || match(TokenType::MINUS)) {
    Token op = previous();

    ExprResult right = parse_term();
    if (right.syntax_error)
      return right;

    left.expression = std::make_unique<BinaryExpr>(
        std::move(left.expression), op, std::move(right.expression));
  }

  return left;
}

ExprResult Parser::parse_term() {
  ExprResult left = parse_unary();
  if (left.syntax_error)
    return left;

  while (match(TokenType::STAR) || match(TokenType::SLASH) ||
         match(TokenType::PERCENT)) {
    Token op = previous();

    ExprResult right = parse_unary();
    if (right.syntax_error)
      return right;

    left.expression = std::make_unique<BinaryExpr>(
        std::move(left.expression), op, std::move(right.expression));
  }

  return left;
}

ExprResult Parser::parse_unary() {
  if (match(TokenType::BANG) || match(TokenType::MINUS)) {
    Token op = previous();
    ExprResult right = parse_unary();
    if (right.syntax_error)
      return right;

    return {std::make_unique<UnaryExpr>(op, std::move(right.expression)),
            nullptr};
  }

  return parse_primary();
}

ExprResult Parser::parse_primary() {
  if (match(TokenType::FALSE_LITERAL)) {
    return {std::make_unique<LiteralExpr>(previous()), nullptr};
  }

  if (match(TokenType::TRUE_LITERAL)) {
    return {std::make_unique<LiteralExpr>(previous()), nullptr};
  }

  if (match(TokenType::NUMBER) || match(TokenType::STRING_LITERAL)) {
    return {std::make_unique<LiteralExpr>(previous()), nullptr};
  }

  if (match(TokenType::IDENTIFIER)) {
    if (peek() == TokenType::LEFT_PAREN) {
      return parse_local();
    }
  }

  if (match(TokenType::LEFT_PAREN)) {
    ExprResult inner = parse_expression();
    if (inner.syntax_error)
      return inner;

    if (!match(TokenType::RIGHT_PAREN)) {
      return PARSER_ERROR("Esperado ')' após expressão");
    }

    return {std::make_unique<GroupingExpr>(std::move(inner.expression)),
            nullptr};
  }

  return PARSER_ERROR("Token inesperado em expressão");
}

ExprResult Parser::parse_local() {
  if (!match(TokenType::IDENTIFIER)) {
    return PARSER_ERROR("Esperado identificador no início de <local>");
  }

  ExprPtr expr = std::make_unique<VariableExpr>(previous());

  while (true) {
    if (match(TokenType::DOT)) {
      if (!match(TokenType::IDENTIFIER)) {
        return PARSER_ERROR("Esperado identificador após '.'");
      }
      expr = std::make_unique<GetExpr>(std::move(expr), previous());
    } else if (match(TokenType::LEFT_BRACKET)) {
      if (!match(TokenType::NUMBER)) {
        return PARSER_ERROR("Esperado número como índice");
      }
      Token index = previous();

      if (!match(TokenType::RIGHT_BRACKET)) {
        return PARSER_ERROR("Esperado ']' após índice");
      }

      expr = std::make_unique<IndexExpr>(std::move(expr), tokens[current - 2],
                                         index);

    } else if (match(TokenType::LEFT_PAREN)) {
      std::vector<ExprPtr> args;

      if (!check(TokenType::RIGHT_PAREN)) {
        while (true) {
          ExprResult arg = parse_expression();
          if (arg.syntax_error)
            return arg;

          args.push_back(std::move(arg.expression));

          if (!match(TokenType::COMMA))
            break;
        }
      }

      if (!match(TokenType::RIGHT_PAREN)) {
        return PARSER_ERROR("Esperado ')' após argumentos");
      }

      expr = std::make_unique<CallExpr>(std::move(expr), previous(),
                                        std::move(args));
    } else {
      break;
    }
  }

  return {std::move(expr), nullptr};
}
