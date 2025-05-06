#include "../include/core/Parser.h"
#include "../include/core/Stmt.h"
#include "../include/core/Expr.h"
#include "../include/core/Error.h"
#include "../include/enum/TokenType.h"

#include <cassert>
#include <memory>
#include <vector>
#include <optional>
#include <string>

/**
 * @brief If a parse error occurs, this macro will create a new StepResult
 * with a nullptr statement and an Error object with the error message.
 */
#define STMT_PARSER_ERROR(_msg)                                                \
  StepResult{nullptr, std::make_unique<Error>(_msg, tokens[current].get_column(), \
                                    tokens[current].get_line())}

/**
 * @brief Similar, but to expr.
 */
#define EXPR_PARSER_ERROR(_msg)                                                \
  ExprResult{nullptr, std::make_unique<Error>(_msg, tokens[current].get_column(), \
                                   tokens[current].get_line())}

void Parser::sync() {
  int brace_level = 0;
  bool at_statement_start = true;

  while (!is_at_end()) {
    TokenType type = peek();

    if (type == TokenType::LEFT_BRACE) {
      brace_level++;
      at_statement_start = true;
    } else if (type == TokenType::RIGHT_BRACE) {
      brace_level--;
      if (brace_level < 0) {
        brace_level = 0;
      }
      at_statement_start = true;
    }

    if (brace_level == 0) {
      if (at_statement_start &&
          (type == TokenType::FUNC || type == TokenType::IF || 
           type == TokenType::WHILE || type == TokenType::FOR || 
           type == TokenType::SEQ || type == TokenType::PAR || 
           type == TokenType::C_CHANNEL || type == TokenType::RETURN || 
           type == TokenType::BREAK || type == TokenType::CONTINUE || 
           type == TokenType::IDENTIFIER)) {
        return;
      }
      if (type == TokenType::RIGHT_BRACE) {
        consume();
        return;
      }
    }

    at_statement_start = (type == TokenType::LEFT_BRACE) ||
                         (brace_level == 0 && (type == TokenType::RIGHT_BRACE ||
                                               type == TokenType::END_OF_FILE));
    consume();
  }
}

void Parser::rewind(int steps) {
  assert(steps > 0 && "Tentativa de retroceder 0 ou menos passos");
  assert(current >= static_cast<unsigned int>(steps) && "Tentativa de retroceder além do início");
  current -= steps;
}

Token Parser::consume() {
  if (!is_at_end()) {
    Token token = tokens[current++];
    return token;
  }
  return Token(TokenType::END_OF_FILE, "", tokens.empty() ? 0 : tokens.back().get_line(), tokens.empty() ? 0 : tokens.back().get_column() + 1);
}

Token Parser::previous() {
  assert(current > 0 && "Tentativa de obter token anterior no início");
  return tokens[current - 1];
}

TokenType Parser::peek() {
  if (is_at_end()) {
    return TokenType::END_OF_FILE;
  }
  return tokens[current].get_type();
}

StepResult Parser::parse_block() {
  std::vector<StmtPtr> body;

  while (!is_at_end() && peek() != TokenType::RIGHT_BRACE) {
    StepResult result = parse_statement();
    if (result.syntax_error) {
      return result;
    }
    body.push_back(std::move(result.statement));
  }

  if (is_at_end() || peek() != TokenType::RIGHT_BRACE) {
    return STMT_PARSER_ERROR("Esperado '}' para fechar o bloco");
  }
  consume();

  BlockStmt block{std::move(body)};
  return {std::make_unique<Stmt>(std::move(block)), nullptr};
}

ParseResult Parser::parse() {
  std::vector<std::unique_ptr<Stmt>> statements;
  std::vector<std::unique_ptr<Error>> syntax_errors;

  while (!is_at_end()) {
    if (peek() == TokenType::END_OF_FILE) {
      break;
    }
    StepResult result = parse_statement();
    if (result.syntax_error) {
      syntax_errors.push_back(std::move(result.syntax_error));
      sync();
      continue;
    }

    if (result.statement) {
        statements.push_back(std::move(result.statement));
    }
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
  if (match(TokenType::FOR)) { 
    return parse_for_stmt(); 
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
    if (!is_at_end(current + 1) && tokens[current + 1].get_type() == TokenType::EQUAL_ASSIGN) {
       return parse_assignment();
    }
    return parse_expression_statement();
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
  if (match(TokenType::LEFT_BRACE)) {
    rewind(1);
    return parse_block();
  }
  return STMT_PARSER_ERROR("Instrução inválida ou inesperada");
}

bool Parser::match(TokenType type) {
  if (is_at_end() || peek() != type) {
    return false;
  }
  current++;
  return true;
}

bool Parser::lookahead_is_declaration() {
  if (is_at_end(current + 1)) {
    return false;
  }
  return tokens[current + 1].get_type() == TokenType::COLON;
}

bool Parser::check(TokenType type) {
  return !is_at_end() && peek() == type;
}

StepResult Parser::parse_function_stmt() {
  if (is_at_end() || peek() != TokenType::IDENTIFIER) {
    return STMT_PARSER_ERROR("Esperado nome da função após 'func'");
  }
  Token function_name = consume();

  if (is_at_end() || peek() != TokenType::LEFT_PAREN) {
    return STMT_PARSER_ERROR("Esperado '(' após nome da função");
  }
  consume();
  
  std::vector<Token> param_names;
  std::vector<Token> param_types;
  if (!check(TokenType::RIGHT_PAREN)) {
    do {
      if (is_at_end() || peek() != TokenType::IDENTIFIER) {
        return STMT_PARSER_ERROR("Esperado nome do parâmetro");
      }
      param_names.push_back(consume());

      if (is_at_end() || peek() != TokenType::COLON) {
        return STMT_PARSER_ERROR("Esperado ':' após nome do parâmetro '" + param_names.back().get_lexeme() + "'");
      }
      consume();

      if (is_at_end() || !(check(TokenType::TYPE_NUMBER) ||
                           check(TokenType::TYPE_BOOL) ||
                           check(TokenType::TYPE_STRING))) {
        return STMT_PARSER_ERROR("Esperado tipo ('number', 'bool', 'string') para o parâmetro '" + param_names.back().get_lexeme() + "'");
      }
      param_types.push_back(consume());

    } while (match(TokenType::COMMA));
  }

  if (is_at_end() || peek() != TokenType::RIGHT_PAREN) {
    return STMT_PARSER_ERROR("Esperado ')' após lista de parâmetros");
  }
  consume();

  if (is_at_end() || peek() != TokenType::ARROW) {
    return STMT_PARSER_ERROR("Esperado '->' após ')' para indicar tipo de retorno");
  }
  consume();

  if (is_at_end() || !(check(TokenType::TYPE_NUMBER) ||
                       check(TokenType::TYPE_BOOL) ||
                       check(TokenType::TYPE_STRING) ||
                       check(TokenType::TYPE_NONE))) {
    return STMT_PARSER_ERROR("Esperado tipo de retorno ('number', 'bool', 'string' ou 'void') após '->'");
  }
  Token return_type = consume();

  if (is_at_end() || peek() != TokenType::LEFT_BRACE) {
    return STMT_PARSER_ERROR("Esperado '{' antes do corpo da função '" + function_name.get_lexeme() + "'");
  }
  consume();

  StepResult body_result = parse_block();
  if (body_result.syntax_error) {
    return body_result;
  }

  if (!body_result.statement || body_result.statement->get_type() != StmtType::BLOCK) {
    return STMT_PARSER_ERROR("Erro interno: parse_block não retornou um bloco válido");
  }
  BlockStmt body_block = body_result.statement->move_block_stmt();

  Params params{std::make_optional(std::move(param_names)), std::make_optional(std::move(param_types))};
  
  return {std::make_unique<Stmt>(FunctionStmt{function_name, std::move(params), return_type, std::move(body_block)}), nullptr};
}

StepResult Parser::parse_if_stmt() {
  StepResult else_body_result{nullptr, nullptr};
  bool has_else_branch = false;
  
  if (is_at_end() || peek() != TokenType::LEFT_PAREN) {
    return STMT_PARSER_ERROR("Esperado '(' após 'if'");
  }
  consume();

  ExprResult condition = parse_expression();
  if (condition.syntax_error) {
    return {nullptr, std::move(condition.syntax_error)};
  }

  if (is_at_end() || peek() != TokenType::RIGHT_PAREN) {
    return STMT_PARSER_ERROR("Esperado ')' após condição do 'if'");
  }
  consume();

  if (is_at_end() || peek() != TokenType::LEFT_BRACE) {
    return STMT_PARSER_ERROR("Esperado '{' após condição do 'if'");
  }
  consume();

  StepResult body_result = parse_block();
  if (body_result.syntax_error) {
    return body_result;
  }

  if (!is_at_end() && peek() == TokenType::ELSE) {
    consume();
    has_else_branch = true;

    if (is_at_end() || peek() != TokenType::LEFT_BRACE) {
      return STMT_PARSER_ERROR("Esperado '{' após 'else'");
    }
    consume();

    else_body_result = parse_block();
    if (else_body_result.syntax_error) {
      return else_body_result;
    }
  }

  if (!body_result.statement || body_result.statement->get_type() != StmtType::BLOCK) {
    return STMT_PARSER_ERROR("Erro interno: Bloco 'then' inválido");
  }
  BlockStmt then_block = body_result.statement->move_block_stmt();

  std::optional<BlockStmt> else_block_opt = std::nullopt;
  if (has_else_branch) {
    if (!else_body_result.statement || else_body_result.statement->get_type() != StmtType::BLOCK) {
      return STMT_PARSER_ERROR("Erro interno: Bloco 'else' inválido");
    }
    else_block_opt = else_body_result.statement->move_block_stmt();
  }

  return {std::make_unique<Stmt>(IfStmt{std::move(condition.expression), std::move(then_block), std::move(else_block_opt), has_else_branch}), nullptr};
}

StepResult Parser::parse_while_stmt() {
  if (is_at_end() || peek() != TokenType::LEFT_PAREN) {
    return STMT_PARSER_ERROR("Esperado '(' após 'while'");
  }
  consume();

  ExprResult condition = parse_expression();
  if (condition.syntax_error) {
    return {nullptr, std::move(condition.syntax_error)};
  }

  if (is_at_end() || peek() != TokenType::RIGHT_PAREN) {
    return STMT_PARSER_ERROR("Esperado ')' após condição do 'while'");
  }
  consume();

  if (is_at_end() || peek() != TokenType::LEFT_BRACE) {
    return STMT_PARSER_ERROR("Esperado '{' após ')' do 'while'");
  }
  consume();

  StepResult body_result = parse_block();
  if (body_result.syntax_error) {
    return body_result;
  }

  if (!body_result.statement || body_result.statement->get_type() != StmtType::BLOCK) {
    return STMT_PARSER_ERROR("Erro interno: Corpo do 'while' inválido");
  }
  BlockStmt body_block = body_result.statement->move_block_stmt();

  return {std::make_unique<Stmt>(WhileStmt{std::move(condition.expression), std::move(body_block)}), nullptr};
}

StepResult Parser::parse_for_stmt() {
  Token for_token = previous();

  if (!match(TokenType::LEFT_PAREN)) {
    return STMT_PARSER_ERROR("Esperado '(' após 'for'");
  }
  
  std::unique_ptr<Stmt> initializer = nullptr;
  if (peek() != TokenType::SEMICOLON) {
      if (check(TokenType::IDENTIFIER) && !lookahead_is_declaration()) {
          StepResult init_res = parse_assignment();
          if (init_res.syntax_error) {
            return init_res;
          }
          if (!init_res.statement || init_res.statement->get_type() != StmtType::ASSIGNMENT) {
            return STMT_PARSER_ERROR("Erro interno: parse_assignment não retornou atribuição válida para inicializador do 'for'");
          }
          initializer = std::move(init_res.statement);
      } else {
        return STMT_PARSER_ERROR("Esperado ';' ou atribuição como inicializador do 'for'");
      }
  }
  if (!match(TokenType::SEMICOLON)) {
    return STMT_PARSER_ERROR("Esperado ';' após inicializador do 'for'");
  }
  
  ExprPtr condition = nullptr;
  if (peek() != TokenType::SEMICOLON) {
      ExprResult cond_res = parse_expression();
      if (cond_res.syntax_error) {
        return {nullptr, std::move(cond_res.syntax_error)};
      }
      condition = std::move(cond_res.expression);
  }

  if (!match(TokenType::SEMICOLON)) {
    return STMT_PARSER_ERROR("Esperado ';' após condição do 'for'");
  }

  std::unique_ptr<Stmt> increment = nullptr;

  if (peek() != TokenType::RIGHT_PAREN) {
      if (check(TokenType::IDENTIFIER) && !lookahead_is_declaration()) {
          StepResult incr_res = parse_assignment(); 
          if (incr_res.syntax_error) {
            return incr_res;
          }

          if (!incr_res.statement || incr_res.statement->get_type() != StmtType::ASSIGNMENT) {
            return STMT_PARSER_ERROR("Erro interno: parse_assignment não retornou atribuição válida para incremento do 'for'");
          }

          increment = std::move(incr_res.statement);
      } else {
        return STMT_PARSER_ERROR("Esperado ')' ou atribuição como incremento do 'for'");
      }
  }

  if (!match(TokenType::RIGHT_PAREN)) {
    return STMT_PARSER_ERROR("Esperado ')' após cláusulas do 'for'");
  }

  if (!check(TokenType::LEFT_BRACE)) {
    return STMT_PARSER_ERROR("Esperado '{' antes do corpo do 'for'");
  }
  consume();

  StepResult body_res = parse_block();
  if (body_res.syntax_error) {
    return body_res;
  }

  if (!body_res.statement || body_res.statement->get_type() != StmtType::BLOCK) {
    return STMT_PARSER_ERROR("Erro interno: parse_block não retornou bloco válido para corpo do 'for'");
  }

  BlockStmt body_block = body_res.statement->move_block_stmt();

  return {std::make_unique<Stmt>(ForStmt{for_token, 
          std::move(initializer), std::move(condition), 
          std::move(increment), std::move(body_block)}), 
          nullptr};
}

StepResult Parser::parse_seq_stmt() {
  if (is_at_end() || peek() != TokenType::LEFT_BRACE) {
    return STMT_PARSER_ERROR("Esperado '{' após 'seq'");
  }
  consume();

  StepResult body_result = parse_block(); 
  if (body_result.syntax_error) {
    return body_result;
  }

  if (!body_result.statement || body_result.statement->get_type() != StmtType::BLOCK) {
    return STMT_PARSER_ERROR("Erro interno: Corpo do 'seq' inválido");
  }
  BlockStmt body_block = body_result.statement->move_block_stmt();

  return {std::make_unique<Stmt>(SeqStmt{std::move(body_block)}), nullptr};
}

StepResult Parser::parse_par_stmt() {
  if (is_at_end() || peek() != TokenType::LEFT_BRACE) {
    return STMT_PARSER_ERROR("Esperado '{' após 'par'");
  }
  consume();

  StepResult body_result = parse_block();
  if (body_result.syntax_error) {
    return body_result;
  }

  if (!body_result.statement || body_result.statement->get_type() != StmtType::BLOCK) {
    return STMT_PARSER_ERROR("Erro interno: Corpo do 'par' inválido");
  }

  BlockStmt body_block = body_result.statement->move_block_stmt();

  return {std::make_unique<Stmt>(ParStmt{std::move(body_block)}), nullptr};
}

StepResult Parser::parse_c_channel_stmt() {
  if (is_at_end() || peek() != TokenType::IDENTIFIER) {
    return STMT_PARSER_ERROR("Esperado identificador (nome do canal) após 'c_channel'");
  }
  Token channel_name = consume();

  if (is_at_end() || peek() != TokenType::IDENTIFIER) {
    return STMT_PARSER_ERROR("Esperado primeiro identificador associado após nome do canal '" + channel_name.get_lexeme() + "'");
  }
  Token id_1 = consume();

  if (is_at_end() || peek() != TokenType::IDENTIFIER) {
    return STMT_PARSER_ERROR("Esperado segundo identificador associado após '" + id_1.get_lexeme() + "'");
  }
  Token id_2 = consume();

  return {std::make_unique<Stmt>(CChannelStmt{channel_name, id_1, id_2}), nullptr};
}

StepResult Parser::parse_declaration_stmt() {
  Token identifier = consume(); 

  if (is_at_end() || peek() != TokenType::COLON) {
    return STMT_PARSER_ERROR("Erro interno: Esperado ':' após ID na declaração");
  }
  consume();

  if (is_at_end() || (!check(TokenType::TYPE_NUMBER) && !check(TokenType::TYPE_BOOL) && !check(TokenType::TYPE_STRING))) {
    return STMT_PARSER_ERROR("Esperado tipo ('number', 'bool', 'string') após ':'");
  }
  Token type = consume();

  if (is_at_end() || peek() != TokenType::EQUAL_ASSIGN) {
    return STMT_PARSER_ERROR("Esperado '=' após tipo na declaração");
  }
  consume();

  if (is_at_end()) {
    return STMT_PARSER_ERROR("Esperado expressão após '=' na declaração");
  }
  ExprResult expr = parse_expression();
  if (expr.syntax_error) {
    return {nullptr, std::move(expr.syntax_error)};
  }
  
  return {std::make_unique<Stmt>(DeclarationStmt{identifier, type, std::move(expr.expression)}), nullptr};
}

StepResult Parser::parse_assignment() {
  if (is_at_end() || peek() != TokenType::IDENTIFIER) {
    return STMT_PARSER_ERROR("Erro interno: Esperado identificador no início da atribuição");
  }
  Token identifier = consume();

  if (is_at_end() || peek() != TokenType::EQUAL_ASSIGN) {
    return STMT_PARSER_ERROR("Esperado '=' após identificador '" + identifier.get_lexeme() + "'");
  }
  consume();

  if (is_at_end()) {
    return STMT_PARSER_ERROR("Esperado expressão após '=' na atribuição");
  }
  ExprResult expr = parse_expression();
  if (expr.syntax_error) {
    return {nullptr, std::move(expr.syntax_error)};
  }

  return {std::make_unique<Stmt>(AssignmentStmt{identifier, std::move(expr.expression)}), nullptr};
}

StepResult Parser::parse_return_stmt() {
  ExprResult expr = parse_expression();
  if (expr.syntax_error) {
    return {nullptr, std::move(expr.syntax_error)};
  }
  return {std::make_unique<Stmt>(ReturnStmt{std::move(expr.expression)}), nullptr};
}

StepResult Parser::parse_break_stmt() {
  Token keyword = previous();
  auto stmt = std::make_unique<Stmt>(BreakStmt{keyword});
  return {std::move(stmt), nullptr};
}

StepResult Parser::parse_continue_stmt() {
  Token keyword = previous();
  auto stmt = std::make_unique<Stmt>(ContinueStmt{keyword});
  return {std::move(stmt), nullptr};
}

ExprResult Parser::parse_expression() {
  return parse_disjunction();
}
ExprResult Parser::parse_disjunction() {
  ExprResult left = parse_conjunction();
  if (left.syntax_error) {
    return left;
  }
  while (match(TokenType::OR_OR)) {
    Token op = previous();
    ExprResult right = parse_conjunction();
    if (right.syntax_error) {
      return right;
    }
    left.expression = std::make_unique<BinaryExpr>(std::move(left.expression), op, std::move(right.expression));
  }
  return left;
}

ExprResult Parser::parse_conjunction() {
  ExprResult left = parse_equality();
  if (left.syntax_error) {
    return left;
  }
  while (match(TokenType::AND_AND)) {
    Token op = previous();
    ExprResult right = parse_equality();
    if (right.syntax_error) {
      return right;
    }
    left.expression = std::make_unique<BinaryExpr>(std::move(left.expression), op, std::move(right.expression));
  }
  return left;
}

ExprResult Parser::parse_equality() {
  ExprResult left = parse_comparison();
  if (left.syntax_error) {
    return left;
  }
  while (match(TokenType::BANG_EQUAL) || match(TokenType::EQUAL_COMPARE)) {
    Token op = previous();
    ExprResult right = parse_comparison();
    if (right.syntax_error) {
      return right;
    }
      left.expression = std::make_unique<BinaryExpr>(std::move(left.expression), op, std::move(right.expression));
  }
  return left;
}

ExprResult Parser::parse_comparison() {
  ExprResult left = parse_sum();
  if (left.syntax_error) {
    return left;
  }
  while (match(TokenType::GREATER) || match(TokenType::GREATER_EQUAL) || match(TokenType::LESS) || match(TokenType::LESS_EQUAL)) {
    Token op = previous();
    ExprResult right = parse_sum();
    if (right.syntax_error) {
      return right;
    }
    left.expression = std::make_unique<BinaryExpr>(std::move(left.expression), op, std::move(right.expression));
  }
  return left;
}

ExprResult Parser::parse_sum() {
  ExprResult left = parse_term();
  if (left.syntax_error) {
    return left;
  }
  while (match(TokenType::PLUS) || match(TokenType::MINUS)) {
    Token op = previous();
    ExprResult right = parse_term();
    if (right.syntax_error) {
      return right;
    }
    left.expression = std::make_unique<BinaryExpr>(std::move(left.expression), op, std::move(right.expression));
  }
  return left;
}

ExprResult Parser::parse_term() {
  ExprResult left = parse_unary();
  if (left.syntax_error) {
    return left;
  }
  while (match(TokenType::STAR) || match(TokenType::SLASH) || match(TokenType::PERCENT)) {
    Token op = previous();
    ExprResult right = parse_unary();
    if (right.syntax_error) {
      return right;
    }
    left.expression = std::make_unique<BinaryExpr>(std::move(left.expression), op, std::move(right.expression));
  }
  return left;
}

ExprResult Parser::parse_unary() {
  if (match(TokenType::BANG) || match(TokenType::MINUS)) {
    Token op = previous();
    ExprResult right = parse_unary();
    if (right.syntax_error) {
      return right;
    }
    return {std::make_unique<UnaryExpr>(op, std::move(right.expression)), nullptr};
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
  if (match(TokenType::NUMBER)) {
    return {std::make_unique<LiteralExpr>(previous()), nullptr};
  }
  if (match(TokenType::STRING_LITERAL)) {
    return {std::make_unique<LiteralExpr>(previous()), nullptr};
  }
  if (match(TokenType::IDENTIFIER)) {
    rewind(1);
    return parse_local();
  }
  if (match(TokenType::LEFT_PAREN)) {
    ExprResult inner = parse_expression();
    if (inner.syntax_error) {
      return inner;
    }
    if (!match(TokenType::RIGHT_PAREN)) {
      return EXPR_PARSER_ERROR("Esperado ')' após expressão em parênteses");
    }
    return {std::make_unique<GroupingExpr>(std::move(inner.expression)), nullptr};
  }

  if (is_at_end()) {
    return EXPR_PARSER_ERROR("Expressão incompleta no final do arquivo");
  }
  return EXPR_PARSER_ERROR("Token inesperado '" + tokens[current].get_lexeme() + "' ao iniciar expressão primária");
}

ExprResult Parser::parse_local() {
  if (!match(TokenType::IDENTIFIER)) {
    return EXPR_PARSER_ERROR("Esperado identificador no início de <local>");
  }
  ExprPtr expr = std::make_unique<VariableExpr>(previous());
  while (true) {
    if (match(TokenType::DOT)) {
      if (!match(TokenType::IDENTIFIER)) {
        return EXPR_PARSER_ERROR("Esperado identificador após '.'");
      }
      expr = std::make_unique<GetExpr>(std::move(expr), previous());
    } else if (match(TokenType::LEFT_BRACKET)) {
      if (!match(TokenType::NUMBER)) {
        return EXPR_PARSER_ERROR("Esperado número como índice dentro de '[]'");
      }
      Token index = previous();
      Token bracket = tokens[current - 2];
      if (!match(TokenType::RIGHT_BRACKET)) {
        return EXPR_PARSER_ERROR("Esperado ']' após índice");
      }
      expr = std::make_unique<IndexExpr>(std::move(expr), bracket, index);
    } else if (match(TokenType::LEFT_PAREN)) {
      Token paren = previous();
      std::vector<ExprPtr> args;
      if (!check(TokenType::RIGHT_PAREN)) {
        do {
          ExprResult arg = parse_expression();
          if (arg.syntax_error) {
            return arg;
          }
          args.push_back(std::move(arg.expression));
        } while (match(TokenType::COMMA));
      }
      if (!match(TokenType::RIGHT_PAREN)) {
        return EXPR_PARSER_ERROR("Esperado ')' após argumentos da chamada");
      }
      expr = std::make_unique<CallExpr>(std::move(expr), paren, std::move(args));
    } else {
      break;
    }
  }
  return {std::move(expr), nullptr};
}

StepResult Parser::parse_expression_statement() {
    ExprResult expr_res = parse_expression();
    if (expr_res.syntax_error) {
        return {nullptr, std::move(expr_res.syntax_error)};
    }
    return {std::make_unique<Stmt>(ExpressionStmt{std::move(expr_res.expression)}), nullptr};
}
