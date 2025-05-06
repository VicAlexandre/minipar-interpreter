#include "../include/core/Semantic.h"
#include "../include/core/Error.h"
#include "../include/core/Token.h"
#include <iostream>
#include <memory>
#include <stack>
#include <stdexcept>
#include <string>
#include <typeinfo>
#include <unordered_map>
#include <utility>
#include <vector>

SemanticAnalyzer::Symbol::Symbol(const Token &n, const Token &t, bool ic)
    : name(n), type(t), is_constant(ic) {}

Token SemanticAnalyzer::create_type_token(TokenType type) {
  std::string lexeme = "none";
  TokenType actual_type = TokenType::TYPE_NONE;
  switch (type) {
  case TokenType::TYPE_NUMBER:
    lexeme = "number";
    actual_type = TokenType::TYPE_NUMBER;
    break;
  case TokenType::TYPE_STRING:
    lexeme = "string";
    actual_type = TokenType::TYPE_STRING;
    break;
  case TokenType::TYPE_BOOL:
    lexeme = "bool";
    actual_type = TokenType::TYPE_BOOL;
    break;
  case TokenType::TYPE_NONE:
    lexeme = "none";
    actual_type = TokenType::TYPE_NONE;
    break;
  default:
    std::cerr << "[WARN] create_type_token chamado com TokenType inesperado: "
              << static_cast<int>(type) << std::endl;
    break;
  }
  return Token(actual_type, lexeme, 0, 0);
}

SemanticAnalyzer::SemanticAnalyzer() {
  push_scope();
  register_builtin_functions();
}

void SemanticAnalyzer::register_builtin_functions() {
  builtin_functions["print"] = {"print", {}, TokenType::TYPE_NONE, false, 1};
  builtin_functions["input"] = {"input", {TokenType::TYPE_STRING}, TokenType::TYPE_STRING, false, 0};
  builtin_functions["to_number"] = {
      "to_number", {TokenType::TYPE_NONE}, TokenType::TYPE_NUMBER};
  builtin_functions["to_string"] = {
      "to_string", {TokenType::TYPE_NONE}, TokenType::TYPE_STRING};
  builtin_functions["to_bool"] = {
      "to_bool", {TokenType::TYPE_NONE}, TokenType::TYPE_BOOL};
  builtin_functions["sleep"] = {
      "sleep", {TokenType::TYPE_NUMBER}, TokenType::TYPE_NONE};
  builtin_functions["len"] = {
      "len", {TokenType::TYPE_NONE}, TokenType::TYPE_NUMBER};
  builtin_functions["send"] = {"send",
                               {TokenType::TYPE_NONE, TokenType::TYPE_NONE},
                               TokenType::TYPE_BOOL};
  builtin_functions["close"] = {
      "close", {TokenType::TYPE_NONE}, TokenType::TYPE_BOOL};
  builtin_functions["isalpha"] = {
      "isalpha", {TokenType::TYPE_STRING}, TokenType::TYPE_BOOL};
  builtin_functions["isnum"] = {
      "isnum", {TokenType::TYPE_STRING}, TokenType::TYPE_BOOL};
}

std::vector<std::unique_ptr<Error>>
SemanticAnalyzer::analyze(std::vector<std::unique_ptr<Stmt>> &statements) {
  errors.clear();
  function_table.clear();
  function_symbols.clear();
  all_declared_symbols.clear();

  for (size_t i = 0; i < statements.size(); ++i) {
    if (statements[i] && statements[i]->get_type() == StmtType::FUNCTION) {
      FunctionStmt &func_stmt = statements[i]->get_function_stmt();
      if (builtin_functions.count(func_stmt.name.get_lexeme())) {
        report_error("Nome de função '" + func_stmt.name.get_lexeme() +
                         "' colide com função built-in.",
                     func_stmt.name);
      } else {
        register_function(func_stmt);
        function_symbols[func_stmt.name.get_lexeme()] = {};
      }
    }
  }

  for (size_t i = 0; i < statements.size(); ++i) {
    try {
      if (statements[i]) {
        visit(statements[i].get());
      } else {
        report_error("Statement nulo encontrado na lista principal (índice " +
                         std::to_string(i) + ")",
                     0, 0);
      }
    } catch (const std::exception &e) {
      std::string msg = "Erro interno (exceção std) no statement " +
                        std::to_string(i) + ": " + e.what();
      report_error(msg, 0, 0);
    } catch (...) {
      std::string msg = "Erro interno (exceção desconhecida) no statement " +
                        std::to_string(i);
      report_error(msg, 0, 0);
    }
  }

  return std::move(errors);
}

void SemanticAnalyzer::push_scope() { scopes.emplace_back(); }

void SemanticAnalyzer::pop_scope() {
  if (!scopes.empty()) {
    scopes.pop_back();
  } else {
    report_error("Tentativa de desempilhar escopo vazio (erro interno)", 0, 0);
  }
}

bool SemanticAnalyzer::declare_symbol(const Symbol &symbol) {
  if (scopes.empty()) {
    report_error(
        "Tentativa de declarar símbolo sem escopo ativo (erro interno)",
        symbol.name);
    return false;
  }
  auto &current_scope = scopes.back();
  const std::string &name = symbol.name.get_lexeme();
  if (current_scope.count(name)) {
    return false;
  }
  current_scope.emplace(name, symbol);
  all_declared_symbols.emplace_back(scopes.size() - 1, symbol);
  return true;
}

SemanticAnalyzer::Symbol *
SemanticAnalyzer::find_symbol(const std::string &name) {
  for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
    auto &scope_map = *it;
    auto found = scope_map.find(name);
    if (found != scope_map.end()) {
      return &(found->second);
    }
  }
  return nullptr;
}

bool SemanticAnalyzer::check_types(const Token &expected_token, const Token &actual_token) {
  TokenType expected = expected_token.get_type();
  TokenType actual = actual_token.get_type();

  if (expected == TokenType::TYPE_NONE) { 
      return actual == TokenType::TYPE_NONE;
  } else {
      return (actual != TokenType::TYPE_NONE) && (expected == actual);
  }
}

bool SemanticAnalyzer::is_numeric_type(const Token &type) {
  return type.get_type() == TokenType::TYPE_NUMBER;
}
bool SemanticAnalyzer::is_boolean_type(const Token &type) {
  TokenType tt = type.get_type();
  return tt == TokenType::TYPE_BOOL || tt == TokenType::TRUE_LITERAL ||
         tt == TokenType::FALSE_LITERAL;
}
bool SemanticAnalyzer::is_valid_type(const Token &type_token) {
  TokenType type = type_token.get_type();
  return type == TokenType::TYPE_NUMBER || type == TokenType::TYPE_STRING ||
         type == TokenType::TYPE_BOOL ||
         type == TokenType::TYPE_NONE;
}

bool SemanticAnalyzer::is_array_type(const Token &type_token [[maybe_unused]]) {
  return false;
}
Token SemanticAnalyzer::get_array_element_type(const Token &array_type_token
                                                [[maybe_unused]]) {
  return create_type_token(TokenType::TYPE_NONE);
}

void SemanticAnalyzer::register_function(const FunctionStmt &func) {
  const std::string &func_name = func.name.get_lexeme();
  if (func_name.empty()) {
    report_error("Nome de função inválido (vazio).", func.name);
    return;
  }
  if (function_table.count(func_name)) {
    report_error("Redefinição da função '" + func_name + "'.", func.name);
    return;
  }
  if (!is_valid_type(func.return_type)) {
    report_error("Tipo de retorno inválido '" + func.return_type.get_lexeme() +
                     "' para a função '" + func_name + "'.",
                 func.return_type);
    return;
  }
  if (func.params.param_types) {
    size_t num_params = func.params.param_types->size();
    if (func.params.param_names &&
        func.params.param_names->size() != num_params) {
      report_error(
          "Inconsistência entre número de nomes e tipos de parâmetros para '" +
              func_name + "'",
          func.name);
      return;
    }
    for (size_t i = 0; i < num_params; ++i) {
      const auto &param_type = (*func.params.param_types)[i];
      if (!is_valid_type(param_type)) {
        const Token *param_name_token =
            func.params.param_names ? &(*func.params.param_names)[i] : nullptr;
        report_error("Tipo de parâmetro inválido '" + param_type.get_lexeme() +
                         "' na função '" + func_name + "'.",
                     param_name_token ? *param_name_token : param_type);
        return;
      }
    }
  } else if (func.params.param_names && !func.params.param_names->empty()) {
    report_error("Nomes de parâmetros fornecidos sem tipos para função '" +
                     func_name + "'",
                 func.name);
    return;
  }
  function_table[func_name] = const_cast<FunctionStmt *>(&func);
}

void SemanticAnalyzer::visit(Stmt *stmt) {
  if (!stmt) {
    report_error("Tentativa de visitar um statement nulo (erro interno)", 0, 0);
    return;
  }
  context_stack.push(stmt);

  switch (stmt->get_type()) {
  case StmtType::DECLARATION:
    visit_declaration(stmt->get_decl_stmt());
    break;
  case StmtType::ASSIGNMENT:
    visit_assignment(stmt->get_assign_stmt());
    break;
  case StmtType::RETURN:
    visit_return(stmt->get_return_stmt());
    break;
  case StmtType::BREAK:
    visit_break(stmt->get_break_stmt());
    break;
  case StmtType::CONTINUE:
    visit_continue(stmt->get_continue_stmt());
    break;
  case StmtType::FUNCTION:
    visit_function(stmt->get_function_stmt());
    break;
  case StmtType::IF:
    visit_if(stmt->get_if_stmt());
    break;
  case StmtType::WHILE:
    visit_while(stmt->get_while_stmt());
    break;
  case StmtType::FOR:
    visit_for(stmt->get_for_stmt());
    break;
  case StmtType::SEQ:
    visit_seq(stmt->get_seq_stmt());
    break;
  case StmtType::PAR:
    visit_par(stmt->get_par_stmt());
    break;
  case StmtType::CCHANNEL:
    visit_cchannel(stmt->get_c_channel_stmt());
    break;
  case StmtType::BLOCK:
    visit_block(stmt->get_block_stmt());
    break;
  case StmtType::EXPRESSION:
    visit_expression_statement(stmt->get_expression_stmt());
    break;
  default:
    report_error("Tipo de statement desconhecido encontrado (enum: " +
                     std::to_string(static_cast<int>(stmt->get_type())) + ")",
                 stmt->get_token());
    break;
  }

  if (!context_stack.empty()) {
    context_stack.pop();
  } else {
    report_error("Pilha de contexto vazia ao tentar desempilhar statement "
                     "(erro interno)",
                 0, 0);
  }
}

void SemanticAnalyzer::visit_block(const BlockStmt &block) {
  push_scope();
  for (const auto &stmt_ptr : block.statements) {
    if (stmt_ptr) {
      visit(stmt_ptr.get());
    }
    else {
      report_error("Statement nulo encontrado dentro de um bloco", 0, 0);
    }
  }
  pop_scope();
}

void SemanticAnalyzer::visit_declaration(DeclarationStmt &decl) {
  if (!is_valid_type(decl.type)) {
    report_error("Tipo inválido na declaração: '" + decl.type.get_lexeme() +
                     "'",
                 decl.type);
    return;
  }

  Token initializer_type = create_type_token(TokenType::TYPE_NONE);
  if (decl.initializer) {
    initializer_type = visit(decl.initializer.get());
  } else {
    report_error("Declaração sem inicializador (erro interno do parser?)",
                 decl.identifier);
    return;
  }

  if (initializer_type.get_type() != TokenType::TYPE_NONE) {
    if (!check_types(decl.type, initializer_type)) {
      report_error("Tipo incompatível na inicialização da variável '" +
                       decl.identifier.get_lexeme() +
                       "'. Esperado: " + decl.type.get_lexeme() +
                       ", Encontrado: " + initializer_type.get_lexeme(),
                   decl.identifier);
    }
  }
  Symbol symbol(decl.identifier, decl.type, false);
  if (!declare_symbol(symbol)) {
    report_error("Variável '" + decl.identifier.get_lexeme() +
                     "' já declarada neste escopo.",
                 decl.identifier);
  } else {
    add_symbol_to_current_function(symbol);
  }
}

void SemanticAnalyzer::visit_assignment(const AssignmentStmt &assign) {
  Symbol *symbol = find_symbol(assign.identifier.get_lexeme());
  if (!symbol) {
    report_error("Variável '" + assign.identifier.get_lexeme() +
                     "' não declarada antes da atribuição.",
                 assign.identifier);
    return;
  }

  Token value_type = create_type_token(TokenType::TYPE_NONE);
  if (assign.value) {
    value_type = visit(assign.value.get());
    if (value_type.get_type() == TokenType::TYPE_NONE && !errors.empty()) {
      return;
    }
  } else {
    report_error("Atribuição sem valor (erro interno do parser?)",
                 assign.identifier);
    return;
  }
  if (value_type.get_type() != TokenType::TYPE_NONE) {
    if (!check_types(symbol->type, value_type)) {
      report_error("Tipos incompatíveis na atribuição para '" +
                       assign.identifier.get_lexeme() +
                       "'. Variável é do tipo '" + symbol->type.get_lexeme() +
                       "', mas a expressão é do tipo '" +
                       value_type.get_lexeme() + "'.",
                   assign.identifier);
    }
  }
}

void SemanticAnalyzer::add_symbol_to_current_function(const Symbol &symbol) {
  std::stack<Stmt *> temp_stack = context_stack;
  FunctionStmt *current_function = nullptr;

  if (!temp_stack.empty() &&
      temp_stack.top()->get_type() == StmtType::DECLARATION) {
    temp_stack.pop();
  }

  while (!temp_stack.empty()) {
    Stmt *stmt_in_stack = temp_stack.top();
    if (stmt_in_stack && stmt_in_stack->get_type() == StmtType::FUNCTION) {
      current_function = &(stmt_in_stack->get_function_stmt());
      break;
    }
    temp_stack.pop();
  }

  if (current_function) {
    const std::string &func_name = current_function->name.get_lexeme();
    auto it = function_symbols.find(func_name);
    if (it != function_symbols.end()) {
      it->second.push_back(symbol);
    } else {
      report_error("Erro interno: Tentando adicionar símbolo a função não "
                       "inicializada '" +
                           func_name + "'.",
                   symbol.name);
    }
  }
}

void SemanticAnalyzer::visit_return(const ReturnStmt &ret) {
  FunctionStmt *current_function = nullptr;
  std::stack<Stmt *> search_stack = context_stack;
  while (!search_stack.empty()) {
    Stmt *stmt_in_stack = search_stack.top();
    search_stack.pop();
    if (stmt_in_stack && stmt_in_stack->get_type() == StmtType::FUNCTION) {
      current_function = &(stmt_in_stack->get_function_stmt());
      break;
    }
  }
  if (!current_function) {
    const Token &error_token = ret.value
        ? get_expr_token(ret.value)
        : Token(TokenType::RETURN, "return", 0, 0);
    report_error("Statement 'return' encontrado fora de uma função.",
                error_token);
    return;
  }
  Token return_value_type = create_type_token(TokenType::TYPE_NONE);
  if (ret.value) {
    return_value_type = visit(ret.value.get());
    if (return_value_type.get_type() == TokenType::TYPE_NONE &&
        !errors.empty()) {
      return;
    }
  } else {
    report_error("Statement 'return' sem expressão (erro interno do parser?)",
                current_function->name);
    return;
  }
  if (return_value_type.get_type() != TokenType::TYPE_NONE) {
    const Token &expected_return_type = current_function->return_type;
    if (!check_types(expected_return_type, return_value_type)) {
      report_error("Tipo de retorno incompatível na função '" +
                       current_function->name.get_lexeme() +
                       "'. Esperado: " + expected_return_type.get_lexeme() +
                       ", Encontrado: " + return_value_type.get_lexeme(),
                   get_expr_token(ret.value));
    }
  }
}

void SemanticAnalyzer::visit_break(const BreakStmt &brk) {
  if (loop_level == 0) {
    report_error(
        "Statement 'break' encontrado fora de um loop ('while' ou 'for').",
        brk.keyword
    );
  }
}

void SemanticAnalyzer::visit_continue(const ContinueStmt &cont) {
  if (loop_level == 0) {
    report_error(
        "Statement 'continue' encontrado fora de um loop ('while' ou 'for').",
        cont.keyword);
  }
}

void SemanticAnalyzer::visit_function(const FunctionStmt &func) {
  const std::string &func_name = func.name.get_lexeme();
  if (!builtin_functions.count(func_name) && !function_table.count(func_name)) {
    report_error("Erro interno: Função '" + func_name +
                     "' não registrada antes da análise do corpo.",
                 func.name);
    return;
  }
  if (function_symbols.find(func_name) == function_symbols.end()) {
    report_error(
        "Erro interno: Mapa de símbolos não inicializado para função '" +
            func_name + "'.",
        func.name);
    function_symbols[func_name] = {};
  }

  push_scope();
  if (func.params.param_names && func.params.param_types) {
    auto &names = *func.params.param_names;
    auto &types = *func.params.param_types;
    if (names.size() == types.size()) {
      for (size_t i = 0; i < names.size(); ++i) {
        Symbol param_symbol(names[i], types[i], false);
        if (!declare_symbol(param_symbol)) {
          report_error("Nome de parâmetro duplicado '" + names[i].get_lexeme() +
                           "' na função '" + func_name + "'.",
                       names[i]);
        } else {
          function_symbols[func_name].push_back(param_symbol);
        }
      }
    }
  }
  visit_block(func.body);
  pop_scope();
}

void SemanticAnalyzer::visit_if(const IfStmt &if_stmt) {
  if (if_stmt.condition) {
    Token cond_type = visit(if_stmt.condition.get());
    if (cond_type.get_type() != TokenType::TYPE_NONE &&
        !is_boolean_type(cond_type)) {
      report_error("A condição do 'if' deve ser do tipo 'bool'.",
                  get_expr_token(if_stmt.condition));
    }
  } else {
    report_error("Statement 'if' sem condição (erro interno do parser?)", 0, 0);
    return;
  }
  visit_block(if_stmt.then_block);
  if (if_stmt.has_else && if_stmt.else_block) {
    visit_block(*if_stmt.else_block);
  }
}

void SemanticAnalyzer::visit_while(const WhileStmt &while_stmt) {
  if (while_stmt.condition) {
    Token cond_type = visit(while_stmt.condition.get());
    if (cond_type.get_type() != TokenType::TYPE_NONE &&
        !is_boolean_type(cond_type)) {
      report_error("A condição do 'while' deve ser do tipo 'bool'.",
                  get_expr_token(while_stmt.condition));
    }
  } else {
    report_error("Statement 'while' sem condição (erro interno do parser?)", 0,
                0);
    return;
  }
  loop_level++;
  try {
    visit_block(while_stmt.body);
  } catch (...) {
    loop_level--;
    throw; 
  }
  loop_level--;
}

void SemanticAnalyzer::visit_for(const ForStmt &for_stmt) {
  if (for_stmt.initializer) {
    visit(for_stmt.initializer.get());
  }
  if (for_stmt.condition) {
    Token cond_type = visit(for_stmt.condition.get());
    if (cond_type.get_type() != TokenType::TYPE_NONE &&
        !is_boolean_type(cond_type)) {
      report_error("A condição do 'for' deve ser do tipo 'bool'.",
                  get_expr_token(for_stmt.condition));
    }
  }
  if (for_stmt.increment) {
    visit(for_stmt.increment.get());
  }
  loop_level++;
  try {
    visit_block(for_stmt.body);
  } catch (...) {
    loop_level--;
    throw;
  }
  loop_level--;
}

void SemanticAnalyzer::visit_seq(const SeqStmt &seq) { visit_block(seq.body); }
void SemanticAnalyzer::visit_par(const ParStmt &par) { visit_block(par.body); }

void SemanticAnalyzer::visit_cchannel(const CChannelStmt &channel) {
  if (!find_symbol(channel.id_1.get_lexeme())) {
    report_error("Identificador '" + channel.id_1.get_lexeme() +
                     "' usado em c_channel não foi declarado.",
                 channel.id_1);
  }
  if (!find_symbol(channel.id_2.get_lexeme())) {
    report_error("Identificador '" + channel.id_2.get_lexeme() +
                     "' usado em c_channel não foi declarado.",
                 channel.id_2);
  }
}

void SemanticAnalyzer::visit_expression_statement(
    const ExpressionStmt &expr_stmt) {
  if (expr_stmt.expression) {
    visit(
        expr_stmt.expression
            .get());
  } else {
    report_error(
        "Expression statement com expressão nula (erro interno do parser?)", 0,
        0);
  }
}

Token SemanticAnalyzer::visit(Expr *expr) {
  if (!expr) {
    report_error("Tentativa de visitar uma expressão nula (erro interno)", 0,
                0);
    return create_type_token(TokenType::TYPE_NONE);
  }
  switch (expr->get_type()) {
  case ExprType::LITERAL:
    return visit_literal(*static_cast<LiteralExpr *>(expr));
  case ExprType::VARIABLE:
    return visit_variable(*static_cast<VariableExpr *>(expr));
  case ExprType::BINARY:
    return visit_binary(*static_cast<BinaryExpr *>(expr));
  case ExprType::UNARY:
    return visit_unary(*static_cast<UnaryExpr *>(expr));
  case ExprType::GROUPING:
    return visit_grouping(*static_cast<GroupingExpr *>(expr));
  case ExprType::CALL:
    return visit_call(*static_cast<CallExpr *>(expr));
  case ExprType::GET:
    return visit_get(*static_cast<GetExpr *>(expr));
  case ExprType::INDEX:
    return visit_index(*static_cast<IndexExpr *>(expr));
  default:
    report_error("Tipo de expressão desconhecido encontrado (enum: " +
                     std::to_string(static_cast<int>(expr->get_type())) + ")",
                 expr->get_token());
    return create_type_token(TokenType::TYPE_NONE);
  }
}

Token SemanticAnalyzer::visit_literal(const LiteralExpr &lit) {
  const Token &token = lit.value;
  switch (token.get_type()) {
  case TokenType::NUMBER:
    if (!token.is_number()) {
      report_error("Token NUMBER sem valor double", token);
      return create_type_token(TokenType::TYPE_NONE);
    }
    return create_type_token(TokenType::TYPE_NUMBER);
  case TokenType::STRING_LITERAL:
    if (!token.is_string()) {
      report_error("Token STRING_LITERAL sem valor string", token);
      return create_type_token(TokenType::TYPE_NONE);
    }
    return create_type_token(TokenType::TYPE_STRING);
  case TokenType::TRUE_LITERAL:
  case TokenType::FALSE_LITERAL:
    if (!token.is_bool()) {
      report_error("Token BOOLEAN sem valor bool", token);
      return create_type_token(TokenType::TYPE_NONE);
    }
    return create_type_token(TokenType::TYPE_BOOL);
  default:
    report_error("Token inesperado (" + to_str(token.get_type()) +
                     ") em expressão literal",
                 token);
    return create_type_token(TokenType::TYPE_NONE);
  }
}

Token SemanticAnalyzer::visit_variable(VariableExpr &var) {
  const std::string &name = var.name.get_lexeme();
  Symbol *symbol = find_symbol(name);
  if (symbol) {
    return symbol->type;
  }
  report_error("Identificador '" + name + "' não foi declarado.",
              var.name);
  return create_type_token(TokenType::TYPE_NONE);
}

Token SemanticAnalyzer::visit_binary(const BinaryExpr &bin) {
  Token left_type = visit(bin.left.get());
  Token right_type = visit(bin.right.get());
  if (left_type.get_type() == TokenType::TYPE_NONE ||
      right_type.get_type() == TokenType::TYPE_NONE) {
    return create_type_token(TokenType::TYPE_NONE);
  }
  TokenType op_type = bin.op.get_type();
  switch (op_type) {
  case TokenType::PLUS:
    if (is_numeric_type(left_type) && is_numeric_type(right_type))
      return create_type_token(TokenType::TYPE_NUMBER);
    if (left_type.get_type() == TokenType::TYPE_STRING &&
        right_type.get_type() == TokenType::TYPE_STRING)
      return create_type_token(TokenType::TYPE_STRING);
    report_error("Operador '+' inválido para os tipos " +
                     left_type.get_lexeme() + " e " + right_type.get_lexeme(),
                 bin.op);
    return create_type_token(TokenType::TYPE_NONE);
  case TokenType::MINUS:
  case TokenType::STAR:
  case TokenType::SLASH:
  case TokenType::PERCENT:
    if (!is_numeric_type(left_type) || !is_numeric_type(right_type)) {
      report_error("Operador '" + bin.op.get_lexeme() +
                       "' exige operandos 'number'",
                   bin.op);
      return create_type_token(TokenType::TYPE_NONE);
    }
    return create_type_token(TokenType::TYPE_NUMBER);
  case TokenType::EQUAL_COMPARE:
  case TokenType::BANG_EQUAL:
    if (!check_types(left_type, right_type)) {
      report_error("Comparação '" + bin.op.get_lexeme() +
                       "' inválida entre tipos " + left_type.get_lexeme() +
                       " e " + right_type.get_lexeme(),
                   bin.op);
      return create_type_token(TokenType::TYPE_NONE);
    }
    return create_type_token(TokenType::TYPE_BOOL);
  case TokenType::GREATER:
  case TokenType::GREATER_EQUAL:
  case TokenType::LESS:
  case TokenType::LESS_EQUAL:
    if (!is_numeric_type(left_type) || !is_numeric_type(right_type)) {
      report_error("Comparação '" + bin.op.get_lexeme() +
                       "' exige operandos 'number'",
                   bin.op);
      return create_type_token(TokenType::TYPE_NONE);
    }
    return create_type_token(TokenType::TYPE_BOOL);
  case TokenType::AND_AND:
  case TokenType::OR_OR:
    if (!is_boolean_type(left_type) || !is_boolean_type(right_type)) {
      report_error("Operador lógico '" + bin.op.get_lexeme() +
                       "' exige operandos 'bool'",
                   bin.op);
      return create_type_token(TokenType::TYPE_NONE);
    }
    return create_type_token(TokenType::TYPE_BOOL);
  default:
    report_error("Operador binário desconhecido: " + bin.op.get_lexeme(),
                bin.op);
    return create_type_token(TokenType::TYPE_NONE);
  }
}

Token SemanticAnalyzer::visit_unary(const UnaryExpr &un) {
  Token expr_type = visit(un.right.get());
  if (expr_type.get_type() == TokenType::TYPE_NONE) {
    return create_type_token(TokenType::TYPE_NONE);
  }
  TokenType op_type = un.op.get_type();
  switch (op_type) {
  case TokenType::MINUS:
    if (!is_numeric_type(expr_type)) {
      report_error("Operador unário '-' exige operando 'number'", un.op);
      return create_type_token(TokenType::TYPE_NONE);
    }
    return create_type_token(TokenType::TYPE_NUMBER);
  case TokenType::BANG:
    if (!is_boolean_type(expr_type)) {
      report_error("Operador unário '!' exige operando 'bool'", un.op);
      return create_type_token(TokenType::TYPE_NONE);
    }
    return create_type_token(TokenType::TYPE_BOOL);
  default:
    report_error("Operador unário desconhecido: " + un.op.get_lexeme(), un.op);
    return create_type_token(TokenType::TYPE_NONE);
  }
}

Token SemanticAnalyzer::visit_grouping(const GroupingExpr &grp) {
  return visit(grp.expression.get());
}

Token SemanticAnalyzer::visit_call(CallExpr &call) {
  std::string func_name = "";
  const Token *func_token = nullptr;
  if (auto var_callee = dynamic_cast<VariableExpr *>(call.callee.get())) {
    func_name = var_callee->name.get_lexeme();
    func_token = &(var_callee->name);
  } else {
    report_error(
        "Expressão complexa usada como nome de função (não suportado).",
        get_expr_token(call.callee));
    return create_type_token(TokenType::TYPE_NONE);
  }
  if (!func_token) {
    func_token = &call.paren;
  }

  auto builtin_it = builtin_functions.find(func_name);
  if (builtin_it != builtin_functions.end()) {
    const FunctionSignature &sig = builtin_it->second;
    size_t actual_args = call.arguments.size();
    if (sig.check_param_count) {
      size_t expected_args = sig.param_types.size();
      if (actual_args != expected_args) {
        report_error("Número incorreto de argumentos para a função built-in '" +
                         func_name +
                         "'. Esperado: " + std::to_string(expected_args) +
                         ", Fornecido: " + std::to_string(actual_args) + ".",
                     call.paren);
      }
    } else {
      if (actual_args < static_cast<size_t>(sig.min_params)) {
        report_error(
            "Número insuficiente de argumentos para a função built-in '" +
                func_name +
                "'. Mínimo esperado: " + std::to_string(sig.min_params) +
                ", Fornecido: " + std::to_string(actual_args) + ".",
            call.paren);
      }
    }
    size_t num_types_to_check = sig.param_types.size();
    for (size_t i = 0; i < actual_args; ++i) {
      if (i < num_types_to_check) {
        Token arg_type = visit(call.arguments[i].get());
        if (arg_type.get_type() != TokenType::TYPE_NONE) {
          Token expected_type = create_type_token(sig.param_types[i]);
          if (!check_types(expected_type, arg_type)) {
            report_error("Tipo incompatível para o argumento " +
                             std::to_string(i + 1) +
                             " na chamada da função built-in '" + func_name +
                             "'. Esperado: " + expected_type.get_lexeme() +
                             ", Encontrado: " + arg_type.get_lexeme() + ".",
                         get_expr_token(call.arguments[i]));
          }
        }
      }
    }
    return create_type_token(sig.return_type);
  }

  auto func_it = function_table.find(func_name);
  if (func_it == function_table.end()) {
    if (find_symbol(func_name)) {
      report_error("Tentativa de chamar uma variável ('" + func_name +
                       "') como função.",
                   *func_token);
    } else {
      report_error("Função '" + func_name + "' não declarada.", *func_token);
    }
    return create_type_token(TokenType::TYPE_NONE);
  }

  FunctionStmt *func = func_it->second;
  size_t expected_args_user =
      func->params.param_names ? func->params.param_names->size() : 0;
  size_t actual_args_user = call.arguments.size();
  if (actual_args_user != expected_args_user) {
    report_error("Número incorreto de argumentos para a função '" + func_name +
                     "'. Esperado: " + std::to_string(expected_args_user) +
                     ", Fornecido: " + std::to_string(actual_args_user) + ".",
                 call.paren);
  }
  if (func->params.param_types) {
    auto &expected_types = *func->params.param_types;
    for (size_t i = 0; i < actual_args_user; ++i) {
      if (i < expected_types.size()) {
        Token arg_type = visit(call.arguments[i].get());
        if (arg_type.get_type() != TokenType::TYPE_NONE) {
          const Token &expected_type = expected_types[i];
          if (expected_type.get_type() != TokenType::TYPE_NONE &&
              !check_types(expected_type, arg_type)) {
            report_error("Tipo incompatível para o argumento " +
                             std::to_string(i + 1) + " na chamada da função '" +
                             func_name +
                             "'. Esperado: " + expected_type.get_lexeme() +
                             ", Encontrado: " + arg_type.get_lexeme() + ".",
                         get_expr_token(call.arguments[i]));
          }
        }
      }
    }
  }
  return func->return_type;
}

Token SemanticAnalyzer::visit_get(const GetExpr &get) {
  report_error("Acesso a membro '.' não suportado.", get.name);
  return create_type_token(TokenType::TYPE_NONE);
}

Token SemanticAnalyzer::visit_index(const IndexExpr &index) {
  Token object_type = visit(index.object.get());
  if (object_type.get_type() == TokenType::TYPE_NONE) {
    return create_type_token(TokenType::TYPE_NONE);
  }
  if (!is_array_type(object_type)) {
    report_error("Tentativa de acessar índice '[]' em um tipo não-array ('" +
                     object_type.get_lexeme() + "').",
                 index.bracket);
    return create_type_token(TokenType::TYPE_NONE);
  }
  const Token &index_token =
      index.index;
  if (index_token.get_type() != TokenType::NUMBER) {
    report_error("Índice de array deve ser um número literal (parser atual).",
                index_token);
    return create_type_token(TokenType::TYPE_NONE);
  }
  return get_array_element_type(object_type);
}

void SemanticAnalyzer::report_error(const std::string &message, const Token &token) {
  std::string full_message = "[Linha " + std::to_string(token.get_line()) +
                              "] Erro: " + message + " na posição " +
                              std::to_string(token.get_column());
  bool found = false;
  for (const auto &err : errors) {
    if (err->get_message() == full_message) {
      found = true;
      break;
    }
  }
  if (!found) {
    errors.push_back(
        std::make_unique<Error>(message, token.get_column(), token.get_line()));
  }
}

void SemanticAnalyzer::report_error(const std::string &message, unsigned int column, unsigned int line) {
  std::string full_message = "[Linha " + std::to_string(line) +
                              "] Erro: " + message + " na posição " +
                              std::to_string(column);
  bool found = false;
  for (const auto &err : errors) {
    if (err->get_message() == full_message) {
      found = true;
      break;
    }
  }
  if (!found) {
    errors.push_back(std::make_unique<Error>(message, column, line));
  }
}

const Token &
SemanticAnalyzer::get_expr_token(const std::unique_ptr<Expr> &expr) {
  static Token dummy_token{TokenType::TYPE_NONE, "[expr]", 0, 0};
  return expr ? expr->get_token() : dummy_token;
}

const std::unordered_map<std::string, FunctionStmt *> &
SemanticAnalyzer::get_function_table() const {
  return function_table;
}
const std::vector<std::unordered_map<std::string, SemanticAnalyzer::Symbol>> &
SemanticAnalyzer::get_symbol_scopes() const {
  return scopes;
}
const std::unordered_map<std::string, std::vector<SemanticAnalyzer::Symbol>> &
SemanticAnalyzer::get_function_symbols() const {
  return function_symbols;
}

void SemanticAnalyzer::imprimirVariaveisPorFuncao() const {
  std::cout << "\n--- Símbolos por Escopo (da Análise Semântica) ---" << std::endl;
  if (!scopes.empty()) {
      const auto& global_scope = scopes[0];
      std::cout << "Escopo: Global" << std::endl;
      if (global_scope.empty()) {
          std::cout << "  (Nenhuma variável global declarada)" << std::endl;
      } else {
          for (const auto& pair : global_scope) {
              const auto& symbol = pair.second;
              std::cout << "  - " << std::left << std::setw(15)
                        << symbol.name.get_lexeme()
                        << " : " << std::setw(10)
                        << symbol.type.get_lexeme()
                        << " (Linha: " << symbol.name.get_line() << ")"
                        << std::endl;
          }
      }
       std::cout << "-------------------------------------------------------" << std::endl;
  } else {
       std::cout << "(Escopo global não encontrado/vazio)" << std::endl;
       std::cout << "-------------------------------------------------------" << std::endl;
  }
  const auto& func_symbols_map = function_symbols;
  if (func_symbols_map.empty()) {
      std::cout << "(Nenhuma função com símbolos locais encontrada para listar separadamente)" << std::endl;
  } else {
      for (const auto& pair : func_symbols_map) {
          const std::string& func_name = pair.first;
          const auto& symbols = pair.second;
          std::cout << "Escopo: Função '" << func_name << "'" << std::endl;
          if (symbols.empty()) {
              std::cout << "  (Nenhum parâmetro ou variável local declarada nesta função)" << std::endl;
          } else {
              for (const auto& symbol : symbols) {
                  std::cout << "  - " << std::left << std::setw(15)
                            << symbol.name.get_lexeme()
                            << " : " << std::setw(10)
                            << symbol.type.get_lexeme()
                            << " (Linha: " << symbol.name.get_line() << ")"
                            << std::endl;
              }
          }
           std::cout << "-------------------------------------------------------" << std::endl;
      }
  }
}

const std::vector<std::pair<int, SemanticAnalyzer::Symbol>>&
SemanticAnalyzer::get_all_declared_symbols() const {
    return all_declared_symbols;
}