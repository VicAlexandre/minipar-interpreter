// src/core/SemanticAnalyzer.cpp
#include "../include/core/Semantic.h"
#include "../include/core/Error.h"
#include "../include/core/Expr.h"
#include "../include/core/Stmt.h"
#include "../include/core/Token.h" 
#include <iomanip>
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
  std::string lexeme = "internal_error_type";
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
  case TokenType::TYPE_ARRAY_NUMBER:
    lexeme = "array_number";
    actual_type = TokenType::TYPE_ARRAY_NUMBER;
    break;
  case TokenType::TYPE_NONE: // Para void
    lexeme = "void";
    actual_type = TokenType::TYPE_NONE;
    break;
  default:
    
    std::cerr
        << "[WARN_SEMANTIC] create_type_token chamado com TokenType não-tipo: "
        << static_cast<int>(type) << " (" << ::to_str(type) << ")" << std::endl;
    lexeme = ::to_str(type) + "_as_type_error"; 
    actual_type = TokenType::TYPE_NONE; 
    break;
  }

  return Token(actual_type, lexeme, 0, 0);
}

SemanticAnalyzer::SemanticAnalyzer() : loop_level(0) {
  push_scope(); // Inicializa o escopo global
  register_builtin_functions();
}

void SemanticAnalyzer::register_builtin_functions() {
  
  builtin_functions["print"] = {"print", {}, TokenType::TYPE_NONE, false, 0}; // Permite múltiplos argumentos, não checa contagem exata
  builtin_functions["input"] = {"input", {TokenType::TYPE_STRING}, TokenType::TYPE_STRING, true, 1}; // Opcional: pode ser 0 ou 1 arg para prompt
  builtin_functions["to_number"] = {"to_number", {TokenType::TYPE_STRING}, TokenType::TYPE_NUMBER, true, 1}; // String para número
  builtin_functions["to_string"] = {"to_string", {TokenType::TYPE_NONE}, TokenType::TYPE_STRING, true, 1}; // Aceita qualquer tipo, checa no executor
  builtin_functions["to_bool"] = {"to_bool", {TokenType::TYPE_NONE}, TokenType::TYPE_BOOL, true, 1};     // Aceita qualquer tipo, checa no executor
  builtin_functions["len"] = {"len", {TokenType::TYPE_NONE}, TokenType::TYPE_NUMBER, true, 1};         // String ou array, checa no executor ou semântica mais fina
  builtin_functions["sleep"] = {"sleep", {TokenType::TYPE_NUMBER}, TokenType::TYPE_NONE, true, 1};
  builtin_functions["isalpha"] = {"isalpha", {TokenType::TYPE_STRING}, TokenType::TYPE_BOOL, true, 1};
  builtin_functions["isnum"] = {"isnum", {TokenType::TYPE_STRING}, TokenType::TYPE_BOOL, true, 1};
  builtin_functions["exp"] = {"exp", {TokenType::TYPE_NUMBER}, TokenType::TYPE_NUMBER, true, 1};
  builtin_functions["random"] = {"random", {}, TokenType::TYPE_NUMBER, true, 0};


  builtin_functions["connect_socket"] = {"connect_socket", {TokenType::TYPE_STRING, TokenType::TYPE_NUMBER}, TokenType::TYPE_NUMBER, true, 2}; // host, port -> socket_handle (ou -1 para erro)
  builtin_functions["send_socket"] =    {"send_socket",    {TokenType::TYPE_NUMBER, TokenType::TYPE_STRING}, TokenType::TYPE_BOOL, true, 2};    // handle, message -> success
  builtin_functions["receive_socket"] = {"receive_socket", {TokenType::TYPE_NUMBER}, TokenType::TYPE_STRING, true, 1}; // handle -> message (ou string vazia em erro/EOF)
  builtin_functions["close_socket"] =   {"close_socket",   {TokenType::TYPE_NUMBER}, TokenType::TYPE_NONE, true, 1};     // handle -> void
}

std::vector<std::unique_ptr<Error>>
SemanticAnalyzer::analyze(std::vector<std::unique_ptr<Stmt>> &statements) {
  errors.clear();
  function_table.clear();
  function_symbols.clear(); 
  all_declared_symbols.clear();
  
  while (scopes.size() > 1) scopes.pop_back(); 
  if (scopes.empty()) push_scope(); 
  else scopes[0].clear(); 

  loop_level = 0; // Reseta nível de loop
  context_stack = std::stack<Stmt*>(); // Reseta pilha de contexto

  // Primeiro passe: registrar todas as funções definidas pelo usuário
  for (const auto &stmt_ptr : statements) {
    if (stmt_ptr && stmt_ptr->get_type() == StmtType::FUNCTION) {
      FunctionStmt &func_stmt = stmt_ptr->get_function_stmt();
      if (builtin_functions.count(func_stmt.name.get_lexeme())) {
        report_error("Nome de função '" + func_stmt.name.get_lexeme() +
                         "' colide com função built-in.",
                     func_stmt.name);
      } else {
        register_function(func_stmt); // Adiciona à function_table
        function_symbols[func_stmt.name.get_lexeme()] = {}; // Prepara para rastrear símbolos da função
      }
    }
  }

  // Segundo passe: analisar todos os statements
  for (const auto &stmt_ptr : statements) {
    try {
      if (stmt_ptr) {
        visit(stmt_ptr.get());
      } else {
        // Isso indicaria um erro no parser que produziu um stmt nulo
        report_error("Statement nulo encontrado na lista principal (erro "
                     "interno do parser?).",
                     0, 0); 
      }
    } catch (const std::exception &e) {
      // Exceção inesperada durante a análise semântica
      report_error("Erro interno durante análise semântica (exceção std): " +
                       std::string(e.what()),
                   0, 0);
    } catch (...) {
      report_error("Erro interno desconhecido durante análise semântica.", 0, 0);
    }
  }
  return std::move(errors); // Retorna os erros coletados
}

void SemanticAnalyzer::push_scope() {
  scopes.emplace_back(); // Adiciona um novo escopo (mapa de símbolos)
}

void SemanticAnalyzer::pop_scope() {
  if (!scopes.empty()) {
    scopes.pop_back();
  } else {
    // Isso seria um erro interno grave
    report_error("Tentativa de desempilhar escopo global (erro interno).", 0, 0);
  }
}

bool SemanticAnalyzer::declare_symbol(const Symbol &symbol) {
  if (scopes.empty()) {
    report_error(
        "Tentativa de declarar símbolo sem escopo ativo (erro interno).",
        symbol.name);
    return false;
  }
  auto &current_scope = scopes.back();
  const std::string &name = symbol.name.get_lexeme();
  if (current_scope.count(name)) {
    // Símbolo já declarado no escopo atual
    return false; 
  }
  current_scope.emplace(name, symbol);
  all_declared_symbols.emplace_back(scopes.size() - 1, symbol); // Rastreia para depuração/análise
  add_symbol_to_current_function(symbol); // Rastreia para análise de variáveis de função
  return true;
}

SemanticAnalyzer::Symbol *
SemanticAnalyzer::find_symbol(const std::string &name) {
  // Procura do escopo mais interno para o mais externo
  for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
    auto &scope_map = *it;
    auto found = scope_map.find(name);
    if (found != scope_map.end()) {
      return &(found->second); // Retorna ponteiro para o símbolo encontrado
    }
  }
  return nullptr; // Símbolo não encontrado em nenhum escopo acessível
}


bool SemanticAnalyzer::check_types(const Token &expected_token,
                                   const Token &actual_token) {
  TokenType expected = expected_token.get_type();
  TokenType actual = actual_token.get_type();

  // 'void' (TYPE_NONE) só é compatível com 'void'
  if (expected == TokenType::TYPE_NONE) {
    return actual == TokenType::TYPE_NONE;
  }
  // Não se pode atribuir 'void' a um tipo não-'void'
  if (actual == TokenType::TYPE_NONE) {
    return false; // Ou expected == TokenType::TYPE_NONE, já coberto acima
  }
  // Para outros tipos, eles devem ser iguais
  return expected == actual;
}


bool SemanticAnalyzer::is_numeric_type(const Token &type_token) {
  return type_token.get_type() == TokenType::TYPE_NUMBER;
}

bool SemanticAnalyzer::is_boolean_type(const Token &type_token) {
  TokenType tt = type_token.get_type();
  // Um tipo booleano pode ser o tipo 'bool' ou um literal true/false
  return tt == TokenType::TYPE_BOOL || tt == TokenType::TRUE_LITERAL || tt == TokenType::FALSE_LITERAL;
}

bool SemanticAnalyzer::is_string_type(const Token &type_token) {
  return type_token.get_type() == TokenType::TYPE_STRING;
}

bool SemanticAnalyzer::is_valid_type(const Token &type_token) {
  TokenType type = type_token.get_type();
  return type == TokenType::TYPE_NUMBER || type == TokenType::TYPE_STRING ||
         type == TokenType::TYPE_BOOL || type == TokenType::TYPE_NONE || // void é um tipo válido (para retornos)
         type == TokenType::TYPE_ARRAY_NUMBER;
}

bool SemanticAnalyzer::is_array_type(const Token &type_token) {
  return type_token.get_type() == TokenType::TYPE_ARRAY_NUMBER;
}


Token SemanticAnalyzer::get_array_element_type(const Token &array_type_token) {
  if (array_type_token.get_type() == TokenType::TYPE_ARRAY_NUMBER) {
    // Atualmente, apenas array_number existe, então o elemento é number
    return create_type_token(TokenType::TYPE_NUMBER);
  }
  // Se outros tipos de array fossem suportados, a lógica seria mais complexa
  report_error("Tentativa de obter tipo de elemento de um não-array: '" +
                   array_type_token.get_lexeme() + "'.",
               array_type_token);
  return create_type_token(TokenType::TYPE_NONE); // Retorna void/erro
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
  // Verifica tipo de retorno
  if (!is_valid_type(func.return_type)) {
    report_error("Tipo de retorno inválido '" + func.return_type.get_lexeme() +
                     "' para a função '" + func_name + "'.",
                 func.return_type);
    return;
  }
  // Verifica tipos de parâmetros
  if (func.params.param_types) {
    for (const auto &param_type_token : *(func.params.param_types)) {
      if (!is_valid_type(param_type_token)) {
        report_error("Tipo de parâmetro inválido '" +
                         param_type_token.get_lexeme() + "' na função '" +
                         func_name + "'.",
                     param_type_token);
        return; // Para após o primeiro erro de tipo de parâmetro
      }
    }
  }
  function_table[func_name] = const_cast<FunctionStmt *>(&func); // Armazena ponteiro para o nó da AST da função
}

void SemanticAnalyzer::add_symbol_to_current_function(const Symbol &symbol) {
  FunctionStmt *current_function_node = nullptr;
  // Procura na pilha de contexto o nó de função mais interno
  std::stack<Stmt *> search_stack = context_stack; // Copia para não modificar a original
  while (!search_stack.empty()) {
    Stmt *stmt_in_context = search_stack.top();
    if (stmt_in_context && stmt_in_context->get_type() == StmtType::FUNCTION) {
      current_function_node = &(stmt_in_context->get_function_stmt());
      break;
    }
    search_stack.pop();
  }

  if (current_function_node) {
    const std::string &func_name = current_function_node->name.get_lexeme();
    auto it = function_symbols.find(func_name);
    if (it != function_symbols.end()) {
      // Adiciona o símbolo (parâmetro ou variável local) à lista de símbolos desta função
      it->second.push_back(symbol);
    } else {
      // Isso não deveria acontecer se a função foi registrada corretamente no primeiro passe
      report_error("Erro interno: Tentando adicionar símbolo à função '" +
                       func_name +
                       "' que não foi inicializada no mapa function_symbols.",
                   symbol.name);
    }
  }
  // Se não estiver dentro de uma função, o símbolo é global ou de um bloco não funcional (como PAR/SEQ no nível superior)
}

// Dispatcher principal para visitar statements
void SemanticAnalyzer::visit(Stmt *stmt) {
  if (!stmt) {
    report_error("Tentativa de visitar um statement nulo (erro interno).", 0, 0);
    return;
  }
  context_stack.push(stmt); // Adiciona o statement atual à pilha de contexto

  switch (stmt->get_type()) {
  case StmtType::DECLARATION:    visit_declaration(stmt->get_decl_stmt()); break;
  case StmtType::ASSIGNMENT:     visit_assignment(stmt->get_assign_stmt()); break;
  case StmtType::RETURN:         visit_return(stmt->get_return_stmt()); break;
  case StmtType::BREAK:          visit_break(stmt->get_break_stmt()); break;
  case StmtType::CONTINUE:       visit_continue(stmt->get_continue_stmt()); break;
  case StmtType::FUNCTION:       visit_function(stmt->get_function_stmt()); break;
  case StmtType::IF:             visit_if(stmt->get_if_stmt()); break;
  case StmtType::WHILE:          visit_while(stmt->get_while_stmt()); break;
  case StmtType::FOR:            visit_for(stmt->get_for_stmt()); break;
  case StmtType::SEQ:            visit_block(stmt->get_seq_stmt().body); break; // Trata corpo de SEQ como um bloco
  case StmtType::PAR:            visit_block(stmt->get_par_stmt().body); break; // Trata corpo de PAR como um bloco
  case StmtType::CCHANNEL:       visit_cchannel(stmt->get_c_channel_stmt()); break;
  case StmtType::BLOCK:          visit_block(stmt->get_block_stmt()); break;
  case StmtType::EXPRESSION:     visit_expression_statement(stmt->get_expression_stmt()); break;
  default:
    report_error("Tipo de statement desconhecido (enum: " +
                     std::to_string(static_cast<int>(stmt->get_type())) + ")",
                 stmt->get_token()); 
    break;
  }

  context_stack.pop(); 
}


void SemanticAnalyzer::visit_block(const BlockStmt &block) {
  push_scope(); // Novo escopo para o bloco
  for (const auto &stmt_ptr : block.statements) {
    if (stmt_ptr) {
      visit(stmt_ptr.get());
    } else {
      report_error("Statement nulo encontrado dentro de um bloco.", 0, 0);
    }
  }
  pop_scope(); 
}

void SemanticAnalyzer::visit_declaration(DeclarationStmt &decl) {
  if (!is_valid_type(decl.type)) {
    report_error("Tipo inválido na declaração: '" + decl.type.get_lexeme() + "'.", decl.type);
    return;
  }

  Token initializer_type = create_type_token(TokenType::TYPE_NONE); 
  if (decl.initializer) {
    initializer_type = visit(decl.initializer.get()); 

    if (initializer_type.get_type() == TokenType::TYPE_NONE && !errors.empty() && decl.type.get_type() != TokenType::TYPE_NONE) {

      return;
    }
  } else {

    report_error("Declaração sem inicializador para '" + decl.identifier.get_lexeme() +
                 "'. (Erro interno ou BNF incorreta)", decl.identifier);
    return;
  }

  // Verifica compatibilidade de tipos
  if (!check_types(decl.type, initializer_type)) {
    // Caso especial: inicializar array_number com [] (array vazio) é permitido
    bool is_empty_array_init = (decl.type.get_type() == TokenType::TYPE_ARRAY_NUMBER &&
                                decl.initializer->get_type() == ExprType::ARRAY_LITERAL &&
                                static_cast<ArrayLiteralExpr*>(decl.initializer.get())->elements.empty());

    if (!is_empty_array_init) {
      report_error("Tipo incompatível na inicialização da variável '" + decl.identifier.get_lexeme() +
                   "'. Esperado: " + decl.type.get_lexeme() + ", Encontrado: " + initializer_type.get_lexeme() + ".",
                   decl.identifier);
    }
  }

  // Declara o símbolo no escopo atual
  Symbol symbol(decl.identifier, decl.type, false /*is_constant*/); // Constantes não parecem suportadas ainda
  if (!declare_symbol(symbol)) {
    report_error("Variável '" + decl.identifier.get_lexeme() + "' já declarada neste escopo.", decl.identifier);
  }
}

void SemanticAnalyzer::visit_assignment(const AssignmentStmt &assign) {
  Token target_type = visit(assign.target.get()); // Obtém o tipo do alvo (L-Value)
  Token value_type = visit(assign.value.get());   // Obtém o tipo do valor (R-Value)

  // Se houve erros ao determinar os tipos, não faz sentido continuar a verificação de tipos
  if ((target_type.get_type() == TokenType::TYPE_NONE || value_type.get_type() == TokenType::TYPE_NONE) && !errors.empty()) {
    return;
  }

  // Verifica se o alvo é um L-Value válido (variável, índice de array, etc.)
  ExprType lvalue_actual_type = assign.target->get_type();
  if (lvalue_actual_type != ExprType::VARIABLE && lvalue_actual_type != ExprType::INDEX && lvalue_actual_type != ExprType::GET) {
    report_error("Alvo da atribuição inválido (não é uma variável, elemento de array ou propriedade).",
                 assign.target->get_token()); // Obtém token do nó da expressão alvo
    return;
  }

  if (lvalue_actual_type == ExprType::VARIABLE) {
    const VariableExpr *var_expr = static_cast<const VariableExpr *>(assign.target.get());
    Symbol *symbol = find_symbol(var_expr->name.get_lexeme());
    if (!symbol) {

      return;
    }

  }

  if (!check_types(target_type, value_type)) {
    report_error("Tipos incompatíveis na atribuição. Alvo é do tipo '" + target_type.get_lexeme() +
                 "', mas a expressão é do tipo '" + value_type.get_lexeme() + "'.",
                 assign.equals_token); // Token '=' como referência
  }
}

void SemanticAnalyzer::visit_return(const ReturnStmt &ret) {
  FunctionStmt *current_function = nullptr;
  // Encontra a função atual na pilha de contexto
  std::stack<Stmt *> temp_search_stack = context_stack;
  while (temp_search_stack.size() > 1) { // >1 para não considerar o próprio return stmt como contexto se for o último
    Stmt *s = temp_search_stack.top();
    temp_search_stack.pop(); // Remove o topo para olhar o anterior
    if (temp_search_stack.empty()) break;
    s = temp_search_stack.top(); // Pega o contexto real

    if (s && s->get_type() == StmtType::FUNCTION) {
      current_function = &(s->get_function_stmt());
      break;
    }
  }
  
  if (!current_function) {
    report_error("Statement 'return' encontrado fora de uma função.",
                 ret.value ? get_expr_token(ret.value) : Token(TokenType::RETURN, "return", ret.value ? get_expr_token(ret.value).get_line() : 0, 0)); // Token do valor ou um 'return' dummy
    return;
  }

  Token return_value_type = create_type_token(TokenType::TYPE_NONE); // Tipo 'void' por padrão
  if (ret.value) { // Se há uma expressão de retorno
    return_value_type = visit(ret.value.get());
    // Se o tipo do valor de retorno for 'void' e houve erros, e o tipo esperado não é 'void'
    if (return_value_type.get_type() == TokenType::TYPE_NONE && !errors.empty() && current_function->return_type.get_type() != TokenType::TYPE_NONE) {
      return; // Erro já reportado
    }
  }

  // Compara o tipo do valor retornado com o tipo de retorno declarado da função
  if (!check_types(current_function->return_type, return_value_type)) {
    report_error(
        "Tipo de retorno incompatível na função '" + current_function->name.get_lexeme() +
        "'. Esperado: " + current_function->return_type.get_lexeme() +
        ", Encontrado: " + return_value_type.get_lexeme() + ".",
        ret.value ? get_expr_token(ret.value) : current_function->name); // Token do valor ou nome da função
  }
}

void SemanticAnalyzer::visit_break(const BreakStmt &brk) {
  if (loop_level == 0) {
    report_error("Statement 'break' encontrado fora de um loop ('while' ou 'for').", brk.keyword);
  }
}

void SemanticAnalyzer::visit_continue(const ContinueStmt &cont) {
  if (loop_level == 0) {
    report_error("Statement 'continue' encontrado fora de um loop ('while' ou 'for').", cont.keyword);
  }
}

// Analisa o corpo da função e seus parâmetros
void SemanticAnalyzer::visit_function(const FunctionStmt &func) {
  push_scope(); // Novo escopo para os parâmetros e corpo da função

  // Declara os parâmetros no novo escopo
  if (func.params.param_names && func.params.param_types) {
    auto &names = *func.params.param_names;
    auto &types = *func.params.param_types;
    if (names.size() == types.size()) {
      for (size_t i = 0; i < names.size(); ++i) {
        if (!is_valid_type(types[i])) { // Deve ter sido pego em register_function, mas verifica de novo
          report_error("Tipo de parâmetro inválido '" + types[i].get_lexeme() +
                       "' para '" + names[i].get_lexeme() + "' na análise do corpo da função.", names[i]);
        }
        Symbol param_symbol(names[i], types[i], false /*is_constant*/);
        if (!declare_symbol(param_symbol)) {
          report_error("Nome de parâmetro duplicado '" + names[i].get_lexeme() +
                       "' na função '" + func.name.get_lexeme() + "'.", names[i]);
        }
      }
    } else {
      // Erro interno, o parser deveria garantir consistência
      report_error("Erro interno: contagem de nomes e tipos de parâmetros inconsistente para função '" +
                   func.name.get_lexeme() + "'.", func.name);
    }
  }
  
  visit_block(func.body); // Visita o corpo da função (que é um bloco)
  pop_scope(); // Remove o escopo da função
}

void SemanticAnalyzer::visit_if(const IfStmt &if_stmt) {
  if (if_stmt.condition) {
    Token cond_type = visit(if_stmt.condition.get());
    // Se o tipo da condição não for booleano (e não for 'void' devido a um erro anterior)
    if (cond_type.get_type() != TokenType::TYPE_NONE && !is_boolean_type(cond_type)) {
      report_error("A condição do 'if' deve ser do tipo 'bool', mas foi '" + cond_type.get_lexeme() + "'.",
                   get_expr_token(if_stmt.condition));
    }
  } else {
    // Parser deveria garantir uma condição
    static Token dummy_if_token(TokenType::IF, "if", context_stack.empty() || !context_stack.top() ? 0 : context_stack.top()->get_token().get_line(), 0);
    report_error("Statement 'if' sem condição (erro interno do parser?).", dummy_if_token);
    return;
  }
  
  visit_block(if_stmt.then_block); // Visita o bloco 'then'
  if (if_stmt.has_else && if_stmt.else_block) { // Se houver um 'else'
    visit_block(*if_stmt.else_block); // Visita o bloco 'else'
  }
}

void SemanticAnalyzer::visit_while(const WhileStmt &while_stmt) {
  if (while_stmt.condition) {
    Token cond_type = visit(while_stmt.condition.get());
    if (cond_type.get_type() != TokenType::TYPE_NONE && !is_boolean_type(cond_type)) {
      report_error("A condição do 'while' deve ser do tipo 'bool', mas foi '" + cond_type.get_lexeme() + "'.",
                   get_expr_token(while_stmt.condition));
    }
  } else {
    static Token dummy_while_token(TokenType::WHILE, "while", context_stack.empty() || !context_stack.top() ? 0 : context_stack.top()->get_token().get_line(), 0);
    report_error("Statement 'while' sem condição (erro interno do parser?).", dummy_while_token);
    return;
  }
  
  loop_level++; // Entra em um loop
  visit_block(while_stmt.body); // Visita o corpo do loop
  loop_level--; // Sai do loop
}

void SemanticAnalyzer::visit_for(const ForStmt &for_stmt) {
  push_scope(); 
  if (for_stmt.initializer) {
    visit(for_stmt.initializer.get());
  }
  
  if (for_stmt.condition) {
    Token cond_type = visit(for_stmt.condition.get());
    if (cond_type.get_type() != TokenType::TYPE_NONE && !is_boolean_type(cond_type)) {
      report_error("A condição do 'for' deve ser do tipo 'bool', mas foi '" + cond_type.get_lexeme() + "'.",
                   get_expr_token(for_stmt.condition));
    }
  }


  if (for_stmt.increment) {
    visit(for_stmt.increment.get()); 
  }
  
  loop_level++;
  visit_block(for_stmt.body); 
  loop_level--;
  
  pop_scope(); 
}



void SemanticAnalyzer::visit_cchannel(const CChannelStmt &channel) {
  // Verifica se a função handler do canal está declarada
  if (function_table.find(channel.channel_name.get_lexeme()) == function_table.end()) {
    report_error("Função manipuladora '" + channel.channel_name.get_lexeme() +
                 "' para c_channel não declarada.", channel.channel_name);
  }

  Symbol *id1_sym = find_symbol(channel.id_1.get_lexeme());
  if (!id1_sym) {
    report_error("Identificador de descrição '" + channel.id_1.get_lexeme() +
                 "' usado em c_channel não foi declarado.", channel.id_1);
  } else if (id1_sym->type.get_type() != TokenType::TYPE_STRING) {
    report_error("Identificador de descrição '" + channel.id_1.get_lexeme() +
                 "' usado em c_channel deve ser do tipo 'string', mas é '" +
                 id1_sym->type.get_lexeme() + "'.", channel.id_1);
  }

  Symbol *id2_sym = find_symbol(channel.id_2.get_lexeme());
  if (!id2_sym) {
    report_error("Identificador de host '" + channel.id_2.get_lexeme() +
                 "' usado em c_channel não foi declarado.", channel.id_2);
  } else if (id2_sym->type.get_type() != TokenType::TYPE_STRING) {
    report_error("Identificador de host '" + channel.id_2.get_lexeme() +
                 "' usado em c_channel deve ser do tipo 'string', mas é '" +
                 id2_sym->type.get_lexeme() + "'.", channel.id_2);
  }
}

void SemanticAnalyzer::visit_expression_statement(const ExpressionStmt &expr_stmt) {
  if (expr_stmt.expression) {
    visit(expr_stmt.expression.get()); 
                                      
                                       
  } else {
    static Token dummy_expr_stmt_token(TokenType::SEMICOLON, ";", context_stack.empty()|| !context_stack.top() ? 0 : context_stack.top()->get_token().get_line(), 0);
    report_error("Expression statement com expressão nula (erro interno do parser?).", dummy_expr_stmt_token);
  }
}


// Dispatcher principal para visitar expressões e retornar seu tipo
Token SemanticAnalyzer::visit(Expr *expr) {
  if (!expr) {
    static Token dummy_expr_token{TokenType::TYPE_NONE, "[expr_nula_interna]", 0, 0};
    report_error("Tentativa de visitar uma expressão nula (erro interno).", dummy_expr_token);
    return create_type_token(TokenType::TYPE_NONE);
  }
  switch (expr->get_type()) {
  case ExprType::LITERAL:        return visit_literal(*static_cast<LiteralExpr *>(expr));
  case ExprType::VARIABLE:       return visit_variable(*static_cast<VariableExpr *>(expr));
  case ExprType::BINARY:         return visit_binary(*static_cast<BinaryExpr *>(expr));
  case ExprType::UNARY:          return visit_unary(*static_cast<UnaryExpr *>(expr));
  case ExprType::GROUPING:       return visit_grouping(*static_cast<GroupingExpr *>(expr));
  case ExprType::CALL:           return visit_call(*static_cast<CallExpr *>(expr));
  case ExprType::GET:            return visit_get(*static_cast<GetExpr *>(expr)); // Propriedades de objeto (se suportado)
  case ExprType::INDEX:          return visit_index(*static_cast<IndexExpr *>(expr));
  case ExprType::ARRAY_LITERAL:  return visit_array_literal(*static_cast<ArrayLiteralExpr *>(expr));
  default:
    report_error("Tipo de expressão desconhecido na AST (enum: " +
                 std::to_string(static_cast<int>(expr->get_type())) + ")", expr->get_token());
    return create_type_token(TokenType::TYPE_NONE);
  }
}


Token SemanticAnalyzer::visit_literal(const LiteralExpr &lit) {
  const Token &token = lit.value;
  switch (token.get_type()) {
  case TokenType::NUMBER:          return create_type_token(TokenType::TYPE_NUMBER);
  case TokenType::STRING_LITERAL:  return create_type_token(TokenType::TYPE_STRING);
  case TokenType::TRUE_LITERAL:
  case TokenType::FALSE_LITERAL:   return create_type_token(TokenType::TYPE_BOOL);
  default:
    report_error("Token inesperado (" + ::to_str(token.get_type()) +
                 ") encontrado em uma expressão LiteralExpr.", token);
    return create_type_token(TokenType::TYPE_NONE);
  }
}

Token SemanticAnalyzer::visit_variable(VariableExpr &var) {
  const std::string &name = var.name.get_lexeme();
  Symbol *symbol = find_symbol(name);
  if (symbol) {
    return symbol->type; // Retorna o tipo da variável declarada
  }
  // Verifica se é uma tentativa de usar um nome de função como variável
  if (builtin_functions.count(name) || function_table.count(name)) {
    report_error("Tentativa de usar função '" + name +
                 "' como variável. Para chamá-la, use parênteses '()'.", var.name);
    return create_type_token(TokenType::TYPE_NONE);
  }
  report_error("Identificador '" + name + "' não foi declarado.", var.name);
  return create_type_token(TokenType::TYPE_NONE); // Tipo erro/void
}

Token SemanticAnalyzer::visit_binary(const BinaryExpr &bin) {
  Token left_type = visit(bin.left.get());
  Token right_type = visit(bin.right.get());

  if ((left_type.get_type() == TokenType::TYPE_NONE || right_type.get_type() == TokenType::TYPE_NONE) && !errors.empty()) {
    return create_type_token(TokenType::TYPE_NONE);
  }

  TokenType op_type = bin.op.get_type();
  switch (op_type) {
  // Operadores Aritméticos
  case TokenType::PLUS:
    if (is_numeric_type(left_type) && is_numeric_type(right_type)) return create_type_token(TokenType::TYPE_NUMBER);
    if (is_string_type(left_type) && is_string_type(right_type))  return create_type_token(TokenType::TYPE_STRING); // Concatenação
    report_error("Operador '+' inválido para os tipos '" + left_type.get_lexeme() + "' e '" + right_type.get_lexeme() + "'.", bin.op);
    return create_type_token(TokenType::TYPE_NONE);
  case TokenType::MINUS:
  case TokenType::STAR:
  case TokenType::SLASH:
  case TokenType::PERCENT:
    if (!is_numeric_type(left_type) || !is_numeric_type(right_type)) {
      report_error("Operador '" + bin.op.get_lexeme() + "' exige operandos do tipo 'number'. Encontrados: '" +
                   left_type.get_lexeme() + "' e '" + right_type.get_lexeme() + "'.", bin.op);
      return create_type_token(TokenType::TYPE_NONE);
    }
    return create_type_token(TokenType::TYPE_NUMBER);
  

  case TokenType::EQUAL_COMPARE: 
  case TokenType::BANG_EQUAL:    

    return create_type_token(TokenType::TYPE_BOOL);


  case TokenType::GREATER:
  case TokenType::GREATER_EQUAL:
  case TokenType::LESS:
  case TokenType::LESS_EQUAL:
    if ((is_numeric_type(left_type) && is_numeric_type(right_type)) ||
        (is_string_type(left_type) && is_string_type(right_type))) { // Números ou strings
      return create_type_token(TokenType::TYPE_BOOL);
    }
    report_error("Comparação de ordem '" + bin.op.get_lexeme() + "' exige operandos 'number' ou 'string' compatíveis. Encontrados: '" +
                 left_type.get_lexeme() + "' e '" + right_type.get_lexeme() + "'.", bin.op);
    return create_type_token(TokenType::TYPE_NONE);

  // Operadores Lógicos
  case TokenType::AND_AND: // &&
  case TokenType::OR_OR:   // ||
    if (!is_boolean_type(left_type) || !is_boolean_type(right_type)) {
      report_error("Operador lógico '" + bin.op.get_lexeme() + "' exige operandos do tipo 'bool'. Encontrados: '" +
                   left_type.get_lexeme() + "' e '" + right_type.get_lexeme() + "'.", bin.op);
      return create_type_token(TokenType::TYPE_NONE);
    }
    return create_type_token(TokenType::TYPE_BOOL);
  
  default:
    report_error("Operador binário desconhecido ou não suportado: " + bin.op.get_lexeme(), bin.op);
    return create_type_token(TokenType::TYPE_NONE);
  }
}

Token SemanticAnalyzer::visit_unary(const UnaryExpr &un) {
  Token expr_type = visit(un.right.get()); // Tipo do operando
  if (expr_type.get_type() == TokenType::TYPE_NONE && !errors.empty()) {
    return create_type_token(TokenType::TYPE_NONE); // Erro no operando
  }

  TokenType op_type = un.op.get_type();
  switch (op_type) {
  case TokenType::MINUS: // Menos unário
    if (!is_numeric_type(expr_type)) {
      report_error("Operador unário '-' exige operando do tipo 'number', mas foi '" + expr_type.get_lexeme() + "'.", un.op);
      return create_type_token(TokenType::TYPE_NONE);
    }
    return create_type_token(TokenType::TYPE_NUMBER);
  case TokenType::BANG: // Negação lógica '!'
    if (!is_boolean_type(expr_type)) {
      report_error("Operador unário '!' exige operando do tipo 'bool', mas foi '" + expr_type.get_lexeme() + "'.", un.op);
      return create_type_token(TokenType::TYPE_NONE);
    }
    return create_type_token(TokenType::TYPE_BOOL);
  default:
    report_error("Operador unário desconhecido ou não suportado: " + un.op.get_lexeme(), un.op);
    return create_type_token(TokenType::TYPE_NONE);
  }
}

Token SemanticAnalyzer::visit_grouping(const GroupingExpr &grp) {
  if (grp.expression) {
    return visit(grp.expression.get()); // O tipo do grupo é o tipo da expressão interna
  }
  report_error("Expressão de agrupamento vazia (erro interno do parser).", grp.get_token());
  return create_type_token(TokenType::TYPE_NONE);
}

Token SemanticAnalyzer::visit_call(CallExpr &call) {
  std::string func_name = "";
  // O 'callee' de uma chamada de função deve ser um identificador (nome da função)
  if (auto var_callee = dynamic_cast<VariableExpr *>(call.callee.get())) {
    func_name = var_callee->name.get_lexeme();
  } else {
    report_error("Chamada de função complexa não suportada (o 'callee' não é um simples identificador).",
                 get_expr_token(call.callee));
    return create_type_token(TokenType::TYPE_NONE);
  }

  // Verifica se é uma função built-in
  auto builtin_it = builtin_functions.find(func_name);
  if (builtin_it != builtin_functions.end()) {
    const FunctionSignature &sig = builtin_it->second;
    // Verifica número de argumentos
    if (sig.check_param_count && call.arguments.size() != sig.param_types.size()) {
      report_error("Número incorreto de argumentos para função built-in '" + func_name +
                   "'. Esperado: " + std::to_string(sig.param_types.size()) +
                   ", Fornecido: " + std::to_string(call.arguments.size()) + ".",
                   call.paren); // Token '(' da chamada
    } else if (!sig.check_param_count && call.arguments.size() < static_cast<size_t>(sig.min_params)) {
       report_error("Número insuficiente de argumentos para função built-in '" + func_name +
                   "'. Mínimo esperado: " + std::to_string(sig.min_params) +
                   ", Fornecido: " + std::to_string(call.arguments.size()) + ".",
                   call.paren);
    }


    if (func_name == "len") { // 'len' aceita string ou array_number
        if (call.arguments.size() == 1) {
            Token arg_type = visit(call.arguments[0].get());
            if (arg_type.get_type() != TokenType::TYPE_STRING && arg_type.get_type() != TokenType::TYPE_ARRAY_NUMBER && arg_type.get_type() != TokenType::TYPE_NONE) {
                report_error("Argumento para função 'len' deve ser do tipo 'string' ou 'array_number', mas foi '" + arg_type.get_lexeme() + "'.", get_expr_token(call.arguments[0]));
            }
        }
    } else if (func_name == "to_string" || func_name == "to_bool") {
       
        if (call.arguments.size() == 1) {
            visit(call.arguments[0].get());
        }
    } else {
        for (size_t i = 0; i < call.arguments.size(); ++i) {
            if (i < sig.param_types.size() && sig.param_types[i] != TokenType::TYPE_NONE) { 
                Token arg_type = visit(call.arguments[i].get());
                if (arg_type.get_type() == TokenType::TYPE_NONE && !errors.empty()) continue;

                Token expected_param_type_token = create_type_token(sig.param_types[i]);
                if (!check_types(expected_param_type_token, arg_type)) {
                report_error("Tipo incompatível para argumento " + std::to_string(i + 1) +
                                " na chamada da função built-in '" + func_name +
                                "'. Esperado: " + expected_param_type_token.get_lexeme() +
                                ", Encontrado: " + arg_type.get_lexeme() + ".",
                                get_expr_token(call.arguments[i]));
                }
            } else if (i < sig.param_types.size()) { // Se o tipo esperado é TYPE_NONE, apenas visita o argumento
                 visit(call.arguments[i].get());
            }
            // Se houver mais argumentos do que parâmetros esperados e check_param_count for false,
            // apenas visitamos os argumentos extras sem checagem de tipo específica aqui.
             else if (!sig.check_param_count && i >= sig.param_types.size()){
                 visit(call.arguments[i].get());
             }
        }
    }
    return create_type_token(sig.return_type); // Retorna o tipo de retorno da função built-in
  }

  // Verifica se é uma função definida pelo usuário
  auto func_it = function_table.find(func_name);
  if (func_it == function_table.end()) {
    report_error("Função '" + func_name + "' não declarada.", get_expr_token(call.callee));
    return create_type_token(TokenType::TYPE_NONE);
  }

  FunctionStmt *func_decl = func_it->second; // Nó da AST da declaração da função
  // Verifica número de argumentos
  size_t expected_args_count = func_decl->params.param_names ? func_decl->params.param_names->size() : 0;
  if (call.arguments.size() != expected_args_count) {
    report_error("Número incorreto de argumentos para função '" + func_name +
                 "'. Esperado: " + std::to_string(expected_args_count) +
                 ", Fornecido: " + std::to_string(call.arguments.size()) + ".",
                 call.paren);
  }

  // Verifica tipos dos argumentos
  if (func_decl->params.param_types) {
    const auto &expected_param_types_tokens = *(func_decl->params.param_types);
    for (size_t i = 0; i < call.arguments.size(); ++i) {
      if (i < expected_param_types_tokens.size()) { // Garante que não acessa fora dos limites
        Token arg_type = visit(call.arguments[i].get());
        if (arg_type.get_type() == TokenType::TYPE_NONE && !errors.empty()) continue; // Erro no argumento

        const Token &expected_type_token = expected_param_types_tokens[i];
        if (!check_types(expected_type_token, arg_type)) {
          report_error("Tipo incompatível para argumento " + std::to_string(i + 1) +
                       " na chamada da função '" + func_name +
                       "'. Esperado: " + expected_type_token.get_lexeme() +
                       ", Encontrado: " + arg_type.get_lexeme() + ".",
                       get_expr_token(call.arguments[i]));
        }
      }
    }
  }
  return func_decl->return_type; // Retorna o tipo de retorno declarado da função
}

Token SemanticAnalyzer::visit_get(const GetExpr &get) {
  // Acesso a membro '.' (propriedades de objeto)
  // Se sua linguagem não suporta objetos e propriedades, isso é um erro.
  report_error("Acesso a membro '.' não suportado nesta versão da linguagem.", get.name);
  return create_type_token(TokenType::TYPE_NONE);
}

Token SemanticAnalyzer::visit_index(const IndexExpr &index_expr_node) {
  Token object_type = visit(index_expr_node.object.get()); // Tipo do objeto sendo indexado (ex: array, string)
  if (object_type.get_type() == TokenType::TYPE_NONE && !errors.empty()) {
    return create_type_token(TokenType::TYPE_NONE); // Erro no objeto
  }

  // Verifica o tipo da expressão do índice
  Token index_value_type = create_type_token(TokenType::TYPE_NONE);
  if (index_expr_node.index_expr) {
    index_value_type = visit(index_expr_node.index_expr.get());
  } else {
    // Parser deve garantir que a expressão de índice exista
    report_error("Expressão de índice nula encontrada (erro interno do parser).", index_expr_node.bracket);
    return create_type_token(TokenType::TYPE_NONE);
  }
  
  // Índice deve ser numérico
  if (index_value_type.get_type() != TokenType::TYPE_NONE && !is_numeric_type(index_value_type)) {
    report_error("A expressão do índice para '[]' deve ser do tipo 'number', mas foi '" +
                 index_value_type.get_lexeme() + "'.", get_expr_token(index_expr_node.index_expr));
    return create_type_token(TokenType::TYPE_NONE); // Retorna erro/void
  }

  // Verifica se o objeto é indexável e qual o tipo do elemento resultante
  if (is_string_type(object_type)) {
    return create_type_token(TokenType::TYPE_STRING); // Indexar uma string resulta em uma string (caractere)
  } else if (is_array_type(object_type)) {
    return get_array_element_type(object_type); // Retorna o tipo do elemento do array
  } else {
    report_error("Tentativa de acessar índice '[]' em um tipo não indexável ('" + object_type.get_lexeme() +
                 "'). Somente 'string' e arrays são indexáveis.", index_expr_node.bracket);
    return create_type_token(TokenType::TYPE_NONE);
  }
}

Token SemanticAnalyzer::visit_array_literal(ArrayLiteralExpr &arr_lit) {
  // Se o array literal estiver vazio, é um array_number válido
  if (arr_lit.elements.empty()) {
    return create_type_token(TokenType::TYPE_ARRAY_NUMBER); 
  }

  // Verifica se todos os elementos são do tipo 'number' para array_number
  // Se outros tipos de array fossem suportados, a lógica seria mais complexa para inferir o tipo do array.
  Token first_element_type = create_type_token(TokenType::TYPE_NONE);
  bool first = true;

  for (size_t i = 0; i < arr_lit.elements.size(); ++i) {
    const auto &element_expr = arr_lit.elements[i];
    if (!element_expr) {
      report_error("Elemento nulo encontrado no literal de array (erro interno parser?).", arr_lit.left_bracket);
      return create_type_token(TokenType::TYPE_NONE);
    }
    Token current_element_type = visit(element_expr.get());

    if (current_element_type.get_type() == TokenType::TYPE_NONE && !errors.empty()) {
      return create_type_token(TokenType::TYPE_NONE); // Erro no elemento
    }

    // Para array_number, todos os elementos devem ser 'number'
    if (!is_numeric_type(current_element_type)) {
      report_error("Todos os elementos em um literal 'array_number' devem ser do tipo 'number'. Elemento " +
                   std::to_string(i) + " é do tipo '" + current_element_type.get_lexeme() + "'.",
                   get_expr_token(element_expr));
      return create_type_token(TokenType::TYPE_NONE); // Tipo de array inválido
    }
    // Se fosse para inferir tipo de array genérico:
    // if (first) {
    //   first_element_type = current_element_type;
    //   first = false;
    // } else if (!check_types(first_element_type, current_element_type)) {
    //   report_error("Elementos de tipos mistos em literal de array não suportado ou inconsistente. Esperado '" +
    //                first_element_type.get_lexeme() + "', encontrado '" + current_element_type.get_lexeme() + "'.",
    //                get_expr_token(element_expr));
    //   return create_type_token(TokenType::TYPE_NONE);
    // }
  }
  return create_type_token(TokenType::TYPE_ARRAY_NUMBER); // Se todos são números, é um array_number
}

// Funções de relatório de erro
void SemanticAnalyzer::report_error(const std::string &message, const Token &token) {
  // Evita mensagens duplicadas exatas se o mesmo erro for detectado várias vezes seguidas
  std::string full_message = "[Linha " + std::to_string(token.get_line()) +
                             "] Erro Semântico: " + message;
  if (!token.get_lexeme().empty() && token.get_type() != TokenType::END_OF_FILE) {
    full_message += " (próximo a '" + token.get_lexeme() + "' na coluna " + std::to_string(token.get_column()) + ")";
  } else if (token.get_type() != TokenType::END_OF_FILE) {
     full_message += " (na coluna " + std::to_string(token.get_column()) + ")";
  } else {
     full_message += " (no final do arquivo)";
  }
  
  // Adiciona erro apenas se for diferente do último ou se a lista estiver vazia
  if (errors.empty() || errors.back()->get_message() != message) { // Compara apenas a 'message' base
     errors.push_back(std::make_unique<Error>(message, token.get_column(), token.get_line()));
  }
}

void SemanticAnalyzer::report_error(const std::string &message, unsigned int column, unsigned int line) {
  std::string full_message = "[Linha " + std::to_string(line) + "] Erro Semântico: " + message;
  if (column > 0) {
    full_message += " na coluna " + std::to_string(column);
  }

  if (errors.empty() || errors.back()->get_message() != message) {
    errors.push_back(std::make_unique<Error>(message, column, line));
  }
}

// Helper para obter um token de uma expressão, para mensagens de erro
const Token &SemanticAnalyzer::get_expr_token(const std::unique_ptr<Expr> &expr) {
  // Token dummy para casos onde a expressão é nula ou malformada
  static Token dummy_token{TokenType::IDENTIFIER, "[expressao_malformada]", 0, 0};
  if (expr) {
    return expr->get_token(); // Retorna o token principal da expressão (ex: operador em binário, nome em variável)
  }
  return dummy_token;
}

// Getters para informações de análise (usados pelo Executor ou para depuração)
const std::unordered_map<std::string, FunctionStmt *> &SemanticAnalyzer::get_function_table() const {
  return function_table;
}
const std::vector<std::unordered_map<std::string, SemanticAnalyzer::Symbol>> &SemanticAnalyzer::get_symbol_scopes() const {
  return scopes;
}
const std::unordered_map<std::string, std::vector<SemanticAnalyzer::Symbol>> &SemanticAnalyzer::get_function_symbols() const {
  return function_symbols;
}
const std::vector<std::pair<int, SemanticAnalyzer::Symbol>> &SemanticAnalyzer::get_all_declared_symbols() const {
  return all_declared_symbols;
}

// Função de impressão para depuração (como no seu código original)
void SemanticAnalyzer::imprimirVariaveisPorFuncao() const {
  std::cout << "\n--- Símbolos por Escopo (Resultado da Análise Semântica) ---" << std::endl;

  if (!scopes.empty()) {
    // Escopo Global (índice 0)
    const auto &global_scope = scopes[0];
    std::cout << "Escopo: Global (Profundidade 0)" << std::endl;
    if (global_scope.empty()) {
      std::cout << "  (Nenhuma variável global declarada)" << std::endl;
    } else {
      for (const auto &pair : global_scope) {
        const auto &symbol = pair.second;
        std::cout << "  - " << std::left << std::setw(20)
                  << symbol.name.get_lexeme() << " : " << std::setw(15)
                  << symbol.type.get_lexeme()
                  << " (Linha: " << symbol.name.get_line() << ")" << std::endl;
      }
    }
  } else {
    std::cout << "(Escopo global não encontrado/vazio)" << std::endl;
  }
  std::cout << "-------------------------------------------------------" << std::endl;

  // Símbolos específicos de funções (parâmetros e locais diretos)
  if (function_symbols.empty()) {
    std::cout << "(Nenhuma função com símbolos locais específicos rastreados para listar)" << std::endl;
  } else {
    for (const auto &func_pair : function_symbols) {
      const std::string &func_name = func_pair.first;
      const auto &symbols_in_func = func_pair.second; // Vetor de Symbol
      std::cout << "Escopo: Função '" << func_name << "'" << std::endl;
      if (symbols_in_func.empty()) {
        std::cout << "  (Nenhum parâmetro ou variável local específica desta função no rastreamento function_symbols)" << std::endl;
      } else {
        for (const auto &symbol : symbols_in_func) {
          std::cout << "  - " << std::left << std::setw(20)
                    << symbol.name.get_lexeme() << " : " << std::setw(15)
                    << symbol.type.get_lexeme()
                    << " (Linha: " << symbol.name.get_line() << ")" << std::endl;
        }
      }
      std::cout << "-------------------------------------------------------" << std::endl;
    }
  }

  std::cout << "\n--- Todos os Símbolos Declarados com Profundidade (all_declared_symbols) ---" << std::endl;
  if (all_declared_symbols.empty()) {
    std::cout << "(Nenhum símbolo declarado)" << std::endl;
  } else {
    for (const auto &pair : all_declared_symbols) {
      int depth = pair.first; // Profundidade do escopo onde foi declarado
      const auto &symbol_info = pair.second;
      std::cout << std::string(depth * 2, ' ') // Indentação pela profundidade
                << "- " << std::left << std::setw(20) << symbol_info.name.get_lexeme() << " : "
                << std::setw(15) << symbol_info.type.get_lexeme()
                << " (Linha: " << symbol_info.name.get_line()
                << ", Profundidade do Escopo: " << depth << ")" << std::endl;
    }
  }
  std::cout << "-------------------------------------------------------" << std::endl;
}