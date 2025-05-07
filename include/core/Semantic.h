#pragma once

#include "Error.h"
#include "Expr.h"
#include "Stmt.h"
#include "Token.h"
#include <iomanip>
#include <memory>
#include <stack>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

struct FunctionSignature {
  std::string name;
  std::vector<TokenType> param_types;
  TokenType return_type;
  bool check_param_count = true;
  int min_params = 0;
};

class SemanticAnalyzer {
public:
  struct Symbol {
    Token name;
    Token type;
    bool is_constant;

    Symbol(const Token &n, const Token &t, bool ic);
  };

  SemanticAnalyzer();

  /**
   * @brief Analisa a lista de statements (AST) quanto a erros semânticos.
   * @param statements Vetor de ponteiros para os statements da AST (gerados
   * pelo Parser).
   * @return Vetor de ponteiros para os erros semânticos encontrados. Retorna
   * vazio se não houver erros.
   */
  std::vector<std::unique_ptr<Error>>
  analyze(std::vector<std::unique_ptr<Stmt>> &statements);

  /**
   * @brief Cria um token interno para representar um tipo básico.
   * Usado internamente para comparações e armazenamento de tipo.
   * @param type O TokenType do tipo desejado (ex: TokenType::TYPE_NUMBER).
   * @return Um Token representando o tipo.
   */
  static Token create_type_token(TokenType type);

  /* Interface para o Runner  */

  const std::unordered_map<std::string, FunctionStmt *> &
  get_function_table() const;
  const std::vector<std::unordered_map<std::string, Symbol>> &
  get_symbol_scopes() const;
  const std::unordered_map<std::string, std::vector<Symbol>> &
  get_function_symbols() const;
  const std::vector<std::pair<int, Symbol>> &get_all_declared_symbols() const;
  void imprimirVariaveisPorFuncao() const;

private:
  int loop_level = 0;
  std::vector<std::unordered_map<std::string, Symbol>> scopes;
  std::stack<Stmt *> context_stack;
  std::unordered_map<std::string, FunctionStmt *> function_table;
  std::unordered_map<std::string, FunctionSignature> builtin_functions;
  std::unordered_map<std::string, std::vector<Symbol>> function_symbols;
  std::vector<std::unique_ptr<Error>> errors;
  std::vector<std::pair<int, Symbol>> all_declared_symbols;

  void register_builtin_functions();

  void push_scope();
  void pop_scope();
  bool declare_symbol(const Symbol &symbol);
  Symbol *find_symbol(const std::string &name);

  bool check_types(const Token &expected, const Token &actual);
  bool is_numeric_type(const Token &type_token);
  bool is_boolean_type(const Token &type_token);
  bool is_string_type(const Token &type_token);
  bool is_valid_type(const Token &type_token);
  bool is_array_type(const Token &type_token);
  Token get_array_element_type(const Token &array_type_token);

  void register_function(const FunctionStmt &func);
  void add_symbol_to_current_function(const Symbol &symbol);

  void visit(Stmt *stmt);
  void visit_block(const BlockStmt &block);
  void visit_declaration(DeclarationStmt &decl);
  void visit_assignment(const AssignmentStmt &assign);
  void visit_return(const ReturnStmt &ret);
  void visit_break(const BreakStmt &brk);
  void visit_continue(const ContinueStmt &cont);
  void visit_function(const FunctionStmt &func);
  void visit_if(const IfStmt &if_stmt);
  void visit_while(const WhileStmt &while_stmt);
  void visit_for(const ForStmt &for_stmt);
  void visit_seq(const SeqStmt &seq);
  void visit_par(const ParStmt &par);
  void visit_cchannel(const CChannelStmt &channel);
  void visit_expression_statement(const ExpressionStmt &expr_stmt);

  Token visit(Expr *expr);
  Token visit_literal(const LiteralExpr &lit);
  Token visit_variable(VariableExpr &var);
  Token visit_binary(const BinaryExpr &bin);
  Token visit_unary(const UnaryExpr &un);
  Token visit_grouping(const GroupingExpr &grp);
  Token visit_call(CallExpr &call);
  Token visit_get(const GetExpr &get);
  Token visit_index(const IndexExpr &index_expr_node);
  Token visit_array_literal(ArrayLiteralExpr &arr_lit);

  void report_error(const std::string &message, const Token &token);
  void report_error(const std::string &message, unsigned int column,
                    unsigned int line);
  const Token &get_expr_token(const std::unique_ptr<Expr> &expr);
};
