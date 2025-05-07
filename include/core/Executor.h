#ifndef EXECUTOR_H
#define EXECUTOR_H

#include "Expr.h"
#include "Semantic.h"
#include "Stmt.h"
#include <math.h>
#include <memory>
#include <stack>
#include <unordered_map>
#include <variant>

class Executor {
public:
  using Value = std::variant<double, std::string, bool, std::vector<double>>;

  Executor();

  void execute(
      const std::vector<std::unique_ptr<Stmt>> &statements,
      const std::unordered_map<std::string, FunctionStmt *> &function_table);

  void execute_global_calls(Expr *expr);

private:
  // Tabela de símbolos (escopos)
  std::vector<std::unordered_map<std::string, Value>> scopes;

  std::unordered_map<std::string, FunctionStmt *> table_function;

  // Pilha de controle de fluxo
  struct FlowControl {
    bool shouldBreak;
    bool shouldContinue;
    bool shouldReturn;
    Value returnValue;
  };

  struct FunctionContext {
    std::unordered_map<std::string, Value> locals;
    Value returnValue;
    bool hasReturned = false;
  };

  std::stack<FlowControl> flowStack;
  std::stack<FunctionContext> functionStack;

  // Funções auxiliares
  void push_scope();
  void pop_scope();

  bool is_array(const Value &value);
  void declare_variable(const std::string &name, const Value &value);
  Value *find_variable(const std::string &name);
  Value *find_function(const std::string &name);
  Value visit_array_literal(const ArrayLiteralExpr &expr);

  // Métodos de visita
  Value visit(Expr *expr);
  Value visit_literal(const LiteralExpr &expr);
  Value visit_variable(const VariableExpr &expr);
  Value visit_binary(const BinaryExpr &expr);
  Value visit_unary(const UnaryExpr &expr);
  Value visit_grouping(const GroupingExpr &expr);
  Value visit_call(CallExpr &expr);
  Value visit_index(const IndexExpr &expr);

  void visit_break(const BreakStmt &brk);
  void visit_for(const ForStmt &stmt);
  void visit_expression(const ExpressionStmt &stmt);
  void visit_continue(const ContinueStmt &cont);
  void visit(Stmt *stmt);
  void visit_block(const BlockStmt &stmt);
  void visit_declaration(const DeclarationStmt &stmt);
  void visit_assignment(const AssignmentStmt &stmt);
  void visit_return(const ReturnStmt &stmt);
  void visit_if(const IfStmt &stmt);
  void visit_while(const WhileStmt &stmt);
  void visit_function(const FunctionStmt &stmt);
  void visit_seq(const SeqStmt &stmt);
  void visit_par(const ParStmt &stmt);
  void visit_cchannel(const CChannelStmt &stmt);

  // Helper functions
  bool is_truthy(const Value &value);
  bool is_equal(const Value &a, const Value &b);
  void check_numeric_operand(const Token &op, const Value &operand);
  void check_numeric_operands(const Token &op, const Value &left,
                              const Value &right);
};

#endif // EXECUTOR_H