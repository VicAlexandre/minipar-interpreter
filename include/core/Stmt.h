#pragma once

#include "core/Expr.h"
#include "core/Token.h"

#include <memory>
#include <optional>
#include <variant>
#include <vector>

struct Stmt;
struct Expr;
using StmtPtr = std::shared_ptr<Stmt>;

struct Params {
  std::optional<std::vector<Token>> param_names;
  std::optional<std::vector<Token>> param_types;
};

struct DeclarationStmt {
  Token identifier;
  Token type;
  std::unique_ptr<Expr> initializer;
};

struct AssignmentStmt {
  Token identifier;
  std::unique_ptr<Expr> value;
};

struct ReturnStmt {
  std::unique_ptr<Expr> value;
};

struct BreakStmt {};

struct ContinueStmt {};

struct BlockStmt {
  std::vector<StmtPtr> statements;
};

struct IfStmt {
  std::unique_ptr<Expr> condition;
  BlockStmt then_block;
  std::optional<BlockStmt> else_block;
  bool has_else;
};

struct WhileStmt {
  std::unique_ptr<Expr> condition;
  BlockStmt body;
};

struct FunctionStmt {
  Token name;
  Params params;
  Token return_type;
  BlockStmt body;
};

struct SeqStmt {
  BlockStmt body;
};

struct ParStmt {
  BlockStmt body;
};

struct CChannelStmt {
  Token identifier;
  Token id_1;
  Token id_2;
};

class Stmt {
public:
  template <typename T> Stmt(T stmt) : node(std::move(stmt)) {}

private:
  using Variant =
      std::variant<DeclarationStmt, AssignmentStmt, ReturnStmt, BreakStmt,
                   ContinueStmt, BlockStmt, IfStmt, CChannelStmt, WhileStmt,
                   FunctionStmt, SeqStmt, ParStmt>;

  Variant node;
};
