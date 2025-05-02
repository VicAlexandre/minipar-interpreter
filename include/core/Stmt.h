#pragma once

#include "core/Expr.h"
#include "core/Token.h"

#include <memory>
#include <optional>
#include <variant>
#include <vector>

/**
 * @brief All possible statement types in the language.
 */
enum class StmtType {
  DECLARATION,
  ASSIGNMENT,
  RETURN,
  BREAK,
  CONTINUE,
  BLOCK,
  IF,
  WHILE,
  FUNCTION,
  SEQ,
  PAR,
  CCHANNEL,
};

/* declaration of Stmt and StmtPtr to avoid circular dependency */
struct Stmt;
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
  template <typename T> Stmt(T stmt) : node(std::move(stmt)) {
    if constexpr (std::is_same_v<T, DeclarationStmt>) {
      type = StmtType::DECLARATION;
    } else if constexpr (std::is_same_v<T, AssignmentStmt>) {
      type = StmtType::ASSIGNMENT;
    } else if constexpr (std::is_same_v<T, ReturnStmt>) {
      type = StmtType::RETURN;
    } else if constexpr (std::is_same_v<T, BreakStmt>) {
      type = StmtType::BREAK;
    } else if constexpr (std::is_same_v<T, ContinueStmt>) {
      type = StmtType::CONTINUE;
    } else if constexpr (std::is_same_v<T, BlockStmt>) {
      type = StmtType::BLOCK;
    } else if constexpr (std::is_same_v<T, IfStmt>) {
      type = StmtType::IF;
    } else if constexpr (std::is_same_v<T, WhileStmt>) {
      type = StmtType::WHILE;
    } else if constexpr (std::is_same_v<T, FunctionStmt>) {
      type = StmtType::FUNCTION;
    } else if constexpr (std::is_same_v<T, SeqStmt>) {
      type = StmtType::SEQ;
    } else if constexpr (std::is_same_v<T, ParStmt>) {
      type = StmtType::PAR;
    } else if constexpr (std::is_same_v<T, CChannelStmt>) {
      type = StmtType::CCHANNEL;
    }
  }

private:
  using Variant =
      std::variant<DeclarationStmt, AssignmentStmt, ReturnStmt, BreakStmt,
                   ContinueStmt, BlockStmt, IfStmt, CChannelStmt, WhileStmt,
                   FunctionStmt, SeqStmt, ParStmt>;

  StmtType type;
  Variant node;
};
