#pragma once

#include "core/Token.h"
#include <memory>
#include <vector>

enum class ExprType {
  LITERAL,
  VARIABLE,
  BINARY,
  UNARY,
  GROUPING,
  CALL,
  GET,
  INDEX,
  ARRAY_LITERAL
};

/**
 * @brief Base class for all expressions.
 */
class Expr {
public:
  virtual ~Expr() = default;
  virtual ExprType get_type() const = 0;
  virtual const Token &get_token() const = 0;
};

using ExprPtr = std::unique_ptr<Expr>;

/**
 * @brief A class representing a binary expression. Such as `a + b` or `a * b`.
 */
class BinaryExpr : public Expr {
public:
  ExprPtr left;
  Token op;
  ExprPtr right;

  BinaryExpr(ExprPtr left, Token op, ExprPtr right)
      : left(std::move(left)), op(op), right(std::move(right)) {}

  ExprType get_type() const override { return ExprType::BINARY; }
  const Token &get_token() const override { return op; }
};

/**
 * @brief A class representing a unary expression. Such as `-a` or `!a`.
 */
class UnaryExpr : public Expr {
public:
  Token op;
  ExprPtr right;

  UnaryExpr(Token op, ExprPtr right) : op(op), right(std::move(right)) {}

  ExprType get_type() const override { return ExprType::UNARY; }
  const Token &get_token() const override { return op; }
};

/**
 * @brief A class representing a grouping expression. Such as `(a + b)`.
 */
class GroupingExpr : public Expr {
public:
  ExprPtr expression;

  GroupingExpr(ExprPtr expression) : expression(std::move(expression)) {}

  ExprType get_type() const override { return ExprType::GROUPING; }

  const Token &get_token() const override { return expression->get_token(); }
};

/**
 * @brief A class representing a literal expression. Such as `1`, `true`, or
 * `"hello"`.
 */
class LiteralExpr : public Expr {
public:
  Token value;

  LiteralExpr(Token value) : value(value) {}

  ExprType get_type() const override { return ExprType::LITERAL; }
  const Token &get_token() const override { return value; }
};

/**
 * @brief A class representing a variable expression. Such as `a` or `b`.
 */
class VariableExpr : public Expr {
public:
  Token name;

  VariableExpr(Token name) : name(name) {}

  ExprType get_type() const override { return ExprType::VARIABLE; }
  const Token &get_token() const override { return name; }
};

/**
 * @brief A class representing a call expression. Such as
 * `a(b, c)` or `a.b(c, d)`.
 */
class CallExpr : public Expr {
public:
  ExprPtr callee;
  Token paren;
  std::vector<ExprPtr> arguments;

  CallExpr(ExprPtr callee, Token paren, std::vector<ExprPtr> arguments)
      : callee(std::move(callee)), paren(paren),
        arguments(std::move(arguments)) {}

  ExprType get_type() const override { return ExprType::CALL; }
  const Token &get_token() const override { return paren; }
};

/**
 * @brief A class representing a get expression. Such as `a.b`.
 */
class GetExpr : public Expr {
public:
  ExprPtr object;
  Token name;

  GetExpr(ExprPtr object, Token name) : object(std::move(object)), name(name) {}

  ExprType get_type() const override { return ExprType::GET; }
  const Token &get_token() const override { return name; }
};

/**
 * @brief A class representing an index expression. Such as `a[b]`.
 */
class IndexExpr : public Expr {
public:
  ExprPtr object;
  Token bracket;
  ExprPtr index_expr;

  IndexExpr(ExprPtr object, Token bracket, ExprPtr index_expr)
      : object(std::move(object)), bracket(bracket),
        index_expr(std::move(index_expr)) {}

  ExprType get_type() const override { return ExprType::INDEX; }
  const Token &get_token() const override { return bracket; }
};

/**
 * @brief Representa um literal de array (ex: [1, 2, 3]).
 */
class ArrayLiteralExpr : public Expr {
public:
  Token left_bracket;
  std::vector<ExprPtr> elements;

  ArrayLiteralExpr(Token left_bracket, std::vector<ExprPtr> elements)
      : left_bracket(left_bracket), elements(std::move(elements)) {}

  ExprType get_type() const override { return ExprType::ARRAY_LITERAL; }
  const Token &get_token() const override { return left_bracket; }
};
