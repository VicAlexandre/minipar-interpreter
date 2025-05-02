#pragma once

#include "core/Token.h"

#include <memory>
#include <vector>

/**
 * @brief Base class for all expressions.
 */
class Expr {
public:
  virtual ~Expr() = default;
};

/**
 * A unique pointer to an expression.
 */
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
};

/**
 * @brief A class representing a unary expression. Such as `-a` or `!a`.
 */
class UnaryExpr : public Expr {
public:
  Token op;
  ExprPtr right;

  UnaryExpr(Token op, ExprPtr right) : op(op), right(std::move(right)) {}
};

/**
 * @brief A class representing a grouping expression. Such as `(a + b)`.
 */
class GroupingExpr : public Expr {
public:
  ExprPtr expression;

  GroupingExpr(ExprPtr expression) : expression(std::move(expression)) {}
};

/**
 * @brief A class representing a literal expression. Such as `1`, `true`, or
 * `"hello"`.
 */
class LiteralExpr : public Expr {
public:
  Token value;

  LiteralExpr(Token value) : value(value) {}
};

/**
 * @brief A class representing a variable expression. Such as `a` or `b`.
 */
class VariableExpr : public Expr {
public:
  Token name;

  VariableExpr(Token name) : name(name) {}
};

/**
 * @brief A class representing a logical expression. Such as `a && b` or `a ||
 */
class AssignmentExpr : public Expr {
public:
  Token name;
  ExprPtr value;

  AssignmentExpr(Token name, ExprPtr value)
      : name(name), value(std::move(value)) {}
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
};

/**
 * @brief A class representing a get expression. Such as `a.b`.
 */
class GetExpr : public Expr {
public:
  ExprPtr object;
  Token name;

  GetExpr(ExprPtr object, Token name) : object(std::move(object)), name(name) {}
};

/**
 * @brief A class representing an index expression. Such as `a[b]`.
 */
class IndexExpr : public Expr {
public:
  ExprPtr object;
  Token bracket;
  Token index;

  IndexExpr(ExprPtr object, Token bracket, Token index)
      : object(std::move(object)), bracket(bracket), index(index) {}
};
