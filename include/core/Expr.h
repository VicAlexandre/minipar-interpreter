#pragma once

#include "core/Token.h"

#include <memory>
#include <vector>

class Expr {
public:
  virtual ~Expr() = default;
};

using ExprPtr = std::unique_ptr<Expr>;

class BinaryExpr : public Expr {
public:
  ExprPtr left;
  Token op;
  ExprPtr right;

  BinaryExpr(ExprPtr left, Token op, ExprPtr right)
      : left(std::move(left)), op(op), right(std::move(right)) {}
};

class UnaryExpr : public Expr {
public:
  Token op;
  ExprPtr right;

  UnaryExpr(Token op, ExprPtr right) : op(op), right(std::move(right)) {}
};

class GroupingExpr : public Expr {
public:
  ExprPtr expression;

  GroupingExpr(ExprPtr expression) : expression(std::move(expression)) {}
};

class LiteralExpr : public Expr {
public:
  Token value;

  LiteralExpr(Token value) : value(value) {}
};

class VariableExpr : public Expr {
public:
  Token name;

  VariableExpr(Token name) : name(name) {}
};

class AssignmentExpr : public Expr {
public:
  Token name;
  ExprPtr value;

  AssignmentExpr(Token name, ExprPtr value)
      : name(name), value(std::move(value)) {}
};

class CallExpr : public Expr {
public:
  ExprPtr callee;
  Token paren;
  std::vector<ExprPtr> arguments;

  CallExpr(ExprPtr callee, Token paren, std::vector<ExprPtr> arguments)
      : callee(std::move(callee)), paren(paren),
        arguments(std::move(arguments)) {}
};

class GetExpr : public Expr {
public:
  ExprPtr object;
  Token name;

  GetExpr(ExprPtr object, Token name) : object(std::move(object)), name(name) {}
};

class IndexExpr : public Expr {
public:
  ExprPtr object;
  Token bracket;
  Token index;

  IndexExpr(ExprPtr object, Token bracket, Token index)
      : object(std::move(object)), bracket(bracket), index(index) {}
};
