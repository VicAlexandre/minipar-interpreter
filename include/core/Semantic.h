#pragma once

#include "core/Expr.h"
#include "core/Stmt.h"
#include "core/Token.h"
#include "core/Error.h"

#include <unordered_map>
#include <vector>
#include <memory>

class Semantic {
public:
    Semantic(std::vector<std::unique_ptr<Stmt>>& statements);
    
    std::vector<std::unique_ptr<Error>> analyze();

private:
    std::vector<std::unique_ptr<Stmt>>& statements;
    std::vector<std::unique_ptr<Error>> errors;
    
    struct symbol_table {
        bool is_constant;
        Token name;
        Token type;
    };
    
    std::vector<std::unordered_map<std::string, symbol_table>> scopes;
    
    void analyze_statement(const StmtPtr& stmt);
    void analyze_expression(const ExprPtr& expr, const Token& expected_type);
    void analyze_block(const BlockStmt& block);
    
    void analyze_declaration(const DeclarationStmt& decl);
    void analyze_assignment(const AssignmentStmt& assign);
    void analyze_function(const FunctionStmt& func);
    
    void push_scope();
    void pop_scope();
    bool declare_symbol(const symbol_table& symbol);
    symbol_table* find_symbol(const std::string& name);
    bool check_types(const Token& expected, const Token& actual);
};