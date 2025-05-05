#pragma once

#include "Expr.h"
#include "Stmt.h"
#include "Token.h"
#include "Error.h"
#include <vector>
#include <memory>
#include <unordered_map>
#include <unordered_set>
#include <stack>
#include <string>
#include <variant>

class SemanticAnalyzer {
public:
    struct Symbol {
        Token name;
        Token type;
        bool is_constant;

        Symbol(const Token& n, const Token& t, bool ic);
    };

    SemanticAnalyzer();

    /**
     * @brief Analisa a lista de statements (AST) quanto a erros semânticos.
     * @param statements Vetor de ponteiros para os statements da AST (gerados pelo Parser).
     * @return Vetor de ponteiros para os erros semânticos encontrados. Retorna vazio se não houver erros.
     */
    std::vector<std::unique_ptr<Error>> analyze(std::vector<std::unique_ptr<Stmt>>& statements);

    /**
     * @brief Cria um token interno para representar um tipo básico.
     * Usado internamente para comparações e armazenamento de tipo.
     * @param type O TokenType do tipo desejado (ex: TokenType::TYPE_NUMBER).
     * @return Um Token representando o tipo.
     */
    static Token create_type_token(TokenType type);

    
                        /* Interface para o Runner  */

    /**
     * @brief Obtém acesso (somente leitura) à tabela de funções após a análise.
     * O Runner pode usar isso para encontrar a definição de uma função durante a execução.
     * @return Uma referência constante para o mapa de nomes de função para ponteiros FunctionStmt.
     * @note Chame este método *após* chamar analyze() com sucesso e verificar se não há erros.
     */
    const std::unordered_map<std::string, FunctionStmt*>& get_function_table() const;


private:
    /* Tabela de símbolos: um vetor de mapas, representando os escopos aninhados. */
    std::vector<std::unordered_map<std::string, Symbol>> scopes;

    /* Pilha de contexto: rastreia em qual statement (função, loop) estamos atualmente.
     * return, break, continue. */
    std::stack<Stmt*> context_stack;

    /* Tabela de funções: mapeia nomes de função para seus nós FunctionStmt na AST.
     * Os ponteiros não possuem ownership (os nós pertencem à AST principal). */
    std::unordered_map<std::string, FunctionStmt*> function_table;

    std::vector<std::unique_ptr<Error>> errors;

    void push_scope();
    void pop_scope();
    bool declare_symbol(const Symbol& symbol);
    Symbol* find_symbol(const std::string& name);

    bool check_types(const Token& expected, const Token& actual);
    bool is_numeric_type(const Token& type);
    bool is_boolean_type(const Token& type);
    bool is_valid_type(const Token& type_token);

    void register_function(const FunctionStmt& func);

    void visit(Stmt* stmt);
    void visit_block(const BlockStmt& block);
    void visit_declaration(DeclarationStmt& decl);
    void visit_assignment(const AssignmentStmt& assign);
    void visit_return(const ReturnStmt& ret);
    void visit_break(const BreakStmt& brk);
    void visit_continue(const ContinueStmt& cont);
    void visit_function(const FunctionStmt& func);
    void visit_if(const IfStmt& if_stmt);
    void visit_while(const WhileStmt& while_stmt);
    void visit_seq(const SeqStmt& seq);
    void visit_par(const ParStmt& par);
    void visit_cchannel(const CChannelStmt& channel);

    Token visit(Expr* expr);
    Token visit_literal(const LiteralExpr& lit);
    Token visit_variable(VariableExpr& var);
    Token visit_binary(const BinaryExpr& bin);
    Token visit_unary(const UnaryExpr& un);
    Token visit_grouping(const GroupingExpr& grp);
    Token visit_call(CallExpr& call);
    Token visit_get(const GetExpr& get);
    Token visit_index(const IndexExpr& index);

    void report_error(const std::string& message, const Token& token);
    void report_error(const std::string& message, unsigned int column, unsigned int line);
    const Token& get_expr_token(const std::unique_ptr<Expr>& expr);
};
