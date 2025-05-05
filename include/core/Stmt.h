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
    FOR,
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

struct ForStmt {
    Token for_keyword;
    std::unique_ptr<Stmt> initializer;
    ExprPtr condition;
    std::unique_ptr<Stmt> increment;
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
        } else if constexpr (std::is_same_v<T, ForStmt>) { 
            type = StmtType::FOR; 
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

    StmtType get_type() const { return type; }

    const Token& get_token() const {
        return std::visit([](auto&& arg) -> const Token& {
            using T = std::decay_t<decltype(arg)>;
            if constexpr (std::is_same_v<T, DeclarationStmt>) {
                return arg.identifier;
            } else if constexpr (std::is_same_v<T, AssignmentStmt>) {
                return arg.identifier;
            } else if constexpr (std::is_same_v<T, ReturnStmt>) {
                return arg.value->get_token();
            } else if constexpr (std::is_same_v<T, FunctionStmt>) {
                return arg.name;
            } else if constexpr (std::is_same_v<T, IfStmt>) {
                return arg.condition->get_token();
            } else if constexpr (std::is_same_v<T, WhileStmt>) {
                return arg.condition->get_token();
            } else if constexpr (std::is_same_v<T, ForStmt>) { 
                return arg.for_keyword;
            } else if constexpr (std::is_same_v<T, CChannelStmt>) {
                return arg.identifier;
            }
            // Default case for statements without obvious token
            static Token dummy{TokenType::TYPE_NONE, "", 0, 0};
            return dummy;
        }, node);
    }

    // Getters for each statement type
    DeclarationStmt& get_decl_stmt() { return std::get<DeclarationStmt>(node); }
    AssignmentStmt& get_assign_stmt() { return std::get<AssignmentStmt>(node); }
    ReturnStmt& get_return_stmt() { return std::get<ReturnStmt>(node); }
    BreakStmt& get_break_stmt() { return std::get<BreakStmt>(node); }
    ContinueStmt& get_continue_stmt() { return std::get<ContinueStmt>(node); }
    BlockStmt& get_block_stmt() { return std::get<BlockStmt>(node); }
    IfStmt& get_if_stmt() { return std::get<IfStmt>(node); }
    WhileStmt& get_while_stmt() { return std::get<WhileStmt>(node); }
    ForStmt& get_for_stmt() { return std::get<ForStmt>(node); }
    FunctionStmt& get_function_stmt() { return std::get<FunctionStmt>(node); }
    SeqStmt& get_seq_stmt() { return std::get<SeqStmt>(node); }
    ParStmt& get_par_stmt() { return std::get<ParStmt>(node); }
    CChannelStmt& get_c_channel_stmt() { return std::get<CChannelStmt>(node); }

    // Move versions of getters
    DeclarationStmt move_decl_stmt() { return std::move(std::get<DeclarationStmt>(node)); }
    AssignmentStmt move_assign_stmt() { return std::move(std::get<AssignmentStmt>(node)); }
    ReturnStmt move_return_stmt() { return std::move(std::get<ReturnStmt>(node)); }
    BreakStmt move_break_stmt() { return std::move(std::get<BreakStmt>(node)); }
    ContinueStmt move_continue_stmt() { return std::move(std::get<ContinueStmt>(node)); }
    BlockStmt move_block_stmt() { return std::move(std::get<BlockStmt>(node)); }
    IfStmt move_if_stmt() { return std::move(std::get<IfStmt>(node)); }
    WhileStmt move_while_stmt() { return std::move(std::get<WhileStmt>(node)); }
    ForStmt move_for_stmt() { return std::move(std::get<ForStmt>(node)); }
    FunctionStmt move_function_stmt() { return std::move(std::get<FunctionStmt>(node)); }
    SeqStmt move_seq_stmt() { return std::move(std::get<SeqStmt>(node)); }
    ParStmt move_par_stmt() { return std::move(std::get<ParStmt>(node)); }
    CChannelStmt move_c_channel_stmt() { return std::move(std::get<CChannelStmt>(node)); }

private:
    using Variant =
        std::variant<DeclarationStmt, AssignmentStmt, ReturnStmt, BreakStmt,
                    ContinueStmt, BlockStmt, IfStmt, CChannelStmt, WhileStmt,
                    ForStmt, FunctionStmt, SeqStmt, ParStmt>;

    StmtType type;
    Variant node;
};