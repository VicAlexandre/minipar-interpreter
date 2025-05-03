#include "Semantic.h"

Semantic::Semantic(std::vector<std::unique_ptr<Stmt>>& statements) 
    : statements(statements) {
    // Escopo global
    push_scope();
}

std::vector<std::unique_ptr<Error>> Semantic::analyze() {
    for (const auto& stmt : statements) {
        analyze_statement(stmt);
    }
    return std::move(errors);
}

void Semantic::analyze_statement(const StmtPtr& stmt) {
    switch (stmt->get_type()) {
        case StmtType::DECLARATION:
            analyze_declaration(stmt->get_decl_stmt());
            break;
        case StmtType::ASSIGNMENT:
            analyze_assignment(stmt->get_assign_stmt());
            break;
        case StmtType::FUNCTION:
            analyze_function(stmt->get_function_stmt());
            break;
        default:
            break;
    }
}

void Semantic::analyze_declaration(const DeclarationStmt& decl) {
    if (find_symbol(decl.identifier.get_string())) {
        errors.push_back(std::make_unique<Error>(
            "Variável '" + decl.identifier.get_string() + "' já declarada",
            decl.identifier.get_column(),
            decl.identifier.get_line()
        ));
        return;
    }
    
    analyze_expression(decl.initializer, decl.type);
    
    symbol_table symbol{decl.identifier, decl.type, false};
    declare_symbol(symbol);
}

void Semantic::analyze_assignment(const AssignmentStmt& assign) {
    symbol_table* symbol = find_symbol(assign.identifier.get_string());
    if (!symbol) {
        errors.push_back(std::make_unique<Error>(
            "Variável '" + assign.identifier.get_string() + "' não declarada",
            assign.identifier.get_column(),
            assign.identifier.get_line()
        ));
        return;
    }
    
    analyze_expression(assign.value, symbol->type);
}

void Semantic::analyze_function(const FunctionStmt& func) {
    symbol_table funcSymbol{func.name, func.return_type, true};
    declare_symbol(funcSymbol);
    
    push_scope();
    
    if (func.params.param_names && func.params.param_types) {
        auto& names = *func.params.param_names;
        auto& types = *func.params.param_types;
        
        for (size_t i = 0; i < names.size(); ++i) {
            symbol_table param{names[i], types[i], false};
            declare_symbol(param);
        }
    }
    
    analyze_block(func.body);
    
    pop_scope();
}