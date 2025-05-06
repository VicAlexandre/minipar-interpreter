#include "../include/core/Executor.h"
#include "../include/core/Error.h"
#include <iostream>
#include <stdexcept>

Executor::Executor(){
    push_scope();
}

void Executor::execute(const std::vector<std::unique_ptr<Stmt>>& statements, const std::unordered_map<std::string, FunctionStmt*>& function_table) {
    table_function.insert(function_table.begin(), function_table.end());
    
     try {
        for (const auto& stmt : statements) {
            if (!stmt) continue;
            visit(stmt.get()); 
        }

    } catch (const std::runtime_error& e) {
        std::cerr << "Runtime error: " << e.what() << std::endl;
    }
    
    pop_scope();
}

void Executor::execute_global_calls(Expr* expr) {
     if (auto call = dynamic_cast<CallExpr*>(expr)) {
        visit_call(*call);
    }
    else if (auto binary = dynamic_cast<BinaryExpr*>(expr)) {
        execute_global_calls(binary->left.get());
        execute_global_calls(binary->right.get());
    }
    else if (auto unary = dynamic_cast<UnaryExpr*>(expr)) {
        execute_global_calls(unary->right.get());
    }
    else if (auto grouping = dynamic_cast<GroupingExpr*>(expr)) {
        execute_global_calls(grouping->expression.get());
    }
}

void Executor::push_scope() {
    scopes.emplace_back();
}

void Executor::pop_scope() {
    if (!scopes.empty()) {
        scopes.pop_back();
    }
}

void Executor::declare_variable(const std::string& name, const Value& value) {
    if (scopes.empty()) {
        throw std::runtime_error("No active scope to declare variable");
    }

    if (scopes.back().count(name) > 0) {
        throw std::runtime_error("Variable '" + name + "' already declared in this scope");
    }
    scopes.back()[name] = value;
}

Executor::Value* Executor::find_variable(const std::string& name) {
    if (!functionStack.empty()) {
        auto& locals = functionStack.top().locals;

        auto it = locals.find(name); 
        if (it != locals.end()) {
            return &it->second; 
        }
    }

    for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) { 
        auto found = it->find(name);
        if (found != it->end()) {
            return &found->second;
        }
    }
    return nullptr;
}

Executor::Value* Executor::find_function(const std::string& name) {
    
}

// Visita de expressões
Executor::Value Executor::visit(Expr* expr) {
    if (!expr) {
        throw std::runtime_error("Null expression");
    }
    
    switch (expr->get_type()) {
        case ExprType::LITERAL: return visit_literal(*static_cast<LiteralExpr*>(expr));
        case ExprType::VARIABLE: return visit_variable(*static_cast<VariableExpr*>(expr));
        case ExprType::BINARY: return visit_binary(*static_cast<BinaryExpr*>(expr));
        case ExprType::UNARY: return visit_unary(*static_cast<UnaryExpr*>(expr));
        case ExprType::GROUPING: return visit_grouping(*static_cast<GroupingExpr*>(expr));
        case ExprType::CALL: return visit_call(*static_cast<CallExpr*>(expr));
        default: throw std::runtime_error("Unknown expression type");
    }
}

Executor::Value Executor::visit_literal(const LiteralExpr& expr) {
    const Token& token = expr.value;
    
    switch (token.get_type()) {
        case TokenType::NUMBER: return token.get_double();
        case TokenType::STRING_LITERAL: return token.get_string();
        case TokenType::TRUE_LITERAL: return true;
        case TokenType::FALSE_LITERAL: return false;
        default: throw std::runtime_error("Invalid literal type");
    }
}

Executor::Value Executor::visit_variable(const VariableExpr& expr) {
    
    Value* var = find_variable(expr.name.get_lexeme());
    if (var) {
        return *var;
    }
    throw std::runtime_error("Undefined variable '" + expr.name.get_lexeme() + "' during execution");

}

Executor::Value Executor::visit_binary(const BinaryExpr& expr) {
    Value left = visit(expr.left.get());
    Value right = visit(expr.right.get());
    
    switch (expr.op.get_type()) {
        // Operadores aritméticos
        case TokenType::PLUS: 
            if (std::holds_alternative<double>(left) && std::holds_alternative<double>(right)) {
                return std::get<double>(left) + std::get<double>(right);
            }
            if (std::holds_alternative<std::string>(left) && std::holds_alternative<std::string>(right)) {
                return std::get<std::string>(left) + std::get<std::string>(right);
            }
            throw std::runtime_error("Operands must be two numbers or two strings");
            
        case TokenType::MINUS:
            check_numeric_operands(expr.op, left, right);
            return std::get<double>(left) - std::get<double>(right);
            
        case TokenType::STAR:
            check_numeric_operands(expr.op, left, right);
            return std::get<double>(left) * std::get<double>(right);
            
        case TokenType::SLASH:
            check_numeric_operands(expr.op, left, right);
            if (std::get<double>(right) == 0) {
                throw std::runtime_error("Division by zero");
            }
            return std::get<double>(left) / std::get<double>(right);
            
        case TokenType::PERCENT:
            check_numeric_operands(expr.op, left, right);
            return fmod(std::get<double>(left), std::get<double>(right));
            
        // Operadores de comparação
        case TokenType::GREATER:
            check_numeric_operands(expr.op, left, right);
            return std::get<double>(left) > std::get<double>(right);
            
        case TokenType::GREATER_EQUAL:
            check_numeric_operands(expr.op, left, right);
            return std::get<double>(left) >= std::get<double>(right);
            
        case TokenType::LESS:
            check_numeric_operands(expr.op, left, right);
            return std::get<double>(left) < std::get<double>(right);
            
        case TokenType::LESS_EQUAL:
            check_numeric_operands(expr.op, left, right);
            return std::get<double>(left) <= std::get<double>(right);
            
        // Operadores de igualdade
        case TokenType::EQUAL_COMPARE:
            return is_equal(left, right);
            
        case TokenType::BANG_EQUAL:
            return !is_equal(left, right);
            
        // Operadores lógicos
        case TokenType::AND_AND:
            return is_truthy(left) && is_truthy(right);
            
        case TokenType::OR_OR:
            return is_truthy(left) || is_truthy(right);
            
        default:
            throw std::runtime_error("Unknown binary operator");
    }
}

Executor::Value Executor::visit_unary(const UnaryExpr& expr) {
    Value right = visit(expr.right.get());
    
    switch (expr.op.get_type()) {
        case TokenType::MINUS:
            check_numeric_operand(expr.op, right);
            return -std::get<double>(right);
            
        case TokenType::BANG:
            return !is_truthy(right);
            
        default:
            throw std::runtime_error("Unknown unary operator");
    }
}

Executor::Value Executor::visit_grouping(const GroupingExpr& expr) {
    return visit(expr.expression.get());
}

Executor::Value Executor::visit_call(CallExpr& expr) {

    if(auto var_expr = dynamic_cast<VariableExpr*>(expr.callee.get())){ 
        std::string func_name = var_expr->name.get_lexeme(); 

        
        if (func_name == "print") { 
            for (auto& arg : expr.arguments) { 
                Value value = visit(arg.get()); 
                std::visit([](auto&& arg) { std::cout << arg; }, value); 
            }
            std::cout << std::endl; 
            return Value{0.0}; 
        }

        
        auto it = table_function.find(func_name); 
        if (it == table_function.end()) { 
             
            throw std::runtime_error("Undefined function '" + func_name + "'"); 
        }
        FunctionStmt* function = it->second; 


        if (function->params.param_names && 
            expr.arguments.size() != function->params.param_names->size()) { 

            throw std::runtime_error("Wrong number of arguments for '" + func_name + "'"); 
        }

        
        FunctionContext context; 

        
        if (function->params.param_names) { 
            auto& param_names = *function->params.param_names; 
            for (size_t i = 0; i < param_names.size(); i++) { 
                Value arg_value = visit(expr.arguments[i].get()); 
                context.locals[param_names[i].get_lexeme()] = arg_value; 
            }
        }


        functionStack.push(std::move(context));

        try {
            visit_block(function->body); 
        } catch (...) {
            functionStack.pop(); 
            throw; 
        }

        if (!functionStack.top().hasReturned &&
            function->return_type.get_type() != TokenType::TYPE_NONE) {
            functionStack.pop(); // [25]
            throw std::runtime_error("Function '" + func_name + "' must return a value");
        }

        Value result = functionStack.top().returnValue;
        functionStack.pop();
        return result;
    }
    throw std::runtime_error("Unknown function call");
}

// Visita de statements
void Executor::visit(Stmt* stmt) {
    if (!stmt) return;
    
    switch (stmt->get_type()) {
        case StmtType::DECLARATION: visit_declaration(stmt->get_decl_stmt()); break;
        case StmtType::ASSIGNMENT: visit_assignment(stmt->get_assign_stmt()); break;
        case StmtType::RETURN: visit_return(stmt->get_return_stmt()); break;
        case StmtType::IF: visit_if(stmt->get_if_stmt()); break;
        case StmtType::WHILE: visit_while(stmt->get_while_stmt()); break;
        case StmtType::FUNCTION: visit_function(stmt->get_function_stmt()); break;
        case StmtType::SEQ: visit_seq(stmt->get_seq_stmt()); break;
        case StmtType::PAR: visit_par(stmt->get_par_stmt()); break;
        case StmtType::CCHANNEL: visit_cchannel(stmt->get_c_channel_stmt()); break;
        case StmtType::BLOCK: visit_block(stmt->get_block_stmt()); break;
        default: throw std::runtime_error("Unknown statement type");
    }
}

void Executor::visit_block(const BlockStmt& stmt) {
    push_scope();
    for (const auto& stmt_ptr : stmt.statements) {
        if (stmt_ptr) {
            visit(stmt_ptr.get());
        }
    }
    pop_scope();
}

void Executor::visit_declaration(const DeclarationStmt& stmt) {
    Value value = visit(stmt.initializer.get());
    declare_variable(stmt.identifier.get_lexeme(), value);
}

void Executor::visit_assignment(const AssignmentStmt& stmt) {
    Value* var = find_variable(stmt.identifier.get_lexeme());
    if (!var) {
        throw std::runtime_error("Undefined assignment '" + stmt.identifier.get_lexeme() + "'");
    }
    *var = visit(stmt.value.get());
}

void Executor::visit_return(const ReturnStmt& stmt) {
    if (!functionStack.empty()) {
        FunctionContext& context = functionStack.top();
        context.hasReturned = true;
        if (stmt.value) {
            context.returnValue = visit(stmt.value.get());
        }
    } else {
        throw std::runtime_error("'return' outside of function");
    }

}

void Executor::visit_if(const IfStmt& stmt) {
    Value condition = visit(stmt.condition.get());
    if (is_truthy(condition)) {
        visit_block(stmt.then_block);
    } else if (stmt.has_else && stmt.else_block) {
        visit_block(*stmt.else_block);
    }
}

void Executor::visit_while(const WhileStmt& stmt) {
    flowStack.push({false, false, false, Value{}});
    
    while (is_truthy(visit(stmt.condition.get()))) {
        visit_block(stmt.body);
        
        if (!flowStack.empty()) {
            if (flowStack.top().shouldBreak) {
                flowStack.pop();
                break;
            }
            if (flowStack.top().shouldContinue) {
                flowStack.top().shouldContinue = false;
                continue;
            }
            if (flowStack.top().shouldReturn) {
                break;
            }
        }
    }
    
    if (!flowStack.empty() && !flowStack.top().shouldReturn) {
        flowStack.pop();
    }
}

void Executor::visit_function(const FunctionStmt& stmt) {
    // visit_block(stmt.body);
}

void Executor::visit_seq(const SeqStmt& stmt) {
    // Execução sequencial é o padrão, então apenas visitamos o bloco
    visit_block(stmt.body);
}

void Executor::visit_par(const ParStmt& stmt) {
    // Implementação simplificada - executa sequencialmente
    // Em uma implementação real, você usaria threads ou outra forma de paralelismo
    visit_block(stmt.body);
}

void Executor::visit_cchannel(const CChannelStmt& stmt) {
    // Implementação simplificada - apenas verifica se as variáveis existem
    if (!find_variable(stmt.id_1.get_lexeme())) {
        throw std::runtime_error("Undefined variable '" + stmt.id_1.get_lexeme() + "' in c_channel");
    }
    if (!find_variable(stmt.id_2.get_lexeme())) {
        throw std::runtime_error("Undefined variable '" + stmt.id_2.get_lexeme() + "' in c_channel");
    }
}

// Funções auxiliares
bool Executor::is_truthy(const Value& value) {
    if (std::holds_alternative<bool>(value)) {
        return std::get<bool>(value);
    }
    if (std::holds_alternative<double>(value)) {
        return std::get<double>(value) != 0;
    }
    if (std::holds_alternative<std::string>(value)) {
        return !std::get<std::string>(value).empty();
    }
    return false;
}

bool Executor::is_equal(const Value& a, const Value& b) {
    if (std::holds_alternative<double>(a) && std::holds_alternative<double>(b)) {
        return std::get<double>(a) == std::get<double>(b);
    }
    if (std::holds_alternative<bool>(a) && std::holds_alternative<bool>(b)) {
        return std::get<bool>(a) == std::get<bool>(b);
    }
    if (std::holds_alternative<std::string>(a) && std::holds_alternative<std::string>(b)) {
        return std::get<std::string>(a) == std::get<std::string>(b);
    }
    return false;
}

void Executor::check_numeric_operand(const Token& op, const Value& operand) {
    if (!std::holds_alternative<double>(operand)) {
        throw std::runtime_error("Operand must be a number for operator " + op.get_lexeme());
    }
}

void Executor::check_numeric_operands(const Token& op, const Value& left, const Value& right) {
    if (!std::holds_alternative<double>(left) || !std::holds_alternative<double>(right)) {
        throw std::runtime_error("Operands must be numbers for operator " + op.get_lexeme());
    }
}