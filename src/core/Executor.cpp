#include "../include/core/Executor.h"
#include "../include/core/Error.h"
#include <iostream>
#include <stdexcept>
#include <mutex>
#include <vector>
#include <future>
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <string.h>

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
    auto it = table_function.find(name);
    if (it != table_function.end()) {
        return nullptr; // Just return nullptr since we handle functions differently
    }
    return nullptr;
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
        case ExprType::ARRAY_LITERAL: return visit_array_literal(*static_cast<ArrayLiteralExpr*>(expr));
        case ExprType::INDEX: return visit_index(*static_cast<IndexExpr*>(expr));
        default: throw std::runtime_error("Unknown expression type");
    }
}

Executor::Value Executor::visit_literal(const LiteralExpr& expr) {
    const Token& token = expr.value;
    
    switch (token.get_type()) {
        case TokenType::NUMBER: return token.get_double();
        case TokenType::STRING_LITERAL: return token.get_string();
        case TokenType::TYPE_ARRAY_NUMBER : return token.get_array();
        case TokenType::TRUE_LITERAL: return true;
        case TokenType::FALSE_LITERAL: return false;
        default: throw std::runtime_error("Invalid literal type");
    }
}

Executor::Value Executor::visit_array_literal(const ArrayLiteralExpr& expr) {
    std::vector<double> elements;
    for (const auto& element_expr : expr.elements) {
        Value element_value = visit(element_expr.get());
        if (!std::holds_alternative<double>(element_value)) {
            throw std::runtime_error("Array elements must be numbers");
        }
        elements.push_back(std::get<double>(element_value));
    }
    return elements;
}

// Adicione esta função para visitar index expressions
Executor::Value Executor::visit_index(const IndexExpr& expr) {
    Value array_value = visit(expr.object.get());
    Value index_value = visit(expr.index_expr.get());
    
    if (!is_array(array_value)) {
        throw std::runtime_error("Cannot index non-array value");
    }
    
    if (!std::holds_alternative<double>(index_value)) {
        throw std::runtime_error("Array index must be a number");
    }
    
    const auto& array = std::get<std::vector<double>>(array_value);
    double index = std::get<double>(index_value);
    
    if (index < 0 || index >= array.size()) {
        throw std::runtime_error("Array index out of bounds");
    }
    
    return array[static_cast<size_t>(index)];
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
            std::string output;
            for (auto& arg : expr.arguments) { 
                Value value = visit(arg.get()); 
                if (std::holds_alternative<double>(value)) {

                    double number = std::get<double>(value);
                    const double epsilon = 1e-12;

                    if (std::abs(number) < epsilon) {
                        std::ostringstream oss;
                        oss << std::setprecision(17) << std::scientific << number;
                        output += oss.str();
                    } else {
                        output += std::to_string(std::get<double>(value));
                    }
                } else if (std::holds_alternative<std::string>(value)) {
                    output += std::get<std::string>(value);
                } else if (std::holds_alternative<bool>(value)) {
                    output += std::get<bool>(value) ? "true" : "false";
                } else if (is_array(value)) {
                    const auto& arr = std::get<std::vector<double>>(value);
                    output += "[";
                    for (size_t i = 0; i < arr.size(); ++i) {
                        if (i > 0) output += ", ";
                        output += std::to_string(arr[i]);
                    }
                    output += "]";
                }
            }
            std::cout << output << std::endl; 
            return Value{output}; // Retorna a string impressa
        }

        if (func_name == "isalpha") {
            if (expr.arguments.size() != 1) {
                throw std::runtime_error("isalpha requer exatamente 1 argumento");
            }
            Value arg = visit(expr.arguments[0].get());
            if (!std::holds_alternative<std::string>(arg)) {
                throw std::runtime_error("Argumento de isalpha deve ser string");
            }
            std::string s = std::get<std::string>(arg);
            return Value{s.length() == 1 && isalpha(s[0])};
        }

        if (func_name == "isnum") {
            if (expr.arguments.size() != 1) {
                throw std::runtime_error("isnum requer exatamente 1 argumento");
            }
            Value arg = visit(expr.arguments[0].get());
            if (!std::holds_alternative<std::string>(arg)) {
                throw std::runtime_error("Argumento de isnum deve ser string");
            }
            std::string s = std::get<std::string>(arg);
            return Value{s.length() == 1 && isdigit(s[0])};
        }

        if (func_name == "len") {
            if (expr.arguments.size() != 1) {
                throw std::runtime_error("len() takes exactly one argument (" + std::to_string(expr.arguments.size()) + " given)");
            }
            Value arg = visit(expr.arguments[0].get());
            if (std::holds_alternative<std::string>(arg)) {
                return Value{static_cast<double>(std::get<std::string>(arg).length())};
            } else if (is_array(arg)) {
                const auto& arr = std::get<std::vector<double>>(arg);
                return Value{static_cast<double>(arr.size())};
            } else {
                throw std::runtime_error("len() argument must be a string or an array");
            }
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
            functionStack.pop();
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
        case StmtType::FOR: visit_for(stmt->get_for_stmt()); break;
        case StmtType::FUNCTION: visit_function(stmt->get_function_stmt()); break;
        case StmtType::SEQ: visit_seq(stmt->get_seq_stmt()); break;
        case StmtType::PAR: visit_par(stmt->get_par_stmt()); break;
        case StmtType::CCHANNEL: visit_cchannel(stmt->get_c_channel_stmt()); break;
        case StmtType::BLOCK: visit_block(stmt->get_block_stmt()); break;
        case StmtType::EXPRESSION: visit_expression(stmt->get_expression_stmt()); break;
        default: throw std::runtime_error("Unknown statement type");
    }
}

void Executor::visit_expression(const ExpressionStmt& stmt) {
    if (stmt.expression) {
        visit(stmt.expression.get());
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
    Value value;
    if (stmt.initializer) {
        value = visit(stmt.initializer.get());
    } else {
        // Valores padrão baseados no tipo
        switch(stmt.type.get_type()) {
            case TokenType::TYPE_NUMBER: value = Value{0.0}; break;
            case TokenType::TYPE_STRING: value = Value{""}; break;
            case TokenType::TYPE_BOOL: value = Value{false}; break;
            case TokenType::TYPE_ARRAY_NUMBER: value = Value{std::vector<double>()}; break;
            default: value = Value{};
        }
    }
    declare_variable(stmt.identifier.get_lexeme(), value);
}

void Executor::visit_assignment(const AssignmentStmt& stmt) {
    Value rvalue = visit(stmt.value.get());

    if (stmt.target->get_type() == ExprType::VARIABLE) {
        const VariableExpr* var_expr = static_cast<const VariableExpr*>(stmt.target.get());
        Value* var_ptr = find_variable(var_expr->name.get_lexeme());
        if (!var_ptr) {
            throw std::runtime_error("Undefined variable in assignment '" + var_expr->name.get_lexeme() + "'");
        }
        *var_ptr = rvalue;

    } else if (stmt.target->get_type() == ExprType::INDEX) {
        const IndexExpr* index_expr = static_cast<const IndexExpr*>(stmt.target.get());
        Value array_object_val = visit(index_expr->object.get());
        Value index_val = visit(index_expr->index_expr.get());

        if (!std::holds_alternative<std::vector<double>>(array_object_val)) {
            throw std::runtime_error("Cannot assign to indexed element of non-array type.");
        }

        if (!std::holds_alternative<double>(index_val)) {
            throw std::runtime_error("Array index must evaluate to a number.");
        }

        std::vector<double>* array_ptr = std::get_if<std::vector<double>>(&array_object_val);
        Value* var_containing_array_ptr = nullptr;
        if (index_expr->object->get_type() == ExprType::VARIABLE) {
            const VariableExpr* array_var_expr = static_cast<const VariableExpr*>(index_expr->object.get());
            var_containing_array_ptr = find_variable(array_var_expr->name.get_lexeme());
            if (!var_containing_array_ptr || !std::holds_alternative<std::vector<double>>(*var_containing_array_ptr)) {
                 throw std::runtime_error("Array variable '" + array_var_expr->name.get_lexeme() + "' not found or not an array.");
            }
        } else {
            throw std::runtime_error("Complex array targets not supported for assignment yet (e.g. function_call()[index] = value).");
        }

        std::vector<double>& actual_array = std::get<std::vector<double>>(*var_containing_array_ptr);
        double idx_double = std::get<double>(index_val);
        size_t index = static_cast<size_t>(idx_double);

        if (idx_double < 0 || index >= actual_array.size()) {
            throw std::runtime_error("Array index out of bounds in assignment.");
        }

        if (!std::holds_alternative<double>(rvalue)) {
            throw std::runtime_error("Cannot assign non-number value to an element of array_number.");
        }
        actual_array[index] = std::get<double>(rvalue);

    } else if (stmt.target->get_type() == ExprType::GET) {
        throw std::runtime_error("Assignment to object properties (GetExpr) not yet implemented.");
    } else {
        throw std::runtime_error("Invalid target for assignment.");
    }
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

void Executor::visit_for(const ForStmt& stmt) {
    // Execute initializer if it exists
     if (stmt.initializer) {
        visit(stmt.initializer.get());
    }

    flowStack.push({false, false, false, Value{}});
    
    // Loop principal do for
    while (true) {
        // Verifica a condição se existir
        if (stmt.condition) {
            Value cond = visit(stmt.condition.get());
            if (!is_truthy(cond)) {
                break;
            }
        }
        
        // Executa o corpo do for
        push_scope();  // Novo escopo para cada iteração
        try {
            visit_block(stmt.body);
        } catch (...) {
            pop_scope();
            throw;
        }
        pop_scope();
        
        // Verifica flags de controle de fluxo
        if (!flowStack.empty()) {
            if (flowStack.top().shouldBreak) {
                flowStack.pop();
                break;
            }
            if (flowStack.top().shouldContinue) {
                flowStack.top().shouldContinue = false;
            }
            if (flowStack.top().shouldReturn) {
                break;
            }
        }
        
        // Executa o incremento se existir
        if (stmt.increment) {
            visit(stmt.increment.get());
        }
    }
    
    if (!flowStack.empty() && !flowStack.top().shouldReturn) {
        flowStack.pop();
    }
}

void Executor::visit_function(const FunctionStmt& stmt) {
    // Just declare the function in the current scope
    // The actual execution happens when the function is called
    // declare_variable(stmt.name.get_lexeme(), Value{});
}

void Executor::visit_seq(const SeqStmt& stmt) {
    // Sequential execution is the default, so just visit the block
    visit_block(stmt.body);
}

void Executor::visit_par(const ParStmt& stmt) {
    // Implementação básica de execução paralela
    // Em uma implementação real, isso usaria threads ou outro mecanismo de paralelismo
    
    // Vector para armazenar os futuros das threads
    std::vector<std::future<void>> futures;
    
    // Mutex para proteger acesso a recursos compartilhados
    std::mutex mtx;
    
    // Flag para controle de erros
    std::atomic<bool> has_error(false);
    std::string error_msg;
    
    for (const auto& stmt_ptr : stmt.body.statements) {
        if (!stmt_ptr) continue;
        
        // Capturamos por cópia o que precisamos para a thread
        futures.emplace_back(std::async(std::launch::async, [&, stmt_ptr]() {
            try {
                // Cada statement do bloco PAR executa em seu próprio escopo
                Executor local_exec;
                local_exec.scopes = this->scopes; // Copia escopo atual
                local_exec.push_scope(); // Adiciona novo escopo para a thread
                
                // Executa o statement
                local_exec.visit(stmt_ptr.get());
                
                // Atualiza escopo pai com mudanças (se necessário)
                std::lock_guard<std::mutex> lock(mtx);
                if (!local_exec.scopes.empty()) {
                    // Merge das variáveis modificadas no escopo global
                    for (auto& var : local_exec.scopes.back()) {
                        if (auto global_var = find_variable(var.first)) {
                            *global_var = var.second;
                        }
                    }
                }
            } catch (const std::exception& e) {
                has_error = true;
                error_msg = e.what();
            } catch (...) {
                has_error = true;
                error_msg = "Unknown error in PAR block";
            }
        }));
    }
    
    // Espera todas as threads terminarem
    for (auto& future : futures) {
        future.get();
    }
    
    if (has_error) {
        throw std::runtime_error("Error in PAR execution: " + error_msg);
    }
}

void Executor::visit_cchannel(const CChannelStmt& stmt) {
    // Obter a função associada ao canal
    auto func_it = table_function.find(stmt.channel_name.get_lexeme());
    if (func_it == table_function.end()) {
        throw std::runtime_error("Undefined channel function '" + stmt.channel_name.get_lexeme() + "'");
    }
    FunctionStmt* function = func_it->second;

    // Obter os parâmetros do canal
    Value* description_val = find_variable(stmt.id_1.get_lexeme());
    Value* host_val = find_variable(stmt.id_2.get_lexeme());
    
    if (!description_val || !host_val) {
        throw std::runtime_error("Undefined variables in c_channel statement");
    }

    if (!std::holds_alternative<std::string>(*description_val) || 
        !std::holds_alternative<std::string>(*host_val)) {
        throw std::runtime_error("Channel parameters must be strings");
    }

    std::string description = std::get<std::string>(*description_val);
    std::string host = std::get<std::string>(*host_val);
    int port = 8585; // Porta fixa conforme o exemplo

    // Criar socket
    int server_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (server_fd < 0) {
        throw std::runtime_error("Failed to create socket");
    }

    // Configurar opções do socket
    int opt = 1;
    if (setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt)) < 0) {
        close(server_fd);
        throw std::runtime_error("Failed to set socket options");
    }

    // Configurar endereço
    struct sockaddr_in address;
    address.sin_family = AF_INET;
    address.sin_port = htons(port);
    
    if (inet_pton(AF_INET, host.c_str(), &address.sin_addr) <= 0) {
        close(server_fd);
        throw std::runtime_error("Invalid address/Address not supported");
    }

    // Vincular socket ao endereço
    if (bind(server_fd, (struct sockaddr*)&address, sizeof(address)) < 0) {
        close(server_fd);
        throw std::runtime_error("Bind failed");
    }

    // Começar a escutar
    if (listen(server_fd, 3) < 0) {
        close(server_fd);
        throw std::runtime_error("Listen failed");
    }

    std::cout << "Channel server '" << stmt.channel_name.get_lexeme() 
              << "' running on " << host << ":" << port << std::endl;
    std::cout << "Description: " << description << std::endl;

    // Aceitar conexões em uma thread separada
    std::thread([this, server_fd, function]() {
        sockaddr_in client_addr;
        socklen_t addr_len = sizeof(client_addr);
        char buffer[1024] = {0};

        while (true) {
            // Aceitar nova conexão
            int client_socket = accept(server_fd, (struct sockaddr*)&client_addr, &addr_len);
            if (client_socket < 0) {
                std::cerr << "Accept failed" << std::endl;
                continue;
            }

            // Ler mensagem do cliente
            ssize_t bytes_read = read(client_socket, buffer, sizeof(buffer));
            if (bytes_read <= 0) {
                close(client_socket);
                continue;
            }

            std::string message(buffer, bytes_read);
            std::cout << "Received: " << message << std::endl;

            try {
                // Chamar a função com a mensagem recebida
                std::vector<std::unique_ptr<Expr>> args;
                args.push_back(std::make_unique<LiteralExpr>(
                    Token(TokenType::STRING_LITERAL, message, 0, 0)
                ));

                CallExpr call(
                    std::make_unique<VariableExpr>(function->name),
                    function->name, // Token fictício para o parêntese
                    std::move(args)
                );

                Value result = visit_call(call);

                // Enviar resposta
                std::string response;
                if (std::holds_alternative<double>(result)) {
                    response = std::to_string(std::get<double>(result));
                } else if (std::holds_alternative<std::string>(result)) {
                    response = std::get<std::string>(result);
                } else if (std::holds_alternative<bool>(result)) {
                    response = std::get<bool>(result) ? "true" : "false";
                } else {
                    response = "INVALID_RESPONSE";
                }

                send(client_socket, response.c_str(), response.size(), 0);
            } catch (const std::exception& e) {
                std::string error = "ERROR: " + std::string(e.what());
                send(client_socket, error.c_str(), error.size(), 0);
            }

            close(client_socket);
            memset(buffer, 0, sizeof(buffer));
        }

        close(server_fd);
    }).detach();
}

// Helper functions
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
        double val_a = std::get<double>(a);
        double val_b = std::get<double>(b);
        const double epsilon = 1e-9;
        return std::abs(val_a - val_b) < epsilon;
    }
    
    if (std::holds_alternative<double>(a) || std::holds_alternative<double>(b)){
         return false;
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

bool Executor::is_array(const Value& value) {
    return std::holds_alternative<std::vector<double>>(value);
}
