#include "../include/core/Executor.h"
#include "../include/core/Error.h"
#include <iostream>
#include <stdexcept> // For runtime_error
#include <mutex>     // For PAR block (if used)
#include <vector>
#include <future>    // For PAR block (if used)
#include <sys/socket.h> // For CChannel
#include <netinet/in.h> // For CChannel
#include <unistd.h>     // For CChannel (close, read, send)
#include <arpa/inet.h>  // For CChannel (inet_pton)
#include <string.h>     // For CChannel (memset)
#include <cmath>        // For fmod, abs, exp
#include <iomanip>      // For print formatting
#include <sstream>      // For print formatting
#include <cctype>       // Required for ::isalpha and ::isdigit
#include <cstdlib>      // Required for rand(), srand()
#include <ctime>        // Required for time()

// Flag to ensure srand is called only once
namespace { // Use an anonymous namespace for internal linkage
    bool srand_called = false;
}

Executor::Executor(){
    push_scope();
    // Seed the random number generator once when the executor is created
    if (!srand_called) {
        srand(static_cast<unsigned int>(time(0)));
        srand_called = true;
    }
}

void Executor::execute(const std::vector<std::unique_ptr<Stmt>>& statements, const std::unordered_map<std::string, FunctionStmt*>& function_table) {
    table_function = function_table;

    try {
        for (const auto& stmt : statements) {
            if (!stmt) continue;
            visit(stmt.get());
        }
    } catch (const std::runtime_error& e) {
        std::cerr << "Runtime error: " << e.what() << std::endl;
    }
}

void Executor::execute_global_calls(Expr* expr) {
     if (!expr) return;

    if (auto call = dynamic_cast<CallExpr*>(expr)) {
        visit_call(*call);
    }
    // Avoid recursive calls here unless specifically intended for nested structures
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
        throw std::runtime_error("Internal error: No active scope to declare variable");
    }

    auto& current_scope = scopes.back();
    current_scope[name] = value;
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
        auto& scope_map = *it;
        auto found = scope_map.find(name);
        if (found != scope_map.end()) {
            return &found->second;
        }
    }

    return nullptr;
}

Executor::Value* Executor::find_function(const std::string& name) {
    auto it = table_function.find(name);
    if (it != table_function.end()) {
        return nullptr;
    }
    return nullptr;
}


Executor::Value Executor::visit(Expr* expr) {
    if (!expr) {
        throw std::runtime_error("Internal error: Trying to visit a null expression");
    }

    switch (expr->get_type()) {
        case ExprType::LITERAL:       return visit_literal(*static_cast<LiteralExpr*>(expr));
        case ExprType::VARIABLE:      return visit_variable(*static_cast<VariableExpr*>(expr));
        case ExprType::BINARY:        return visit_binary(*static_cast<BinaryExpr*>(expr));
        case ExprType::UNARY:         return visit_unary(*static_cast<UnaryExpr*>(expr));
        case ExprType::GROUPING:      return visit_grouping(*static_cast<GroupingExpr*>(expr));
        case ExprType::CALL:          return visit_call(*static_cast<CallExpr*>(expr));
        case ExprType::ARRAY_LITERAL: return visit_array_literal(*static_cast<ArrayLiteralExpr*>(expr));
        case ExprType::INDEX:         return visit_index(*static_cast<IndexExpr*>(expr));
        default:
            throw std::runtime_error("Unknown or unsupported expression type encountered during execution");
    }
}

Executor::Value Executor::visit_literal(const LiteralExpr& expr) {
    const Token& token = expr.value;

    switch (token.get_type()) {
        case TokenType::NUMBER:         return token.get_double();
        case TokenType::STRING_LITERAL: return token.get_string();
        case TokenType::TRUE_LITERAL:   return true;
        case TokenType::FALSE_LITERAL:  return false;
        default:
            throw std::runtime_error("Invalid token type for literal expression");
    }
}

Executor::Value Executor::visit_array_literal(const ArrayLiteralExpr& expr) {
    std::vector<double> elements;
    elements.reserve(expr.elements.size());

    for (const auto& element_expr : expr.elements) {
        Value element_value = visit(element_expr.get());
        if (!std::holds_alternative<double>(element_value)) {
            throw std::runtime_error("Array literal elements must evaluate to numbers");
        }
        elements.push_back(std::get<double>(element_value));
    }
    return elements;
}

Executor::Value Executor::visit_index(const IndexExpr& expr) {
    Value object_value = visit(expr.object.get());
    Value index_value = visit(expr.index_expr.get());

    if (!is_array(object_value)) {
        throw std::runtime_error("Cannot index a non-array value");
    }

    if (!std::holds_alternative<double>(index_value)) {
        throw std::runtime_error("Array index must evaluate to a number");
    }

    const auto& array_vec = std::get<std::vector<double>>(object_value);
    double index_double = std::get<double>(index_value);

    if (index_double < 0 || index_double >= array_vec.size() || index_double != floor(index_double)) {
         throw std::runtime_error("Array index out of bounds or not an integer");
    }
    size_t index = static_cast<size_t>(index_double);

    return array_vec[index];
}


Executor::Value Executor::visit_variable(const VariableExpr& expr) {
    Value* var_ptr = find_variable(expr.name.get_lexeme());
    if (var_ptr) {
        return *var_ptr;
    }
    throw std::runtime_error("Undefined variable '" + expr.name.get_lexeme() + "'");
}

Executor::Value Executor::visit_binary(const BinaryExpr& expr) {
    Value left = visit(expr.left.get());
    Value right; // Declare right here, evaluate only if needed for short-circuiting

    TokenType op_type = expr.op.get_type();

    if (op_type == TokenType::AND_AND) {
        if (!is_truthy(left)) return false;
        right = visit(expr.right.get()); // Evaluate right only if left is true
        return is_truthy(right);
    }
    if (op_type == TokenType::OR_OR) {
        if (is_truthy(left)) return true;
        right = visit(expr.right.get()); // Evaluate right only if left is false
        return is_truthy(right);
    }

    // For non-short-circuiting operators, evaluate right now
    right = visit(expr.right.get());

    if (op_type == TokenType::PLUS || op_type == TokenType::MINUS ||
        op_type == TokenType::STAR || op_type == TokenType::SLASH ||
        op_type == TokenType::PERCENT)
    {
        if (op_type == TokenType::PLUS && std::holds_alternative<std::string>(left) && std::holds_alternative<std::string>(right)) {
            return std::get<std::string>(left) + std::get<std::string>(right);
        }

        check_numeric_operands(expr.op, left, right);
        double left_num = std::get<double>(left);
        double right_num = std::get<double>(right);

        switch (op_type) {
            case TokenType::PLUS:    return left_num + right_num;
            case TokenType::MINUS:   return left_num - right_num;
            case TokenType::STAR:    return left_num * right_num;
            case TokenType::SLASH:
                if (right_num == 0.0) throw std::runtime_error("Division by zero");
                return left_num / right_num;
            case TokenType::PERCENT:
                if (right_num == 0.0) throw std::runtime_error("Modulo by zero");
                return fmod(left_num, right_num);
            default: break;
        }
    }

    if (op_type == TokenType::GREATER || op_type == TokenType::GREATER_EQUAL ||
        op_type == TokenType::LESS || op_type == TokenType::LESS_EQUAL)
    {
        if (std::holds_alternative<double>(left) && std::holds_alternative<double>(right)) {
            double left_num = std::get<double>(left);
            double right_num = std::get<double>(right);
            switch (op_type) {
                case TokenType::GREATER:       return left_num > right_num;
                case TokenType::GREATER_EQUAL: return left_num >= right_num;
                case TokenType::LESS:          return left_num < right_num;
                case TokenType::LESS_EQUAL:    return left_num <= right_num;
                default: break;
            }
        } else if (std::holds_alternative<std::string>(left) && std::holds_alternative<std::string>(right)) {
             const std::string& left_str = std::get<std::string>(left);
             const std::string& right_str = std::get<std::string>(right);
             switch (op_type) {
                case TokenType::GREATER:       return left_str > right_str;
                case TokenType::GREATER_EQUAL: return left_str >= right_str;
                case TokenType::LESS:          return left_str < right_str;
                case TokenType::LESS_EQUAL:    return left_str <= right_str;
                default: break;
            }
        } else {
            throw std::runtime_error("Comparison operators require two numbers or two strings, got incompatible types for " + expr.op.get_lexeme());
        }
    }

    if (op_type == TokenType::EQUAL_COMPARE || op_type == TokenType::BANG_EQUAL) {
        bool result = is_equal(left, right);
        return (op_type == TokenType::EQUAL_COMPARE) ? result : !result;
    }

    throw std::runtime_error("Unknown or unsupported binary operator: " + expr.op.get_lexeme());
}

Executor::Value Executor::visit_unary(const UnaryExpr& expr) {
    Value right = visit(expr.right.get());
    TokenType op_type = expr.op.get_type();

    if (op_type == TokenType::MINUS) {
        check_numeric_operand(expr.op, right);
        return -std::get<double>(right);
    }

    if (op_type == TokenType::BANG) {
        return !is_truthy(right);
    }

    throw std::runtime_error("Unknown or unsupported unary operator: " + expr.op.get_lexeme());
}

Executor::Value Executor::visit_grouping(const GroupingExpr& expr) {
    return visit(expr.expression.get());
}

Executor::Value Executor::visit_call(CallExpr& expr) {
    std::string func_name;
    if (auto var_expr = dynamic_cast<VariableExpr*>(expr.callee.get())) {
        func_name = var_expr->name.get_lexeme();
    } else {
        throw std::runtime_error("Calling non-function values or complex callees is not supported");
    }

    // --- Built-in Function Handling ---
    if (func_name == "print") {
        std::stringstream ss;
        for (size_t i = 0; i < expr.arguments.size(); ++i) {
            Value value = visit(expr.arguments[i].get());
            if (std::holds_alternative<double>(value)) {
                ss << std::get<double>(value);
            } else if (std::holds_alternative<std::string>(value)) {
                ss << std::get<std::string>(value);
            } else if (std::holds_alternative<bool>(value)) {
                ss << (std::get<bool>(value) ? "true" : "false");
            } else if (is_array(value)) {
                const auto& arr = std::get<std::vector<double>>(value);
                ss << "[";
                for (size_t j = 0; j < arr.size(); ++j) {
                    ss << arr[j] << (j == arr.size() - 1 ? "" : ", ");
                }
                ss << "]";
            } else {
                 ss << "<unprintable_value>";
            }
            if (i < expr.arguments.size() - 1) {
                 ss << " ";
            }
        }
        std::cout << ss.str() << std::endl;
        return Value{};
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

     if (func_name == "isalpha") {
        if (expr.arguments.size() != 1) throw std::runtime_error("isalpha requires exactly 1 argument");
        Value arg = visit(expr.arguments[0].get());
        if (!std::holds_alternative<std::string>(arg)) throw std::runtime_error("Argument of isalpha must be string");
        std::string s = std::get<std::string>(arg);
        return Value{s.length() == 1 && ::isalpha(static_cast<unsigned char>(s[0]))};
    }
    if (func_name == "isnum") {
        if (expr.arguments.size() != 1) throw std::runtime_error("isnum requires exactly 1 argument");
        Value arg = visit(expr.arguments[0].get());
        if (!std::holds_alternative<std::string>(arg)) throw std::runtime_error("Argument of isnum must be string");
        std::string s = std::get<std::string>(arg);
        return Value{s.length() == 1 && ::isdigit(static_cast<unsigned char>(s[0]))};
    }
     if (func_name == "to_string") {
         if (expr.arguments.size() != 1) throw std::runtime_error("to_string requires exactly 1 argument");
         Value arg = visit(expr.arguments[0].get());
         if (std::holds_alternative<double>(arg)) return std::to_string(std::get<double>(arg));
         if (std::holds_alternative<bool>(arg)) return std::string(std::get<bool>(arg) ? "true" : "false");
         if (std::holds_alternative<std::string>(arg)) return std::get<std::string>(arg);
         throw std::runtime_error("to_string argument must be number, bool, or string");
     }
     // --- ADDED: Implementation for 'random' ---
     if (func_name == "random") {
         if (!expr.arguments.empty()) {
             throw std::runtime_error("random() takes no arguments (" + std::to_string(expr.arguments.size()) + " given)");
         }
         // Generate random double between 0.0 and 1.0
         return static_cast<double>(rand()) / RAND_MAX;
     }
     // --- ADDED: Implementation for 'exp' ---
     if (func_name == "exp") {
         if (expr.arguments.size() != 1) {
             throw std::runtime_error("exp() takes exactly one argument (" + std::to_string(expr.arguments.size()) + " given)");
         }
         Value arg = visit(expr.arguments[0].get());
         // Check if the argument is a number
         check_numeric_operand(expr.paren, arg); // Use expr.paren as a token for location if needed
         // Calculate and return e^x
         return std::exp(std::get<double>(arg));
     }
     // Add other built-ins (to_number, sleep, etc.) here if needed

    // --- User-Defined Function Handling ---
    auto it = table_function.find(func_name);
    if (it == table_function.end()) {
        // Check again for built-ins in case it wasn't handled above
        // This prevents throwing "Undefined function" for unhandled built-ins
        if (func_name == "sleep" || func_name == "input" || func_name == "to_number" || func_name == "to_bool" /* add others */) {
             throw std::runtime_error("Built-in function '" + func_name + "' is declared but not implemented in Executor::visit_call");
        }
        throw std::runtime_error("Undefined function '" + func_name + "'");
    }
    FunctionStmt* function = it->second;

    size_t expected_args = function->params.param_names ? function->params.param_names->size() : 0;
    if (expr.arguments.size() != expected_args) {
        throw std::runtime_error("Wrong number of arguments for function '" + func_name + "'. Expected " +
                                 std::to_string(expected_args) + ", got " + std::to_string(expr.arguments.size()));
    }

    FunctionContext context;

    if (function->params.param_names) {
        auto& param_names = *function->params.param_names;
        for (size_t i = 0; i < param_names.size(); ++i) {
            Value arg_value = visit(expr.arguments[i].get());
            context.locals[param_names[i].get_lexeme()] = arg_value;
        }
    }

    functionStack.push(std::move(context));

    Value return_value;
    try {
        visit_block(function->body);
    } catch (...) {
        functionStack.pop();
        throw;
    }

    FunctionContext& finished_context = functionStack.top();
    if (finished_context.hasReturned) {
        return_value = finished_context.returnValue;
    } else if (function->return_type.get_type() != TokenType::TYPE_NONE) {
        functionStack.pop();
        throw std::runtime_error("Function '" + func_name + "' did not return a value");
    }

    functionStack.pop();
    return return_value;
}


void Executor::visit(Stmt* stmt) {
    if (!stmt) {
         return;
    }

    if (!functionStack.empty() && functionStack.top().hasReturned) {
        return;
    }
    if (!flowStack.empty()) {
        if (flowStack.top().shouldBreak || flowStack.top().shouldContinue) {
            return;
        }
    }

    switch (stmt->get_type()) {
        case StmtType::DECLARATION: visit_declaration(stmt->get_decl_stmt()); break;
        case StmtType::ASSIGNMENT:  visit_assignment(stmt->get_assign_stmt()); break;
        case StmtType::RETURN:      visit_return(stmt->get_return_stmt()); break;
        case StmtType::IF:          visit_if(stmt->get_if_stmt()); break;
        case StmtType::WHILE:       visit_while(stmt->get_while_stmt()); break;
        case StmtType::FOR:         visit_for(stmt->get_for_stmt()); break;
        case StmtType::FUNCTION:    visit_function(stmt->get_function_stmt()); break;
        case StmtType::SEQ:         visit_seq(stmt->get_seq_stmt()); break;
        case StmtType::PAR:         visit_par(stmt->get_par_stmt()); break;
        case StmtType::CCHANNEL:    visit_cchannel(stmt->get_c_channel_stmt()); break;
        case StmtType::BLOCK:       visit_block(stmt->get_block_stmt()); break;
        case StmtType::EXPRESSION:  visit_expression(stmt->get_expression_stmt()); break;
        case StmtType::BREAK:       visit_break(stmt->get_break_stmt()); break;
        case StmtType::CONTINUE:    visit_continue(stmt->get_continue_stmt()); break;
        default:
            throw std::runtime_error("Unknown or unsupported statement type encountered during execution");
    }
}

void Executor::visit_expression(const ExpressionStmt& stmt) {
    if (stmt.expression) {
        visit(stmt.expression.get());
    }
}

void Executor::visit_block(const BlockStmt& stmt) {
    push_scope();
    try {
        for (const auto& stmt_ptr : stmt.statements) {
            if (stmt_ptr) {
                visit(stmt_ptr.get());
                if (!functionStack.empty() && functionStack.top().hasReturned) break;
                if (!flowStack.empty() && (flowStack.top().shouldBreak || flowStack.top().shouldContinue)) break;
            }
        }
    } catch (...) {
        pop_scope();
        throw;
    }
    pop_scope();
}

void Executor::visit_declaration(const DeclarationStmt& stmt) {
    Value initial_value;
    if (stmt.initializer) {
        initial_value = visit(stmt.initializer.get());
    } else {
        throw std::runtime_error("Variable declaration without initializer is not supported: '" + stmt.identifier.get_lexeme() + "'");
    }
    declare_variable(stmt.identifier.get_lexeme(), initial_value);
}

void Executor::visit_assignment(const AssignmentStmt& stmt) {
    Value rvalue = visit(stmt.value.get());

    if (stmt.target->get_type() == ExprType::VARIABLE) {
        const VariableExpr* var_expr = static_cast<const VariableExpr*>(stmt.target.get());
        Value* var_ptr = find_variable(var_expr->name.get_lexeme());
        if (!var_ptr) {
            throw std::runtime_error("Cannot assign to undefined variable '" + var_expr->name.get_lexeme() + "'");
        }
        *var_ptr = rvalue;

    } else if (stmt.target->get_type() == ExprType::INDEX) {
        const IndexExpr* index_expr = static_cast<const IndexExpr*>(stmt.target.get());

        Value index_val = visit(index_expr->index_expr.get());
        if (!std::holds_alternative<double>(index_val)) {
            throw std::runtime_error("Array index must evaluate to a number.");
        }
        double idx_double = std::get<double>(index_val);
        if (idx_double < 0 || idx_double != floor(idx_double)) {
            throw std::runtime_error("Array index must be a non-negative integer.");
        }
        size_t index = static_cast<size_t>(idx_double);


        if (index_expr->object->get_type() == ExprType::VARIABLE) {
            const VariableExpr* array_var_expr = static_cast<const VariableExpr*>(index_expr->object.get());
            Value* var_containing_array_ptr = find_variable(array_var_expr->name.get_lexeme());

            if (!var_containing_array_ptr) {
                 throw std::runtime_error("Cannot assign to element of undefined array variable '" + array_var_expr->name.get_lexeme() + "'.");
            }
            if (!std::holds_alternative<std::vector<double>>(*var_containing_array_ptr)) {
                 throw std::runtime_error("Cannot assign to indexed element because variable '" + array_var_expr->name.get_lexeme() + "' is not an array.");
            }

            std::vector<double>& actual_array = std::get<std::vector<double>>(*var_containing_array_ptr);

            if (index >= actual_array.size()) {
                throw std::runtime_error("Array index " + std::to_string(index) + " out of bounds for array '" + array_var_expr->name.get_lexeme() + "' (size " + std::to_string(actual_array.size()) + ").");
            }

            if (!std::holds_alternative<double>(rvalue)) {
                throw std::runtime_error("Cannot assign non-number value to an element of array_number.");
            }

            actual_array[index] = std::get<double>(rvalue);

        } else {
            throw std::runtime_error("Assignment target must be a variable or an array element accessed via variable (e.g., var[index] = value).");
        }

    } else {
        throw std::runtime_error("Invalid target for assignment.");
    }
}


void Executor::visit_return(const ReturnStmt& stmt) {
    if (functionStack.empty()) {
        throw std::runtime_error("'return' statement outside of a function");
    }

    FunctionContext& context = functionStack.top();
    context.hasReturned = true;
    if (stmt.value) {
        context.returnValue = visit(stmt.value.get());
    } else {
        context.returnValue = Value{};
    }
}

void Executor::visit_if(const IfStmt& stmt) {
    Value condition_result = visit(stmt.condition.get());
    if (is_truthy(condition_result)) {
        visit_block(stmt.then_block);
    } else if (stmt.has_else && stmt.else_block) {
        visit_block(*stmt.else_block);
    }
}

void Executor::visit_while(const WhileStmt& stmt) {
    flowStack.push({false, false, false, Value{}});

    try {
        while (is_truthy(visit(stmt.condition.get()))) {
            visit_block(stmt.body);

            if (!flowStack.empty()) {
                 FlowControl& current_flow = flowStack.top();
                 if (current_flow.shouldBreak) {
                    break;
                 }
                 if (current_flow.shouldContinue) {
                    current_flow.shouldContinue = false;
                    continue;
                 }
                 if (current_flow.shouldReturn) {
                     break;
                 }
            }
        }
    } catch (...) {
        if (!flowStack.empty()) flowStack.pop();
        throw;
    }

    if (!flowStack.empty()) flowStack.pop();
}


void Executor::visit_for(const ForStmt& stmt) {
    push_scope();
    flowStack.push({false, false, false, Value{}});

    try {
        if (stmt.initializer) {
            visit(stmt.initializer.get());
        }

        while (true) {
            if (stmt.condition) {
                if (!is_truthy(visit(stmt.condition.get()))) {
                    break;
                }
            }

            visit_block(stmt.body);

             if (!flowStack.empty()) {
                 FlowControl& current_flow = flowStack.top();
                 if (current_flow.shouldBreak) {
                    break;
                 }
                 if (current_flow.shouldContinue) {
                    current_flow.shouldContinue = false;
                 }
                  if (current_flow.shouldReturn) {
                     break;
                 }
            }

            if (stmt.increment) {
                visit(stmt.increment.get());
            }
        }

    } catch (...) {
        if (!flowStack.empty()) flowStack.pop();
        pop_scope();
        throw;
    }

    if (!flowStack.empty()) flowStack.pop();
    pop_scope();
}


void Executor::visit_break(const BreakStmt& /*brk*/) {
    if (flowStack.empty()) {
        throw std::runtime_error("'break' statement outside of a loop");
    }
    flowStack.top().shouldBreak = true;
}

void Executor::visit_continue(const ContinueStmt& /*cont*/) {
     if (flowStack.empty()) {
        throw std::runtime_error("'continue' statement outside of a loop");
    }
    flowStack.top().shouldContinue = true;
}


void Executor::visit_function(const FunctionStmt& /*stmt*/) {
    // No runtime action needed for function definition itself
}

void Executor::visit_seq(const SeqStmt& stmt) {
    visit_block(stmt.body);
}

void Executor::visit_par(const ParStmt& stmt) {
    std::vector<std::future<void>> futures;
    std::mutex mtx;
    std::atomic<bool> error_flag(false);
    std::string error_message;

    for (const auto& stmt_ptr : stmt.body.statements) {
        if (!stmt_ptr) continue;

        futures.emplace_back(std::async(std::launch::async, [&, stmt_ptr]() {
            try {
                  visit(stmt_ptr.get());
            } catch (const std::runtime_error& e) {
                std::lock_guard<std::mutex> lock(mtx);
                if (!error_flag.load()) {
                    error_message = e.what();
                    error_flag = true;
                }
            } catch (...) {
                 std::lock_guard<std::mutex> lock(mtx);
                 if (!error_flag.load()) {
                    error_message = "Unknown error occurred in parallel block.";
                    error_flag = true;
                 }
            }
        }));
    }

    for (auto& fut : futures) {
        try {
            fut.get();
        } catch (const std::runtime_error& e) {
             std::lock_guard<std::mutex> lock(mtx);
             if (!error_flag.load()) {
                error_message = e.what();
                error_flag = true;
             }
        } catch (...) {
             std::lock_guard<std::mutex> lock(mtx);
             if (!error_flag.load()) {
                 error_message = "Unknown error occurred waiting for parallel task.";
                 error_flag = true;
             }
        }
    }

    if (error_flag.load()) {
        throw std::runtime_error("Error during PAR execution: " + error_message);
    }
}

void Executor::visit_cchannel(const CChannelStmt& stmt) {
    auto func_it = table_function.find(stmt.channel_name.get_lexeme());
    if (func_it == table_function.end()) {
        throw std::runtime_error("Handler function '" + stmt.channel_name.get_lexeme() + "' for c_channel not found.");
    }
    FunctionStmt* handler_function = func_it->second;

    Value* desc_val_ptr = find_variable(stmt.id_1.get_lexeme());
    Value* host_val_ptr = find_variable(stmt.id_2.get_lexeme());

    if (!desc_val_ptr || !host_val_ptr) {
        throw std::runtime_error("Variable(s) '" + stmt.id_1.get_lexeme() + "' or '" + stmt.id_2.get_lexeme() + "' not found for c_channel.");
    }
    if (!std::holds_alternative<std::string>(*desc_val_ptr) || !std::holds_alternative<std::string>(*host_val_ptr)) {
         throw std::runtime_error("c_channel parameters (description, host) must be strings.");
    }

    std::string description = std::get<std::string>(*desc_val_ptr);
    std::string host_address = std::get<std::string>(*host_val_ptr);
    int port = 8585;

    int server_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (server_fd == -1) {
        perror("socket failed");
        throw std::runtime_error("Failed to create channel socket.");
    }

    int opt = 1;
    if (setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt)) == -1) {
        perror("setsockopt(SO_REUSEADDR) failed");
        close(server_fd);
        throw std::runtime_error("Failed to set channel socket options.");
    }

    struct sockaddr_in address;
    memset(&address, 0, sizeof(address));
    address.sin_family = AF_INET;
    address.sin_port = htons(port);

    if (inet_pton(AF_INET, host_address.c_str(), &address.sin_addr) <= 0) {
        perror("inet_pton failed");
        close(server_fd);
        throw std::runtime_error("Invalid channel address or address not supported: " + host_address);
    }

    if (bind(server_fd, (struct sockaddr*)&address, sizeof(address)) == -1) {
        perror("bind failed");
        close(server_fd);
        throw std::runtime_error("Failed to bind channel socket to " + host_address + ":" + std::to_string(port));
    }

    if (listen(server_fd, 5) == -1) {
        perror("listen failed");
        close(server_fd);
        throw std::runtime_error("Failed to listen on channel socket.");
    }

    std::cout << "Channel server '" << stmt.channel_name.get_lexeme()
              << "' running on " << host_address << ":" << port << std::endl;
    std::cout << "Description: " << description << std::endl;

    std::thread([this, server_fd, handler_function, func_name = stmt.channel_name.get_lexeme()]() {
        while (true) {
            struct sockaddr_in client_address;
            socklen_t client_len = sizeof(client_address);
            int client_socket = accept(server_fd, (struct sockaddr*)&client_address, &client_len);

            if (client_socket == -1) {
                perror("accept failed");
                continue;
            }

            std::thread([this, client_socket, handler_function, func_name](int sock) {
                char buffer[1024] = {0};
                ssize_t bytes_read = read(sock, buffer, sizeof(buffer) - 1);

                if (bytes_read > 0) {
                    std::string message(buffer, bytes_read);

                    try {
                        if (!handler_function->params.param_names || handler_function->params.param_names->size() != 1) {
                             throw std::runtime_error("Channel handler function '" + func_name + "' must accept exactly one argument.");
                        }

                        std::vector<std::unique_ptr<Expr>> args;
                        args.push_back(std::make_unique<LiteralExpr>(
                            Token(TokenType::STRING_LITERAL, message, 0, 0, message)
                        ));

                        Token dummy_paren(TokenType::LEFT_PAREN, "(", 0, 0);
                        CallExpr call_node(
                            std::make_unique<VariableExpr>(handler_function->name),
                            dummy_paren,
                            std::move(args)
                        );

                        Value result = visit_call(call_node);

                        std::string response;
                        if (std::holds_alternative<double>(result)) {
                            response = std::to_string(std::get<double>(result));
                        } else if (std::holds_alternative<std::string>(result)) {
                            response = std::get<std::string>(result);
                        } else if (std::holds_alternative<bool>(result)) {
                            response = std::get<bool>(result) ? "true" : "false";
                        } else {
                            response = "<void_or_unsupported_return_type>";
                        }

                        send(sock, response.c_str(), response.length(), 0);

                    } catch (const std::runtime_error& e) {
                        std::cerr << "Error executing channel handler '" << func_name << "': " << e.what() << std::endl;
                        std::string error_response = "RUNTIME_ERROR: ";
                        error_response += e.what();
                        send(sock, error_response.c_str(), error_response.length(), 0);
                    }
                } else if (bytes_read == 0) {
                    // Connection closed
                } else {
                    perror("read from client failed");
                }
                close(sock);
            }, client_socket).detach();
        }
    }).detach();
}


bool Executor::is_truthy(const Value& value) {
    if (std::holds_alternative<bool>(value)) {
        return std::get<bool>(value);
    }
    if (std::holds_alternative<double>(value)) {
        return std::get<double>(value) != 0.0;
    }
    if (std::holds_alternative<std::string>(value)) {
        return !std::get<std::string>(value).empty();
    }
     if (is_array(value)) {
        return !std::get<std::vector<double>>(value).empty();
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
    if (std::holds_alternative<std::string>(a) && std::holds_alternative<std::string>(b)) {
        return std::get<std::string>(a) == std::get<std::string>(b);
    }
    if (std::holds_alternative<bool>(a) && std::holds_alternative<bool>(b)) {
        return std::get<bool>(a) == std::get<bool>(b);
    }
    if (is_array(a) && is_array(b)) {
        const auto& arr_a = std::get<std::vector<double>>(a);
        const auto& arr_b = std::get<std::vector<double>>(b);
        if (arr_a.size() != arr_b.size()) return false;
        const double epsilon = 1e-9;
        for (size_t i = 0; i < arr_a.size(); ++i) {
            if (std::abs(arr_a[i] - arr_b[i]) >= epsilon) {
                return false;
            }
        }
        return true;
    }

    return false;
}

void Executor::check_numeric_operand(const Token& op, const Value& operand) {
    if (!std::holds_alternative<double>(operand)) {
        throw std::runtime_error("Operand for operator '" + op.get_lexeme() + "' must be a number");
    }
}

void Executor::check_numeric_operands(const Token& op, const Value& left, const Value& right) {
    if (!std::holds_alternative<double>(left) || !std::holds_alternative<double>(right)) {
        throw std::runtime_error("Operands for operator '" + op.get_lexeme() + "' must both be numbers");
    }
}

bool Executor::is_array(const Value& value) {
    return std::holds_alternative<std::vector<double>>(value);
}
