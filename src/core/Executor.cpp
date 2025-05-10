// src/core/Executor.cpp
#include "../include/core/Executor.h"
#include "../include/core/Error.h"   
#include "../include/core/Token.h"  
#include "../include/core/Expr.h"   
#include "../include/core/Stmt.h"   


#include <cctype>      
#include <cmath>       
#include <cstdlib>     
#include <ctime>       
#include <future>      
#include <iomanip>    
#include <iostream>    
#include <sstream>     
#include <stdexcept>   
#include <string.h>    
#include <atomic>     

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h> 
#include <arpa/inet.h> 
#include <unistd.h>     
#include <netdb.h>      
// #include <fcntl.h>



namespace { 
bool srand_called = false;
}

Executor::Executor() {
  push_scope(); 
  if (!srand_called) {
    srand(static_cast<unsigned int>(time(nullptr))); 
    srand_called = true;
  }
}

Executor::~Executor() {
  
  for (auto const& pair_handle_fd : active_client_sockets) {
    close(pair_handle_fd.second); 
  }
  active_client_sockets.clear(); 
}

void Executor::execute(
    const std::vector<std::unique_ptr<Stmt>> &statements,
    const std::unordered_map<std::string, FunctionStmt *> &function_table_from_semantic) {
  this->table_function = function_table_from_semantic; 

  try {
    for (const auto &stmt : statements) {
      if (!stmt) continue; 
      visit(stmt.get());
    }
  } catch (const std::runtime_error &e) {
    std::cerr << "Runtime error: " << e.what() << std::endl;
    
  }
}

void Executor::execute_global_calls(Expr *expr) {
    
    if (!expr) return;
    if (auto call = dynamic_cast<CallExpr *>(expr)) {
       
        visit_call(*call);
    }
}


void Executor::push_scope() { 
   
    scopes.emplace_back(); 
}

void Executor::pop_scope() {
    if (!scopes.empty()) {
        scopes.pop_back();
    } else {
      
        throw std::runtime_error("Internal error: Attempt to pop an empty scope stack.");
    }
}

void Executor::declare_variable(const std::string &name, const Value &value) {
  if (scopes.empty()) {
   
    throw std::runtime_error("Internal error: No active scope to declare variable '" + name + "'.");
  }
 
    if (!functionStack.empty()) {
        functionStack.top().locals[name] = value;
    } else if (!scopes.empty()){ 
        scopes.back()[name] = value;
    } else {
        
         throw std::runtime_error("Internal error: No available scope for variable declaration.");
    }
}

Executor::Value *Executor::find_variable(const std::string &name) {
 
  if (!functionStack.empty()) {
    auto &locals = functionStack.top().locals;
    auto it = locals.find(name);
    if (it != locals.end()) {
      return &it->second;
    }
  }

 
  for (auto scope_it = scopes.rbegin(); scope_it != scopes.rend(); ++scope_it) {
    auto &current_scope_map = *scope_it;
    auto var_it = current_scope_map.find(name);
    if (var_it != current_scope_map.end()) {
      return &(var_it->second);
    }
  }
  return nullptr; // Variável não encontrada
}

Executor::Value *Executor::find_function(const std::string &name) {
    
  auto it = table_function.find(name); 
  if (it != table_function.end()) {

    return nullptr; 
  }
  return nullptr;
}


Executor::Value Executor::visit(Expr *expr) {
  if (!expr) {
    throw std::runtime_error("Internal error: Trying to visit a null expression");
  }

  switch (expr->get_type()) {
  case ExprType::LITERAL:        return visit_literal(*static_cast<LiteralExpr *>(expr));
  case ExprType::VARIABLE:       return visit_variable(*static_cast<VariableExpr *>(expr));
  case ExprType::BINARY:         return visit_binary(*static_cast<BinaryExpr *>(expr));
  case ExprType::UNARY:          return visit_unary(*static_cast<UnaryExpr *>(expr));
  case ExprType::GROUPING:       return visit_grouping(*static_cast<GroupingExpr *>(expr));
  case ExprType::CALL:           return visit_call(*static_cast<CallExpr *>(expr));
  case ExprType::ARRAY_LITERAL:  return visit_array_literal(*static_cast<ArrayLiteralExpr *>(expr));
  case ExprType::INDEX:          return visit_index(*static_cast<IndexExpr *>(expr));

  default:
    throw std::runtime_error("Unknown or unsupported expression type encountered during execution: " + std::to_string(static_cast<int>(expr->get_type())));
  }
}

Executor::Value Executor::visit_literal(const LiteralExpr &expr) {
  const Token &token = expr.value;
  switch (token.get_type()) {
  case TokenType::NUMBER:         return token.get_double();
  case TokenType::STRING_LITERAL: return token.get_string();
  case TokenType::TRUE_LITERAL:   return true;
  case TokenType::FALSE_LITERAL:  return false;
  default:
    throw std::runtime_error("Invalid token type for literal expression: " + token.to_string());
  }
}

Executor::Value Executor::visit_array_literal(const ArrayLiteralExpr &expr) {
  std::vector<double> elements;
  elements.reserve(expr.elements.size());
  for (const auto &element_expr : expr.elements) {
    Value element_value = visit(element_expr.get());
    if (!std::holds_alternative<double>(element_value)) {
      throw std::runtime_error("Array literal (array_number) elements must evaluate to numbers.");
    }
    elements.push_back(std::get<double>(element_value));
  }
  return elements;
}

Executor::Value Executor::visit_index(const IndexExpr &expr) {
  Value object_value = visit(expr.object.get());
  Value index_value = visit(expr.index_expr.get());

  if (is_array(object_value)) { // Indexando um array_number
    const auto &array_vec = std::get<std::vector<double>>(object_value);
    if (!std::holds_alternative<double>(index_value)) {
      throw std::runtime_error("Array index must evaluate to a number.");
    }
    double index_double = std::get<double>(index_value);
    if (index_double < 0 || index_double >= array_vec.size() || index_double != floor(index_double)) {
      throw std::runtime_error("Array index out of bounds or not an integer: " + std::to_string(index_double));
    }
    size_t valid_index = static_cast<size_t>(index_double);
    return array_vec[valid_index];
  } else if (std::holds_alternative<std::string>(object_value)) { // Indexando uma string
    const auto& str_val = std::get<std::string>(object_value);
    if (!std::holds_alternative<double>(index_value)) {
        throw std::runtime_error("String index must evaluate to a number.");
    }
    double index_double = std::get<double>(index_value);
    if (index_double < 0 || index_double >= str_val.length() || index_double != floor(index_double)) {
         throw std::runtime_error("String index out of bounds or not an integer: " + std::to_string(index_double));
    }
    size_t valid_index = static_cast<size_t>(index_double);
    return std::string(1, str_val[valid_index]); // Retorna o caractere como uma nova string
  }else {
    throw std::runtime_error("Cannot index a non-array/non-string value.");
  }
}

Executor::Value Executor::visit_variable(const VariableExpr &expr) {
  Value *var_ptr = find_variable(expr.name.get_lexeme());
  if (var_ptr) {
    return *var_ptr;
  }
  throw std::runtime_error("Undefined variable '" + expr.name.get_lexeme() + "'.");
}

Executor::Value Executor::visit_binary(const BinaryExpr &expr) {
  Value left = visit(expr.left.get());
  TokenType op_type = expr.op.get_type();

  // Avaliação preguiçosa para AND e OR
  if (op_type == TokenType::AND_AND) {
    if (!is_truthy(left)) return false;
    Value right_and = visit(expr.right.get()); // Só avalia se left é truthy
    return is_truthy(right_and);
  }
  if (op_type == TokenType::OR_OR) {
    if (is_truthy(left)) return true;
    Value right_or = visit(expr.right.get()); // Só avalia se left é falsy
    return is_truthy(right_or);
  }

  Value right = visit(expr.right.get()); // Avalia operando direito para outros operadores

  if (op_type == TokenType::PLUS) {
    if (std::holds_alternative<double>(left) && std::holds_alternative<double>(right)) {
      return std::get<double>(left) + std::get<double>(right);
    }
    if (std::holds_alternative<std::string>(left) && std::holds_alternative<std::string>(right)) {
      return std::get<std::string>(left) + std::get<std::string>(right);
    }
    throw std::runtime_error("Operator '+' requires two numbers or two strings.");
  }
  if (op_type == TokenType::MINUS || op_type == TokenType::STAR ||
      op_type == TokenType::SLASH || op_type == TokenType::PERCENT) {
    check_numeric_operands(expr.op, left, right);
    double left_num = std::get<double>(left);
    double right_num = std::get<double>(right);
    switch (op_type) {
    case TokenType::MINUS:  return left_num - right_num;
    case TokenType::STAR:   return left_num * right_num;
    case TokenType::SLASH:
      if (right_num == 0.0) throw std::runtime_error("Division by zero.");
      return left_num / right_num;
    case TokenType::PERCENT:
      if (right_num == 0.0) throw std::runtime_error("Modulo by zero.");
      return fmod(left_num, right_num);
    default: break; 
    }
  }

  if (op_type == TokenType::GREATER || op_type == TokenType::GREATER_EQUAL ||
      op_type == TokenType::LESS || op_type == TokenType::LESS_EQUAL) {
    if (std::holds_alternative<double>(left) && std::holds_alternative<double>(right)) {
      double left_num = std::get<double>(left);
      double right_num = std::get<double>(right);
      switch (op_type) {
      case TokenType::GREATER:        return left_num > right_num;
      case TokenType::GREATER_EQUAL:  return left_num >= right_num;
      case TokenType::LESS:           return left_num < right_num;
      case TokenType::LESS_EQUAL:     return left_num <= right_num;
      default: break;
      }
    } else if (std::holds_alternative<std::string>(left) && std::holds_alternative<std::string>(right)) {
      const std::string &left_str = std::get<std::string>(left);
      const std::string &right_str = std::get<std::string>(right);
      switch (op_type) {
      case TokenType::GREATER:        return left_str > right_str;
      case TokenType::GREATER_EQUAL:  return left_str >= right_str;
      case TokenType::LESS:           return left_str < right_str;
      case TokenType::LESS_EQUAL:     return left_str <= right_str;
      default: break;
      }
    } else {
      throw std::runtime_error("Comparison operators require two numbers or two strings for '" + expr.op.get_lexeme() + "'.");
    }
  }

  if (op_type == TokenType::EQUAL_COMPARE || op_type == TokenType::BANG_EQUAL) {
    bool result = is_equal(left, right);
    return (op_type == TokenType::EQUAL_COMPARE) ? result : !result;
  }

  throw std::runtime_error("Unknown or unsupported binary operator: " + expr.op.get_lexeme());
}

Executor::Value Executor::visit_unary(const UnaryExpr &expr) {
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

Executor::Value Executor::visit_grouping(const GroupingExpr &expr) {
  return visit(expr.expression.get());
}

// --- Implementação de visit_call com built-ins de socket ---
Executor::Value Executor::visit_call(CallExpr &expr) {
  std::string func_name;
  if (auto var_expr = dynamic_cast<VariableExpr *>(expr.callee.get())) {
    func_name = var_expr->name.get_lexeme();
  } else {
    throw std::runtime_error("Calling non-function values or complex callees is not supported");
  }

  // Funções Built-in (incluindo as de socket)
  if (func_name == "print") {
    std::stringstream ss;
    for (size_t i = 0; i < expr.arguments.size(); ++i) {
      Value value = visit(expr.arguments[i].get());
      if (std::holds_alternative<double>(value)) ss << std::get<double>(value);
      else if (std::holds_alternative<std::string>(value)) ss << std::get<std::string>(value);
      else if (std::holds_alternative<bool>(value)) ss << (std::get<bool>(value) ? "true" : "false");
      else if (is_array(value)) {
        const auto &arr = std::get<std::vector<double>>(value);
        ss << "[";
        for (size_t j = 0; j < arr.size(); ++j) {
          ss << arr[j] << (j == arr.size() - 1 ? "" : ", ");
        }
        ss << "]";
      } else ss << "<unprintable_value>";
      if (i < expr.arguments.size() - 1) ss << " ";
    }
    std::cout << ss.str() << std::endl;
    return Value{}; // void
  }
  if (func_name == "len") {
    if (expr.arguments.size() != 1) throw std::runtime_error("len() takes exactly one argument.");
    Value arg = visit(expr.arguments[0].get());
    if (std::holds_alternative<std::string>(arg)) return static_cast<double>(std::get<std::string>(arg).length());
    if (is_array(arg)) return static_cast<double>(std::get<std::vector<double>>(arg).size());
    throw std::runtime_error("len() argument must be a string or an array.");
  }
   if (func_name == "sleep") {
        if (expr.arguments.size() != 1) throw std::runtime_error("sleep() takes exactly one argument (seconds).");
        Value arg_val = visit(expr.arguments[0].get());
        if (!std::holds_alternative<double>(arg_val)) throw std::runtime_error("sleep() argument must be a number.");
        double duration_sec = std::get<double>(arg_val);
        if (duration_sec < 0) duration_sec = 0;
        usleep(static_cast<useconds_t>(duration_sec * 1000000.0));
        return Value{}; // void
    }
    if (func_name == "input") {
        if (expr.arguments.size() > 1) throw std::runtime_error("input() takes at most one argument (prompt string).");
        if (expr.arguments.size() == 1) {
            Value prompt_val = visit(expr.arguments[0].get());
            if (std::holds_alternative<std::string>(prompt_val)) std::cout << std::get<std::string>(prompt_val);
            // else: não imprimir prompt se não for string, ou converter? Para simplicidade, só string.
        }
        std::string line_input;
        if (!std::getline(std::cin, line_input)) {
             if (std::cin.eof()) return std::string(""); // EOF retorna string vazia
            throw std::runtime_error("Error reading input.");
        }
        return line_input;
    }
    if (func_name == "to_number") {
        if (expr.arguments.size() != 1) throw std::runtime_error("to_number() takes one argument.");
        Value arg = visit(expr.arguments[0].get());
        if (std::holds_alternative<std::string>(arg)) {
            try { return std::stod(std::get<std::string>(arg)); }
            catch (const std::exception&) { throw std::runtime_error("Cannot convert string '" + std::get<std::string>(arg) + "' to number."); }
        }
        if (std::holds_alternative<double>(arg)) return std::get<double>(arg);
        if (std::holds_alternative<bool>(arg)) return std::get<bool>(arg) ? 1.0 : 0.0;
        throw std::runtime_error("to_number() argument must be a string, number, or bool.");
    }
    if (func_name == "to_string") {
        if (expr.arguments.size() != 1) throw std::runtime_error("to_string() takes one argument.");
        Value arg = visit(expr.arguments[0].get());
        if (std::holds_alternative<double>(arg)) return std::to_string(std::get<double>(arg));
        if (std::holds_alternative<std::string>(arg)) return std::get<std::string>(arg);
        if (std::holds_alternative<bool>(arg)) return std::get<bool>(arg) ? "true" : "false";
        if (is_array(arg)) { /* ... converter array para string ... */ return "[array_representation]"; }
        return "<unstringifiable_value>";
    }
    if (func_name == "to_bool") {
        if (expr.arguments.size() != 1) throw std::runtime_error("to_bool() takes one argument.");
        return is_truthy(visit(expr.arguments[0].get()));
    }
    if (func_name == "isalpha") {
        if (expr.arguments.size() != 1) throw std::runtime_error("isalpha() takes one string argument.");
        Value arg = visit(expr.arguments[0].get());
        if (!std::holds_alternative<std::string>(arg)) throw std::runtime_error("isalpha() argument must be a string.");
        std::string s = std::get<std::string>(arg);
        if (s.length() != 1) return false; // isalpha é para um único caractere
        return static_cast<bool>(::isalpha(static_cast<unsigned char>(s[0])));
    }
    if (func_name == "isnum") {
        if (expr.arguments.size() != 1) throw std::runtime_error("isnum() takes one string argument.");
        Value arg = visit(expr.arguments[0].get());
        if (!std::holds_alternative<std::string>(arg)) throw std::runtime_error("isnum() argument must be a string.");
        std::string s = std::get<std::string>(arg);
        if (s.length() != 1) return false; // isnum é para um único caractere
        return static_cast<bool>(::isdigit(static_cast<unsigned char>(s[0])));
    }
    if (func_name == "exp") {
        if (expr.arguments.size() != 1) throw std::runtime_error("exp() takes one numeric argument.");
        Value arg = visit(expr.arguments[0].get());
        check_numeric_operand(expr.paren, arg); // expr.paren como token de referência
        return std::exp(std::get<double>(arg));
    }
    if (func_name == "random") {
        if (!expr.arguments.empty()) throw std::runtime_error("random() takes no arguments.");
        return static_cast<double>(rand()) / RAND_MAX;
    }

  // Novas Funções de Socket
  if (func_name == "connect_socket") {
    if (expr.arguments.size() != 2) throw std::runtime_error("connect_socket() requires host (string) and port (number).");
    Value host_val = visit(expr.arguments[0].get());
    Value port_val = visit(expr.arguments[1].get());
    if (!std::holds_alternative<std::string>(host_val)) throw std::runtime_error("connect_socket() host argument must be a string.");
    if (!std::holds_alternative<double>(port_val)) throw std::runtime_error("connect_socket() port argument must be a number.");
    std::string host_str = std::get<std::string>(host_val);
    int port_num = static_cast<int>(std::get<double>(port_val));

    int sock_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (sock_fd < 0) return -1.0; // Erro criação

    struct sockaddr_in serv_addr;
    memset(&serv_addr, 0, sizeof(serv_addr)); // Zera a estrutura
    serv_addr.sin_family = AF_INET;
    serv_addr.sin_port = htons(port_num);

    if (inet_pton(AF_INET, host_str.c_str(), &serv_addr.sin_addr) <= 0) {
      close(sock_fd); return -2.0; // Endereço inválido
    }
    if (connect(sock_fd, (struct sockaddr *)&serv_addr, sizeof(serv_addr)) < 0) {
      close(sock_fd); return -3.0; 
    }
    int handle = next_socket_handle++;
    active_client_sockets[handle] = sock_fd;
    return static_cast<double>(handle);
  }
  if (func_name == "send_socket") {
    if (expr.arguments.size() != 2) throw std::runtime_error("send_socket() requires socket_handle (number) and message (string).");
    Value handle_val = visit(expr.arguments[0].get());
    Value message_val = visit(expr.arguments[1].get());
    if (!std::holds_alternative<double>(handle_val)) throw std::runtime_error("send_socket() socket_handle must be a number.");
    if (!std::holds_alternative<std::string>(message_val)) throw std::runtime_error("send_socket() message must be a string.");
    int handle = static_cast<int>(std::get<double>(handle_val));
    std::string message_str = std::get<std::string>(message_val);
    auto it = active_client_sockets.find(handle);
    if (it == active_client_sockets.end()) throw std::runtime_error("send_socket(): Invalid socket handle: " + std::to_string(handle));
    ssize_t bytes_sent = send(it->second, message_str.c_str(), message_str.length(), 0);
   
    return bytes_sent >= 0; 
  }
  if (func_name == "receive_socket") {
    if (expr.arguments.size() != 1) throw std::runtime_error("receive_socket() requires socket_handle (number).");
    Value handle_val = visit(expr.arguments[0].get());
    if (!std::holds_alternative<double>(handle_val)) throw std::runtime_error("receive_socket() socket_handle must be a number.");
    int handle = static_cast<int>(std::get<double>(handle_val));
    auto it = active_client_sockets.find(handle);
    if (it == active_client_sockets.end()) throw std::runtime_error("receive_socket(): Invalid socket handle: " + std::to_string(handle));
    
    char buffer[4096] = {0}; 
    ssize_t bytes_read = recv(it->second, buffer, sizeof(buffer) - 1, 0);
    if (bytes_read < 0) return std::string(""); 
    if (bytes_read == 0) return std::string(""); 
    return std::string(buffer, bytes_read);
  }
  if (func_name == "close_socket") {
    if (expr.arguments.size() != 1) throw std::runtime_error("close_socket() requires socket_handle (number).");
    Value handle_val = visit(expr.arguments[0].get());
    if (!std::holds_alternative<double>(handle_val)) throw std::runtime_error("close_socket() socket_handle must be a number.");
    int handle = static_cast<int>(std::get<double>(handle_val));
    auto it = active_client_sockets.find(handle);
    if (it != active_client_sockets.end()) {
      close(it->second);
      active_client_sockets.erase(it);
    } 
    return Value{}; 
  }

  // Chamada a Funções Definidas pelo Usuário
  auto it_user_func = table_function.find(func_name);
  if (it_user_func == table_function.end()) {
    throw std::runtime_error("Undefined function '" + func_name + "'");
  }
  FunctionStmt *function = it_user_func->second;
  size_t expected_args = function->params.param_names ? function->params.param_names->size() : 0;
  if (expr.arguments.size() != expected_args) {
    throw std::runtime_error("Wrong number of arguments for function '" + func_name + "'. Expected " +
                             std::to_string(expected_args) + ", got " + std::to_string(expr.arguments.size()));
  }
  FunctionContext context;
  if (function->params.param_names) {
    auto &param_names_vec = *function->params.param_names;
    for (size_t i = 0; i < param_names_vec.size(); ++i) {
      context.locals[param_names_vec[i].get_lexeme()] = visit(expr.arguments[i].get());
    }
  }
  functionStack.push(std::move(context));
  Value return_value = Value{}; // Default para void
  bool returned_explicitly = false;
  try {
    visit_block(function->body);
    if (!functionStack.empty() && functionStack.top().hasReturned) {
      return_value = functionStack.top().returnValue;
      returned_explicitly = true;
    }
  } catch (...) {
    if(!functionStack.empty()) functionStack.pop();
    throw;
  }
  if (!functionStack.empty()) functionStack.pop();
  else { throw std::runtime_error("Function stack underflow after call to '" + func_name + "'."); }

  if (!returned_explicitly && function->return_type.get_type() != TokenType::TYPE_NONE) {
    throw std::runtime_error("Function '" + func_name + "' did not return a value but was expected to.");
  }
  return return_value;
}


void Executor::visit(Stmt *stmt) {
  if (!stmt) return;

  if (!functionStack.empty() && functionStack.top().hasReturned) {

      return;
  }

  switch (stmt->get_type()) {
  case StmtType::DECLARATION:    visit_declaration(stmt->get_decl_stmt()); break;
  case StmtType::ASSIGNMENT:     visit_assignment(stmt->get_assign_stmt()); break;
  case StmtType::RETURN:         visit_return(stmt->get_return_stmt()); break;
  case StmtType::IF:             visit_if(stmt->get_if_stmt()); break;
  case StmtType::WHILE:          visit_while(stmt->get_while_stmt()); break;
  case StmtType::FOR:            visit_for(stmt->get_for_stmt()); break;
  case StmtType::FUNCTION:       visit_function(stmt->get_function_stmt()); break;
  case StmtType::SEQ:            visit_seq(stmt->get_seq_stmt()); break;
  case StmtType::PAR:            visit_par(stmt->get_par_stmt()); break;
  case StmtType::CCHANNEL:       visit_cchannel(stmt->get_c_channel_stmt()); break;
  case StmtType::BLOCK:          visit_block(stmt->get_block_stmt()); break;
  case StmtType::EXPRESSION:     visit_expression(stmt->get_expression_stmt()); break;
  case StmtType::BREAK:          visit_break(stmt->get_break_stmt()); break;
  case StmtType::CONTINUE:       visit_continue(stmt->get_continue_stmt()); break;
  default:
    throw std::runtime_error("Unknown or unsupported statement type encountered during execution");
  }
}

void Executor::visit_expression(const ExpressionStmt &stmt) {
  if (stmt.expression) {
    visit(stmt.expression.get()); 
  }
}

void Executor::visit_block(const BlockStmt &stmt) {
  push_scope(); 
  try {
    for (const auto &stmt_ptr : stmt.statements) {
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

void Executor::visit_declaration(const DeclarationStmt &stmt) {
  Value initial_value;
  if (stmt.initializer) {
    initial_value = visit(stmt.initializer.get());
  } else {
    
    throw std::runtime_error("Variable declaration without initializer: '" + stmt.identifier.get_lexeme() + "'.");
  }
  declare_variable(stmt.identifier.get_lexeme(), initial_value);
}

void Executor::visit_assignment(const AssignmentStmt &stmt) {
  Value rvalue = visit(stmt.value.get());
  if (stmt.target->get_type() == ExprType::VARIABLE) {
    const VariableExpr *var_expr = static_cast<const VariableExpr *>(stmt.target.get());
    Value *var_ptr = find_variable(var_expr->name.get_lexeme());
    if (!var_ptr) throw std::runtime_error("Cannot assign to undefined variable '" + var_expr->name.get_lexeme() + "'.");
    *var_ptr = rvalue;
  } else if (stmt.target->get_type() == ExprType::INDEX) {
    const IndexExpr *index_expr = static_cast<const IndexExpr *>(stmt.target.get());
    Value index_val = visit(index_expr->index_expr.get());
    if (!std::holds_alternative<double>(index_val)) throw std::runtime_error("Array index must be a number.");
    double idx_double = std::get<double>(index_val);
    if (idx_double < 0 || idx_double != floor(idx_double)) throw std::runtime_error("Array index must be a non-negative integer.");
    size_t valid_idx = static_cast<size_t>(idx_double);

    // O objeto do IndexExpr (o array em si) deve ser um L-Value (uma variável que contém o array)
    if (index_expr->object->get_type() == ExprType::VARIABLE) {
      const VariableExpr *array_var_expr = static_cast<const VariableExpr *>(index_expr->object.get());
      Value *var_containing_array_ptr = find_variable(array_var_expr->name.get_lexeme());
      if (!var_containing_array_ptr) throw std::runtime_error("Array variable '" + array_var_expr->name.get_lexeme() + "' not found for assignment.");
      if (!is_array(*var_containing_array_ptr)) throw std::runtime_error("Variable '" + array_var_expr->name.get_lexeme() + "' is not an array.");
      
      std::vector<double> &actual_array = std::get<std::vector<double>>(*var_containing_array_ptr);
      if (valid_idx >= actual_array.size()) throw std::runtime_error("Array index " + std::to_string(valid_idx) + " out of bounds for array '" + array_var_expr->name.get_lexeme() + "'.");
      if (!std::holds_alternative<double>(rvalue)) throw std::runtime_error("Cannot assign non-number to array_number element.");
      actual_array[valid_idx] = std::get<double>(rvalue);
    } else {
      throw std::runtime_error("Assignment to indexed element requires the array to be a variable.");
    }
  } else {
    throw std::runtime_error("Invalid target for assignment.");
  }
}

void Executor::visit_return(const ReturnStmt &stmt) {
  if (functionStack.empty()) throw std::runtime_error("'return' statement outside of a function.");
  FunctionContext &context = functionStack.top();
  context.hasReturned = true;
  if (stmt.value) context.returnValue = visit(stmt.value.get());
  else context.returnValue = Value{}; // void
}

void Executor::visit_if(const IfStmt &stmt) {
  Value condition_result = visit(stmt.condition.get());
  if (is_truthy(condition_result)) {
    visit_block(stmt.then_block);
  } else if (stmt.has_else && stmt.else_block) {
    visit_block(*stmt.else_block);
  }
}

void Executor::visit_while(const WhileStmt &stmt) {
  flowStack.push({false, false, false, Value{}}); // Entra no loop
  try {
    while (is_truthy(visit(stmt.condition.get()))) {
      visit_block(stmt.body);
      FlowControl &current_loop_flow = flowStack.top();
      if (current_loop_flow.shouldBreak) break; // Sai do while
      if (current_loop_flow.shouldContinue) current_loop_flow.shouldContinue = false; // Reseta para próxima iteração
      if (current_loop_flow.shouldReturn || (!functionStack.empty() && functionStack.top().hasReturned)) break; // Propaga return
    }
  } catch (...) {
    if (!flowStack.empty()) flowStack.pop(); throw;
  }
  if (!flowStack.empty()) flowStack.pop(); // Sai do loop
}

void Executor::visit_for(const ForStmt &stmt) {
  push_scope(); // Escopo para inicializador e loop
  flowStack.push({false, false, false, Value{}});
  try {
    if (stmt.initializer) visit(stmt.initializer.get());
    while (stmt.condition ? is_truthy(visit(stmt.condition.get())) : true) { // Condição vazia é true
      visit_block(stmt.body);
      FlowControl &current_loop_flow = flowStack.top();
      if (current_loop_flow.shouldBreak) break;
      if (current_loop_flow.shouldContinue) current_loop_flow.shouldContinue = false;
      if (current_loop_flow.shouldReturn || (!functionStack.empty() && functionStack.top().hasReturned)) break;
      if (stmt.increment) visit(stmt.increment.get());
    }
  } catch (...) {
    if (!flowStack.empty()) flowStack.pop(); pop_scope(); throw;
  }
  if (!flowStack.empty()) flowStack.pop();
  pop_scope();
}

void Executor::visit_break(const BreakStmt &) {
  if (flowStack.empty()) throw std::runtime_error("'break' statement outside of a loop.");
  flowStack.top().shouldBreak = true;
}

void Executor::visit_continue(const ContinueStmt &) {
  if (flowStack.empty()) throw std::runtime_error("'continue' statement outside of a loop.");
  flowStack.top().shouldContinue = true;
}

void Executor::visit_function(const FunctionStmt &) {
  
}

void Executor::visit_seq(const SeqStmt &stmt) {
  
  visit_block(stmt.body);
}

void Executor::visit_par(const ParStmt &stmt) {
  std::vector<std::future<void>> futures;
  std::mutex error_reporting_mutex_local; 
  std::atomic<bool> error_flag_local(false);
  std::string error_message_str_local; 

  for (const auto &stmt_ptr_par : stmt.body.statements) {
    if (!stmt_ptr_par) continue;
    futures.emplace_back(std::async(std::launch::async, [this, stmt_ptr_par, &error_reporting_mutex_local, &error_flag_local, &error_message_str_local]() {
      std::lock_guard<std::mutex> guard(this->execution_mutex); 
      try {
        this->visit(stmt_ptr_par.get());
      } catch (const std::runtime_error &e) {
        std::lock_guard<std::mutex> lock(error_reporting_mutex_local); 
        if (!error_flag_local.load()) {
          error_message_str_local = e.what();
          error_flag_local = true;
        }
      } catch (...) {
        std::lock_guard<std::mutex> lock(error_reporting_mutex_local); 
        if (!error_flag_local.load()) {
          error_message_str_local = "Unknown error occurred in parallel block.";
          error_flag_local = true;
        }
      }
    }));
  }
  for (auto &fut : futures) {
    try {
      fut.get(); 
    } catch (const std::runtime_error &e) { // Exceções do std::async podem ser relançadas por fut.get()
      std::lock_guard<std::mutex> lock(error_reporting_mutex_local);
      if (!error_flag_local.load()) { error_message_str_local = e.what(); error_flag_local = true; }
    } catch (...) {
      std::lock_guard<std::mutex> lock(error_reporting_mutex_local);
      if (!error_flag_local.load()) { error_message_str_local = "Unknown error during future.get()."; error_flag_local = true; }
    }
  }
  if (error_flag_local.load()) {
    throw std::runtime_error("Error during PAR execution: " + error_message_str_local);
  }
}

void Executor::visit_cchannel(const CChannelStmt &stmt) {
  auto func_it = table_function.find(stmt.channel_name.get_lexeme());
  if (func_it == table_function.end()) {
    throw std::runtime_error("Handler function '" + stmt.channel_name.get_lexeme() + "' for c_channel not found.");
  }
  FunctionStmt *handler_function_ptr = func_it->second; // Ponteiro para o nó da função

  Value *desc_val_ptr = find_variable(stmt.id_1.get_lexeme());
  Value *host_val_ptr = find_variable(stmt.id_2.get_lexeme());
  if (!desc_val_ptr || !host_val_ptr) throw std::runtime_error("Variable(s) for c_channel description or host not found.");
  if (!std::holds_alternative<std::string>(*desc_val_ptr) || !std::holds_alternative<std::string>(*host_val_ptr)) {
    throw std::runtime_error("c_channel parameters (description, host) must be strings.");
  }
  std::string description_str = std::get<std::string>(*desc_val_ptr);
  std::string host_address_str = std::get<std::string>(*host_val_ptr);
  int port_num_cchannel = 8585; // Porta fixa para c_channel

  int server_fd = socket(AF_INET, SOCK_STREAM, 0);
  if (server_fd == -1) throw std::runtime_error("Failed to create c_channel server socket.");
  int opt_reuse = 1;
  if (setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR, &opt_reuse, sizeof(opt_reuse)) == -1) {
    close(server_fd); throw std::runtime_error("Failed to set c_channel socket options (SO_REUSEADDR).");
  }
  struct sockaddr_in server_address_cchannel;
  memset(&server_address_cchannel, 0, sizeof(server_address_cchannel));
  server_address_cchannel.sin_family = AF_INET;
  server_address_cchannel.sin_port = htons(port_num_cchannel);
  if (inet_pton(AF_INET, host_address_str.c_str(), &server_address_cchannel.sin_addr) <= 0) {
    close(server_fd); throw std::runtime_error("Invalid c_channel host address: " + host_address_str);
  }
  if (bind(server_fd, (struct sockaddr *)&server_address_cchannel, sizeof(server_address_cchannel)) == -1) {
    close(server_fd); throw std::runtime_error("Failed to bind c_channel socket to " + host_address_str + ":" + std::to_string(port_num_cchannel));
  }
  if (listen(server_fd, 10) == -1) { // Backlog aumentado para 10
    close(server_fd); throw std::runtime_error("Failed to listen on c_channel socket.");
  }
  std::cout << "c_channel server '" << stmt.channel_name.get_lexeme() << "' (handler: " << handler_function_ptr->name.get_lexeme() 
            << ") running on " << host_address_str << ":" << port_num_cchannel << std::endl;
  std::cout << "Description: " << description_str << std::endl;

  std::thread([this, server_fd, handler_function_node = *handler_function_ptr /* Copia o nó da função */, func_name_str = handler_function_ptr->name.get_lexeme()]() {
    while (true) {
      struct sockaddr_in client_addr_cchannel;
      socklen_t client_len_cchannel = sizeof(client_addr_cchannel);
      int client_sock_fd = accept(server_fd, (struct sockaddr *)&client_addr_cchannel, &client_len_cchannel);
      if (client_sock_fd == -1) {
         break; 
      }

      std::thread([this, client_sock_fd, captured_handler_node = handler_function_node, captured_func_name = func_name_str](int sock) {
        char buffer_cchannel[1024] = {0};
        ssize_t bytes_read_cchannel = recv(sock, buffer_cchannel, sizeof(buffer_cchannel) - 1, 0);
        if (bytes_read_cchannel > 0) {
          std::string message_from_client(buffer_cchannel, bytes_read_cchannel);
          Value result_from_handler;
          try {
            std::lock_guard<std::mutex> exec_guard(this->execution_mutex); // Protege chamada ao handler
            if (!captured_handler_node.params.param_names || captured_handler_node.params.param_names->size() != 1) {
              throw std::runtime_error("c_channel handler '" + captured_func_name + "' must accept exactly one string argument.");
            }
            
            
            std::vector<std::unique_ptr<Expr>> args_for_handler;
            args_for_handler.push_back(std::make_unique<LiteralExpr>(Token(TokenType::STRING_LITERAL, message_from_client, 0,0, message_from_client)));
            
            Token dummy_paren_cchannel(TokenType::LEFT_PAREN, "(", 0,0);
            auto callee_expr_cchannel = std::make_unique<VariableExpr>(captured_handler_node.name); // Usa o token nome da função copiada
            CallExpr call_node_cchannel(std::move(callee_expr_cchannel), dummy_paren_cchannel, std::move(args_for_handler));
            
            result_from_handler = this->visit_call(call_node_cchannel);

          } catch (const std::runtime_error &e) {
            std::cerr << "Error in c_channel handler '" << captured_func_name << "': " << e.what() << std::endl;
            std::string err_resp = "HANDLER_ERROR: " + std::string(e.what());
            send(sock, err_resp.c_str(), err_resp.length(), 0);
            close(sock); return;
          }
          std::string response_to_client;
          if (std::holds_alternative<double>(result_from_handler)) response_to_client = std::to_string(std::get<double>(result_from_handler));
          else if (std::holds_alternative<std::string>(result_from_handler)) response_to_client = std::get<std::string>(result_from_handler);
          else if (std::holds_alternative<bool>(result_from_handler)) response_to_client = (std::get<bool>(result_from_handler) ? "true" : "false");
          else response_to_client = "<handler_returned_void_or_unsupported_type>";
          send(sock, response_to_client.c_str(), response_to_client.length(), 0);
        }

        close(sock);
      }, client_sock_fd).detach();
    }

  }).detach();
}



bool Executor::is_truthy(const Value &value) {
  if (std::holds_alternative<bool>(value)) return std::get<bool>(value);
  if (std::holds_alternative<double>(value)) return std::get<double>(value) != 0.0;
  if (std::holds_alternative<std::string>(value)) return !std::get<std::string>(value).empty();
  if (is_array(value)) return !std::get<std::vector<double>>(value).empty();
  return false; 
}

bool Executor::is_equal(const Value &a, const Value &b) {
  if (a.index() != b.index()) return false; // Tipos diferentes no variant não são iguais
  if (std::holds_alternative<double>(a)) return std::abs(std::get<double>(a) - std::get<double>(b)) < 1e-9;
  if (std::holds_alternative<std::string>(a)) return std::get<std::string>(a) == std::get<std::string>(b);
  if (std::holds_alternative<bool>(a)) return std::get<bool>(a) == std::get<bool>(b);
  if (is_array(a)) {
    const auto &arr_a = std::get<std::vector<double>>(a);
    const auto &arr_b = std::get<std::vector<double>>(b);
    if (arr_a.size() != arr_b.size()) return false;
    for (size_t i = 0; i < arr_a.size(); ++i) {
      if (std::abs(arr_a[i] - arr_b[i]) >= 1e-9) return false;
    }
    return true;
  }

  return true; 
}

void Executor::check_numeric_operand(const Token &op, const Value &operand) {
  if (!std::holds_alternative<double>(operand)) {
    throw std::runtime_error("Operand for operator '" + op.get_lexeme() + "' must be a number.");
  }
}

void Executor::check_numeric_operands(const Token &op, const Value &left, const Value &right) {
  if (!std::holds_alternative<double>(left) || !std::holds_alternative<double>(right)) {
    throw std::runtime_error("Operands for operator '" + op.get_lexeme() + "' must both be numbers.");
  }
}

bool Executor::is_array(const Value &value) {
  return std::holds_alternative<std::vector<double>>(value);
}