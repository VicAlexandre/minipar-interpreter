#include "../include/core/Scanner.h"
#include "../include/core/Minipar.h"
#include "../include/enum/TokenType.h"

#include <unordered_map>
#include <iostream>
#include <string> 
#include <stdexcept> 

std::unordered_map<std::string, TokenType> keywords = {
    {"SEQ", TokenType::SEQ},
    {"PAR", TokenType::PAR},
    {"if", TokenType::IF},
    {"else", TokenType::ELSE},
    {"while", TokenType::WHILE},
    {"for", TokenType::FOR},
    {"return", TokenType::RETURN},
    {"break", TokenType::BREAK},
    {"continue", TokenType::CONTINUE},
    {"func", TokenType::FUNC},
    {"c_channel", TokenType::C_CHANNEL},
    {"number", TokenType::TYPE_NUMBER},
    {"bool", TokenType::TYPE_BOOL},
    {"string", TokenType::TYPE_STRING},
    {"true", TokenType::TRUE_LITERAL},
    {"false", TokenType::FALSE_LITERAL},
    {"void", TokenType::TYPE_NONE}
};

static inline bool is_digit(char c) { return c >= '0' && c <= '9'; }

static inline bool is_alpha(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

static inline bool is_alpha_numeric(char c) {
  return is_alpha(c) || is_digit(c);
}

std::vector<Token> Scanner::scan_tokens() {
  while (!source.empty()) {
    start = current;
    if (current >= source.size()) {
      break;
    }
    scan_token();

    if (Minipar::faulted()) {
      return tokens;
    }
  }

  tokens.emplace_back(TokenType::END_OF_FILE, "",
                      tokens.empty() ? 1 : tokens.back().get_line(),
                      tokens.empty() ? 1 : tokens.back().get_column() + tokens.back().get_lexeme().length());

  return tokens;
}

void Scanner::scan_token() {
  char c = source[current];
  unsigned int current_col_guess = current - start + 1;
  current++;

  switch (c) {
  case '(': 
    add_token(TokenType::LEFT_PAREN); 
    break;
  case ')': 
    add_token(TokenType::RIGHT_PAREN); 
    break;
  case '{': 
    add_token(TokenType::LEFT_BRACE); 
    break;
  case '}': 
    add_token(TokenType::RIGHT_BRACE); 
    break;
  case '[': 
    add_token(TokenType::LEFT_BRACKET); 
    break;
  case ']': 
    add_token(TokenType::RIGHT_BRACKET); 
    break;
  case ',': 
    add_token(TokenType::COMMA); 
    break;
  case '.': 
    add_token(TokenType::DOT); 
    break;
  case '+': 
    add_token(TokenType::PLUS); 
    break;
  case '*': 
    add_token(TokenType::STAR); 
    break;
  case '/': 
    add_token(TokenType::SLASH); 
    break;
  case '%': 
    add_token(TokenType::PERCENT); 
    break;
  case ':': 
    add_token(TokenType::COLON); 
    break;
  case ';': 
    add_token(TokenType::SEMICOLON); 
    break;
  case '-': 
    add_token(match_token('>') ? TokenType::ARROW : TokenType::MINUS); 
    break;
  case '=': 
    add_token(match_token('=') ? TokenType::EQUAL_COMPARE : TokenType::EQUAL_ASSIGN);
    break;
  case '!': 
    add_token(match_token('=') ? TokenType::BANG_EQUAL : TokenType::BANG); 
    break;
  case '>': 
    add_token(match_token('=') ? TokenType::GREATER_EQUAL : TokenType::GREATER); break;
  case '<': 
    add_token(match_token('=') ? TokenType::LESS_EQUAL : TokenType::LESS); 
    break;
  case '|':
    if (match_token('|')) {
      add_token(TokenType::OR_OR);
    } else {
      Minipar::report_error("Caractere '|' inesperado. Esperado '||' para operador OR.", std::to_string(current_col_guess), line);
      return;
    }
    break;
   case '&':
    if (match_token('&')) {
      add_token(TokenType::AND_AND);
    } else {
      Minipar::report_error("Caractere '&' inesperado. Esperado '&&' para operador AND.", std::to_string(current_col_guess), line);
      return;
    }
    break;

  case '#':
    while (current < source.size() && source[current] != '\n') {
      current++;
    }
    break;

  case '"':
    scan_string();
    break;

  case ' ':
  case '\r':
  case '\t':
    break;

  case '\n':
    line++; 
    break;

  default:
    if (is_digit(c)) {
      scan_number();
    } else if (is_alpha(c)) {
      scan_identifier();
    } else {
      std::string msg = "Caractere inválido: '";
      msg += c;
      msg += "'";
      Minipar::report_error(msg, std::to_string(current_col_guess), line);
      return;
    }
    break;
  }
}

void Scanner::add_token(TokenType type) {
    add_token(type, ValueType{}, NONE);
}

void Scanner::add_token(TokenType type, ValueType literal, enum LiteralType lit) {
  std::string text = source.substr(start, current - start);
  unsigned int column_approx = start + 1;
  switch (lit) {
  case STRING:
    tokens.emplace_back(type, text, line, column_approx, std::get<std::string>(literal));
    break;
  case NUMBER:
    tokens.emplace_back(type, text, line, column_approx, std::get<double>(literal));
    break;
  case BOOL:
    tokens.emplace_back(type, text, line, column_approx, std::get<bool>(literal));
    break;
  case NONE:
    tokens.emplace_back(type, text, line, column_approx);
    break;
  default:
    Minipar::report_error("Tipo literal desconhecido em add_token (erro interno)", std::to_string(column_approx), line);
    break;
  }
}

bool Scanner::match_token(char expected) {
  if (current >= source.size()) {
    return false;
  }
  if (source[current] != expected) {
    return false;
  }
  current++;
  return true;
}

void Scanner::scan_string() {
  std::string str_value = ""; 
  while (current < source.size() && source[current] != '"') {
    if (source[current] == '\n') {
      line++;
    }
    str_value += source[current];
    current++;
  }

  if (current >= source.size()) {
    Minipar::report_error("String não terminada. Esperado '\"'", std::to_string(start + 1), line);
    return;
  }

  current++;
  add_token(TokenType::STRING_LITERAL, str_value, STRING);
}

void Scanner::scan_number() {
  while (current < source.size() && is_digit(source[current])) {
    current++;
  }

  if (current < source.size() && source[current] == '.') {
      if (current + 1 < source.size() && is_digit(source[current + 1])) {
          current++;
          while (current < source.size() && is_digit(source[current])) {
              current++;
          }
      }
  }

  std::string num_str = source.substr(start, current - start);
  try {
      double number = std::stod(num_str);
      add_token(TokenType::NUMBER, number, NUMBER);
  } catch (const std::invalid_argument& ia) {
      Minipar::report_error("Número inválido: '" + num_str + "'", std::to_string(start + 1), line);
  } catch (const std::out_of_range& oor) {
      Minipar::report_error("Número fora do intervalo representável: '" + num_str + "'", std::to_string(start + 1), line);
  }
}

void Scanner::scan_identifier() {
  while (current < source.size() && is_alpha_numeric(source[current])) {
    current++;
  }
  std::string text = source.substr(start, current - start);

  TokenType type;
  auto keyword_it = keywords.find(text);
  if (keyword_it != keywords.end()) {
    type = keyword_it->second;

    if (type == TokenType::TRUE_LITERAL) {
      add_token(type, true, BOOL);
    } else if (type == TokenType::FALSE_LITERAL) {
      add_token(type, false, BOOL); 
    } else {
      add_token(type);
    }

  } else {
    type = TokenType::IDENTIFIER;
    add_token(type);
  }
}
