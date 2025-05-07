#include "../include/core/Scanner.h"
#include "../include/core/Minipar.h"
#include "../include/enum/TokenType.h"

#include <iostream>
#include <stdexcept>
#include <string>
#include <unordered_map>

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
    {"void", TokenType::TYPE_NONE},
    {"array", TokenType::TYPE_ARRAY_NUMBER}};

static inline bool is_digit(char c) { return c >= '0' && c <= '9'; }

static inline bool is_alpha(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

static inline bool is_alpha_numeric(char c) {
  return is_alpha(c) || is_digit(c);
}

std::vector<Token> Scanner::scan_tokens() {
  while (current < source.size()) {
    start = current;
    scan_token();

    if (Minipar::faulted()) {
      break;
    }
  }

  unsigned int eof_col = 1;
  if (!tokens.empty()) {
    size_t last_newline_pos_eof =
        source.rfind('\n', current > 0 ? current - 1 : 0);
    if (current > 0 && last_newline_pos_eof != std::string::npos &&
        last_newline_pos_eof < (current > 0 ? current - 1 : 0)) {
      eof_col = (current - 1) - last_newline_pos_eof;
    } else if (current > 0 && line == 1) {
      eof_col = current;
    } else if (tokens.back().get_line() == line) {
      eof_col =
          tokens.back().get_column() + tokens.back().get_lexeme().length();
      if (tokens.back().get_lexeme().empty() &&
          tokens.back().get_type() != TokenType::END_OF_FILE) {
        eof_col = tokens.back().get_column() + 1;
      }
    }
    if (eof_col == 0)
      eof_col = 1;
  }

  tokens.emplace_back(TokenType::END_OF_FILE, "", line, eof_col);
  return tokens;
}

void Scanner::scan_token() {
  char c = source[current];

  size_t last_newline_pos = source.rfind('\n', current);
  unsigned int current_char_col = (last_newline_pos == std::string::npos)
                                      ? (current + 1)
                                      : (current - last_newline_pos);
  if (current_char_col == 0)
    current_char_col = 1;

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
    add_token(match_token('=') ? TokenType::EQUAL_COMPARE
                               : TokenType::EQUAL_ASSIGN);
    break;
  case '!':
    add_token(match_token('=') ? TokenType::BANG_EQUAL : TokenType::BANG);
    break;
  case '>':
    add_token(match_token('=') ? TokenType::GREATER_EQUAL : TokenType::GREATER);
    break;
  case '<':
    add_token(match_token('=') ? TokenType::LESS_EQUAL : TokenType::LESS);
    break;

  case '|':
    if (match_token('|')) {
      add_token(TokenType::OR_OR);
    } else {
      Minipar::report_error(
          "Caractere '|' inesperado. Esperado '||' para operador OR.",
          std::to_string(current_char_col), line);
    }
    break;
  case '&':
    if (match_token('&')) {
      add_token(TokenType::AND_AND);
    } else {
      Minipar::report_error(
          "Caractere '&' inesperado. Esperado '&&' para operador AND.",
          std::to_string(current_char_col), line);
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
      Minipar::report_error(msg, std::to_string(current_char_col), line);
    }
    break;
  }
}

void Scanner::add_token(TokenType type) { add_token(type, ValueType{}, NONE); }

void Scanner::add_token(TokenType type, ValueType literal,
                        enum LiteralType lit_type) {
  std::string text = source.substr(start, current - start);

  size_t last_newline_pos = source.rfind('\n', start);
  unsigned int column_of_token = (last_newline_pos == std::string::npos)
                                     ? (start + 1)
                                     : (start - last_newline_pos);
  if (column_of_token == 0)
    column_of_token = 1;

  switch (lit_type) {
  case STRING:
    tokens.emplace_back(type, text, line, column_of_token,
                        std::get<std::string>(literal));
    break;
  case NUMBER:
    tokens.emplace_back(type, text, line, column_of_token,
                        std::get<double>(literal));
    break;
  case BOOL:
    tokens.emplace_back(type, text, line, column_of_token,
                        std::get<bool>(literal));
    break;
  case NONE:
    tokens.emplace_back(type, text, line, column_of_token);
    break;
  default:
    Minipar::report_error(
        "Tipo literal desconhecido em add_token (erro interno do scanner)",
        std::to_string(column_of_token), line);
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
  unsigned int string_start_line = line;

  size_t last_newline_pos_str = source.rfind('\n', start);
  unsigned int string_start_col = (last_newline_pos_str == std::string::npos)
                                      ? (start + 1)
                                      : (start - last_newline_pos_str);
  if (string_start_col == 0)
    string_start_col = 1;

  while (current < source.size() && source[current] != '"') {
    if (source[current] == '\n') {
      line++;
    }
    str_value += source[current];
    current++;
  }

  if (current >= source.size()) {
    Minipar::report_error("String não terminada. Esperado '\"'.",
                          std::to_string(string_start_col), string_start_line);
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

  size_t last_nl_num = source.rfind('\n', start);
  unsigned int col_num_start =
      (last_nl_num == std::string::npos) ? (start + 1) : (start - last_nl_num);
  if (col_num_start == 0)
    col_num_start = 1;

  try {
    double number_val = std::stod(num_str);
    add_token(TokenType::NUMBER, number_val, NUMBER);
  } catch (const std::invalid_argument &ia) {
    Minipar::report_error("Número inválido: '" + num_str + "'.",
                          std::to_string(col_num_start), line);
  } catch (const std::out_of_range &oor) {
    Minipar::report_error("Número fora do intervalo representável: '" +
                              num_str + "'.",
                          std::to_string(col_num_start), line);
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
