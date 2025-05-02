#include "core/Scanner.h"
#include "core/Minipar.h"
#include "enum/TokenType.h"

#include <unordered_map>

std::unordered_map<std::string, TokenType> keywords = {
    {"SEQ", TokenType::SEQ},
    {"PAR", TokenType::PAR},
    {"if", TokenType::IF},
    {"else", TokenType::ELSE},
    {"while", TokenType::WHILE},
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

  /* add EOF token after all tokens have been scanned */
  add_token(TokenType::END_OF_FILE);

  return tokens;
}

void Scanner::scan_token() {
  /* scan the next token */
  char c = source[current];
  int current_pos_in_line = current - start;
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
      std::string pos_str = std::to_string(current_pos_in_line);
      Minipar::report_error("Esperado '|'", pos_str, line);
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
      std::string pos_str = std::to_string(current_pos_in_line);
      printf("caracter invalido: %2x\n", c);
      Minipar::report_error("Caractere inválido", pos_str, line);
      return;
    }
    break;
  }
}

void Scanner::add_token(TokenType type) { add_token(type, "", NONE); }

void Scanner::add_token(TokenType type, ValueType literal,
                        enum LiteralType lit) {
  std::string text = source.substr(start, current - start);
  unsigned int column = (current - start);

  switch (lit) {
  case STRING:
    tokens.push_back(
        Token(type, text, line, column, std::get<std::string>(literal)));
    break;
  case NUMBER:
    tokens.push_back(
        Token(type, text, line, column, std::get<double>(literal)));
    break;
  case BOOL:
    tokens.push_back(Token(type, text, line, column, std::get<bool>(literal)));
    break;
  case NONE:
    tokens.push_back(Token(type, text, line, column));
    break;
  default:
    __builtin_unreachable();
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
  std::string str = "";
  while (current < source.size() && source[current] != '"') {
    if (source[current] == '\n') {
      line++;
    }
    str += source[current];
    current++;
  }

  if (current >= source.size()) {
    Minipar::report_error("Esperado '\"'", std::to_string(current), line);
    return;
  }

  current++;
  add_token(TokenType::STRING_LITERAL, str, STRING);
}

void Scanner::scan_number() {
  while (current < source.size() && is_digit(source[current])) {
    current++;
  }

  if (current < source.size() && source[current] == '.') {
    current++;
    while (current < source.size() && is_digit(source[current])) {
      current++;
    }
  }

  std::string num_str = source.substr(start, current - start);
  double number = std::stod(num_str);
  add_token(TokenType::NUMBER, number, NUMBER);
}

void Scanner::scan_identifier() {
  while (current < source.size() && is_alpha_numeric(source[current])) {
    current++;
  }

  std::string text = source.substr(start, current - start);

  TokenType type = keywords.find(text) != keywords.end()
                       ? keywords[text]
                       : TokenType::IDENTIFIER;

  add_token(type);
}
