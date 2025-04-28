#include "core/Scanner.h"
#include "core/Minipar.h"

std::vector<Token> Scanner::scan_tokens() {
  while (!source.empty()) {
    start = current;
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

  case ' ':
  case '\r':
  case '\t':
    break;
  case '\n':
    line++;
    break;
  default:
    std::string pos_str = std::to_string(current_pos_in_line);
    Minipar::report_error("Caractere inválido", pos_str, line);
    return;
    break;
  }
}

void Scanner::add_token(TokenType type) { add_token(type, nullptr, NONE); }

void Scanner::add_token(TokenType type, ValueType literal,
                        enum LiteralType lit) {
  std::string text = source.substr(start, current - start);
  switch (lit) {
  case STRING:
    tokens.push_back(Token(type, text, line, std::get<std::string>(literal)));
    break;
  case NUMBER:
    tokens.push_back(Token(type, text, line, std::get<double>(literal)));
    break;
  case BOOL:
    tokens.push_back(Token(type, text, line, std::get<bool>(literal)));
    break;
  case NONE:
    tokens.push_back(Token(type, text, line));
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
