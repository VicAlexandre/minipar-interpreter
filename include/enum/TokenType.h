#pragma once

#include <string>

/**
 * @brief All token types recognized by the Minipar's scanner.
 */
enum class TokenType {
  LEFT_PAREN,    /** ( */
  RIGHT_PAREN,   /** ) */
  LEFT_BRACE,    /** { */
  RIGHT_BRACE,   /** } */
  LEFT_BRACKET,  /** [ */
  RIGHT_BRACKET, /** ] */
  COMMA,         /** , */
  DOT,           /** . */
  PLUS,          /** + */
  STAR,          /** * */
  SLASH,         /** / */
  PERCENT,       /** % */
  COLON,         /** : */

  /* One or two character tokens */
  MINUS,         /** - */
  ARROW,         /** -> */
  EQUAL_ASSIGN,  /** = */
  EQUAL_COMPARE, /** == */
  BANG,          /** ! */
  BANG_EQUAL,    /** != */
  GREATER,       /** > */
  GREATER_EQUAL, /** >= */
  LESS,          /** < */
  LESS_EQUAL,    /** <= */
  OR_OR,         /** || */
  AND_AND,       /** && */

  /* Literals */
  IDENTIFIER,     /** IDENT */
  NUMBER,         /** NUMBER */
  STRING_LITERAL, /** STRING */

  /* Keywords */
  SEQ,           /** seq */
  PAR,           /** par */
  IF,            /** if */
  ELSE,          /** else */
  WHILE,         /** while */
  RETURN,        /** return */
  BREAK,         /** break */
  CONTINUE,      /** continue */
  FUNC,          /** func */
  C_CHANNEL,     /** c_channel */
  TYPE_NUMBER,   /** number */
  TYPE_BOOL,     /** bool */
  TYPE_STRING,   /** string */
  TRUE_LITERAL,  /** true */
  FALSE_LITERAL, /** false */

  /* Special */
  END_OF_FILE, /** End of file */
};

/**
 * @brief Converts a TokenType to its string representation.
 */
std::string to_str(TokenType type);
