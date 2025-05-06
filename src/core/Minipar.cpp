#include "../include/core/Minipar.h"
#include "../include/core/Parser.h"
#include "../include/core/Scanner.h"
#include "../include/core/Token.h"
#include "../include/core/Semantic.h"
#include "../include/core/Executor.h"

#include <filesystem>
#include <fstream>
#include <iostream>
#include <sysexits.h>

int Minipar::run_file(const std::string filename) {
  std::cout << "Executando o script " << filename << std::endl;
  std::filesystem::path script_path(filename);
  std::ifstream script_file(script_path);

  if (!script_file.is_open()) {
    std::cerr << "ERROR: Could not open file " << filename << std::endl;
    return -1;
  }

  std::string line;
  std::string script;
  while (std::getline(script_file, line)) {
    script += line + "\n";
  }

  run(script);

  if (has_error) {
    exit(EX_DATAERR);
  }

  return 0;
}

int Minipar::run(const std::string script) {
  Scanner scanner(script);
  std::vector<Token> tokens = scanner.scan_tokens();
  Parser parser(tokens);
  ParseResult parse_res = parser.parse();

  if (parse_res.syntax_errors.size() > 0) {
    for (const auto &error : parse_res.syntax_errors) {
      std::cerr << error->get_message() << std::endl;
    }
    return -1;
  }

  SemanticAnalyzer semantic;
  auto semanticErrors = semantic.analyze(parse_res.statements);
  
  if (!semanticErrors.empty()) {
      for (const auto& error : semanticErrors) {
          std::cerr << error->get_message() << std::endl;
      }
      return EXIT_FAILURE;
  } 

  Executor executor;
  executor.execute(parse_res.statements, semantic.get_function_table());

  semantic.imprimirVariaveisPorFuncao();
   if (!has_error) {
   } else {
        std::cout << "Execução cancelada devido a erros." << std::endl;
   }


  return 0;
}

int Minipar::report_error(std::string msg, std::string where, int line_number) {
  std::cerr << "[Linha " << line_number << "] Erro: " << msg << " na posição "
            << where << std::endl;

  get_instance().has_error = true;

  return 0;
}
