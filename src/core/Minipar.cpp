#include "../include/core/Minipar.h"
#include "../include/core/Parser.h"
#include "../include/core/Scanner.h"
#include "../include/core/Token.h"
#include "../include/core/Semantic.h"

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

  // semantic.imprimirVariaveisPorFuncao();
  //Mostra globais
  //const auto& scopes_from_semantic = semantic.get_symbol_scopes();
  //if (!scopes_from_semantic.empty()) {
  //    for (const auto& [var_name, symbol_info] : scopes_from_semantic[0]) {
  //        std::cout << "  -> Global: " << symbol_info.name.get_lexeme()
  //                  << " : " << symbol_info.type.get_lexeme() << std::endl;
  //    }
  //}
  //Mostra os funções
  //const auto& func_symbols_map = semantic.get_function_symbols();
  //if (!func_symbols_map.empty()) {
  //    for (const auto& [func_name, symbols_vector] : func_symbols_map) {
  //        if (!symbols_vector.empty()) {
  //            for (const auto& symbol_info : symbols_vector) {
  //                std::cout << "  -> Funções: " << symbol_info.name.get_lexeme() 
  //                          << " : " << symbol_info.type.get_lexeme()         
  //                          << " (Linha: " << symbol_info.name.get_line() << ")" << std::endl;
  //          }
  //        }
  //    }
  //}
  //Outros simbolos locais descartados
  //const auto& all_symbols = semantic.get_all_declared_symbols();
  //if (!all_symbols.empty()) {
  //    for (const auto& pair : all_symbols) {
  //        int depth = pair.first;
  //        const auto& symbol_info = pair.second;
  //        std::cout << std::string(depth * 2, ' ')
  //                  << "- " << symbol_info.name.get_lexeme()
  //                  << " : " << symbol_info.type.get_lexeme()
  //                  << " (Linha: " << symbol_info.name.get_line()
  //                  << ", Profundidade: " << depth << ")"
  //                  << std::endl;
  //    }
  //}


  return 0;
}

int Minipar::report_error(std::string msg, std::string where, int line_number) {
  std::cerr << "[Linha " << line_number << "] Erro: " << msg << " na posição "
            << where << std::endl;

  get_instance().has_error = true;

  return 0;
}
