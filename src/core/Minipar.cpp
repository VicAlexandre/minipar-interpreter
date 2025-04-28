#include "core/Minipar.h"
#include "core/Scanner.h"
#include "core/Token.h"

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
    script += line;
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

  for (auto &token : tokens) {
    std::cout << token.to_string() << std::endl;
  }

  return 0;
}

int Minipar::report_error(std::string msg, std::string where, int line_number) {
  std::cerr << "[Linha " << line_number << "] Erro: " << msg << " na posição "
            << where << std::endl;

  get_instance().has_error = true;

  return 0;
}
