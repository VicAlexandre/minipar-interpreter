#include "core/Minipar.h"

#include <filesystem>
#include <fstream>
#include <iostream>
#include <sysexits.h>

Minipar::Minipar() {}

Minipar::~Minipar() {}

int Minipar::run_file(const std::string filename) {
  std::cout << "Executando o script " << filename << std::endl;
  std::filesystem::path script_path(filename);
  std::ifstream script_file(script_path);

  if (!script_file.is_open()) {
    std::cerr << "ERROR: Could not open file " << filename << std::endl;
    return -1;
  }

  std::string line;
  while (std::getline(script_file, line)) {
    run(line);

    if (has_error) {
      exit(EX_DATAERR);
    }
  }

  return 0;
}

int Minipar::run(const std::string line) {
  std::cout << "Executando a linha: " << line << std::endl;

  return 0;
}

int Minipar::report_error(std::string msg, std::string where, int line_number) {
  std::cerr << "[line " << line_number << "] Error" << where << ": " << msg
            << std::endl;

  has_error = true;

  return 0;
}
