#include "../include/core/Minipar.h"

#include <cstdlib>
#include <iostream>
#include "sysexits.h"

int main(int argc, char *argv[]) {
  if (argc < 2 || argc > 2) {
    std::cerr << "*******************************************************"
              << std::endl;
    std::cerr << "*\t" << "Interpretador Minipar" << std::endl;
    std::cerr << "*\t" << "Erro: arquivo de script não especificado."
              << std::endl;
    std::cerr << "*\t" << "Uso: " << argv[0] << " <script>" << std::endl;
    std::cerr << "*******************************************************"
              << std::endl;
    exit(EX_USAGE);
  }

  Minipar &minipar = Minipar::get_instance();
  if (minipar.run_file(argv[1]) != 0) {
    exit(EXIT_FAILURE);
  }
  
  return 0;
}
