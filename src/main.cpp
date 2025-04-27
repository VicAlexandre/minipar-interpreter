#include "core/Minipar.h"

#include <iostream>

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
    exit(64);
  }

  Minipar minipar = Minipar();
  if (minipar.run_file(argv[1]) != 0) {
    std::cerr << "Erro ao executar o script." << std::endl;
    exit(64);
  }

  return 0;
}
