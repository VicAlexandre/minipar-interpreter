#include <iostream>

int main(int argc, char *argv[]) {
  if (argc < 2) {
    std::cerr << "Interpretador Minipar" << std::endl;
    std::cerr << "Uso: " << argv[0] << " <script>" << std::endl;
    return 1;
  }
  std::cout << "Hello, World!" << std::endl;
  return 0;
}
