#ifndef MINIPAR_H
#define MINIPAR_H

#include <string>

/**
 * @brief The minipar interpreter class.
 */
class Minipar {
public:
  Minipar();
  ~Minipar();

  /**
   * @brief Read and run the script file.
   *
   * @param filename The name of the script file to be executed.
   * @retval 0 if the script was executed successfully.
   * @retval -INT if there was an error during the execution.
   */
  int run_file(const std::string filename);

private:
  /*
   * @brief Flag to indicate if there was an error during the execution.
   */
  bool has_error = false;

  /*
   * @brief Run a single line of the script.
   */
  int run(const std::string line);

  /*
   * @brief Report an error.
   *
   * @param msg The error message to be reported.
   * @param where The location of the error (e.g., line number).
   * @param line_number The line number where the error occurred.
   * @retval 0 if the error was reported successfully.
   */
  int report_error(std::string msg, std::string where, int line_number);
};

#endif /* MINIPAR_H */
