#pragma once

#include <string>

/**
 * @brief The minipar interpreter class.
 */
class Minipar {
public:
  static Minipar &get_instance() {
    static Minipar instance;
    return instance;
  }

  /**
   * @brief Read and run the script file.
   *
   * @param filename The name of the script file to be executed.
   * @retval 0 if the script was executed successfully.
   * @retval -INT if there was an error during the execution.
   */
  int run_file(const std::string filename);

  /**
   * @brief Report an error.
   *
   * @param msg The error message to be reported.
   * @param where The location of the error (e.g., line number).
   * @param line_number The line number where the error occurred.
   * @retval 0 if the error was reported successfully.
   */
  static int report_error(std::string msg, std::string where, int line_number);

  /**
   * @brief Check if there was an error during the execution.
   *
   * @retval true if there was an error, false otherwise.
   */
  static bool faulted() { return get_instance().has_error; }

private:
  /* Minipar is implemented as a Singleton */
  Minipar() {}
  Minipar(const Minipar &) = delete;
  Minipar &operator=(const Minipar &) = delete;

  /*
   * @brief Flag to indicate if there was an error during the execution.
   */
  bool has_error = false;

  /*
   * @brief Run the script.
   */
  int run(const std::string script);
};
