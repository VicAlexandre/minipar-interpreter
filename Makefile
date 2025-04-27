CXX := g++
CXXFLAGS := -std=c++11 -Wall -Wextra -pedantic

SRC_DIR := src
BUILD_DIR := build
BIN := minipar

SRC := $(wildcard $(SRC_DIR)/*.cpp)
OBJ := $(patsubst $(SRC_DIR)/%.cpp,$(BUILD_DIR)/%.o,$(SRC))

all: $(BIN)

$(BIN): $(OBJ)
	$(CXX) -o $@ $^ $(CXXFLAGS)

$(BUILD_DIR)/%.o: $(SRC_DIR)/%.cpp
	@mkdir -p $(BUILD_DIR)
	$(CXX) -c $< -o $@ $(CXXFLAGS)

clean:
	rm -rf $(BUILD_DIR) $(BIN)

.PHONY: all clean
