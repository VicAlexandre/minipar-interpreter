CXX := g++
CXXFLAGS := -std=c++17 -Wall -Wextra -pedantic

SRC_DIR := src
INC_DIR := include
BUILD_DIR := build
BIN := minipar

SRC := $(shell find $(SRC_DIR) -name '*.cpp')

OBJ := $(patsubst $(SRC_DIR)/%.cpp, $(BUILD_DIR)/%.o, $(SRC))

DEPS := $(OBJ:.o=.d)

all: $(BIN)

$(BIN): $(OBJ)
	$(CXX) $(CXXFLAGS) -o $@ $^

$(BUILD_DIR)/%.o: $(SRC_DIR)/%.cpp
	@mkdir -p $(dir $@)
	$(CXX) $(CXXFLAGS) -I$(INC_DIR) -MMD -c $< -o $@

clean:
	rm -rf $(BUILD_DIR) $(BIN)

-include $(DEPS)

.PHONY: all clean
