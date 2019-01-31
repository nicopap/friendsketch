BROWSER ?= firefox
BUILD_DIR = build/friendk
NET_DIR = assets
LOG_LEVEL ?= info

# --- Goals ---
.DEFAULT_GOAL = experiment
.PHONY: clean experiment backend frontend debug

ELM_FLAGS = --warn
debug : ELM_FLAGS += --debug
debug : LOG_LEVEL = debug
debug : experiment
debug-frontend : ELM_FLAGS += --debug
debug-frontend : frontend

# --- Static content ---
CONTENT = $(patsubst $(NET_DIR)/%,$(BUILD_DIR)/%,$(shell find $(NET_DIR) -type f))

# --- Javascript content ---
ELM_SOURCE = $(shell find front -type f)
RUST_SOURCE = $(shell find server -type f)
# Rules
$(BUILD_DIR)/games/classic/code.js : front/Pintclone.elm $(ELM_SOURCE)
	elm-make $(ELM_FLAGS) $< --output $@
$(BUILD_DIR)/lobby/code.js : front/Lobby.elm $(ELM_SOURCE)
	elm-make $(ELM_FLAGS) $< --output $@
# List of target files to build
JS_TARGETS = $(BUILD_DIR)/lobby/code.js $(BUILD_DIR)/games/classic/code.js

# copy assets into the server filesystem
$(CONTENT) : $(BUILD_DIR)/% : $(NET_DIR)/%
	(cd $(NET_DIR) && cp --parents $(patsubst $(NET_DIR)/%,%,$<) ../$(BUILD_DIR))


experiment : backend frontend
	$(BROWSER) "http://localhost:8080/friendk/lobby" &
	RUST_BACKTRACE=1 RUST_LOG=$(LOG_LEVEL) ./target/debug/friendsketch


frontend : $(JS_TARGETS) $(CONTENT)
backend : $(RUST_SOURCE) Cargo.toml
	cargo build

clean:
	rm -rf $(BUILD_DIR)/*
	cargo clean
