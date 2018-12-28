BROWSER ?= firefox
BUILD_DIR = build
NET_DIR = assets

# --- Goals ---
.DEFAULT_GOAL = experiment
.PHONY: clean experiment backend frontend debug

ELM_FLAGS = --warn
debug : ELM_FLAGS += --debug
debug : experiment
debug-frontend : ELM_FLAGS += --debug
debug-frontend : frontend

# --- Static content ---
CONTENT = $(patsubst $(NET_DIR)/%,$(BUILD_DIR)/%,$(shell find $(NET_DIR) -type f))

# --- Javascript content ---
ELM_SOURCE = $(shell find front -type f)
RUST_SOURCE = $(shell find server -type f)
# Rules
$(BUILD_DIR)/games/pintclone/code.js : front/Pintclone.elm $(ELM_SOURCE)
	elm-make $(ELM_FLAGS) $< --output $@
$(BUILD_DIR)/lobby/code.js : front/Lobby.elm $(ELM_SOURCE)
	elm-make $(ELM_FLAGS) $< --output $@
# List of target files to build
JS_TARGETS = $(BUILD_DIR)/lobby/code.js $(BUILD_DIR)/games/pintclone/code.js

# copy assets into the server filesystem
$(CONTENT) : $(BUILD_DIR)/% : $(NET_DIR)/%
	(cd $(NET_DIR) && cp --parents $(patsubst $(NET_DIR)/%,%,$<) ../$(BUILD_DIR))


experiment : backend frontend
	(sleep 1 ; $(BROWSER) "http://localhost:8080/") &
	cargo build
	RUST_LOG=debug ./target/debug/friendsketch


frontend : $(JS_TARGETS) $(CONTENT)
backend : $(RUST_SOURCE) Cargo.toml
	cargo build

clean:
	rm -rf $(BUILD_DIR)/*
	cargo clean
