BROWSER ?= firefox
BUILD_DIR = build
NET_DIR = assets
LOG_LEVEL ?= info
elm19 = ~/.local/bin/elm

# --- Goals ---
.DEFAULT_GOAL = experiment
.PHONY: clean experiment backend frontend debug release

debug : LOG_LEVEL = debug
debug : debug-frontend experiment

release : release-frontend release-backend $(CONTENT)

# --- Static content ---
CONTENT = $(patsubst $(NET_DIR)/%,$(BUILD_DIR)/%,$(shell find $(NET_DIR) -type f))

# --- Javascript content ---
ELM_SOURCE = $(shell find front -type f)
LOBBY_SOURCE = $(shell find lobby -type f)
RUST_SOURCE = $(shell find server -type f)
# Rules
$(BUILD_DIR)/games/classic/code.js : front/Pintclone.elm $(ELM_SOURCE)
	elm make  $< --output $@
$(BUILD_DIR)/lobby/code.js : lobby/Main.elm  $(LOBBY_SOURCE)
	$(elm19) make $< --output=$@

# List of target files to build
JS_TARGETS = $(BUILD_DIR)/games/classic/code.js $(BUILD_DIR)/lobby/code.js

# copy assets into the server filesystem
$(CONTENT) : $(BUILD_DIR)/% : $(NET_DIR)/%
	(cd $(NET_DIR) && cp --parents $(patsubst $(NET_DIR)/%,%,$<) ../$(BUILD_DIR))

debug-frontend :
	$(elm19) make --debug --output=$(BUILD_DIR)/lobby/code.js lobby/Main.elm
	elm make  --debug front/Pintclone.elm --output $(BUILD_DIR)/games/classic/code.js

release-frontend : $(LOBBY_SOURCE) $(ELM_SOURCE) $(CONTENT)
	$(elm19) make --optimize --output=$(BUILD_DIR)/lobby/code.js lobby/Main.elm
	uglifyjs $(BUILD_DIR)/lobby/code.js \
		--compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters=true,keep_fargs=false,unsafe_comps=true,unsafe=true,passes=2' \
		--output=$(BUILD_DIR)/lobby/code.js \
		&& uglifyjs $(BUILD_DIR)/lobby/code.js --mangle --output=$(BUILD_DIR)/lobby/code.js
	elm make  front/Pintclone.elm --output $(BUILD_DIR)/games/classic/code.js
	uglifyjs $(BUILD_DIR)/games/classic/code.js \
		--compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters=true,keep_fargs=false,unsafe_comps=true,unsafe=true,passes=2' \
		--output=$(BUILD_DIR)/games/classic/code.js \
		&& uglifyjs $(BUILD_DIR)/games/classic/code.js --mangle --output=$(BUILD_DIR)/games/classic/code.js

release-backend : $(RUST_SOURCE)
	cargo build --release

experiment : backend frontend
	$(BROWSER) "http://localhost:8080/lobby" &
	RUST_BACKTRACE=1 RUST_LOG=$(LOG_LEVEL) ./target/debug/friendsketch


frontend : $(JS_TARGETS) $(CONTENT)
backend : $(RUST_SOURCE) Cargo.toml
	cargo build

clean:
	rm -rf $(BUILD_DIR)/*
	cargo clean
