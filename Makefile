BROWSER ?= firefox
BUILD_DIR = build
NET_DIR = assets
ALT_WAI_ROUTES = ~/code/gitimpo/wai-routes

# --- Goals ---
.DEFAULT_GOAL = experiment
.PHONY: clean experiment backend frontend recabal debug

ELM_FLAGS = --warn
debug : ELM_FLAGS += --debug
debug : experiment
debug-frontend : ELM_FLAGS += --debug
debug-frontend : frontend

# --- Static content ---
CONTENT = $(patsubst $(NET_DIR)/%,$(BUILD_DIR)/%,$(shell find $(NET_DIR) -type f))

# --- Javascript content ---
ELM_SOURCE = $(shell find front -type f)
HASKELL_SOURCE = $(shell find server -type f)
# Rules
$(BUILD_DIR)/games/pintclone/code.js : front/Pintclone.elm $(ELM_SOURCE)
	elm-make $(ELM_FLAGS) $< --output $@
$(BUILD_DIR)/lobby/code.js : front/Lobby.elm $(ELM_SOURCE)
	elm-make $(ELM_FLAGS) $< --output $@
$(BUILD_DIR)/lobby/join/code.js : front/Lobby/Join.elm $(ELM_SOURCE)
	elm-make $(ELM_FLAGS) $< --output $@
# List of target files to build
JS_TARGETS = $(BUILD_DIR)/lobby/code.js $(BUILD_DIR)/games/pintclone/code.js $(BUILD_DIR)/lobby/join/code.js

# copy assets into the server filesystem
$(CONTENT) : $(BUILD_DIR)/% : $(NET_DIR)/%
	(cd $(NET_DIR) && cp --parents $(patsubst $(NET_DIR)/%,%,$<) ../$(BUILD_DIR))


experiment : backend frontend
	(sleep 2 ; $(BROWSER) "http://localhost:8080/lobby") &
	cabal run


frontend : $(JS_TARGETS) $(CONTENT)
backend : $(HASKELL_SOURCE) friendsketch.cabal
	cabal build

recabal :
	cabal sandbox delete
	cabal sandbox init
	cabal sandbox add-source $(ALT_WAI_ROUTES)
	cabal update
	cabal install --dependencies-only
clean:
	rm -rf $(BUILD_DIR)/*
