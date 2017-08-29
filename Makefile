cc = elm-make --warn
BROWSER ?= chrome
BUILD_DIR = build
NET_DIR = net
SRC_DIR = src

CONTENT = $(patsubst $(NET_DIR)/%,$(BUILD_DIR)/%,$(filter-out %.html,$(wildcard $(NET_DIR)/*)))
PAGES = $(patsubst $(NET_DIR)/%,$(BUILD_DIR)/%,$(wildcard $(NET_DIR)/*.html))


.PHONY: run debug clean build compile

run : build
	$(BROWSER) $(BUILD_DIR)/Index.html

debug : build_db
	$(BROWSER) $(BUILD_DIR)/Index.html

clean:
	rm -rf $(BUILD_DIR)/*

build : $(PAGES) $(CONTENT) compile

build_db : $(PAGES) $(CONTENT) compile_db

compile : src
	$(cc) src/Index.elm --output build/Index.js

compile_db :
	$(cc) src/Index.elm --output build/Index.js --debug

$(PAGES) : $(BUILD_DIR)/%.html : $(NET_DIR)/%.html
	cp $< $@

$(CONTENT) : $(BUILD_DIR)/% : $(NET_DIR)/%
	cp $< $@
