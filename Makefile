cc = elm-make --warn
BROWSER ?= chrome
BUILD_DIR = build
NET_DIR = net
SRC_DIR = src

CONTENT = $(patsubst $(NET_DIR)/%,$(BUILD_DIR)/%,$(filter-out %.html,$(wildcard $(NET_DIR)/*)))
PAGES = $(patsubst $(NET_DIR)/%,$(BUILD_DIR)/%,$(wildcard $(NET_DIR)/*.html))


.PHONY: run clean build

run : build
	$(BROWSER) $(BUILD_DIR)/Index.html

clean:
	rm -rf $(BUILD_DIR)/*

build : $(PAGES) $(CONTENT)

$(BUILD_DIR)/%.js : $(SRC_DIR)/%.elm
	$(cc) $< --output $@

$(PAGES) : $(BUILD_DIR)/%.html : $(NET_DIR)/%.html $(BUILD_DIR)/%.js
	cp $< $@

$(CONTENT) : $(BUILD_DIR)/% : $(NET_DIR)/%
	cp $< $@
