cc = elm-make --warn
BROWSER ?= chrome
BUILD_DIR = build
DOC_FILE = elm-docs.json
NET_DIR = net
OUTPUT = $(BUILD_DIR)/main.js
INPUT_HTML = $(NET_DIR)/index.html
FAVICON = $(NET_DIR)/favicon.png
FINAL_PAGE = $(BUILD_DIR)/index.html

.PHONY: build run debug gendoc agregate build_db update clean update_db

run: build agregate
	$(BROWSER) $(FINAL_PAGE)

update: build agregate

update_db: build_db agregate

clean:
	rm -rf $(BUILD_DIR)

build_db: src
	$(cc) src/Main.elm --output $(OUTPUT) --debug

build: src
	$(cc) src/Main.elm --output $(OUTPUT)

agregate: $(OUTPUT) $(NET_DIR)
	cp $(NET_DIR)/* $(BUILD_DIR)


debug: build_db agregate
	$(BROWSER) $(FINAL_PAGE)

