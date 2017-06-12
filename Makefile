cc = elm-make --warn
BROWSER ?= chrome
BUILD_DIR = build
DOC_FILE = elm-docs.json
CSS_DIR = css
OUTPUT = $(BUILD_DIR)/main.js
INPUT_HTML = index.html
FINAL_PAGE = $(BUILD_DIR)/$(INPUT_HTML)

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

agregate: $(OUTPUT) index.html
	cp $(INPUT_HTML) $(BUILD_DIR)
	cp $(CSS_DIR)/* $(BUILD_DIR)


debug: build_db agregate
	$(BROWSER) $(FINAL_PAGE)

gendoc:
	$(cc) src/Main.elm --docs $(DOC_FILE)
