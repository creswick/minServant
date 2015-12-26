

WEBPACK=node_modules/.bin/webpack
GENJS=../dist/build/genjs/genjs

SOURCE_DIR = src
GENERATED_DIR = $(SOURCE_DIR)/generated
CLIENT_SRC_JS := $(shell find $(SOURCE_DIR) -name '*.js?')
CLIENT_SRC_CSS := $(shell find $(SOURCE_DIR) -name '*.css')

client: index.html jquery.min.js ../static/build.js
	cp index.html jquery.min.js ../static

$(GENERATED_DIR)/server.js: ${GENJS}
	mkdir -p $(GENERATED_DIR)
	${GENJS} $@

../static/build.js: node_modules $(GENERATED_DIR)/server.js ${CLIENT_SRC_JS} ${CLIENT_SRC_CSS}
	${WEBPACK} --progress

node_modules: package.json
	npm install

clean:
	rm -rf ../static/*
	rm -rf $(GENERATED_DIR)
