.PHONY: all clean

DOCS = ../docs
SOURCE_DIR = src
JAVASCRIPT_DIR = ${DOCS}/js
TARGETS = ${JAVASCRIPT_DIR}/Presentation.js ${JAVASCRIPT_DIR}/Demo.js

all: ${TARGETS}
	@echo "finished"

${JAVASCRIPT_DIR}/%.js: ${SOURCE_DIR}/%.elm ${JAVASCRIPT_DIR} 
	elm make --output=$@ $<

${JAVASCRIPT_DIR}:
	mkdir -p $@