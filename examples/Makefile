.PHONY: all

DOCS = ../docs
SOURCE_DIR = src
JAVASCRIPT_DIR = ${DOCS}/js
TARGETS = ${JAVASCRIPT_DIR}/Presentation.min.js ${JAVASCRIPT_DIR}/Demo.min.js

all: ${TARGETS}
	@echo "finished"

${JAVASCRIPT_DIR}/%.min.js: ${JAVASCRIPT_DIR}/%.js
	uglifyjs $< --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output=$@
  

${JAVASCRIPT_DIR}/%.js: ${SOURCE_DIR}/%.elm ${JAVASCRIPT_DIR} 
	elm make --optimize --output=$@ $<

${JAVASCRIPT_DIR}:
	mkdir -p $@