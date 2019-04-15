.PHONY: all clean

DOCS = ../docs
PRESENTATION_DIR = ${DOCS}/presentation/js
PRESENTATION = ${PRESENTATION_DIR}/presentation.js
DEMO_DIR = ${DOCS}/demo/js
DEMO = ${DEMO_DIR}/demo.js
SOURCE_DIR = src
PRESENTATION_SOURCE = ${SOURCE_DIR}/Presentation.elm
DEMO_SOURCE = ${SOURCE_DIR}/Demo.elm


all: ${PRESENTATION} ${DEMO}
	@echo "finished"

${PRESENTATION}: ${PRESENTATION_SOURCE} ${PRESENTATION_DIR}
	elm make --output $@ $<

${DEMO}: ${DEMO_SOURCE} ${DEMO_DIR}
	elm make --output $@ $<

${PRESENTATION_DIR}:
	mkdir -p $@

${DEMO_DIR}:
	mkdir -p $@

clean:
	rm -rf ${PRESENTATION}
	rm -rf ${DEMO}
