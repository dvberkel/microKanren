.PHONY: all clean

DOCS = docs
PRESENTATION_DIR = ${DOCS}/presentation
PRESENTATION = ${PRESENTATION_DIR}/index.html
DEMO_DIR = ${DOCS}/demo
DEMO = ${DEMO_DIR}/index.html
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
