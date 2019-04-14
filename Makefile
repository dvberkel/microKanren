.PHONY: clean

DOCS = docs
PRESENTATION_DIR = ${DOCS}/presentation
PRESENTATION = ${PRESENTATION_DIR}/index.html
SOURCE_DIR = src
PRESENTATION_SOURCE = ${SOURCE_DIR}/Presentation.elm

${PRESENTATION}: ${PRESENTATION_SOURCE} ${PRESENTATION_DIR}
	elm make --output $@ $<

${PRESENTATION_DIR}:
	mkdir -p $@

clean:
	rm -rf ${PRESENTATION}
