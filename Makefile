.PHONY: website clear

SOURCE_DIR = src
SOURCE_FILE = Main
SOURCE = ${SOURCE_DIR}/${SOURCE_FILE}.elm
TARGET_DIR = docs
TARGET_FILE = index.html
TARGET = ${TARGET_DIR}/${TARGET_FILE}

website: ${TARGET}
	echo "finished"

${TARGET}: ${SOURCE} ${TARGET_DIR}
	elm make $<
	mv ${TARGET_FILE} $@


${TARGET_DIR}:
	mkdir $@

clean:
	rm -rf ${TARGET_DIR}
