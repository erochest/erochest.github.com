
FLAGS=--pedantic
RUN=stack exec --

all: test docs package

test: build
	stack test $(FLAGS)

run: build
	$(RUN) errsite --help

# docs:
# generate api documentation
#
# package:
# build a release tarball or executable
#
# dev:
# start dev server or process. `vagrant up`, `yesod devel`, etc.
#
# install:
# generate executable and put it into `/usr/local`

watch-site:
	$(RUN) errsite hakyll watch

watch-code:
	stack build $(FLAGS) --file-watch

tags:
	hasktags --ctags app src

deploy:
	$(RUN) errsite deploy

hlint:
	hlint *.hs src specs

clean:
	stack clean
	-rm tags

distclean: clean

build:
	stack build $(FLAGS)

rebuild: clean configure build

restart: distclean build

.PHONY: all test run clean distclean configure build rebuild hlint watch-site watch-code
