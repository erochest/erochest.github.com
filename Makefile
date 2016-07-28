
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

watch-site-clean: clean build watch-site

watch-code:
	stack build $(FLAGS) --fast --file-watch

watch-code-site:
	stack build $(FLAGS) --file-watch --exec "make clean-site build-site"

serve:
	warp -d _site -p 8000

tags:
	hasktags --ctags app src

deploy:
	$(RUN) errsite deploy

check: hlint
	$(RUN) errsite hakyll check

hlint:
	hlint *.hs src specs

clean: clean-site clean-code

clean-site:
	-$(RUN) errsite hakyll clean

clean-code:
	stack clean
	-rm tags

distclean: clean

build: build-code build-site

build-code:
	stack build $(FLAGS)

build-site:
	$(RUN) errsite hakyll build

rebuild: clean build

restart: distclean build

.PHONY: all test run clean distclean build rebuild hlint watch-site watch-code
