NAME="$(shell basename `pwd`)"

.PHONY: all config build doc test small-tests big-tests demo clean clobber

all: build doc test

config: dist/setup-config

dist/setup-config:
	cabal sandbox init
	cabal install --only-dependencies
	cabal configure

build: config
	cabal build

doc: build
	find src demo -name '*.hs' | xargs haddock --optghc=-package-db --optghc="$$(ls -d .cabal-sandbox/*-packages.conf.d)" --no-warnings --odir=doc --html


test: small-tests big-tests

small-tests: build
	find src demo -name '*.hs' | xargs doctest -package-db "$$(ls -d .cabal-sandbox/*-packages.conf.d)"
	@echo


big-tests: $(patsubst tests/%.expected,proofs/%.proof,$(shell find tests -name '*.expected'))
	-@echo '*** ALL TESTS OK ***'

proofs/%.proof: proofs/%.java tests/%.expected
	diff $^
	touch $@

proofs/%.java: tests/%.m build
	mkdir -p $(dir $@)
	./dist/build/$(NAME)/$(NAME) < $< > $@


demo: build
	./dist/build/$(NAME)/$(NAME) < tests/hello.m


clean:
	rm -rf proofs

clobber: clean
	rm -rf dist doc

distclean: clobber
	rm -rf .cabal-sandbox cabal.sandbox.config
