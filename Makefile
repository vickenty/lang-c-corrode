.PHONY: all test clean

all: test

MDTESTC=mdtestc/target/release/mdtestc
MDTESTS=$(subst .md,.rs,$(wildcard mdtests/*.md))

test: $(MDTESTS)
	cargo test

mdtests/%.rs: mdtests/%.md $(MDTESTC)
	$(MDTESTC) $< $@

$(MDTESTC): $(wildcard mdtestc/src/*.rs) mdtestc/Cargo.toml
	(cd mdtestc && cargo build --release)

clean:
	(cd mdtestc && cargo clean)
	rm -f $(MDTESTS)
