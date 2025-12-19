TARGET_DIR := target/release
LIARC := $(TARGET_DIR)/liarc
LAIR := $(TARGET_DIR)/lair
LIAR_LINT := $(TARGET_DIR)/liar-lint
LIAR_SPEC_OK := target/liar-spec.ok
LIARLIAR_LIR := $(TARGET_DIR)/liarliar.lir
LIARLIAR_BIN := $(TARGET_DIR)/liarliar
LINT_FILES ?= lib/*.liar liarliar/*.liar

$(LIARC):
	cargo build --release -p liar

$(LAIR):
	cargo build --release -p lir-lair

$(LIARLIAR_LIR): $(LIARC)
	@mkdir -p $(TARGET_DIR)
	$(LIARC) liarliar/main.liar > $(LIARLIAR_LIR)

$(LIARLIAR_BIN): $(LIARLIAR_LIR) $(LAIR)
	$(LAIR) $(LIARLIAR_LIR) -o $(LIARLIAR_BIN)

$(LIAR_SPEC_OK): $(LIARC)
	cargo test -p liar-spec
	@mkdir -p $(dir $@)
	@touch $@

.PHONY: liar-spec
liar-spec: $(LIAR_SPEC_OK)

.PHONY: all
all: $(LIARC) $(LAIR) $(LIARLIAR_BIN)

.PHONY: test
test: $(LIAR_SPEC_OK)
	cargo test

.PHONY: lint
lint: $(LIAR_LINT)
	$(LIAR_LINT) $(LINT_FILES)
$(LIAR_LINT): $(LIARC)
