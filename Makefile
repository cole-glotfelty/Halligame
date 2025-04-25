# Build all the things!
# Michael Daniels, 2025-04-21

SPHINXUV      ?= uv run --with=furo,sphinx,myst-parser
SPHINXAPI     ?= $(SPHINXUV) sphinx-apidoc
SPHINXBUILD   ?= $(SPHINXUV) sphinx-build
SOURCEDIR     = docs/source
BUILDDIR      = docs/html_python

all: pyproject cli serverbroker

pyproject: pyproject.toml $(wildcard **/*.py)
	uv sync

cli:
	cd src/cli && rebar3 compile

serverbroker:
	cd src/serverbroker && rebar3 compile

# Slow, and potentially not needed.
wheel: pyproject
	uv build

public: all
	find . -type d | xargs chmod a+rx
	chmod --recursive a+r .

docs: pydocs erldocs
erldocs: cli-erldocs serverbroker-erldocs

pydocs: pyproject
	# uv run pdoc -o html -t doc_template src/cli src/halligame
	$(SPHINXAPI) -f -o "docs/source" "src"
	$(SPHINXBUILD) "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

cli-erldocs:
	cd src/cli && rebar3 edoc

serverbroker-erldocs:
	cd src/serverbroker && rebar3 edoc