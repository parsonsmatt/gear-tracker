NPROC = `nproc`
ifeq ($(OS),Darwin)
	NPROC = `sysctl -n hw.logicalcpu`
endif

GHC_OPTIONS ?= "-j$(NPROC) +RTS -A128m -n2m -qg -RTS -O0"

help: ## Print documentation
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

ghcid: ## Rebuild the application in ghcid.
	ghcid --command "stack ghci"

build: ## Build the entire application
	stack build --fast --ghc-options $(GHC_OPTIONS)

test: ##  Run tests for the application
	stack test --fast --ghc-options $(GHC_OPTIONS)

docs: ## Create documentation for the application.
	stack haddock 

open-docs: ## Open documentation for the application
	stack haddock --open

watch: ## Run a file-watcher build process
	stack build --fast --file-watch --ghc-options $(GHC_OPTIONS)

init_db: ## Create the initial database.
	createdb geartracker
	stack exec -- migrate

.PHONY: ghcid help build test watch init_db
