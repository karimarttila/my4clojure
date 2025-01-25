# List Just options.
@list:
   just --list

# Start backend repl.
@backend:
    clj -M:dev:test:common:backend:kari -m nrepl.cmdline --middleware '[com.gfredericks.debug-repl/wrap-debug-repl]'  -i -C

# NOTE: Not needed if you use Calva connection "backend + frontend"
# Start backend repl.
# In VSCode: 1. Calva: Connect to a Running REPL, 2: clojure-backend
@backend-calva:
    clj -M:dev:test:common:backend:calva-backend:kari -i -C

# Init node packages.
@init:
   mkdir -p target
   mkdir -p classes
   npm install

# Node compilation (1/2).
@shadow-node:
    npm run dev

# Start node process (2/2).
@run-node:
    node target/main.js

# Update dependencies.
@outdated:
    clojure -M:outdated --upgrade

# Lint.
@lint:
    clj -M:dev:backend:common:node:test -m clj-kondo.main --lint src test


# Clean .cpcache and .shadow-cljs directories, run npm install.
@clean:
    rm -rf .cpcache/*
    rm -rf .shadow-cljs/*
    rm -rf target/*
    rm -rf out/*
    npm install
