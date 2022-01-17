
# List Just options.
@list:
   just --list

# Start backend repl.
@backend:
    clj -M:dev:test:common:backend:kari -m nrepl.cmdline --middleware '[com.gfredericks.debug-repl/wrap-debug-repl]'  -i -C
    # clj -M:dev:test:common:backend -m nrepl.cmdline -i -C

# Init node packages.
@init:
   mkdir -p target
   mkdir -p classes
   npm install

# Start frontend auto-compilation.
@frontend:
    npm run dev

# SASS auto-compile.
@css:
    npm run build:sass

# Update dependencies.
@outdated:
    clj -M:dev:test:common:backend:frontend:outdated --update

# Create uberjar.
@uberjar:
    mkdir -p prod-resources/public/js
    mkdir -p prod-resources/public/css
    mkdir -p target/shadow/prod/resources/public/data
    cp dev-resources/public/css/bulma*.css target/shadow/prod/resources/public/css/.
    npm run build:sass
    clj -M:common:frontend -m shadow.cljs.devtools.cli release app
    clj -M:common:backend:uberjar

# Run uberjar (run as PROFILE=prod just run-uberjar).
run-uberjar:
    java -cp target/vega.jar clojure.main -m vega.backend.main

# Lint.
@lint:
    clj -M:dev:backend:common:frontend:test -m clj-kondo.main --lint src test
    # clj -A:dev -m clj-kondo.main --lint src
# Test.
@test options:
    ./run-tests.sh {{options}}

# Clean .cpcache and .shadow-cljs directories, run npm install.
@clean:
    rm -rf .cpcache/*
    rm -rf .shadow-cljs/*
    rm -rf target/*
    rm -rf dev-resources/public/js/*
    rm -rf dev-resources/public/css/*
    rm -rf prod-resources/public/js/*
    rm -rf prod-resources/public/css/*
    rm -rf dynamodb/dev-resources/*
    rm -rf out/*
    npm install
