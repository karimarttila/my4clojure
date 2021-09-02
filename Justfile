@list:
   just --list

# Start backend repl with my toolbox.
@backend-kari:
    clj -A:dev:test:backend:kari -m nrepl.cmdline -i -C

# Lint
@lint:
    clj -A:dev -m clj-kondo.main --lint src

# Test
@test db:
    ./run-tests.sh {{db}}

