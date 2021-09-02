clj \
  -J-Dvlaaad.reveal.prefs='{:theme :light}' \
  -M:dev:test:common:backend:kari:reveal -e "(require '[com.gfredericks.debug-repl] '[hashp.core] )" \
  -m nrepl.cmdline --middleware '[com.gfredericks.debug-repl/wrap-debug-repl vlaaad.reveal.nrepl/middleware]' \
  -i -C

