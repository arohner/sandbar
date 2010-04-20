#!/bin/sh
java -cp lib/'*':src jline.ConsoleRunner clojure.main -i bin/repl_config.clj -r

