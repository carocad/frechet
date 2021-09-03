(ns scripts.repl
  ;; https://github.com/cursive-ide/cursive/issues/835
  (:require [cljs.repl :as repl]
            [cljs.repl.node :as node]))

(repl/repl (node/repl-env))
