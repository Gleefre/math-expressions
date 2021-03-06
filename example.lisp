(load "gen.lisp")
(load "LaTex-writer.lisp")

;;; Example using genexpr and print-tex-expr

(with-open-file (*standard-output* "example.tex"
                                   :direction :output
                                   :if-exists :supersede)
  (print-tex-expr (loop repeat 45
                        for result = (random-between -100 100)
                        collect (list
                                 '=
                                 (genexpr :start result
                                          :operations '(+ / * / - -))
                                 result))))
