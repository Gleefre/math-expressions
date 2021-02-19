;;;; Writes expressions as LaTex formulas

;;; LaTex formula from expression in prefix notation

(defun tex-expression (expr)
  (labels ((atom->tex (atom)
             (if (numberp atom)
                 (multiple-value-bind (integer-part fraction-part)
                     (floor (abs atom))
                   (concatenate 'string
                                (if (< atom 0) "-")
                                (if (or (not (zerop integer-part)) (zerop fraction-part))
                                    (write-to-string integer-part))
                                (if (not (zerop fraction-part))
                                    (format nil "\\frac{~a}{~a}"
                                            (numerator fraction-part)
                                            (denominator fraction-part)))))
                 (format nil "~a" atom)))
           (operation->tex (expr)
             (case (first expr)
               (+ (format nil "~a + ~a"
                          (expression->tex (second expr))
                          (expression->tex (third expr))))
               (- (format nil "~a - ~a"
                          (expression->tex (second expr))
                          (expression->parens-tex (third expr))))
               (* (format nil "~a \\cdot ~a"
                          (expression->parens-tex (second expr))
                          (expression->parens-tex (third expr))))
               ((/ \:) (format nil "\\frac{~a}{~a}"
                               (expression->tex (second expr))
                               (expression->tex (third expr))))
               (= (format nil "~a = ~a"
                          (expression->tex (second expr))
                          (expression->tex (third expr))))))
           (expression->tex (expr)
             (if (atom expr)
                 (atom->tex expr)
                 (operation->tex expr)))
           (expression->parens-tex (expr)
               (if (atom expr)
                   (atom->tex expr)
                   (case (first expr)
                     ((* \: /) (expression->tex expr))
                     (otherwise (format nil "\\left(~a\\right)"
                                        (expression->tex expr)))))))
    (expression->tex expr)))

;;; Prints a number of expressions as LaTex
;;; Uses gather environment

(defun print-tex-expr (expressions &key (expr-on-page 15))
  (labels ((group-by (n lst)
             (let ((result ()))
               (loop for i from 0
                     for el in lst
                     if (zerop (mod i n))
                     do (push () result)
                     do (push el (car result)))
               (reverse (mapcar #'reverse result)))))
    (format t "\\documentclass{article}~%~
               \\usepackage{amsmath}~2%~
               \\begin{document}~2%~
                 ~:{~
                 \\begin{gather}~%~
                   ~@{    ~a~^ \\\\~%~}~%~
                 \\end{gather}~2%~
                 ~}~
               \\end{document}~%"
            (group-by expr-on-page
                      (mapcar #'tex-expression expressions)))))
