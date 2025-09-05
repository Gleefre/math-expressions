;;;; Generates mathematical expressions in prefix notation

;;; Random functions

(defun random-between (a b &key (non-zero nil))
  (if (and non-zero (<= a 0 b))
      (let ((num (random-between a (1- b))))
        (+ num (if (minusp num) 0 1)))
      (+ a (random (- b a -1)))))

(defun random-rational (&key (num-above 1) (num-below 100) (denom-above 1) (denom-below 100) (non-zero nil))
  (/ (random-between num-above num-below :non-zero non-zero)
     (random-between denom-above denom-below :non-zero t)))

(defun random-cons-cell (expr)
  (if (null (cdr expr))
      expr
      (let ((ind (1+ (random 2))))
        (if (atom (nth ind expr))
            (nthcdr ind expr)
            (random-cons-cell (nth ind expr))))))

;;; Operations for expressions

(defun inverse-func (func)
  (case func
    (+ #'-)
    (- #'+)
    (* #'/)
    ((/ \:) #'*)))

(defun split-rational (func q-rational &key (denom-below 10) (num-below 9)
                                            (denom-above 1) (num-above 1))
  (let ((p-rational (random-rational :denom-below denom-below
                                     :num-below num-below
                                     :denom-above denom-above
                                     :num-above num-above
                                     :non-zero (member func '(/ \: *))))
        (inv-func (inverse-func func)))
    (list func (funcall inv-func q-rational p-rational)
          p-rational)))

;;; Expression generator

(defun genexpr (&key (start 0) (operations (list '+ '* '- '\:))
                     (denom-below 10) (num-below 9)
                     (denom-above 1) (num-above 1))
  (let ((expr (split-rational (first operations)
                              start
                              :denom-below denom-below
                              :num-below num-below
                              :denom-above denom-above
                              :num-above num-above)))
    (loop for func in (rest operations)
          do (let ((place (random-cons-cell expr)))
               (setf (first place)
                     (split-rational func (first place)
                                     :denom-below denom-below
                                     :num-below num-below
                                     :denom-above denom-above
                                     :num-above num-above))))
    expr))

;;; Converter to infix notation

(defun refact-expr (expr)
  (if (atom expr)
      expr
      (list (refact-expr (second expr)) (first expr) (refact-expr (third expr)))))
