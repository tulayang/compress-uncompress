(define it-n
  (lambda (x n)
    (if (> n 1)
      (list (list n x)) ; ((3 1))
      (list x))))       ; (1)

(define compare
  (lambda (seq last-seq n x)
    (if (eq? last-seq '())
      (append seq (it-n x n))
      (let ((i (car last-seq)))
        (if (= x i)
          (compare seq (cdr last-seq) (+ n 1) i)  
          (compare (append seq (it-n x n)) (cdr last-seq) 1 i))))))

(define compress
  (lambda (seq)
    (if (list? seq)
      (compare '() (cdr seq) 1 (car seq))
      seq)))

(define unof
  (lambda (lst n x)
    (if (> n 0)
      (unof (append lst (list x)) (- n 1) x)
      lst)))                                 

(define unlice
  (lambda (seq it last-seq)
    (if (eq? last-seq '())
      (if (list? it)
        (let ((n (car it)) (x (car (cdr it))))
          (append seq (unof '() n x)))
        (append seq (list it)))
      (if (list? it)
        (let ((n (car it)) (x (car (cdr it))))
          (unlice (append seq (unof '() n x)) (car last-seq) (cdr last-seq)))
        (unlice (append seq (list it)) (car last-seq) (cdr last-seq))))))

(define uncompress
  (lambda (seq)
    (if (list? seq)
      (unlice '() (car seq) (cdr seq))
      seq)))

; (1 1 1 0 1 0 0 0 0 1) => ((3 1) 0 1 (3 0) 0 1)
(display "(1 1 1 0 1 0 0 0 0 1) => ") 
(display (compress '(1 1 1 0 1 0 0 0 0 1)))
(newline)
; ((3 1) 0 1 (3 0) 0 1) => (1 1 1 0 1 0 0 0 0 1)
(display "((3 1) 0 1 (3 0) 0 1) => ")
(display (uncompress '((3 1) 0 1 (3 0) 0 1)))
(newline)
; (0 0 1 0 1 0 0 0 0 0 0) => ((2 0) 1 0 1 (6 0)) 
(display "(0 0 1 0 1 0 0 0 0 0 0) => ")
(display (compress '(0 0 1 0 1 0 0 0 0 0 0)))
(newline)
; ((2 0) 1 0 1 (6 0)) => (0 0 1 0 1 0 0 0 0 0 0)
(display "((2 0) 1 0 1 (6 0)) => ")
(display (uncompress '((2 0) 1 0 1 (6 0))))