#lang racket

(require rackunit
         "program1.rkt")

;; CS 441 - Fall 2026 - Program 1
;; Tests independently derived from the written specification and examples.

;; ---------- Literal and variable evaluation ----------

(check-equal? (eval-expr '(lit 5) #hash())
              '(lit 5))
(check-equal? (eval-expr '(lit "hello") #hash())
              '(lit "hello"))
(check-equal? (eval-expr '(lit maybe) #hash())
              '(lit maybe))
(check-equal? (eval-expr '(var "x") #hash(("x" . 42)))
              '(lit 42))
(check-equal? (eval-expr '(var "x") #hash())
              '(lit maybe))

;; ---------- Arithmetic and constant folding ----------

(check-equal? (eval-expr '(binary-op "+" (lit 5) (lit 7)) #hash())
              '(lit 12))
(check-equal? (eval-expr '(binary-op "-" (lit 10) (lit 4)) #hash())
              '(lit 6))
(check-equal? (eval-expr '(binary-op "*" (lit 3) (lit 4)) #hash())
              '(lit 12))
(check-equal? (eval-expr '(binary-op "/" (lit 20) (lit 4)) #hash())
              '(lit 5))
(check-equal? (eval-expr '(binary-op "+"
                                  (binary-op "/" (lit 20)
                                               (binary-op "-" (lit 8) (lit 4)))
                                  (lit 5))
                         #hash())
              '(lit 10))

;; ---------- Division by zero ----------

(check-equal? (eval-expr '(binary-op "/" (lit 5) (lit 0)) #hash())
              '(lit maybe))
(check-equal? (eval-expr '(binary-op "-"
                                  (binary-op "/" (lit 10) (lit 0))
                                  (lit 0))
                         #hash())
              '(lit maybe))

;; ---------- Type mismatch ----------

(check-equal? (eval-expr '(binary-op "-" (lit yes) (lit yes)) #hash())
              '(lit maybe))
(check-equal? (eval-expr '(binary-op "*" (lit maybe) (lit 1)) #hash())
              '(lit maybe))
(check-equal? (eval-expr '(binary-op "+" (lit 5) (lit "hello")) #hash())
              '(lit maybe))
(check-equal? (eval-expr '(binary-op "~" (lit "number ") (lit 5)) #hash())
              '(lit maybe))
(check-equal? (eval-expr '(binary-op "+" (lit "number ") (lit 5)) #hash())
              '(lit maybe))

;; ---------- Algebraic identities ----------

;; x + 0 -> x -> environment lookup.
(check-equal? (eval-expr '(binary-op "+" (var "x") (lit 0))
                         #hash(("x" . 42)))
              '(lit 42))
(check-equal? (eval-expr '(binary-op "+" (var "x") (lit 0))
                         #hash())
              '(lit maybe))

;; y - 0 -> y.
(check-equal? (eval-expr '(binary-op "-" (var "y") (lit 0))
                         #hash(("y" . -15.5)))
              '(lit -15.5))

;; x * 1 -> x.
(check-equal? (eval-expr '(binary-op "*" (var "score") (lit 1))
                         #hash(("score" . 88.7)))
              '(lit 88.7))

;; x * 0 -> 0 without looking up x.
(check-equal? (eval-expr '(binary-op "*" (var "total") (lit 0))
                         #hash(("total" . 500)))
              '(lit 0))
(check-equal? (eval-expr '(binary-op "*" (var "unbound") (lit 0))
                         #hash())
              '(lit 0))

;; x - x -> 0 without looking up x.
(check-equal? (eval-expr '(binary-op "-" (var "z") (var "z"))
                         #hash(("z" . 99)))
              '(lit 0))
(check-equal? (eval-expr '(binary-op "-" (var "z") (var "z"))
                         #hash())
              '(lit 0))

;; ---------- Required 6A - 3A simplification ----------

(check-equal? (eval-expr '(binary-op "-"
                                  (binary-op "*" (lit 6) (var "A"))
                                  (binary-op "*" (lit 3) (var "A")))
                         #hash(("A" . 10)))
              '(lit 30))
(check-equal? (eval-expr '(binary-op "-"
                                  (binary-op "*" (lit 6) (var "A"))
                                  (binary-op "*" (lit 3) (var "A")))
                         #hash())
              '(lit maybe))

;; ---------- String concatenation ----------

(check-equal? (eval-expr '(binary-op "~"
                                  (lit "this ")
                                  (binary-op "~" (lit "and ") (lit "that")))
                         #hash())
              '(lit "this and that"))

;; ---------- Relational operations ----------

(check-equal? (eval-expr '(binary-op "==" (lit 5) (lit 5)) #hash())
              '(lit yes))
(check-equal? (eval-expr '(binary-op "/=" (lit 5) (lit 6)) #hash())
              '(lit yes))
(check-equal? (eval-expr '(binary-op "<" (lit 4) (lit 5)) #hash())
              '(lit yes))
(check-equal? (eval-expr '(binary-op "<=" (lit 5) (lit 5)) #hash())
              '(lit yes))
(check-equal? (eval-expr '(binary-op ">" (lit 6) (lit 5)) #hash())
              '(lit yes))
(check-equal? (eval-expr '(binary-op ">=" (lit 5) (lit 5)) #hash())
              '(lit yes))
(check-equal? (eval-expr '(binary-op "==" (lit 50) (lit "fifty")) #hash())
              '(lit maybe))
(check-equal? (eval-expr '(binary-op "<" (lit "a") (lit "b")) #hash())
              '(lit yes))

;; ---------- Three-valued logic ----------

(check-equal? (eval-expr '(binary-op "and" (lit yes) (lit no)) #hash())
              '(lit no))
(check-equal? (eval-expr '(binary-op "or" (lit yes) (lit no)) #hash())
              '(lit yes))
(check-equal? (eval-expr '(not (lit no)) #hash())
              '(lit yes))
(check-equal? (eval-expr '(not (lit yes)) #hash())
              '(lit no))
(check-equal? (eval-expr '(not (lit maybe)) #hash())
              '(lit maybe))
(check-equal? (eval-expr '(binary-op "and" (lit no) (lit maybe)) #hash())
              '(lit no))
(check-equal? (eval-expr '(binary-op "and" (lit maybe) (lit yes)) #hash())
              '(lit maybe))
(check-equal? (eval-expr '(binary-op "and" (lit yes) (lit maybe)) #hash())
              '(lit maybe))
(check-equal? (eval-expr '(binary-op "or" (lit yes) (lit maybe)) #hash())
              '(lit yes))
(check-equal? (eval-expr '(binary-op "or" (lit maybe) (lit no)) #hash())
              '(lit maybe))
(check-equal? (eval-expr '(binary-op "or" (lit no) (lit no)) #hash())
              '(lit no))

;; Unbound value becomes maybe, then 3VL decides the result.
(check-equal? (eval-expr '(binary-op "and"
                                  (var "unbound_flag")
                                  (lit no))
                         #hash())
              '(lit no))
(check-equal? (eval-expr '(binary-op "or"
                                  (binary-op "+" (lit 5) (lit "hello"))
                                  (lit yes))
                         #hash())
              '(lit yes))
(check-equal? (eval-expr '(not (binary-op "/" (lit 10) (lit 0))) #hash())
              '(lit maybe))
(check-equal? (eval-expr '(binary-op "<"
                                  (var "x")
                                  (lit 100))
                         #hash(("x" . 50)))
              '(lit yes))
(check-equal? (eval-expr '(binary-op "or"
                                  (binary-op ">" (var "y") (lit 10))
                                  (lit yes))
                         #hash())
              '(lit yes))

;; ---------- Deep fixed-point evaluation ----------

(check-equal? (eval-expr
               '(binary-op "*"
                            (lit 1)
                            (binary-op "+"
                                        (binary-op "*" (lit 2) (var "x"))
                                        (lit 0)))
               #hash(("x" . 7)))
              '(lit 14))

(check-equal? (eval-expr
               '(not
                 (binary-op "=="
                            (binary-op "+" (lit 2) (lit 3))
                            (lit 5)))
               #hash())
              '(lit no))

(displayln "All CS 441 Program 1 tests passed.")
