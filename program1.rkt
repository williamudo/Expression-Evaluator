#lang racket

(require racket/match)

;; CS 441 - Fall 2026 - Program 1
;; The Expression Evaluator
;;
;; The evaluator works directly on the S-expression AST specified by the
;; assignment.  It repeatedly reduces an expression until it reaches a
;; fixed point.

;; ---------- AST / value helpers ----------

(define (literal? expr)
  (and (list? expr)
       (= (length expr) 2)
       (eq? (first expr) 'lit)))

(define (literal-value expr)
  (second expr))

(define (make-literal value)
  `(lit ,value))

(define (three-valued? value)
  (and (symbol? value)
       (memq value '(yes no maybe))))

;; ---------- 3-valued logic ----------

(define (logical-value op left right)
  (if (and (three-valued? left) (three-valued? right))
      (case op
        [("and")
         (cond
           [(or (eq? left 'no) (eq? right 'no)) 'no]
           [(and (eq? left 'yes) (eq? right 'yes)) 'yes]
           [else 'maybe])]
        [("or")
         (cond
           [(or (eq? left 'yes) (eq? right 'yes)) 'yes]
           [(and (eq? left 'no) (eq? right 'no)) 'no]
           [else 'maybe])]
        [else 'maybe])
      'maybe))

(define (not-value value)
  (cond
    [(eq? value 'yes) 'no]
    [(eq? value 'no) 'yes]
    [(eq? value 'maybe) 'maybe]
    [else 'maybe]))

;; ---------- Primitive operations ----------

(define (arithmetic-value op left right)
  (if (and (number? left) (number? right))
      (case op
        [("+") (+ left right)]
        [("-") (- left right)]
        [("*") (* left right)]
        [("/")
         (if (= right 0)
             'maybe
             (/ left right))]
        [else 'maybe])
      'maybe))

(define (concatenate-value left right)
  (if (and (string? left) (string? right))
      (string-append left right)
      'maybe))

(define (same-comparison-type? left right)
  (cond
    [(and (number? left) (number? right)) #t]
    [(and (string? left) (string? right)) #t]
    [else #f]))

(define (comparison-value op left right)
  (if (not (same-comparison-type? left right))
      'maybe
      (cond
        [(number? left)
         (case op
           [("==") (if (= left right) 'yes 'no)]
           [("/=") (if (= left right) 'no 'yes)]
           [("<")  (if (< left right) 'yes 'no)]
           [("<=") (if (<= left right) 'yes 'no)]
           [(">")  (if (> left right) 'yes 'no)]
           [(">=") (if (>= left right) 'yes 'no)]
           [else 'maybe])]
        [(string? left)
         (case op
           [("==") (if (string=? left right) 'yes 'no)]
           [("/=") (if (string=? left right) 'no 'yes)]
           [("<")  (if (string<? left right) 'yes 'no)]
           [("<=") (if (string<=? left right) 'yes 'no)]
           [(">")  (if (string>? left right) 'yes 'no)]
           [(">=") (if (string>=? left right) 'yes 'no)]
           [else 'maybe])]
        [else 'maybe])))

(define (binary-value op left right)
  (cond
    [(member op '("+" "-" "*" "/"))
     (arithmetic-value op left right)]
    [(equal? op "~")
     (concatenate-value left right)]
    [(member op '("==" "/=" "<" "<=" ">" ">="))
     (comparison-value op left right)]
    [(member op '("and" "or"))
     (logical-value op left right)]
    [else 'maybe]))

;; ---------- Algebraic / structural simplification ----------
;;
;; These rules are deliberately applied before variable lookup.  This is
;; important for identities such as x * 0 and x - x: the expression can be
;; reduced without needing to know the value of x.

(define (structural-simplification op left right)
  (cond
    ;; Addition identities.
    [(and (equal? op "+")
          (literal? right)
          (equal? (literal-value right) 0))
     left]
    [(and (equal? op "+")
          (literal? left)
          (equal? (literal-value left) 0))
     right]

    ;; Subtraction identities.
    [(and (equal? op "-")
          (literal? right)
          (equal? (literal-value right) 0))
     left]
    [(and (equal? op "-")
      (not (literal? left))
      (equal? left right))
 (make-literal 0)]

    ;; Multiplication identities.
    [(and (equal? op "*")
          (literal? right)
          (equal? (literal-value right) 1))
     left]
    [(and (equal? op "*")
          (literal? left)
          (equal? (literal-value left) 1))
     right]
    [(and (equal? op "*")
          (literal? right)
          (number? (literal-value right))
          (= (literal-value right) 0))
     (make-literal 0)]
    [(and (equal? op "*")
          (literal? left)
          (number? (literal-value left))
          (= (literal-value left) 0))
     (make-literal 0)]

    ;; Division identity.
    [(and (equal? op "/")
          (literal? right)
          (equal? (literal-value right) 1))
     left]

    ;; Factor a common symbolic term:
    ;;   (a * x) - (b * x)  ->  (a-b) * x
    ;; This is needed for the assignment's 6A - 3A example.
    [(and (equal? op "-")
          (list? left)
          (list? right)
          (= (length left) 4)
          (= (length right) 4)
          (eq? (first left) 'binary-op)
          (eq? (first right) 'binary-op)
          (equal? (second left) "*")
          (equal? (second right) "*")
          (literal? (third left))
          (literal? (third right))
          (number? (literal-value (third left)))
          (number? (literal-value (third right)))
          (equal? (fourth left) (fourth right)))
     (let ([coefficient
            (- (literal-value (third left))
               (literal-value (third right)))])
       (cond
         [(= coefficient 0)
          (make-literal 0)]
         [(= coefficient 1)
          (fourth left)]
         [else
          `(binary-op "*" (lit ,coefficient) ,(fourth left))]))]

    [else #f]))

;; ---------- One reduction step ----------

(define (eval-step expr env)
  (match expr
    [`(lit ,value)
     expr]

    [`(var ,id)
     (if (and (string? id) (hash-has-key? env id))
         (make-literal (hash-ref env id))
         (make-literal 'maybe))]

    [`(not ,subexpr)
     (define evaluated-subexpr (eval-expr subexpr env))
     (if (literal? evaluated-subexpr)
         (make-literal (not-value (literal-value evaluated-subexpr)))
         `(not ,evaluated-subexpr))]

    [`(binary-op ,op ,left ,right)
     ;; Apply structural identities first so x*0 and x-x do not require a
     ;; variable lookup, matching the assignment's examples.
     (define structural-result
       (structural-simplification op left right))
     (if structural-result
         structural-result
         (let ([evaluated-left (eval-expr left env)]
               [evaluated-right (eval-expr right env)])
           (if (and (literal? evaluated-left)
                    (literal? evaluated-right))
               (make-literal
                (binary-value op
                              (literal-value evaluated-left)
                              (literal-value evaluated-right)))
               `(binary-op ,op ,evaluated-left ,evaluated-right))))]

    [else
     (error 'eval-expr "Invalid AST node structure: ~a" expr)]))

;; ---------- Public evaluator ----------

;; eval-expr repeatedly applies eval-step until the AST reaches a fixed point.
;; For valid ASTs and an immutable hash environment, every returned result is
;; either an unchanged expression or a literal AST of the form (lit value).
(define (eval-expr expr env)
  (let loop ([current expr])
    (define next (eval-step current env))
    (if (equal? next current)
        current
        (loop next))))

(provide eval-expr)
