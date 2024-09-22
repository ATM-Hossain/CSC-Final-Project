;; ----------------------------------------------------------------------------
;; 1. Write and prove correct a program to input an LD number (as previously defined) which returns a list of all lists of
;; 1s and 2s which map to that number. Use this program to check your solution to the takehome part of the midterm. 
;; ----------------------------------------------------------------------------
;; Spring 2024
;; Project




;; I ask for complete developments and proved, working code for three problems:

;; 1.  Write and prove correct a program to input an LD number (as previously defined) which returns a list of all lists of
;; 1s and 2s which map to that number.  Use this program to check your solution to the takehome part of the midterm. 

;; 2.  Write and prove correct a syntax checker for TLS-scheme, as specified in HW 11.
; start of custom helper functions, may require using them on the main function itself
;; CSc 335
;; Spring 2024
;; Project

;; I ask for complete developments and proved, working code for three problems:

;; 1.  Write and prove correct a program to input an LD number (as previously defined) which returns a list of all lists of
;; 1s and 2s which map to that number.  Use this program to check your solution to the takehome part of the midterm. 

;; 2.  Write and prove correct a syntax checker for TLS-scheme, as specified in HW 11.
;; 3.  Write and prove correct an interpreter for TLS extended by let*.



;this function takes a list and adds an element to the back of the list
(define (cons-end lst elem)
    (append lst (list elem)))
 ; This function takes a list and an input function and applies the function on each element of the list returns a newlist
;where every element had the function applied on it
; The main function works as such it finds the


; Proof for all examples theres a 1 and 2 on the outside
; The Function scans all 1s (open brackets) from left to right [8 1 1 2 1 3 2 2 1 5 6 1 2] -> [8 ( 1 2 1 3 2 2 1 5 6 1 2] - > [8 1 ( 2 1 3 2 2 1 5 6 1 2] ...
; for each of the ones chosen the function pairs that specific one with all 2s to the right of it. It matches that one with every single 2 until all there are no more 2s left.
; example [8 ( 1 2 1 3 2 2 1 5 6 1 2] -> [8 ( 1 ) 1 3 2 2 1 5 6 1 2] - > [8 1 ( 2 1 3 ) 2 1 5 6 1 2] -> etc
; With the 2s you can either choose to create a list or keep it as a number. ex -> [8 ( 1 2 1 3 2 2 1 5 6 1 2] or [8 ( 1 2 1 3 2 ) 1 5 6 1 2]. If you do choose to use the 2 as a bracket
; to make a list you have to find all the permutations of the little list.
; example (1 2 1 3 2) is a sublist and we treat it like the larger list (8 1 1 2 1 3 2 2 1 5 6 1 2)and input the smaller (1 2 1 3 2) and generate all its permutations. 
;[example (( 2 ) 3 2) , (( 2 1 3 ))... ]. In the larger context it looks like this  (8 (( 2 ) 3 2) 1 5 6 1 2).

(define (flatmap op lst)
    (cond
      ((null? lst) '()) ;We've exhausted the ld-lst whilst constructing a sublist
      (else (append (op (car lst)) (flatmap op (cdr lst))))))
;The sub variable checks if we are inside the list, or not. 
;Base Case is when the all elements in the list has been processed returning all possible combinations
;Inductive Hypothesis: assume the function works
;Inductive Step: for very call we encounter a 2, we can choose to consume it and accept it as a value or use it as
;as a parenthesis. If we use it as a number we simply add it to the sublist, if we "close" our sublist using two as )
;then we take the new list formed by the 2 and generate all sublists formed by the minilist. [example from earlier (1 2 1 3 2)]
;For every call if we see an element which is 1 we also have two choices, we can either add the number to the list and treat it like a
; normal number or we can use the 1 to start a new sublist. Therefor for every encounter of 1s and 2s we either can choose them as numbers
; or as bracket elements which branchs off to two possibilities. All The branches from the K+1th call will lead to all possible Ld combinations
; which will then be added to our list of lists.

(define (perms lst)

  (define (perm p new sub rem)
    (cond
      ((null? rem)
       (if sub '() (list p)))
       
      (sub
       (cond
         ((null? new); Sublist is empty no longer can nest inside itself so we add it to the list
          (perm p (cons-end new (car rem)) #t (cdr rem)))
         ((eq? 2 (car rem))
          (append
           (perm p (cons-end new 2) #t (cdr rem))
           (flatmap
            (lambda (item) (perm (cons-end p item) '() #f (cdr rem)))
            (perms new))))
         (else (perm p (cons-end new (car rem)) #t (cdr rem)))))
      (else
       (cond
         ((eq? 1 (car rem))
          (append
           (perm (cons-end p 1) '() #f (cdr rem))
           (perm p '() #t (cdr rem))))
         (else (perm (cons-end p (car rem)) '() #f (cdr rem)))))))
  
  (perm '() '() #f lst))
; Example usage
(length(perms (list 9 1 8 7 2 3 0 8 6 6 1 2 7 8 7 4 9 6 9 1 2 3 2 2 4 3 1 2 1 6 2 9 0 6 2 1 2 1 2 1 6 0 1)))
(display "\n")

;; ----------------------------------------------------------------------------
;; Helper Functions (For Question 2 and 3)
;; ----------------------------------------------------------------------------

;; Helper functions to manipulate pairs and lists

; pre-condition : x and y are valid values
; post-condition : Returns a pair (cons cell) with x as the car and y as the cdr.
(define (pair-cons x y) (cons x y))



(define (pair-car z) (if (pair? z) (car z) (error "pair-car: expected a pair, got " z)))
(define (pair-cdr z) (if (pair? z) (cdr z) (error "pair-cdr: expected a pair, got " z)))
(define (pair-cadr z) (pair-car (pair-cdr z)))
(define (pair-caddr z) (pair-car (pair-cdr (pair-cdr z))))
(define (pair-cadddr z) (pair-car (pair-cdr (pair-cdr (pair-cdr z)))))
(define (make-list . elements) (if (null? elements) '() (pair-cons (pair-car elements) (apply make-list (pair-cdr elements)))))
(define (is-pair? x) (pair? x))
(define (my-if condition then-clause else-clause) (if condition then-clause else-clause))
(define (my-cond . clauses)
  (if (null? clauses)
      #f
      (let ((clause (car clauses)))
        (if (eval (car clause))
            (eval (pair-cadr clause))
            (apply my-cond (cdr clauses))))))
(define (all? pred lst) (if (null? lst) #t (and (pred (car lst)) (all? pred (cdr lst)))))
(define (void) '())

(define build-pair
  (lambda (s1 s2)
    (pair-cons s1 (pair-cons s2 (quote ())))))

(define first pair-car)

(define second pair-cadr)

(define third pair-caddr)

(define is-atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;; Error implementation
(define (error msg . args)
  (define (display-args args)
    (if (not (null? args))
        (begin
          (display (car args))
          (display-args (cdr args)))))
  (display msg)
  (display-args args)
  (newline)
  (void))

;; ----------------------------------------------------------------------------
;; 2. Write and prove correct a syntax checker for TLS-scheme, as specified in HW 11.
;; ----------------------------------------------------------------------------

(define (is-atom? x) (and (not (pair? x)) (not (null? x))))

(define (simple-check expr)
  (cond
    ((number? expr) #t)
    ((boolean? expr) #t)
    ((symbol? expr) #t)
    ((is-pair? expr)
     (case (pair-car expr)
       ((quote) (and (is-pair? (pair-cdr expr)) (null? (pair-cddr expr))))
       ((lambda) (and (is-pair? (pair-cdr expr))
                      (is-pair? (pair-cadr expr))
                      (all? symbol? (pair-cadr expr))
                      (simple-check (pair-caddr expr))))
       ((cond) (all? (lambda (clause)
                        (and (is-pair? clause)
                             (simple-check (pair-car clause))
                             (simple-check (pair-cadr clause))))
                      (pair-cdr expr)))
       (else (all? simple-check (pair-cdr expr)))))
    (else #f)))

(define (extended-syntax-checker expr env)
  (cond
    ((number? expr) #t)
    ((boolean? expr) #t)
    ((symbol? expr) #t)
    ((is-pair? expr)
     (case (pair-car expr)
       ((quote) (and (is-pair? (pair-cdr expr)) (null? (pair-cddr expr))))
       ((lambda) (let ((params (pair-cadr expr))
                       (body (pair-caddr expr)))
                   (and (all? symbol? params)
                        (extended-syntax-checker body (append params env)))))
       ((cond) (all? (lambda (clause)
                        (and (is-pair? clause)
                             (extended-syntax-checker (pair-car clause) env)
                             (extended-syntax-checker (pair-cadr clause) env)))
                      (pair-cdr expr)))
       (else (let ((fn (pair-car expr))
                   (args (pair-cdr expr)))
               (and (extended-syntax-checker fn env)
                    (all? (lambda (arg) (extended-syntax-checker arg env)) args)
                    (let ((arity (get-arity fn env)))
                      (eq? (length args) arity)))))))
     (else #f)))

(define (bound? sym env) (member sym env))

(define (get-arity fn env)
  (cond
    ((eq? fn 'cons) 2)
    ((eq? fn 'car) 1)
    ((eq? fn 'cdr) 1)
    ((eq? fn 'null?) 1)
    ((eq? fn 'eq?) 2)
    ((eq? fn 'atom?) 1)
    ((eq? fn 'zero?) 1)
    ((eq? fn 'add1) 1)
    ((eq? fn 'mul) 2)
    ((eq? fn 'sub1) 1)
    ((eq? fn 'number?) 1)
    ((eq? fn 'square) 1)
    ((eq? fn '+) 2)
    ((eq? fn '-) 2)
    ((eq? fn '*) 2)
    ((eq? fn '/) 2)
    (else #f)))

;; ----------------------------------------------------------------------------
;; 3. Write and prove correct an interpreter for TLS extended by let*.
;; ----------------------------------------------------------------------------

(define *let 
  (lambda (expr env) 
    (let* 
    ((bindings (pair-cadr expr))
     (vars (map pair-car bindings))
     (vals (map pair-cadr bindings))
     (body (pair-caddr expr))
     (lambda-expr (pair-cons (list (quote lambda) vars body) vals)))
      (evaluate-expr lambda-expr env))))

(define *let*
  (lambda (expr env)
    (let*-eval (pair-cadr expr) env (pair-caddr expr))))

(define let*-eval
  (lambda (bindings env body)
    (if (null? bindings)
        (evaluate-expr body env)
        (let* ((binding (pair-car bindings))
               (var (pair-car binding))
               (val (evaluate-expr (pair-cadr binding) env)))
          (let*-eval (pair-cdr bindings) (extend-env (new-binding (list var) (list val)) env) body)))))

(define lookup-in-env
  (lambda (name env default-fn)
    (cond 
      ((null? env) (default-fn name))
      (else (lookup-in-binding name
                             (pair-car env)
                             (lambda (name)
                               (lookup-in-env name
                                                (pair-cdr env)
                                                default-fn)))))))

(define extend-env pair-cons)

(define lookup-in-binding
  (lambda (name binding default-fn)
    (lookup-in-binding-helper name
                          (binding-vars binding)
                          (binding-vals binding)
                          default-fn)))

(define lookup-in-binding-helper
  (lambda (name vars vals default-fn)
    (cond
      ((null? vars) (default-fn name))
      ((eq? (pair-car vars) name) (pair-car vals))
      (else (lookup-in-binding-helper name
                                  (pair-cdr vars)
                                  (pair-cdr vals)
                                  default-fn)))))

(define new-binding build-pair)

(define binding-vars
  (lambda (binding) (pair-car binding)))

(define binding-vals
  (lambda (binding) (pair-cadr binding)))

(define evaluate-expr
  (lambda (expr env)
    (interpret expr env)))

(define interpret
  (lambda (expr env)
    ((expression-to-action expr) expr env)))

(define expression-to-action
  (lambda (expr)
    (cond
      ((is-atom? expr) (atom-to-action expr))
      (else (list-to-action expr)))))

(define atom-to-action
  (lambda (expr)
    (cond
      ((number? expr) *const)
      ((eq? expr #t) *const)
      ((eq? expr #f) *const)
      ((eq? expr (quote cons)) *const)
      ((eq? expr (quote car)) *const)
      ((eq? expr (quote cdr)) *const)
      ((eq? expr (quote null?)) *const)
      ((eq? expr (quote eq?)) *const)
      ((eq? expr (quote atom?)) *const)
      ((eq? expr (quote zero?)) *const)
      ((eq? expr (quote add1)) *const)
      ((eq? expr (quote mul)) *const)
      ((eq? expr (quote sub1)) *const)
      ((eq? expr (quote number?)) *const)
      ((eq? expr (quote +)) *const)
      ((eq? expr (quote -)) *const)
      ((eq? expr (quote *)) *const)
      ((eq? expr (quote /)) *const)
      (else *identifier))))

(define list-to-action
  (lambda (expr)
    (cond
      ((is-atom? (pair-car expr))
       (cond 
         ((eq? (pair-car expr) (quote quote))
          *quote)
         ((eq? (pair-car expr) (quote lambda))
          *lambda)
         ((eq? (pair-car expr) (quote cond))
          *cond)
         ((eq? (pair-car expr) (quote let)) 
          *let)
         ((eq? (pair-car expr) (quote let*))
          *let*)
         (else *application)))
      (else *application))))

(define *const
  (lambda (expr env)
    (cond 
      ((number? expr) expr)
      ((eq? expr #t) #t)
      ((eq? expr #f) #f)
      (else (build-pair (quote primitive) expr)))))

(define *quote
  (lambda (expr env)
    (text-of expr)))

(define text-of pair-cadr)

(define *identifier
  (lambda (expr env)
    (lookup-in-env expr env initial-env)))

(define initial-env
  (lambda (name)
    (pair-car (quote ()))))

(define *lambda
  (lambda (expr env)
    (build-pair (quote non-primitive)
           (pair-cons env (pair-cdr expr)))))

(define env-of first)
(define formals-of second)
(define body-of third)
    
(define evaluate-condition
  (lambda (lines env)
    (cond 
      ((else? (question-of (pair-car lines)))
       (evaluate-expr (answer-of (pair-car lines)) env))
      ((evaluate-expr (question-of (pair-car lines)) env)
       (evaluate-expr (answer-of (pair-car lines)) env))
      (else (evaluate-condition (pair-cdr lines) env)))))

(define else?
  (lambda (x)
    (cond
      ((is-atom? x) (eq? x (quote else)))
      (else #f))))

(define question-of first)
(define answer-of second)

(define *cond
  (lambda (expr env)
    (evaluate-condition (cond-lines-of expr) env)))

(define cond-lines-of pair-cdr)

(define evaluate-args
  (lambda (args env)
    (cond
      ((null? args) (quote ()))
      (else
       (pair-cons (evaluate-expr (pair-car args) env)
                (evaluate-args (pair-cdr args) env))))))

(define *application
  (lambda (expr env)
    (apply-function
     (evaluate-expr (function-of expr) env)
     (evaluate-args (arguments-of expr) env))))

(define function-of pair-car)
(define arguments-of pair-cdr)

(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))

(define non-primitive?
  (lambda (l)
    (eq? (first l) (quote non-primitive))))

(define apply-function
  (lambda (fun vals)
    (cond
      ((primitive? fun)
       (apply-primitive
        (second fun) vals))
      ((non-primitive? fun)
       (apply-closure
        (second fun) vals)))))

(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? name (quote cons))
       (pair-cons (first vals) (second vals)))
      ((eq? name (quote car))
       (pair-car (first vals)))
      ((eq? name (quote cdr))
       (pair-cdr (first vals)))
      ((eq? name (quote null?))
       (null? (first vals)))
      ((eq? name (quote eq?))
       (eq? (first vals) (second vals)))
      ((eq? name (quote atom?))
       (is-atom? (first vals)))
      ((eq? name (quote zero?))
       (zero? (first vals)))
      ((eq? name (quote add1))
       (if (number? (first vals))
           ((lambda (x) (+ x 1)) (first vals))
           (error "add1: expected a number, got " (first vals))))
      ((eq? name (quote mul))
       (if (and (number? (first vals)) (number? (second vals)))
           (* (first vals) (second vals))
           (error "mul: expected numbers, got " (first vals) " and " (second vals))))
      ((eq? name (quote sub1))
       (if (number? (first vals))
           ((lambda (x) (- x 1)) (first vals))
           (error "sub1: expected a number, got " (first vals))))
      ((eq? name (quote number?))
       (number? (first vals)))
      ((eq? name (quote +))
       (if (and (number? (first vals)) (number? (second vals)))
           (+ (first vals) (second vals))
           (error "+: expected numbers, got " (first vals) " and " (second vals))))
      ((eq? name (quote -))
       (if (and (number? (first vals)) (number? (second vals)))
           (- (first vals) (second vals))
           (error "-: expected numbers, got " (first vals) " and " (second vals))))
      ((eq? name (quote *))
       (if (and (number? (first vals)) (number? (second vals)))
           (* (first vals) (second vals))
           (error "*: expected numbers, got " (first vals) " and " (second vals))))
      ((eq? name (quote /))
       (if (and (number? (first vals)) (number? (second vals)))
           (/ (first vals) (second vals))
           (error "/: expected numbers, got " (first vals) " and " (second vals)))))))

(define apply-closure
  (lambda (closure vals)
    (evaluate-expr (body-of closure)
             (extend-env
              (new-binding
               (formals-of closure)
               vals)
              (env-of closure)))))

;; Define evaluate function
(define (evaluate expr)
  (evaluate-expr expr (quote ())))

;; ----------------------------------------------------------------------------
;; Test Cases
;; ----------------------------------------------------------------------------

;; Testing functions one by one
(define (test-value)
  (let ((input '(+ 1 2))
        (expected-output 3))
    (let ((actual-output (evaluate input)))
      (if (equal? actual-output expected-output)
          (display "Test test-value passed\n")
          (display (string-append "Test test-value failed: expected " (write-to-string expected-output) " but got " (write-to-string actual-output) "\n"))))))

(define (test-lambda)
  (let ((input '((lambda (x) (add1 x)) 5))
        (expected-output 6))
    (let ((actual-output (evaluate input)))
      (if (equal? actual-output expected-output)
          (display "Test test-lambda passed\n")
          (display (string-append "Test test-lambda failed: expected " (write-to-string expected-output) " but got " (write-to-string actual-output) "\n"))))))

(define (test-nested-lambda)
  (let ((input '((lambda (x) ((lambda (y) (mul x y)) 2)) 3))
        (expected-output 6))
    (let ((actual-output (evaluate input)))
      (if (equal? actual-output expected-output)
          (display "Test test-nested-lambda passed\n")
          (display (string-append "Test test-nested-lambda failed: expected " (write-to-string expected-output) " but got " (write-to-string actual-output) "\n"))))))

(define (test-let)
  (let ((input '(let ((x 5) (y 10)) (mul x y)))
        (expected-output 50))
    (let ((actual-output (evaluate input)))
      (if (equal? actual-output expected-output)
          (display "Test test-let passed\n")
          (display (string-append "Test test-let failed: expected " (write-to-string expected-output) " but got " (write-to-string actual-output) "\n"))))))

(define (test-let*)
  (let ((input '(let* ((x 5) (y (add1 x))) (mul x y)))
        (expected-output 30))
    (let ((actual-output (evaluate input)))
      (if (equal? actual-output expected-output)
          (display "Test test-let* passed\n")
          (display (string-append "Test test-let* failed: expected " (write-to-string expected-output) " but got " (write-to-string actual-output) "\n"))))))

(define (test-cond)
  (let ((input '(cond ((zero? 0) 1) ((zero? 1) 2) (else 3)))
        (expected-output 1))
    (let ((actual-output (evaluate input)))
      (if (equal? actual-output expected-output)
          (display "Test test-cond passed\n")
          (display (string-append "Test test-cond failed: expected " (write-to-string expected-output) " but got " (write-to-string actual-output) "\n"))))))

(define (test-simple-check)
  (let ((input '(lambda (x) (add1 x)))
        (expected-output #t))
    (let ((actual-output (simple-check input)))
      (if (equal? actual-output expected-output)
          (display "Test test-simple-check passed\n")
          (display (string-append "Test test-simple-check failed: expected " (write-to-string expected-output) " but got " (write-to-string actual-output) "\n"))))))

(define (test-extended-syntax-checker)
  (let ((input '(lambda (x) (add1 x)))
        (env '())
        (expected-output #t))
    (let ((actual-output (extended-syntax-checker input env)))
      (if (equal? actual-output expected-output)
          (display "Test test-extended-syntax-checker passed\n")
          (display (string-append "Test test-extended-syntax-checker failed: expected " (write-to-string expected-output) " but got " (write-to-string actual-output) "\n"))))))

(display (evaluate '(cond ((zero? 5) (add1 10)) (else (sub1 10)))))
(newline)
(display (evaluate '(let ((a 5)) (let ((a 10)) (add1 a)))))
(newline)
(display (evaluate '(let ((a 5)) (let ((a 10)) (add1 b)))))
(newline)

;; Run tests one by one
(test-value)
(test-lambda)
(test-nested-lambda)
(test-let)
(test-let*)
(test-cond)
(test-simple-check)
(test-extended-syntax-checker)