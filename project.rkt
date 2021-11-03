;; PL Project - Fall 2018
;; NUMEX interpreter

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for NUMEX programs

;; CHANGE add the missing ones

(struct var (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct num (int) #:transparent)  ;; a constant number, e.g., (num 17)
(struct bool (boolean) #:transparent)
(struct plus (e1 e2) #:transparent)  ;; add two expressions
(struct minus (e1 e2) #:transparent)
(struct mult (e1 e2) #:transparent)
(struct div (e1 e2) #:transparent)
(struct neg (e) #:transparent)
(struct andalso (e1 e2) #:transparent)
(struct orelse (e1 e2) #:transparent)
(struct cnd (e1 e2 e3) #:transparent)
(struct iseq (e1 e2) #:transparent)
(struct ifnzero (e1 e2 e3) #:transparent)
(struct ifleq (e1 e2 e3 e4) #:transparent)
(struct lam (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct apply (funexp actual) #:transparent) ;; function application
(struct with (s e1 e2) #:transparent)
(struct apair (e1 e2) #:transparent)
(struct 1st (e) #:transparent)
(struct 2nd (e) #:transparent)
(struct munit () #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e) #:transparent) ;; if e1 is unit then true else false
(struct closure (env f) #:transparent) ;; a closure is not in "source" programs; it is what functions evaluate to 
(struct letrec (s1 e1 s2 e2 e3) #:transparent) ;; a letrec expression for recursive definitions
(struct key (s e) #:transparent) ;; key holds corresponding value of s which is e
(struct record (k r) #:transparent) ;; record holds several keys
(struct value (s r) #:transparent) ;; value returns corresponding value of s in r

;; Problem 1

(define (racketlist->numexlist xs)  (if (null? xs) (munit) (apair (car xs)(racketlist->numexlist (cdr xs))))) ;;(if test-expr then-expr else-expr)
(define (numexlist->racketlist xs) (if (munit? xs) '() (cons (apair-e1 xs)(numexlist->racketlist (apair-e2 xs)))))


;; Problem 2

;; lookup a variable in an environment
;; Complete this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]
  )
 )

;; Complete more cases for other kinds of NUMEX expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  
  (cond [(num? e)
         (if (integer? (num-int e)) e (error "num-int is not an integer"))
         ]
        

        [(bool? e)
         (if (boolean? (bool-boolean e)) e (error "bool-boolean is not a boolean"))
         ]


        [(munit? e)
         (munit)]
        

        [(lam? e)
         (closure env e)
         ]


        [(closure? e) e]


        [(var? e) 
         (envlookup env (var-string e))]

        
        [(plus? e) 
         (let ([v1 (eval-under-env (plus-e1 e) env)]
               [v2 (eval-under-env (plus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (+ (num-int v1) 
                       (num-int v2)))
               (error "NUMEX addition applied to non-number")))]

        
        [(minus? e) 
         (let ([v1 (eval-under-env (minus-e1 e) env)]
               [v2 (eval-under-env (minus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (- (num-int v1) 
                       (num-int v2)))
               (error "NUMEX subtraction applied to non-number")))]

        
        [(mult? e) 
         (let ([v1 (eval-under-env (mult-e1 e) env)]
               [v2 (eval-under-env (mult-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (* (num-int v1) 
                       (num-int v2)))
               (error "NUMEX multiplication applied to non-number")))]

        
        [(div? e) 
         (let ([v1 (eval-under-env (div-e1 e) env)]
               [v2 (eval-under-env (div-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (if (equal? (num-int v2) 0)
                   (error "Division by zero is undefined")
               (num (quotient (num-int v1) 
                       (num-int v2))))
               (error "NUMEX division applied to non-number")))]

        
        [(andalso? e)
            (let ([v1 (eval-under-env (andalso-e1 e) env )])
              (if (bool? v1) (if (equal? (bool-boolean v1) #f) (bool #f)
                                  (let ([v2 (eval-under-env (andalso-e2 e) env)])
                                          (if (bool? v2) v2 (error "andalso-e2 is not bool"))))
                  (error "andalso-e1 is not bool")))]

        
        [(orelse? e)
            (let ([v1 (eval-under-env (orelse-e1 e) env )])
              (if (bool? v1) (if (equal? (bool-boolean v1) #t) (bool #t)
                                  (let ([v2 (eval-under-env (orelse-e2 e) env)])
                                          (if (bool? v2) v2 (error "orelse-e2 is not bool"))))
                  (error "orelse-e1 is not bool")))]
        

        [(neg? e)
            (let ([v (eval-under-env (neg-e e) env )])
              (cond [(num? v) (num (-(num-int v)))]
                    [(bool? v) (bool (not (bool-boolean v)))]
                    [#t (error "neg-e is not num or bool")]
              ))]


        [(cnd? e)
             (let ([v1 (eval-under-env (cnd-e1 e) env)])
               (if (bool? v1)
                   (if (equal? (bool-boolean v1) #t)
                       (eval-under-env (cnd-e2 e) env)
                       (eval-under-env (cnd-e3 e) env))
                   (error "cnd-e1 is not bool"))
               )]


        [(iseq? e)
         (let ([v1 (eval-under-env (iseq-e1 e) env)]
               [v2 (eval-under-env (iseq-e2 e) env)])
           (cond [(and (num? v1) (num? v2)) (if (equal? (num-int v1) (num-int v2)) (bool #t) (bool #f))]
                 [(and (bool? v1) (bool? v2)) (if (equal? (bool-boolean v1) (bool-boolean v2)) (bool #t) (bool #f))]
                 [#t (bool #f)])
           )
         ]


        [(ifnzero? e)
           (let ([v1 (eval-under-env (ifnzero-e1 e) env)])
             (cond [(num? v1) (if (equal? (num-int v1) 0) (eval-under-env (ifnzero-e3 e) env) (eval-under-env (ifnzero-e2 e) env))]
                   [#t (error "ifnzero-e1 is not num")])
           )]
        
        
        [(ifleq? e)
            (let ([v1 (eval-under-env (ifleq-e1 e) env)]
                  [v2 (eval-under-env (ifleq-e2 e) env)])
              (cond [(and (num? v1) (num? v2)) (if (> (num-int v1) (num-int v2)) (eval-under-env (ifleq-e4 e) env) (eval-under-env (ifleq-e3 e) env))]
                    [#t (error "ifleq-e1 or ifleq-e2 is not num")])
              )
         ]

        
        [(with? e)
            (let* ([v1 (eval-under-env (with-e1 e) env)]
                   [env2 (cons (cons (with-s e) v1) env)])
              (eval-under-env (with-e2 e) env2)
              )
         ]


        [(apply? e)
         (let ([v1 (if (lam? (eval-under-env (apply-funexp e) env))
                       (closure env (eval-under-env (apply-funexp e) env))
                       (eval-under-env (apply-funexp e) env))])
           (cond [(closure? v1) (let* ([f1 (closure-f v1)]
                                       [env2 (closure-env v1)]
                                       [env3 (cons (cons (lam-formal f1) (eval-under-env(apply-actual e) env)) env2)]
                                       [env4 (if (equal? (lam-nameopt f1) null) env3 (cons (cons (lam-nameopt f1) v1) env3))])
                                  (eval-under-env (lam-body f1) env4)
                                  )]
                 [#t (error "apply-funexp is not a closure or lam")])
           )
         ]


        [(apair? e)
            (let ([v1 (eval-under-env (apair-e1 e) env)]
                  [v2 (eval-under-env (apair-e2 e) env)])
              (apair v1 v2)
            )]


        [(1st? e)
            (let ([v (eval-under-env (1st-e e) env)])
              (if (apair? v) (apair-e1 v) (error "1st-e is not an apair"))
              )
         ]


        [(2nd? e)
            (let ([v (eval-under-env (2nd-e e) env)])
              (if (apair? v) (apair-e2 v) (error "2nd-e is not an apair"))
              )
         ]


        [(ismunit? e)
            (let ([v (eval-under-env (ismunit-e e) env)])
              (if (munit? v) (bool #t) (bool #f))
              )
         ]


        [(letrec? e)
         (let* ([env2 (cons (cons (letrec-s1 e) (letrec-e1 e)) env)]
               [env3 (cons (cons (letrec-s2 e) (letrec-e2 e)) env2)])
           (eval-under-env (letrec-e3 e) env3)
              )
         ]


        [(key? e)
            (cond [(string? (key-s e))
                     (let ([v (eval-under-env (key-e e) env)])
                       (key (key-s e) v))]
                  [#t (error "key-s is not a Racket string")])
         ]


        [(record? e)
            (let ([k1 (eval-under-env (record-k e) env)]
                  [r1 (eval-under-env (record-r e) env)])
              (cond [(and (key? k1) (ismunit r1)) (record k1 r1)]
                    [(and (key? k1) (record? r1)) (record k1 r1)]
                    [#t (error "record-k is not a key or record-r is not a record/null")])
              )
         ]


        [(value? e)
           (cond [(string? (value-s e)) (let ([r1 (eval-under-env (value-r e) env)])
                                          (if (record? r1) (let ([k1 (eval-under-env (record-k r1) env)])
                                                             (if (string=? (value-s e) (key-s k1))
                                                                  (eval-under-env (key-e k1) env)
                                                                  (if (munit? (record-r r1)) (munit) (eval-under-env (value (value-s e) (record-r r1)) env))))
                                              (error "value-r is not a record")))]
                 [#t (error "value-s is not a string")])
        ]

        
        
        [#t (error (format "bad NUMEX expression: ~v" e))]))



;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))


;; Problem 3

;; (define (ifmunit e1 e2 e3) (if (equal? e1 (munit)) e2 e3))
(define (ifmunit e1 e2 e3) (cnd (ismunit e1) e2 e3))


(define (with* bs e2)
  (if (null? bs)
      e2
      (with (car (car bs)) (cdr (car bs)) (with* (cdr bs) e2)))
  )


(define (ifneq e1 e2 e3 e4) (cnd (iseq e1 e2) e4 e3))


;; Problem 4

(define numex-filter (lam "applyToElement" "f" 
                        (lam "applyToList"  "numexList" 
                           (cnd   (ismunit (var "numexList")) (munit)
                                        (ifnzero (apply (var "f") (1st (var "numexList")))
                                                 (apair (apply (var "f") (1st (var "numexList"))) (apply (var "applyToList") (2nd (var "numexList"))))
                                                 (apply (var "applyToList") (2nd (var "numexList"))))))))


(define numex-all-gt
  (with "filter" numex-filter ;;notice filter is now in NUMEX scope
        (lam "N-A-G" "i"
             (apply numex-filter  (lam "f1" "listElement" (ifleq (var "listElement") (var "i") (num 0) (var "listElement")))))))


;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))

(define fibonachi (lam "fibo" "n"
                       (cnd (iseq (var "n") (num 1))
                            (num 1)
                            (cnd (iseq (var "n") (num 2))
                                 (num 1)
                                 (plus (apply (var "fibo") (minus (var "n") (num 1)))
                                       (apply (var "fibo") (minus (var "n") (num 2)))))
                             
                             
                            )))

(define factoriel (lam "factor" "n"
                       (cnd (iseq (var "n") (num 1)) (num 1) (mult (apply (var "factor") (minus (var "n") (num 1))) (var "n")))
                       ))

(eval-exp (apply fibonachi (num 9))) 
(eval-exp (apply factoriel (num 5))) 