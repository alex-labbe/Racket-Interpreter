#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301, FALL 2022
;;
;; Lab #1
;;
;; Alexandre Labbe
;; W01561156
;;
;; Implement Lookup and Evaluate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide lookup
	 evaluate
	 special-form?
	 evaluate-special-form)


;; Closure data type
(define closure
  (lambda (vars body env)
    (mcons 'closure (mcons env (mcons vars body)))))
(define closure?
  (lambda (clos) (and (mpair? clos) (eq? (mcar clos) 'closure))))
(define closure-env
  (lambda (clos) (mcar (mcdr clos))))
(define closure-vars
  (lambda (clos) (mcar (mcdr (mcdr clos)))))
(define closure-body
  (lambda (clos) (mcdr (mcdr (mcdr clos)))))
(define set-closure-env!
  (lambda (clos new-env) (set-mcar! (mcdr clos) new-env)))


(define lookup
  (lambda (symbol environment)
    (cond
      [
       	;return an error if the symbol is not a symbol
      	(not (symbol? symbol)) (exn:fail?) 
      ]
      [
        ;return an error if symbol is not in the environment
       	(empty? environment) (exn:fail?) 
      ]
      [
	;symbol is equal to the first value of the first sublist
        (eq? symbol (car (car environment))) (car(cdr (car environment)))
      ]
      [
        ;if none of these cases go through, go to next sublist
        (lookup symbol (cdr environment))
      ]
    )  
  )
)



(define special-form?
  (lambda (expression)
    (cond
      [
        (list? expression)
	(cond
	  [ ;car expression = 'if or 'cond or 'let
	    (equal? 'if (car expression))
            #t
	  ]
	  [ ;cond
	    (equal? 'cond (car expression))
            #t
	  ]
	  [ ;let
	    (equal? 'let (car expression))
            #t
	  ]
          [ ;lambda
	    (equal? 'lambda (car expression))
            #t
	  ]
          [ ;letrec
            (equal? 'letrec (car expression))
          ]          
	  [ ;else, return false
	    else
            #f
	  ]
	)
      ]
      [ ;not list, return false
        else
        #f
      ]
    )  
  )
)

;let-evaluate returns what is to be added to the environment (temporarily) to evaluate the expression at the end of our let statement.
(define let-evaluate
  (lambda (let-expression environment)
    (map (lambda (item) (list (car item) (evaluate (car (cdr item)) environment))) let-expression)
  )
)


(define evaluate-letrec
  (lambda (expression environment)
    ;run mini-env-closures (updates closures to have new-env). new env is mini-env+old-env, then evaluate the body of the letrec expression with new-env as the environment
    (let*([old-env environment]
	  [mini-env (let-evaluate(list-ref expression 0) environment)]
	  [new-env (append mini-env old-env)])
          (mini-env-closures mini-env new-env) (evaluate (list-ref expression 1) new-env)
    )	    
  )   
)


(define mini-env-closures
  (lambda (mini-env new-env)
    (cond
      [ ;base case for recursive call, empty list, do nothing and end this function call.
        (empty? mini-env)
      ]
      [ ;if value of first variable-value pair in mini-env is a closure, update closure environment to be new-env then recurse call with rest of mini-env  
        (closure? (car(cdr(car mini-env))))
        (set-closure-env! (car(cdr(car mini-env))) new-env)
	(mini-env-closures (cdr mini-env) new-env)
      ]
      [ ;if value of first varible-value pair in mini-env is NOT a closure, recurse call with rest of mini-env
        else
	(mini-env-closures (cdr mini-env) new-env)
      ]
    )
  )
)

(define evaluate-special-form
  (lambda (expression environment)
    (define cond-rec 
      (lambda cond-expression
	(cond
	  [ ;base case if theres no else statement and none of the conds eval to true, just incase bad input 
	    (empty? cond-expression)
            (exn:fail?)
	  ]
	  [ ;if statement (such as (= 1 1)) evaluates to true, return the other statement in the list that (= 1 1) is in
	    (evaluate (car (car (car cond-expression))) environment)
            (evaluate (list-ref (car (car cond-expression)) 1 ) environment) 
	  ]
	  [ ;if statement evaluates as false, run cond-rec on the(* x x) rest of the list
	    else
            (cond-rec (cdr (car cond-expression)))
	  ]
	)	
      )
    )
    (cond
      [
        (equal? 'if (car expression))
	(cond
	  [ ;if the statement evaluates as false, return the evaluation of the last statement
	    (not (evaluate (car (cdr expression)) environment))
            (evaluate (list-ref expression 3) environment) 
	  ]
	  [ ;if anything else than false, return the evaluation of the 2nd to last statement
	    else
            (evaluate (list-ref expression 2) environment)
	  ]
	)
      ]
      [ ;if cond, run cond-rec with list of all the lists of statements
        (equal? 'cond (car expression))
	(cond-rec (cdr expression))
      ]
      [ ;if let, run evaluate with expression being the final expression of the let form, and environment being our original environment with the result of let-rec appended to it. 
        (equal? 'let (car expression))
	(evaluate (list-ref expression 2) (append (let-evaluate (list-ref expression 1) environment) environment)) 
      ]
      [ ;if lambda, create a closure from the arguments of the lambda, the body, and the current environment
        (equal? 'lambda (car expression))
        (closure (car (cdr expression)) (list-ref expression 2) environment)
      ]
      [ ;if letrec, run evaluate-letrec with the rest of the expression
        (equal? 'letrec (car expression))
	(evaluate-letrec (cdr expression) environment)
      ]
    )

  )  
)

(define evaluate-list
  (lambda (expression environment)
    ;apply closure evaluates the body of the closure with an edited environment
    ;the edited environment is the closure-variables mapped to the values passed into apply closure, appended to the saved environment
    (define apply-closure
      (lambda (closure values)
        (evaluate (closure-body closure) (append (map list (closure-vars closure) values) (closure-env closure))
	)
      )
    )
    ;apply function either applies function to expressions, or evaluates a closure.
    (define apply-function
      (lambda (function arguments)
        (cond
          [ ;if a procedure is passed in, apply the function to all the expressions
            (procedure? function)
            (apply function arguments)
          ]

          [ ;if a closure is passed in, run apply-closure with the closure as the function and the values as the arguments
            (closure? function)
            (apply-closure function arguments)
          ]
          [ ;unknown function type returns a failure
            else
            (exn:fail)
          ]
        )
      )
    )
    ;evaluate list runs apply function with the first value of the mapped expression and the rest of the mapped expression
    (let(
         [mapped (map (lambda (item) (evaluate item environment)) expression)])
         (apply-function (car mapped) (cdr mapped))
    )
  )
)

(define evaluate
  (lambda (expression environment)
   (cond
      [ ;if expression is a number, return it
	(number? expression)	
	expression
      ]
      [ ;use lookup to return what the expression stands for
      	(symbol? expression)
	(lookup expression environment)
      ]
      [ ;if expression is a special form, run evaluate-special-form on it
      	(special-form? expression)
	(evaluate-special-form expression environment)
      ]
      [ ;if expression is a list, run new evaluate-list function
        (list? expression)
	(evaluate-list expression environment)
      ]
      [ ;fail if anything else is passed in
        else (exn:fail?) 
      ]  
    )
  )
)






