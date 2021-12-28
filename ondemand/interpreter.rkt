#lang plai-typed

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TYPES DEFINITION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Definition os the primitive expressions. These expressions can be directly interpreted.
; Also, note that ExprC can have multiple Expr by using consC for example
(define-type ExprC
  [numC    (n : number)]
  [idC     (s : symbol)]
  [plusC   (l : ExprC) (r : ExprC)]
  [multC   (l : ExprC) (r : ExprC)]
  [lamC    (arg : symbol) (body : ExprC)]
  [appC    (fun : ExprC) (arg : ExprC)]
  [ifC     (cond : ExprC) (y : ExprC) (n : ExprC)]
  [consC   (car : ExprC) (cdr : ExprC)]; Creates cell with a pair
  [carC    (pair : ExprC)]; Gets 1st element of a pair
  [cdrC    (pair : ExprC)]; Gets 2nd element of a pair
  [letrecC (var : symbol) (expression : ExprC) (body : ExprC)]
  [quoteC  (sym : symbol)]
  [readloopC ]
  [eqC (a : ExprC) (b : ExprC)]
  )

; Definition of the expressions with or without sugar expressions.
; Note that both sugar and sugarless expressions have a specific type defined.
(define-type ExprS
  [numS    (n : number)]
  [idS     (s : symbol)]
  [lamS    (arg : symbol) (body : ExprS)]
  [appS    (fun : ExprS) (arg : ExprS)]
  [plusS   (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [uminusS (e : ExprS)]
  [multS   (l : ExprS) (r : ExprS)]
  [ifS     (c : ExprS) (y : ExprS) (n : ExprS)]
  [consS   (car : ExprS) (cdr : ExprS)]
  [carS    (pair : ExprS)]
  [cdrS    (pair : ExprS)]
  [letS    (var : symbol) (exp : ExprS) (body : ExprS)]
  [let*S    (var1 : symbol) (exp1 : ExprS) (var2 : symbol) (exp2 : ExprS) (body : ExprS)]
  [letrecS (var : symbol)  (exp : ExprS)  (body : ExprS)]
  [quoteS  (sym : symbol)]
  [readloopS ]
  [eqS (a : ExprS) (b : ExprS)]
  )

; Definition for the partially and completely computed expressions
(define-type Value
  [numV  (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)]
  [consV (car : Promise) (cdr : Promise)]
  [symV (sym : symbol)]
  [suspV (exp : ExprC) (env : Env)]
)

(define-type Promise
  [aPromise (valueBox : (boxof  Value))]
)

; Definition of the Env type
; Note that Env remains the same, we only change the Binding
; Also, Bindings associate symbol with Values
(define-type Binding
        [bind (name : symbol) (val : (boxof Promise))])
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

; Storage's operations are similar to Env's
;   bind <-> cell
;   mt-env <-> mt-store
;   extend-env <-> override-store

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; STEPS OF THE PROCESS
; 1. Parse: receive a quoted expression (s-expression) and parsers into an expression possibly with syntax sugar (ExprS)
;    Note that the quoted expression can be an array, a number, a symbol, etc
; 2. Desugar: expand syntax sugar (ExprS) into primitive expressions (ExprC)
; 3. Interp: execute the primitive expressions (ExprC) and return the value it got (Value)

; 1. Parse: receive a quoted expression (s-expression) and parsers into an expression possibly with syntax sugar (ExprS)
; Note that the quoted expression can be an array, a number, a symbol, etc
(define (parse [s : s-expression]) : ExprS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-symbol? s) (idS (s-exp->symbol s))] ; pode ser um símbolo livre nas definições de função
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(-) (bminusS (parse (second sl)) (parse (third sl)))]
         [(~) (uminusS (parse (second sl)))]
         [(lambda) (lamS (s-exp->symbol (second sl)) (parse (third sl)))] ; definição
         [(call) (appS (parse (second sl)) (parse (third sl)))]
         [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [(cons) (consS (parse (second sl)) (parse (third sl)))]
         [(car) (carS (parse (second sl)))]
         [(cdr) (cdrS (parse (second sl)))]
         [(quote) (quoteS (s-exp->symbol (second sl)))]
         [(let) (letS (s-exp->symbol (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [(let*) (let*S (s-exp->symbol (second sl))
                        (parse (third sl))
                        (s-exp->symbol (fourth sl))
                        (parse (fourth (rest sl)))
                        (parse (fourth (rest (rest sl)))))]
         [(let) (letS (s-exp->symbol (second sl))
                      (parse (third sl))
                      (parse (fourth sl)))]
         [(letrec) (letrecS (s-exp->symbol (second sl))
                            (parse (third sl))
                            (parse (fourth sl)))]
         [(read-loop)(readloopS)]
         [(equal?) (eqS (parse (second sl)) (parse (third sl)))]
         
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

; 2. Desugar: expand syntax sugar (ExprS) into primitive expressions (ExprC)
(define (desugar [as : ExprS]) : ExprC
  (type-case ExprS as
    [numS    (n)        (numC n)]
    [idS     (s)        (idC s)]
    [lamS    (a b)      (lamC a (desugar b))]
    [appS    (fun arg)  (appC (desugar fun) (desugar arg))]
    [plusS   (l r)      (plusC (desugar l) (desugar r))]
    [multS   (l r)      (multC (desugar l) (desugar r))]
    [bminusS (l r)      (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS (e)        (multC (numC -1) (desugar e))]
    [ifS     (c y n)    (ifC (desugar c) (desugar y) (desugar n))]
    [consS   (b1 b2)    (consC (desugar b1) (desugar b2))]
    [carS    (c)        (carC (desugar c))]
    [cdrS    (c)        (cdrC (desugar c))]
    [letS    (v e b)    (appC (lamC v (desugar b)) (desugar e))]
    [let*S    (v1 e1 v2 e2 b)    (appC (lamC v1 (appC (lamC v2 (desugar b) )
                                                      (desugar e2)))
                                       (desugar e1))]
    [letrecS    (v e  b)  (letrecC v (desugar e) (desugar b))]
    [quoteS  (sym) (quoteC sym)]
    [readloopS  () (readloopC)]
    [eqS (a b) (eqC (desugar a) (desugar b))]
    ))


; 3. Interp: execute the primitive expressions (ExprC) and return the value it got (Value)
(define (interp [a : ExprC] [env : Env] ) : Value
  (type-case ExprC a
    [numC (n) (numV n) ]
    [idC (n)  (query-promise (unbox (lookup n env)))]; we need to unbox the value in the environment before using it
    [lamC (a b) (closV a b env) ]

    ; application of function
    [appC (f a) (let ((closure (interp f env))
                (argvalue (aPromise (box (suspV a env)))))
            (type-case Value closure
              [closV (parameter body env-closure)
                     (interp body (extend-env (bind parameter (box argvalue)) env-closure))]
              [else (error 'interp "operation app aplied to non-closure")]
              ))]

    ;plusC
    [plusC (l r)
             (let ((left (interp l env))
                   (right (interp r env)))
               (num+ left right))]
    ;multC
    [multC (l r)
           (let ( (left (interp l env))
                  (right (interp r env)))
             (if (numV? left)
                 (if (numV? right)
                     (num* left right)
                     (error 'interp "second argument of multiplication not a number value"))
                 (error 'interp "first argument of multiplication not a number value"))
                 )]
    ; ifC serializes
    [ifC (c s n) (type-case Value (interp c env)
                   [numV (value)
                        (if (zero? value)
                            (interp n env )
                            (interp s env ))]
                   [else (error 'interp "condition not a number")]
                   )]

    ; Working with lists
    [consC (b1 b2) (consV (aPromise (box (suspV b1 env))) (aPromise (box (suspV b2 env))))]
    [carC (c) (type-case Value (interp c env)
                [consV (car cdr) (query-promise car)]
                [else (error 'interp "car applied to non-cell")]
                )]
    [cdrC (c) (type-case Value (interp c env)
                [consV (car cdr) (query-promise cdr)]
                [else (error 'interp "cdr applied to non-cell")]
                )]

    [letrecC (v e b) (let* ((new-binding (bind v (box (aPromise (box (symV 'nothing))))))
                            (new-env (extend-env new-binding env))
                            (closure (interp e new-env)))
                       (begin (set-box! (lookup v (closV-env closure))
                                        (aPromise (box closure)))
                              (interp b new-env)))]

    [quoteC  (s) (symV s)]
    [readloopC () (letrec ( (read-till-end (lambda ()
                                              (let ( (input (read)))
                                                (if (and (s-exp-symbol? input )
                                                         (eq? (s-exp->symbol input) '@END))
                                                    (begin (display 'FINISHED-READLOOP)
                                                           (symV  'END_OF_loop))
                                                    (begin (display (interp (desugar (parse input)) env))
                                                           (read-till-end)))))))
                     (read-till-end))]
    [eqC (a b) (if (equal? (interp a env) (interp b env)) (numV 1) (numV 0))]

   ))


; Facilitator
(define (interpS [s : s-expression]) (interp (desugar (parse s)) mt-env))
(define interpAll (lambda () (interpS '(read-loop))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; AUXILIAR FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (query-promise [prom : Promise]) : Value
  (let ((theBox (aPromise-valueBox prom)))
    (type-case Value (unbox theBox)
      [suspV (body susp-env)
            (let* ((finalValue (interp body susp-env)))
                  (begin
                    (set-box! theBox finalValue)
                    finalValue
                  )
            )
      ]
      [else (unbox theBox)]
    )
  )
)

; lookup changes its return type
(define (lookup [varName : symbol] [env : Env]) : (boxof Promise); lookup returns the box, we need this to change the value later
       (cond
            [(empty? env) (error 'lookup (string-append (symbol->string varName) " não foi encontrado"))] ; livre (não definida)
            [else (cond
                    [(symbol=? varName (bind-name (first env)))   ; achou!
                     (bind-val (first env))]
                    [else (lookup varName (rest env))])]))        ; vê no resto



; Primitive operators
(define (num+ [l : Value] [r : Value]) : Value
    (cond
        [(and (numV? l) (numV? r))
             (numV (+ (numV-n l) (numV-n r)))]
        [else
             (error 'num+ "Um dos argumentos não é número")]))

(define (num* [l : Value] [r : Value]) : Value
    (cond
        [(and (numV? l) (numV? r))
             (numV (* (numV-n l) (numV-n r)))]
        [else
             (error 'num* "Um dos argumentos não é número")]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TESTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (interp (carC (consC (numC 10) (numC 20))) mt-env)
      (numV 10))
(appC (lamC 'x (plusC (numC 4) (numC 0)))
      (plusC (numC 4) (numC 0)))
(test (interpS '(letrec fat (lambda x (if x (* x (call fat (- x 1))) 1 )) (call fat 5)))
      (numV 120))
(test (interpS '(letrec inf-fat (lambda x (if x (* x (call inf-fat (+ x 1))) 1 )) (car (cons 3 (call inf-fat 6)))))
      (numV 3))