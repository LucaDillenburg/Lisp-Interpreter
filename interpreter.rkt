#lang plai-typed

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; AUTHORS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; - Luca Dillenburg (NUSP: 11796580)
; - Arthur (NUSP: 10297647)

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
  [letrecC (varsym : symbol) (vararg : symbol) (varbody : ExprC) (exp : ExprC)]
  [quoteC (s : symbol)]
  [loopC]
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
  [letS    (varsym : symbol) (varexp : ExprS) (exp : ExprS)]
  [let*S   (var1sym : symbol) (var1exp : ExprS) (var2sym : symbol) (var2exp : ExprS) (exp : ExprS)]
  [letrecS (varsym : symbol) (varexp : ExprS) (exp : ExprS)]
  [quoteS (q : symbol)]
  [loopS]
  )

; Definition for the partially and completely computed expressions
(define-type Value
  [numV  (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)]
  [consV (car : Value) (cdr : Value)]
  [symV (s : symbol)]
  )

; Definition of the Env type
; Note that Env remains the same, we only change the Binding
; Also, Bindings associate symbol with Values
(define-type Binding
        [bind (name : symbol) (val : (boxof Value))])
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
         [(lambda) (lamS  (s-exp->symbol (second sl)) (parse (third sl)))] ; definição
         [(call) (appS (parse (second sl)) (parse (third sl)))]
         [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [(cons) (consS (parse (second sl)) (parse (third sl)))]
         [(car) (carS (parse (second sl)))]
         [(cdr) (cdrS (parse (second sl)))]
         [(let) (letS (s-exp->symbol (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [(let*) (
                   let*S
                   (s-exp->symbol (second sl))
                   (parse (third sl))
                   (s-exp->symbol (fourth sl))
                   (parse (list-ref sl 4))
                   (parse (list-ref sl 5))
                 )]
         [(letrec) (
                   letrecS
                   (s-exp->symbol (second sl))
                   (parse (third sl))
                   (parse (fourth sl))
                  )]
         [(quote) (quoteS (s-exp->symbol (second sl)))]
         [(read-loop) (loopS)]
         [else (error 'parse "invalid list input")] ))]
    [else (error 'parse "invalid list input")] )) 

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
    [letS    (varsym varexp exp)  (appC (lamC varsym (desugar exp)) (desugar varexp))]
    [let*S   (var1sym var1exp var2sym var2exp exp)
             (appC (lamC var1sym (appC (lamC var2sym (desugar exp)) (desugar var2exp))) (desugar var1exp))]
    [letrecS (varsym varexp exp) (type-case ExprS varexp
                                   [lamS (vararg varbody) (letrecC varsym vararg (desugar varbody) (desugar exp))]
                                   [else (desugar (letS varsym varexp exp))]
                                  )]
    [quoteS (c) (quoteC c)]
    [loopS () (loopC)]
    ))

; 3. Interp: execute the primitive expressions (ExprC) and return the value it got (Value)
(define (interp [a : ExprC] [env : Env] ) : Value
  (type-case ExprC a
    [numC (n) (numV n)]
    [idC (n)  (unbox (lookup n env))]; cascading search, first in env then in store
    [lamC (a b) (closV a b env)]
 
    ; application of function
    [appC (f a)
          (let ((closure (interp f env))
                (argvalue (interp a env)))
            (type-case Value closure
              [closV (parameter body env)
                     (interp body (extend-env (bind parameter (box argvalue)) env))]
              [else (error 'interp "operation app aplied to non-closure")]
              ))]

    ; letrec
    [letrecC (varsym vararg varbody exp) (let* (
                                                (updatedenv (extend-env (bind varsym (box (numV 0))) env)) ; any value
                                                (newClos (closV vararg varbody updatedenv))
                                               )
                                           (begin (set-box! (lookup varsym updatedenv) newClos)
                                                  (interp exp updatedenv))) ]
   
    ;plusC
    [plusC (l r)
             (let ((left (interp l env))
                   (right (interp r env)))
               (if (numV? left)
                 (if (numV? right)
                     (num+ left right)
                     (error 'interp "second argument of sum not a number value"))
                 (error 'interp "first argument of sum not a number value"))
               )]
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

    ; with lists
    [consC (b1 b2) (let ( (car (interp b1 env))
                          (cdr (interp b2 env)))
                     (consV car cdr))]
    [carC (c) (type-case Value (interp c env)
                [consV (car cdr)
                       car]
                [else (error 'interp "car applied to non-cell")]
                )]
    [cdrC (c) (type-case Value (interp c env)
                [consV (car cdr)
                       cdr]
                [else (error 'interp "cdr applied to non-cell")]
                )]
    ;quoteC
    [quoteC (q) (symV q)]

    ;loopC
    [loopC () (loop mt-env)]
    ))

; Facilitator
(define (interpS [s : s-expression]) (interp (desugar (parse s)) mt-env))
(define interpAll (lambda () (interpS '(read-loop))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; AUXILIAR FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; lookup changes its return type
(define (lookup [varName : symbol] [env : Env]) : (boxof Value)
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
(define (loop env)
    (let ([input (read)])
      (if (eq? (symbol->s-exp '@END) input)
          (symV '@ACABOU)
          (begin
            (display (interp (desugar (parse input)) env))
            (display "\n")
            (loop env)
          )
      )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TESTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (interpS '(quote alan))
      (symV 'alan))

(test (interpS '(let x 5 (* x 4)))
      (numV 20))

(test (interpS '(let* x 2 y (+ x 13) (* x y)))
      (numV 30))

(test (interpS '(letrec fatorial (lambda x (if x (* x (call fatorial (- x 1))) 1)) (call fatorial 5)))
      (numV 120))
