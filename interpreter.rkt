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
  )

; Definition for the partially and completely computed expressions
(define-type Value
  [numV  (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)]
  [consV (car : Value) (cdr : Value)]
  )

; Definition of the Env type
; Note that Env remains the same, we only change the Binding
; Also, Bindings associate symbol with Values
(define-type Binding
        [bind (name : symbol) (val : Value)])
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

