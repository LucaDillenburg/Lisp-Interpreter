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
  [ifC     (cond : ExprC) (y : ExprC) (n : ExprC)]
  [letC (var : symbol) (expression : ExprC) (body : ExprC)]
  [quoteC  (sym : symbol)]
  [nullC]
  [seqC  (statement1 : ExprC) (statement2 : ExprC)]
  [setC  (varName : symbol) (statement : ExprC)]
  [classC  (superClass : symbol) (instVar : symbol) (method1 : ExprC ) (method2 : ExprC)]
  [regularMethodC (name : symbol) (arg : symbol) (body : ExprC)]
  [primitiveMethodC (name : symbol) (primNumber : number)]
  [newC (class : symbol) (attribute : ExprC)]
  [sendC (object : symbol) (methodName : symbol) (param : ExprC)]
  [readloopC ]
  )

; Definition of the expressions with or without sugar expressions.
; Note that both sugar and sugarless expressions have a specific type defined.
(define-type ExprS
  [numS    (n : number)]
  [idS     (s : symbol)]
  [plusS   (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [uminusS (e : ExprS)]
  [multS   (l : ExprS) (r : ExprS)]
  [ifS     (c : ExprS) (y : ExprS) (n : ExprS)]
  [letS    (var : symbol) (exp : ExprS) (body : ExprS)]
  [quoteS  (sym : symbol)]
  [seqS (statement1 : ExprS) (statement2 : ExprS)]
  [setS (variable : symbol) (statement : ExprS)]
  [classS  (superClass : symbol) (instVar : symbol) (method1 : ExprS ) (method2 : ExprS)]
  [regularMethodS (name : symbol) (arg : symbol) (body : ExprS)]
  [primitiveMethodS (name : symbol) (primNumber : number)]
  [newS (class : symbol) (value : ExprS)]
  [sendS (receiver : symbol) (methodName : symbol) (arg : ExprS)]
  [nullS  ]
  [readloopS]
  )

(define-type MethodDefinition
  [regularMethod (arg : symbol) (body : ExprC)]
  [primitiveMethod (num : number)]
)

; We need a new value for the box
(define-type Value
  [numV  (n : number)]
  [methodV (name : symbol) (definition : MethodDefinition)]
  [symV (sym : symbol)]
  [classV (superClass : symbol) (instVar : symbol)
          (m1Name : symbol) (m1Definition : MethodDefinition)
          (m2Name : symbol) (m2Definition : MethodDefinition)]
  [objectV (class : symbol) (env : Env)]
  [nullV ]
  )

; Bindings associate symbol with Values
(define-type Binding
        [bind (name : symbol) (val : (boxof Value))])
; Env remains the same, we only change the Binding
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

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
    [(s-exp-symbol? s) (idS (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(-) (bminusS (parse (second sl)) (parse (third sl)))]
         [(~) (uminusS (parse (second sl)))]
          [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [(quote) (quoteS (s-exp->symbol (second sl)))]
         [(let) (letS (s-exp->symbol (second sl))
                      (parse (third sl))
                      (parse (fourth sl)))]
          [(set!) (setS (s-exp->symbol (second sl))
                      (parse (third sl)))]
         [(seq) (seqS (parse (second sl))
                      (parse (third sl)))]
         [(class) (classS (s-exp->symbol (second sl))
                          (s-exp->symbol (third sl))
                          (parse (fourth sl))
                          (parse (list-ref sl 4)))]
         [(regularMethod) (regularMethodS (s-exp->symbol (second sl))
                                          (s-exp->symbol (third sl))
                                          (parse (fourth sl)))]
         [(primitiveMethod) (primitiveMethodS (s-exp->symbol (second sl))
                                          (s-exp->number (third sl)))]
         [(send) (sendS (s-exp->symbol (second sl)) (s-exp->symbol (third sl)) (parse (fourth sl)))]
         [(new)  (newS (s-exp->symbol (second sl)) (parse (third sl)))]
         [(read-loop) (readloopS)]
        [else (error 'parse "invalid list input")]
         ))]
    [else (error 'parse "invalid input")]
    ))

; 2. Desugar: expand syntax sugar (ExprS) into primitive expressions (ExprC)
(define (desugar [as : ExprS]) : ExprC
  (type-case ExprS as
    [numS    (n)        (numC n)]
    [idS     (s)        (idC s)]
    [plusS   (l r)      (plusC (desugar l) (desugar r))]
    [multS   (l r)      (multC (desugar l) (desugar r))]
    [bminusS (l r)      (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS (e)        (multC (numC -1) (desugar e))]
    [ifS     (c y n)    (ifC (desugar c) (desugar y) (desugar n))]
    [letS    (v e b)    (letC v (desugar e) (desugar b))]
    [quoteS  (sym) (quoteC sym)]
    [nullS  ()  (nullC)]
    [seqS (st1 st2) (seqC (desugar st1) (desugar st2))]
    [setS (var st)  (setC var (desugar st))]
    [classS (superClass instVar method1 method2) (classC superClass instVar (desugar method1) (desugar method2))]
    [regularMethodS (name arg body) (regularMethodC name arg (desugar body))]
    [primitiveMethodS (name primNumber) (primitiveMethodC name primNumber)]
    [newS (class attribute) (newC class (desugar attribute))]
    [sendS (object methodName param) (sendC object methodName (desugar param))]
    [readloopS () (readloopC)]
    ))

; 3. Interp: execute the primitive expressions (ExprC) and return the value it got (Value)
(define (interp [a : ExprC] [objectEnv : Env]) : Value
  (type-case ExprC a
    [nullC () (nullV)]
    [numC (n) (numV n) ]
    [idC (n)  (unbox (lookup n objectEnv))]; cascading search, first in env then in sto
    ;plusC
    [plusC (l r)
             (let ((left (interp l objectEnv ))
                   (right (interp r objectEnv )))
               (if (numV? left)
                   (if (numV? right)
                       (num+ left right)
                       (error 'interp "second argument of addition not a number value")
                   )
                   (error 'interp "first argument of addition not a number value")
               )
             )]
    ;multC
    [multC (l r)
           (let ( (left (interp l objectEnv ))
                  (right (interp r objectEnv )))
             ;in this case type cheking is a little different
             (if (numV? left)
                 (if (numV? right)
                     (num* left right)
                     (error 'interp "second argument of multiplication not a number value"))
                 (error 'interp "first argument of multiplication not a number value"))
                 )]
    ; ifC serializes
    [ifC (c s n) (type-case Value (interp c objectEnv )
                   [numV (value)
                        (if (zero? value)
                            (interp n objectEnv )
                            (interp s objectEnv ))]
                   [else (error 'interp "condition not a number")]
                   )]
    [quoteC  (s) (symV s)]
    [letC (variable expression body)
          (let ((value (interp expression objectEnv )))
            (interp body
                    (extend-env (bind variable (box value)) objectEnv)
                    ))]
    [seqC (firstCommand secondCommand)
          (begin (interp firstCommand objectEnv)
                 (interp secondCommand objectEnv))]
    [setC  (variableName statement)
           (let ((varBox (lookup variableName objectEnv))
                 (value (interp statement objectEnv)))
             (begin (set-box! varBox value)
                    value))]

    [classC (superClass instVar method1 method2)
            (let ((interpMethod1 (interp method1 objectEnv)) (interpMethod2 (interp method2 objectEnv)))
              (type-case Value interpMethod1
                [methodV (m1Name m1Definition) (type-case Value interpMethod2
                                                 [methodV (m2Name m2Definition)
                                                          (classV superClass instVar m1Name m1Definition m2Name m2Definition)]
                                                 [else (error 'interp "invalid method in class definition")]
                                                )]
                [else (error 'interp "invalid method in class definition")]
              ))]
    [regularMethodC (name arg body) (methodV name (regularMethod arg body))]
    [primitiveMethodC (name primNumber) (methodV name (primitiveMethod primNumber))]
    [newC (class attribute) (objectV class
                                     (let ((insideObjEnv (setClassIntoEnv class (createObjEnv class objectEnv mt-env))))
                                       (begin
                                         (let ((attr (interp attribute objectEnv))
                                               (classValue (unbox (lookup class objectEnv))))
                                           (type-case Value classValue
                                             [classV (superClass instVar m1Name m1Definition m2Name m2Definition)
                                                     (set-box! (lookup instVar insideObjEnv) attr)]
                                             [else (error 'new "invalid class name")]
                                           ))
                                         insideObjEnv)))]
    [sendC (object methodName param) (type-case Value (objectLookup object objectEnv)
          [objectV (objClass objEnv) (let ((maybeMethod (methodLookup objClass objClass methodName objEnv)))
                                       (type-case Value maybeMethod
                                         [methodV (name definition) (type-case MethodDefinition definition
                                            [regularMethod (arg body)
                                                           (interp body (extend-env (bind arg (box (interp param objectEnv))) objEnv))]
                                            [primitiveMethod (num)
                                                             (if (> num (vector-length primitiveMethodVector))
                                                                 ((vector-ref primitiveMethodVector 0) (numV num))
                                                                 ((vector-ref primitiveMethodVector num) (interp param objectEnv)))]
                                            )]
                                       [else (error 'interp "invalid method definition")]
                                         ))]
          [else (error 'interp "invalid object")]
        )]

    ;readloopC
    [readloopC () (letrec ( (read-till-end (lambda ()
                                              (let ( (input (read)))
                                                (if (and (s-exp-symbol? input )
                                                         (eq? (s-exp->symbol input) '@END))
                                                    (begin (display 'FINISHED-READLOOP)
                                                           (symV  'END_OF_loop))
                                                    (begin (display (interp (desugar (parse input)) objectEnv))
                                                           (read-till-end)))))))
                     (read-till-end))]
    ))

; Facilitator
; Enviromnent needs to be intialzed with the association for the Object class, which needs to be defined elsewhere
(define Object (classV 'null 'null 'unknownMessage (primitiveMethod 1) 'null (regularMethod 'x (numC 0))))
(define initialObjectEnv (extend-env (bind 'class (box (symV 'null))) (extend-env (bind 'Object (box Object)) mt-env)))
(define (interpS [s : s-expression]) : Value (interp (desugar (parse s)) initialObjectEnv ))

(define interpAll (lambda () (interpS '(read-loop))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; AUXILIAR FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (objectLookup [object : symbol] [objectEnv : Env]) : Value
  (if (eq? object 'self)
      (type-case Value (unbox (lookup 'class objectEnv))
        [symV (sym) (objectV sym objectEnv)]
        [else (error 'objectLookup (string-append (symbol->string object) " not found"))]
       )
      (unbox (lookup object objectEnv))))

(define (methodLookup [initialClass : symbol] [class : symbol] [methodName : symbol] [objectEnv : Env]) : Value
  (if (eq? class 'null)
      (methodLookup initialClass initialClass 'unknownMessage objectEnv) ; call unknownMessage from object
      (let ((classValue (unbox (lookup class objectEnv))))
        (type-case Value classValue
          [classV (superClass instVar m1Name m1Method m2Name m2Method)
                  (if (eq?  m1Name methodName) (methodV m1Name m1Method)
                      (if (eq? m2Name methodName) (methodV m2Name m2Method)
                          (methodLookup initialClass superClass methodName objectEnv)))]
          [else (error 'methodLookup (string-append (symbol->string class) " invalid superclass"))]
          ))))

(define (setClassIntoEnv [class : symbol] [objEnv : Env]) : Env
  (extend-env (bind 'class (box (symV class))) objEnv))

(define (createObjEnv [class : symbol] [outsideEnv : Env] [insideEnv : Env]) : Env
  (if (eq? class 'null) insideEnv
      (let ((classValue (unbox (lookup class outsideEnv))))
        (type-case Value classValue
          [classV (superClass instVar m1Name m1Method m2Name m2Method)
                  (let ((envWithClass (extend-env (bind class (box classValue)) insideEnv)))
                    (let ((envWithClassAndAttr (extend-env (bind instVar (box (nullV))) envWithClass)))
                      (createObjEnv superClass outsideEnv envWithClassAndAttr)))]
          [else (error 'createObjEnv (string-append (symbol->string class) " invalid superclass"))]
        ))))

; auxiary functions for messageLookup                                         
(define (lookup [varName : symbol] [env : Env]) : (boxof Value)
       (cond
            [(empty? env) (error 'lookup (string-append (symbol->string varName) " not found"))]
            [else (cond
                    [(symbol=? varName (bind-name (first env)))   ; found
                     (bind-val (first env))]
                    [else (lookup varName (rest env))])]))        ; searches the rest



; Primitive operators
(define (num+ [l : Value] [r : Value]) : Value
    (cond
        [(and (numV? l) (numV? r))
             (numV (+ (numV-n l) (numV-n r)))]
        [else
             (error 'num+ "One of the arguments is not a number")]))

(define (num* [l : Value] [r : Value]) : Value
    (cond
        [(and (numV? l) (numV? r))
             (numV (* (numV-n l) (numV-n r)))]
        [else
             (error 'num* "One of the arguments is not a number")]))


(define primitiveMethodVector
  (make-vector 2 (lambda ([ x : Value] ) : Value
                   (error 'primitive "invalid primitive method")))); 0
;add primitive 1 for 'unknownMessage
(vector-set! primitiveMethodVector 1
             (lambda ([methodName : Value]) (type-case Value methodName
                                 [symV (symbolValue) (error 'messaging
                                                            (string-append "unknownMessage:" (symbol->string symbolValue)))]
                                 [else (error 'wrongArgument
                                              "Wrong Argument: primitive 1 should receive a symV")])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TESTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test 
 (interpS '(let classe1 (class Object i (regularMethod m1 x i) (regularMethod m2 x (send self m1 x)))
            (let classe2 (class classe1 j (regularMethod m1 x (quote subclassregularMethod)) (regularMethod m3 y y))
              (let object2 (new classe2 200) (send object2 m2 55)))))
 (symV 'subclassregularMethod))

(test 
 (interpS '(let classe1 (class Object i (regularMethod m1 x x) (regularMethod m2 x (send self m1 x)))
            (let classe2 (class classe1 j (regularMethod m1 x 10) (regularMethod m3 y y))
              (let object2 (new classe2 200) (send object2 m1 55)))))
 (numV 10))

(test
  (interpS
    '(let Wallet
             (class Object money
                    (regularMethod credit amount (set! money (+ money amount)))
                    (regularMethod debit amount (set! money (- money amount))) )
       (let wallet (new Wallet 0)
         (seq (send wallet credit 10)
              (send wallet debit 3)))))
  (numV 7))

(test
 (interpS '(let Wallet (class Object money
                          (regularMethod unknownMessage x 19)
                          (regularMethod debit amount (set! money (- money amount))))
              (let wallet (new Wallet 0)
                (send wallet invalid 1))))
 (numV 19))
