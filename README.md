# Lisp Interpreters
*This collection of basic lisp interpreters using Lisp (```plaid-typed```).*

## Interpreters
1. [Default](default/): functional and synchronous lisp interpreter
    ```
    (interpS '(cons 1 (cons 2 (cons 3 (cons 4 5)))))
    ; result: (consV (numV 1) (consV (numV 2) (consV (numV 3) (consV (numV 4) (numV 5)))))
    ```

2. [Demand Driven Evaluation](ondemand/): asynchronous lisp interpreter using demand driven evaluation
    ```
    (letrec
        construct (lambda x (cons x (call construct (+ x 1))))
        (car (cdr (cdr (call construct 1)))))
    ; result: (numV 3)
    ```

3. [Object Oriented Programming](oop/): object oriented lisp interpreter
    ```
    (let Wallet (class Object money
            (regularMethod credit amount (set! money (+ money amount)))
            (regularMethod debit amount (set! money (- money amount))))
        (let wallet (new Wallet 0)
            (seq (send wallet credit 10)
                (send wallet debit 3))))
    ; result: (numV 7)
    ```

## Usage
- **Interpret one line:** To interpret a single line of code, use the command below:
    ```
    (interpS '(...))
    ```

- **Interpret multiple lines of code:** To interpret multiple lines of code, use the command below:
    ```
    (interpAll)
    ; lines of code
    ; use @END to quit
    ```

---

### How does it actually work? Steps to interpret
1. Parse: receive a quoted expression (s-expression) and parsers into an expression possibly with syntax sugar (ExprS).

    Note that the quoted expression can be an array, a number, a symbol, etc

2. Desugar: expand syntax sugar (ExprS) into primitive expressions (ExprC)

3. Interpret: execute the primitive expressions (ExprC) and return the value it got (Value)

**All steps:** ```(interp (desugar (parse '(...))) mt-env)```
