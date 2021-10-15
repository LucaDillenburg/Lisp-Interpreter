# Lisp Interpreter
*This is a basic lisp interpreter using Lisp (```plaid-typed```) to interpret it.*

### Steps:
1. Parse: receive a quoted expression (s-expression) and parsers into an expression possibly with syntax sugar (ExprS).

    Note that the quoted expression can be an array, a number, a symbol, etc

2. Desugar: expand syntax sugar (ExprS) into primitive expressions (ExprC)

3. Interpret: execute the primitive expressions (ExprC) and return the value it got (Value)

### Usage
- **Separate Steps:** You can run the steps separately by using some of the commands below:
    ```
    (define s '(...))
    (interp (desugar (parse s)) mt-env)
    ```

- **Interpret one line:** To interpret a single line of code, use the command below:
    ```
    (define s '(...))
    (interpS s)
    ```

- **Interpret multiple lines of code:** To interpret multiple lines of code, use the command below:
    ```
    (interpAll)
    ; lines of code
    ; use @END to quit
    ```

### Available Operations
*List*
- ```car```: get first element of the list
- ```cdr```: get list from the second element
- ```cons```: create a list with first and second parameters

*Arithmetic*
- ```+```: sum first and second parameters
- ```-```: subtracts first and second parameters
- ```*```: multiplies first and second parameters
- ```/```: divides first and second parameters

*Variable*
- ```let```: creates a variable and calls the expression with it at disposal
- ```let*```: creates two variables (in which the second one can access the first) and calls the expression with them at disposal
- ```letrec```: create a variable that can use itself in the lambda definition and calls the expression with it at disposal

*More*
- ```quote```: creates a symbolic expression with the parameter
- ```if```: condition
- ```lambda```: creates a function
- ```call```: call function

### Example
```
> (interpAll)
    (quote yourname)
    (cons (quote x) (quote z))
    (let x (+ 4 5) (* x 7))
    (let* x 4
        y x
        (+ x y))
    (let* x (+ 4 5) y (+ x x) (+ x y))

    (let x (+ 4 0)
        (let* x (+ x x) y (+ x x)
            (+ x y)))

    (letrec func (lambda x (if x (* x (call func (- x 1))) 1)) (call func 6))
```

### Authors
- Luca Dillenburg - 11796580
- Arthur - 10297647
