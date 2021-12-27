# Functional and Synchronous Lisp Interpreter
*Basic functional and synchronous lisp interpreter using Lisp (```plaid-typed```) to interpret.*

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
