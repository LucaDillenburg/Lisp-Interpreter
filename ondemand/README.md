# Demand Driven Evaluation Lisp Interpreter
*Basic asynchronous with demand driven evaluation lisp interpreter using Lisp (```plaid-typed```) to interpret.*

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
    (letrec
        construct (lambda x (cons x (call construct (+ x 1))))
        (car (cdr (cdr (call construct 1)))))
```
