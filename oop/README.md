# Object Oriented Programming Lisp Interpreter
*Basic oriented programming programming lisp interpreter using Lisp (```plaid-typed```) to interpret.*

### Available Operations
*Class and OOP*
- ```class```: defines a class from a subclass, with an attribute and two methods
- ```new```: creates an instance of a class
- ```send```: call a method of an instance
ps: ```self``` is a reference to the class object

*Arithmetic*
- ```+```: sum first and second parameters
- ```-```: subtracts first and second parameters
- ```*```: multiplies first and second parameters
- ```/```: divides first and second parameters

*Variable*
- ```let```: creates a variable and calls the expression with it at disposal

*More*
- ```quote```: creates a symbolic expression with the parameter
- ```if```: condition

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
