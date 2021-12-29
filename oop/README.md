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
    (let Wallet
             (class Object money
                    (regularMethod credit amount (set! money (+ money amount)))
                    (regularMethod debit amount (set! money (- money amount))))
       (let wallet (new Wallet 0)
         (seq (send wallet credit 10)
              (send wallet debit 3))))
```

### Catch unknown method
To catch unknown methods in the class, you can create override 'mensagemDesconhecida
```
> (interpAll)
    (interpS '(let Wallet (class Object money
                          (regularMethod mensagemDesconhecida x 19)
                          (regularMethod debit amount (set! money (- money amount))))
              (let wallet (new Wallet 0)
                (send wallet invalid 1))))
    ; (numV 19)
```
