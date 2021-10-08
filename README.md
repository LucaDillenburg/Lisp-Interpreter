# Lisp Interpreter

### Examples
```
> (interpS '(quote alan))
  - Value
  (symV 'alan)
> (interpS '(cons (quote x) (quote z)))
  - Value
  (consV (symV 'x) (symV 'z))
> (interpS '(let x (+ 4 5) (* x 7)))
  - Value
  (numV 63)
> (interpS '(let* x 4 y x (+ x y)))
  - Value
  (numV 8)
> (interpS '(let* x (+ 4 5) y (+ x x) (+ x y)))
  - Value
  (numV 27)
> (interpS '(let x (+ 4 0)(let* x (+ x x) y (+ x x) (+ x y)))); cuidado com os valores 
  - Value
  (numV 24)
> (interpS '(letrec func (lambda x (if x (* x (call func (- x 1))) 1)) (call func 6)))
  - Value
  (numV 720)
```

### Outros materiais
- [Material da disciplina sobre plaid-typed](https://edisciplinas.usp.br/pluginfile.php/6450238/mod_resource/content/4/Gubi-Plai_Typed.pdf)

### Dúvida
Já que o let e let* vão ter um número fixo de argumentos, qual deve ser a sintaxe do mesmo?
- ```(let (a 1) a)``` ou ```(let ((a 1)) a)``` ?
- ```(let* (a 1) (b a) b)``` ou ```(let ((a 1) (b a)) b)``` ?

### Authors
- Luca Dillenburg - 11796580
- Arthur - 10297647
