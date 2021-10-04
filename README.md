# Lisp Interpreter

### Examples
```
; let
(let ((a 5)) (+ a 3)) ; expect 8

; let*
(let* ((a 5) (b (+ a 3))) (+ b 2)) ; expect 10

; letrec
(letrec ((contains (lambda n (if n 10 (call contains (- n 1)))))) (call contains 5)) ; expect 10
```

### Outros materiais
- [Material da disciplina sobre plaid-typed](https://edisciplinas.usp.br/pluginfile.php/6450238/mod_resource/content/4/Gubi-Plai_Typed.pdf)

### Dúvida
Já que o let e let* vão ter um número fixo de argumentos, qual deve ser a sintaxe do mesmo?
- ```(let (a 1) a)``` ou ```(let ((a 1)) a)``` ?
- ```(let* (a 1) (b a) b)``` ou ```(let ((a 1) (b a)) b)``` ?

### Authors
- Luca Dillenburg - 11796580
- Arthur