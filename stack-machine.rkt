#lang racket
(require "opcodes.rkt")

(provide make-stack-machine)
(provide run-stack-machine)
(provide get-stack)
(provide get-varnames)
(provide get-consts)
(provide get-names)
(provide get-code)
(provide get-IC)
(provide empty-stack)
(provide make-stack)
(provide push)
(provide pop)
(provide top)


;; TODO 1:
;; Alegeți metoda de reprezentarea a unei stive.
;; Implementați:
(define empty-stack '())
(define (make-stack) empty-stack)

(define push (λ(value stack) (cons value stack)))
(define top (λ(stack) (car stack))) 
(define pop (λ(stack) (cdr stack)))
;; TODO 2:
;; Alegeți metoda de reprezentare a unei mașini stivă.
;; Definiți make-stack-machine, acesta trebuie sa primeasca cele 4 segmente de date
;; Veți avea nevoie de o stivă pentru execuție și un counter ca să stiți
;; la ce instrucțiune sunteți.
(define (make-stack-machine stack co-varnames co-consts co-names co-code IC)
  (list stack co-varnames co-consts co-names co-code IC))

;; Definiți funcțiile `get-varnames`, `get-consts`, `get-names`,
;; `get-code`, `get-stack`, `get-IC` care primesc o mașina stivă și întorc
;; componenta respectivă

;; ex:
;; > (get-varnames (make-stack-machine empty-stack 'dummy-co-varnames (hash) (hash) (list) 0))
;; 'dummy-co-varnames
(define get-varnames (λ(stack-machine) (cadr stack-machine)))

;; ex:
;; > (get-consts (make-stack-machine empty-stack (hash) 'dummy-co-consts (hash) (list) 0))
;; 'dummy-co-consts
(define get-consts (λ(stack-machine) (caddr stack-machine)))

;; ex:
;; > (get-names (make-stack-machine empty-stack (hash) (hash) 'dummy-co-names (list) 0))
;; 'dummy-co-names
(define get-names (λ(stack-machine) (cadddr stack-machine)))

;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) 'dummy-co-code 0))
;; dummy-co-code
(define get-code (λ(stack-machine) (cadr (reverse stack-machine))))

;; Întoarce stiva de execuție.
;; ex:
;; > (get-code (make-stack-machine 'dummy-exec-stack (hash) (hash) (hash) (list) 0))
;; dummy-exec-stack
(define get-stack (λ(stack-machine) (car stack-machine)))

;; Întoarce instruction counterul.
;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) (list) 0))
;; 0
(define get-IC (λ(stack-machine) (car (reverse stack-machine))))



(define symbols (list 'STACK 'CO-VARNAMES 'CO-CONSTS 'CO-NAMES 'CO-CODE 'INSTRUCTION-COUNTER))

;; TODO 3:
;; Definiți funcția get-symbol-index care gasește index-ul simbolului in listă.
(define (get-symbol-index symbol)
  (let loop ((iter 0) (lst symbols))
    (if(eq? symbol (car lst)) iter
       (loop (+ iter 1) (cdr lst)))))

;; Definiți funcția update-stack-machine care intoarce o noua mașina stivă
;; înlocuind componenta corespondentă simbolului cu item-ul dat în paremetri.
;; > (get-varnames (update-stack-machine "new-varnames" 'CO-VARNAMES stack-machine))
;; "new-varnames"
;; > (get-varnames (update-stack-machine "new-names" 'CO-NAMES stack-machine))
;; "new-names"
(define (replace lst index item)
         (append (take lst index) (list item) (drop lst (add1 index))))
(define (update-stack-machine item symbol stack-machine)
  (let ((index (get-symbol-index symbol)))
    (replace stack-machine index item)))

;; Definiți funcția push-exec-stack care primește o masină stivă și o valoare
;; și intoarce o noua mașina unde valoarea este pusă pe stiva de execuție
(define (push-exec-stack value stack-machine)
  (update-stack-machine (push value (get-stack stack-machine)) 'STACK stack-machine))

;;  Definiți funcția pop-exec-stack care primește o masină stivă
;;  și intoarce o noua mașina aplicând pop pe stiva de execuție.
(define (pop-exec-stack stack-machine)
        (update-stack-machine (pop (get-stack stack-machine)) 'STACK stack-machine))

;; TODO 4:
;; Definiți funcția run-stack-machine care execută operații pană epuizează co-code.

(define (run-stack-machine stack-machine)
    (let* ((ic (get-IC stack-machine)) (instr (car (list-ref (get-code stack-machine) ic))) (iparam (cdr (list-ref (get-code stack-machine) ic))))
         (case instr
           [(LOAD_CONST) (run-stack-machine (update-stack-machine (add1 ic) 'INSTRUCTION-COUNTER (push-exec-stack (hash-ref (get-consts stack-machine) iparam) stack-machine)))]   
           [(LOAD_FAST) (run-stack-machine (update-stack-machine (add1 ic) 'INSTRUCTION-COUNTER (push-exec-stack (hash-ref (get-varnames stack-machine) iparam) stack-machine)))]
           [(STORE_FAST) (run-stack-machine (update-stack-machine (add1 ic) 'INSTRUCTION-COUNTER (pop-exec-stack  (update-stack-machine (hash-set (get-varnames stack-machine) iparam (car (get-stack stack-machine))) 'CO-VARNAMES stack-machine))))] 
           [(BINARY_MODULO INPLACE_MODULO) (run-stack-machine(update-stack-machine (add1 ic) 'INSTRUCTION-COUNTER (push-exec-stack (modulo(cadr(get-stack stack-machine))(car(get-stack stack-machine))) (pop-exec-stack(pop-exec-stack stack-machine)))))]
           [(BINARY_ADD INPLACE_ADD) (run-stack-machine(update-stack-machine (add1 ic) 'INSTRUCTION-COUNTER (push-exec-stack (+(cadr(get-stack stack-machine))(car(get-stack stack-machine))) (pop-exec-stack(pop-exec-stack stack-machine)))))]
           [(BINARY_SUBTRACT INPLACE_SUBTRACT) (run-stack-machine(update-stack-machine (add1 ic) 'INSTRUCTION-COUNTER (push-exec-stack (-(cadr(get-stack stack-machine))(car(get-stack stack-machine))) (pop-exec-stack(pop-exec-stack stack-machine)))))]
           [(JUMP_ABSOLUTE) (run-stack-machine(update-stack-machine(/ iparam 2) 'INSTRUCTION-COUNTER stack-machine))]
           [(COMPARE_OP) (run-stack-machine(update-stack-machine (add1 ic) 'INSTRUCTION-COUNTER (push-exec-stack  ((get-cmpop iparam)(cadr(get-stack stack-machine))(car(get-stack stack-machine))) (pop-exec-stack(pop-exec-stack stack-machine)))))]
           [(POP_JUMP_IF_FALSE) (if (eq? #f (car(get-stack stack-machine))) (run-stack-machine(update-stack-machine (/ iparam 2) 'INSTRUCTION-COUNTER (pop-exec-stack stack-machine)))
                                                                            (run-stack-machine(update-stack-machine (add1 ic) 'INSTRUCTION-COUNTER (pop-exec-stack stack-machine))))]
           [(POP_JUMP_IF_TRUE) (if (eq? #t (car(get-stack stack-machine))) (run-stack-machine(update-stack-machine (/ iparam 2) 'INSTRUCTION-COUNTER (pop-exec-stack stack-machine)))
                                                                            (run-stack-machine(update-stack-machine (add1 ic) 'INSTRUCTION-COUNTER (pop-exec-stack stack-machine))))]
           [(SETUP_LOOP POP_BLOCK GET_ITER) (run-stack-machine (update-stack-machine (add1 ic) 'INSTRUCTION-COUNTER stack-machine))]
           [(FOR_ITER)  (if(null? (car(get-stack stack-machine))) (run-stack-machine(update-stack-machine(+ ic (add1(/ iparam 2))) 'INSTRUCTION-COUNTER (pop-exec-stack stack-machine)))
                           (run-stack-machine(update-stack-machine(add1 ic) 'INSTRUCTION-COUNTER (push-exec-stack (caar(get-stack stack-machine)) (push-exec-stack (cdar(get-stack stack-machine)) (pop-exec-stack stack-machine))))))]
           
           [(RETURN_VALUE) stack-machine])))