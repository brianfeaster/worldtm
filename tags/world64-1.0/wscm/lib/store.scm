; Object which is a fancy persistent association list / file store
; TODO Not thread safe
(define (Store filename . debug)
 (define (self x) (eval x))
 (define STORE '()) ; Association list
 (define MUTATED #f)
 (define SHUTDOWN #f)
 (define DB (if (pair? debug) (car debug) (lambda x x)))

 (define (save)
   (if MUTATED
     (let ((fp (open-file filename #t)))
       (or (port? fp) (set! fp (open-new-file filename)))
       (DB "\nSaving")
       (write STORE fp)
       (close fp)
       (set! MUTATED #f))))

 (define (load)
   (let ((fp (open-file filename #t)))
     (if (port? fp)
       (begin
         (set! STORE (read fp))
         (set! MUTATED #f)
         (close fp)))))

 (define (shutdown)
   (set! SHUTDOWN #t)
   (save))

 (define (keys)
   (let ~ ((db STORE))
     (if (null? db) ()
       (cons (caar db) (~ (cdr db))))))

 (define (exist? k)
   (pair? (assv k STORE)))

 (define (add k . v)
   (set! STORE (cons (cons k
                           (if (pair? v) (car v) ()))
                     STORE))
   (set! MUTATED #t))

 (define (set k e)
   (let ((v (assv k STORE)))
     (if (pair? v)
       (begin (set-cdr! v e)
              (set! MUTATED #t)))))

 (define (get k)
   (let ((v (assv k STORE)))
     (if (pair? v) (cdr v) #f)))

 (define (dump)
   (DB "\n::Store\n filename = " filename "\n keys = " (keys))
   (map
     (lambda (a)
       (DB "\n  " (car a) " = " (cdr a)))
     STORE))

 ; MAIN

 ; Thread to autosave the store
 (thread (let ~ ()
   (sleep (* 1000 60 5)) ; Every 10 minutes backup the store
   ; If the store has been shutdown, stop autosaving
   (or SHUTDOWN
     (begin (save)
            (~)))))

 (load)
 self)
