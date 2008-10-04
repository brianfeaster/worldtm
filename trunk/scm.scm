;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Serializing - implements display and write

;; display ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This implementation first converts the object into a flattened
; list of strings that comprise the object serialized representation.
; For example (1 2) => ("(" " " "2" ")").  It does this by recursively
; flattening and serializing the object as it goes.  The benefit is now
; there is a object->string (display-list-serialize) function.  Not sure
; what to name it officially.

; TODO: Only one thread should be in this call at a time.
; Need to implement kernel semaphores?
; Now that kernel semaphores are implemented, need to
; block not on a call to display but the port.

(define (display-list-serialize-list o r)
  (display-list-serialize
     (car o) (if (pair? (cdr o))
                 (cons " " (display-list-serialize-list (cdr o) r))
                 (if (null? (cdr o))
                     r
                     (cons " . " (display-list-serialize (cdr o) r))))))

(define (display-list-serialize-vector v i max r)
 (display-list-serialize
   (vector-ref v i) (if (< i max)
                        (cons " "
                              (display-list-serialize-vector v (+ i 1) max r))
                        r)))

; Return flatten list of strings representing o (object)'s external
; represenation.  Recursive so r (result) must initially be NULL.
(define (display-list-serialize o r)
 (if (pair? o)
     (if (and (= (car o) 'quote)
                 (pair? (cdr o)))
         (cons "'" (display-list-serialize (car (cdr o)) r))
         (cons "(" (display-list-serialize-list o (cons ")" r))))
 (if (vector? o)
     (if (= o #())
       (cons #() r)
       (cons "#(" (display-list-serialize-vector o 0 (- (vector-length o) 1)
                                                 (cons ")" r))))
 (if (port? o)
     (list "#PORT<"
           (serialize-display (vector-ref o 0)) " "
           (serialize-display (vector-ref o 1)) " "
           (serialize-display (vector-ref o 2)) " "
           (serialize-display (vector-ref o 3)) " "
           (serialize-display (vector-ref o 4)) ">")
 (cons (serialize-display o) r)))))

; Display an object to a string.
(define (display->string x)
 (apply string (display-list-serialize x ())))

(define (display x . p)
 (set! p (if (null? p) stdout (car p))) ; Default port is STDIN.
 (for-each (lambda (s) (send s p))
           (display-list-serialize x ())))



;; write ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (write-list-serialize-list o r)
  (write-list-serialize
     (car o) (if (pair? (cdr o))
                 (cons " " (write-list-serialize-list (cdr o) r))
                 (if (null? (cdr o))
                     r
                     (cons " . " (write-list-serialize (cdr o) r))))))

(define (write-list-serialize-vector v i max r)
 (write-list-serialize
   (vector-ref v i) (if (< i max)
                        (cons " "
                              (write-list-serialize-vector v (+ i 1) max r))
                        r)))

; Return flatten list of strings representing o (object)'s external
; represenation.  Recursive so r (result) must initially be NULL.
(define (write-list-serialize o r)
 (if (pair? o)
     (if (and (= (car o) 'quote)
                 (pair? (cdr o)))
         (cons "'" (write-list-serialize (car (cdr o)) r))
         (cons "(" (write-list-serialize-list o (cons ")" r))))
 (if (vector? o)
     (if (= o #())
       (cons #() r)
       (cons "#(" (write-list-serialize-vector o 0 (- (vector-length o) 1)
                                                 (cons ")" r))))
 (cons (serialize-write o) r))))

; write an object to a string.
(define (write->string x)
 (apply string (write-list-serialize x ())))

(define (write x . p)
 (set! p (if (null? p) stdout (car p))) ; Default port is STDIN.
 (for-each (lambda (s) (send s p))
           (write-list-serialize x ())))

;; Serializing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (integer->char i) (vector-ref characters i))

(define NEWLINE (integer->char 10))
(define RETURN (integer->char 13))

(define (newline) (display NEWLINE))
(define (return)  (display RETURN))
(define (beep)    (send "\a" stdout))

(define ( caar x)      (car (car x)))
(define ( cadr x)      (car (cdr x)))
(define ( cdar x)      (cdr (car x)))
(define ( cddr x)      (cdr (cdr x)))
(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (length l) (if (null? l) 0 (+ 1 (length (cdr l)))))

(define (eqv? a b)
 (or (eq? a b)        ; Pointer (#\a #t #f () #() ""
     (= a b)          ; Numeric 1
     (string=? a b))) ; String

(define (equal? l1 l2)
 (or (eqv? l1 l2)
     (and (pair? l1)
          (pair? l2)
          (equal? (car l1) (car l2))
          (equal? (cdr l1) (cdr l2)))))

(define (car-string str) (string-ref str 0))
(define (cdr-string str) (substring str 1 (string-length str)))

(define (string->list s)
 (let ~ ((i 0)
         (l (string-length s)))
  (if (= i l) '()
      (cons (string-ref s i) (~ (+ i 1) l)))))

; Returns a pair containing the string split on the first delimeter character.
;   (strtok "abc-123" #\-)  =>  ("abc" . "123")
;   (strtok "abc=123" #\-)  =>  ("abc=123")
(define (strtok str delimeter)
 (let ((len (string-length str)))
 (let ~ ((i 0))
   (if (= i len) (cons str ())
   (if (eq? (string-ref str i) delimeter)
       (cons (substring str 0 i)
             (substring str (+ i 1) len))
   (~ (+ i 1)))))))

(define (memq o l)
 (if (null? l) ()
  (if (eq? o (car l)) l
   (memq o (cdr l)))))

(define (memv o l)
 (if (null? l) ()
  (if (eqv? o (car l)) l
   (memv o (cdr l)))))

(define (member o l)
 (if (null? l) ()
  (if (equal? o (car l)) l
   (member o (cdr l)))))

(define (assq o l)
 (if (null? l) ()
  (if (eq? o (caar l)) (car l)
   (assq o (cdr l)))))

(define (assv o l)
 (if (null? l) ()
  (if (eqv? o (caar l)) (car l)
   (assv o (cdr l)))))

(define (assoc o l)
 (if (null? l) ()
  (if (equal? o (caar l)) (car l)
   (assoc o (cdr l)))))

(define (make-vector-vector y x a)
 (let ((v (make-vector y ())))
  (let ~ ((i 0))
    (if (>= i y) v
        (begin (vector-set! v i (make-vector x a))
               (~ (+ i 1)))))))

(define (vector-set-vector! v1 s v2 t)
 (let ~ ((a s)
         (b t))
   (if (and (< a (vector-length v1))
            (< b (vector-length v2)))
      (begin
        (vector-set! v1 a (vector-ref v2 b))
        (~ (+ a 1)
           (+ b 1))))))

(define (map f x)
 (if (null? x) ()
  (cons (f (car x))
        (map f (cdr x)))))

(define (for-each f l)
 (if (not (null? l))
     (begin (f (car l))
            (for-each f (cdr l)))))

(define (vector-map fn v)
 (let ~ ((newv (make-vector (vector-length v)))
         (i 0))
  (if (= i (vector-length v)) newv
      (let ((ret (fn (vector-ref v i))))
        (vector-set! newv i ret)
        (~ newv (+ i 1))))))

(define (vector-map! fn v)
 (let ~ ((i 0))
  (if (= i (vector-length v)) v
        (begin (vector-set! v i (fn (vector-ref v i)))
               (~ (+ i 1))))))

(define (vector-vector-map! fn vv)
 (let ~~ ((y 0)(x 0))
  (if (< y (vector-length vv)) ; Done with Y iteration?
    (let ((v (vector-ref vv y))) ; Consider row.
      (let ~ ((x 0))
        (if (< x (vector-length v)) ; Done with X iteration?
          (begin (vector-set! v x (fn (vector-ref v x)))
                 (~ (+ x 1)))
          (~~ (+ y 1) 0))))
    vv)))                      ; Return vector-vector done.

(define (list . x) x)

(define (append l t)
 (if (pair? l)
     (cons (car l) (append (cdr l) t))
     t))

(define (append! lst t)
 (if (pair? lst)
   (let ~ ((l lst))
     (if (pair? (cdr l))
       (~ (cdr l))
       (set-cdr! l t))))
 lst)

(define (flatten l)
     (let ~ ((l l)
             (r ()))
      (if (pair? l)
          (~ (car l)
             (if (null? (cdr l))
                 r
                 (~ (cdr l) r)))
          (cons l r))))

; Iterative
(define (reverse l)
 (if (pair? l)
     (let ~ ((l l) (r ()))
      (if (null? l)
          r
          (~ (cdr l) (cons (car l) r))))
     l))

; Return new list omitting 1st occurance of e in l.
(define (list-delete l e)
 (if (pair? l)
   (if (equal? (car l) e)
       (cdr l)
       (cons (car l) (list-delete (cdr l) e)))
   l))
 


(define (displayl . l)
 (let ~ ((l l))
 (if (not (null? l))
     (begin (display (car l))
            (~ (cdr l))))))
(define (printl . l)
 (let ~ ((l l))
 (if (null? l) (newline)
     (begin (display (car l))
            (~ (cdr l))))))

(define (++ n) (+ n 1))
(define (-- n) (- n 1))
(define (max a b) (if (> a b) a b))
(define (min a b) (if (< a b) a b))
(define (random-bool . r) (= 0 (random (if (null? r) 2 (car r)))))
(define (^2 x) (* x x))
(define (distance v1 v2)
 (sqrt (+ (^2 (- (car v1)
                 (car v2)))
          (^2 (- (cadr v1)
                 (cadr v2)))
          (^2 (- (caddr v1)
                 (caddr v2))))))

;(define (eval-string s) (eval (read-string s)))

(define (load fn)
 (if (string? fn) (load (open fn))
 (if (not fn) (displayl "*load done*")
 (let ((exp (read fn)))
   (or (eof-object? exp)
       (begin (eval exp)
              (load fn)))))))

;  Default error handlers for all thread IDs:  Shutdown the entire machine.
(define ERRORS (make-vector 1024 
  (lambda (x)
    (displayl 'ERROR:: x)
    (quit))))

(define WELCOME-MESSAGE "Welcome to \e[1;31mW\e[33mO\e[32mR\e[34mL\e[35mD\e[01;m.\r\n\e[?25l")


; The read eval print loop.
(define (repl)
  (display (cons "MSG:" (call/cc (lambda (c) (vector-set! ERRORS (tid) c) WELCOME-MESSAGE))))
  (let ~ ()
  (display "\nwscm>")
  (let ((ret (read stdin)))
    (display (eval ret))
    (if (not (eof-object? ret)) (~)))))


; If wscm is run with a command line argument that isn't a switch, then 
; assume it's a file to run otherwise begin the REPL.
(if (and (> (vector-length argv) 1)
         (not (string=? "-" (vector-ref argv 1))))
 (load (vector-ref argv 1))
 (repl))
