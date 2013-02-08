;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Serializing - implements display and write
;;  display_section
;;  write_section
;;
;; This implementation first converts the object into a flattened
;; list of strings that comprise the object serialized representation.
;; For example (1 2) => ("(" " " "2" ")").  It does this by recursively
;; flattening and serializing the object as it goes.  The benefit is now
;; there is a object->string (display-list-serialize) function.  Not sure
;; what to name it officially.
;;
;; TODO Block not on a call to display to prevent multiple threads
;;      from printing at the same time to the same port.

; Keep track of internal implemenations.
(define sdisplay display)
(define swrite write)

; Shorter aliases
(define gc garbage-collect)
(define db debugger)

;;;;;;;;;;;;;;;;;;;;
; display_section
(define (display-list-serialize-list o r)
  (display-list-serialize
     (car o) (if (pair? (cdr o))
                 (cons " " (display-list-serialize-list (cdr o) r))
                 (if (null? (cdr o))
                     r
                     (cons " . " (display-list-serialize (cdr o) r))))))

(define (display-list-serialize-vector v r)
 (let ~ ((i (- (vector-length v) 1))
         (r (cons ")" r)))
 (if (= i 0)
   (cons "#(" (display-list-serialize (vector-ref v 0) r))
   (~ (- i 1)
      (cons " " (display-list-serialize (vector-ref v i) r))))))

; Return flatten list of strings representing o (object)'s external
; represenation.  Recursive so r (result) must initially be NULL.
(define (display-list-serialize o r)
 (if (pair? o)
     (if (and (eq? (car o) 'quote)
                 (pair? (cdr o)))
         (cons "'" (display-list-serialize (car (cdr o)) r))
         (cons "(" (display-list-serialize-list o (cons ")" r))))
 (if (vector? o)
     (if (eq? o #())
       (cons "#()" r)
       (display-list-serialize-vector o r))
 (cons (serialize-display o) r))))

; Serialize an object into a list of "display" strings.
(define (display->strings x)
 (display-list-serialize x ()))

; Serialize an object into one "display" string.
(define (display->string x)
 (apply string (display-list-serialize x ())))

(define (display x . p)
 (set! p (if (null? p) stdout
         (if (and (pair? p)
                  (port? (car p)))
             (car p)  ; Make sure 2nd arg is a port object
             (begin (send (display->string (list "WARNING: display: " p " is not a port")) stdout)
                    stdout)))) ; Default port is STDIN.
 (for-each (lambda (s) (send s p))
           (display->strings x)))


;;;;;;;;;;;;;;;;;;;;
; write_section
(define (write-list-serialize-list o r)
  (write-list-serialize
     (car o) (if (pair? (cdr o))
                 (cons " " (write-list-serialize-list (cdr o) r))
                 (if (null? (cdr o))
                     r
                     (cons " . " (write-list-serialize (cdr o) r))))))

(define (write-list-serialize-vector v r)
 (let ~ ((i (- (vector-length v) 1))
         (r (cons ")" r)))
 (if (= i 0)
   (cons "#(" (write-list-serialize (vector-ref v 0) r))
   (~ (- i 1)
      (cons " " (write-list-serialize (vector-ref v i) r))))))


; Return flatten list of strings representing o (object)'s external
; represenation.  Recursive so r (result) must initially be NULL.
(define (write-list-serialize o r)
 (if (pair? o)
     (if (and (eq? (car o) 'quote)
                 (pair? (cdr o)))
         (cons "'" (write-list-serialize (car (cdr o)) r))
         (cons "(" (write-list-serialize-list o (cons ")" r))))
 (if (vector? o)
     (if (eq? o #())
       (cons "#()" r)
       (write-list-serialize-vector o r))
 (cons (serialize-write o) r))))

; Serialize an object into a list of "write" strings.
(define (write->strings x)
  (write-list-serialize x ()))

; Serialize an object into a "write" string.
(define (write->string x)
  (apply string (write-list-serialize x ())))

(define (write x . p)
 (set! p (if (null? p) stdout
         (if (and (pair? p)
                  (port? (car p)))
             (car p)  ; Make sure 2nd arg is a port object
             (begin (send (display->string (list "WARNING: write " p " is not a port")) stdout)
                    stdout)))) ; Default port is STDIN.
 (for-each (lambda (s) (send s p))
           (write->strings x)))

(define (hex i) (number->string i 16))
;;
;; Serializing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; Little endian binary number I/O.  Send or receive
; 1, 2, and 3 byte numbers.  The implementation is
; a hack which adds string objects as if they were
; number objects (at the byte level).  #eof is returned
; if the entire word length can't be read
(define (recvByte p)
  (let ((n (recv 1 #f p)))
    (if (eof-object? n) n (+ n))))

(define (recvWord p)
  (let ((n (recv 2 #f p)))
    (if (or (!= 2 (string-length n))
            (eof-object? n))
        #eof
        (+ n))))

(define (recvLong p)
  (let ((n (recv 4 #f p)))
    (if (or (!= 4 (string-length n))
            (eof-object? n))
        #eof
        (+ n))))

(define (sendByte n p)
  (send (string (integer->char (logand n #b11111111))) p))

(define (sendWord n p)
  (send (string (integer->char (logand n #b11111111))
                (integer->char (logand (/ n 256) #b11111111))) p))

(define (sendLong n p)
  (send (string (integer->char (logand n #b11111111))
                (integer->char (logand (/ n 256) #b11111111))
                (integer->char (logand (/ n 65536) #b11111111))
                (integer->char (logand (/ n 16777216) #b11111111))) p))



;(define (socket-port s) (if (port? s) (vector-ref s 2) #f))

(define (seek-set port offset) (seek port offset 0))
(define (seek-current port offset) (seek port offset 1))
(define (seek-end port offset) (seek port offset 2))

(define (file-r-user? port)  (!= 0 (logand #b100000000 (vector-ref (file-stat port) 2))))
(define (file-w-user? port)  (!= 0 (logand #b010000000 (vector-ref (file-stat port) 2))))
(define (file-x-user? port)  (!= 0 (logand #b001000000 (vector-ref (file-stat port) 2))))
(define (file-r-group? port) (!= 0 (logand #b100000 (vector-ref (file-stat port) 2))))
(define (file-w-group? port) (!= 0 (logand #b010000 (vector-ref (file-stat port) 2))))
(define (file-x-group? port) (!= 0 (logand #b001000 (vector-ref (file-stat port) 2))))
(define (file-r-other? port) (!= 0 (logand #b100 (vector-ref (file-stat port) 2))))
(define (file-w-other? port) (!= 0 (logand #b010 (vector-ref (file-stat port) 2))))
(define (file-x-other? port) (!= 0 (logand #b001 (vector-ref (file-stat port) 2))))

(define (char->integer i) (+ 0 i)) ; TODO Hack that works currently
(define (integer->char i) (vector-ref characters i))

(define (char-upcase c) (if (and (<= 97 (+ 0 c)) (<= (+ 0 c) 122)) (integer->char (- (+ 0 c) 32)) c))
(define (char-downcase c) (if (and (<= 65 (+ 0 c)) (<= (+ 0 c) 90)) (integer->char (+ (+ 0 c) 32)) c))

(define CHAR-CTRL-@   (integer->char #x00))
(define CHAR-CTRL-A   (integer->char #x01))
(define CHAR-CTRL-B   (integer->char #x02))
(define CHAR-CTRL-C   (integer->char #x03))
(define CHAR-CTRL-D   (integer->char #x04))
(define CHAR-CTRL-E   (integer->char #x05))
(define CHAR-CTRL-F   (integer->char #x06))
(define CHAR-CTRL-G   (integer->char #x07))
(define CHAR-CTRL-H   (integer->char #x08))
(define CHAR-CTRL-I   (integer->char #x09))
(define CHAR-CTRL-J   (integer->char #x0a))
(define CHAR-CTRL-K   (integer->char #x0b))
(define CHAR-CTRL-L   (integer->char #x0c))
(define CHAR-CTRL-M   (integer->char #x0d))
(define CHAR-CTRL-Q   (integer->char #x11))
(define CHAR-CTRL-W   (integer->char #x17))
(define CHAR-ESC      (integer->char #x1b))
(define CHAR-CTRL-_   (integer->char #x1f))
(define CHAR-SPACE    (integer->char #x20))
(define CHAR-CTRL-?   (integer->char #x7f))

(define SPACE      CHAR-SPACE)
(define TAB        CHAR-CTRL-I)
(define NEWLINE    CHAR-CTRL-J)
(define RETURN     CHAR-CTRL-M)

(define (newline) (display NEWLINE))
(define (return)  (display RETURN))
(define (beep)    (send "\a" stdout))

(define (list . x) x)

; TODO FIX THIS HACK
(define char=? eq?)

(define char-whitespace-list (list SPACE TAB NEWLINE RETURN))
(define (char-numeric? c) (and (<= #\0 c) (<= c #\9)))
(define (char-whitespace? c) (pair? (memq c char-whitespace-list)))

(define (abs x) (if (< x 0) (- x) x))

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


; This creates symbols bound to closures implementing syntatic expressions
(define (cdr p) (cdr p))
(define (car p) (car p))


(define (length l) (if (null? l) 0 (+ 1 (length (cdr l)))))
(define (last l) (if (pair? l) (if (pair? (cdr l)) (last (cdr l)) (if (null? (cdr l)) (car l) (cdr l))) l))
(define (list-skip l i) (cond ((null? l) l) ((= 0 i) l) (else (list-skip (cdr l) (- i 1)))))

(define (car-string str) (string-ref str 0))
(define (cdr-string str) (substring str 1 (string-length str)))

(define (eqv? a b)
 (or (eq? a b)        ; Pointer (#\a #t #f () #() "")
     (= a b)          ; Numeric 1
     (string=? a b))) ; String

(define (equal? l1 l2)
 (or (eqv? l1 l2)
     (and (pair? l1)
          (pair? l2)
          (equal? (car l1) (car l2))
          (equal? (cdr l1) (cdr l2)))))

(define (neq? a b)    (not (eq? a b)))
(define (neqv? a b)   (not (eqv? a b)))
(define (nequal? a b) (not (equal? a b)))

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

(define (string->list s)
 (let ~ ((i 0)
         (l (string-length s)))
  (if (= i l) '()
      (cons (string-ref s i) (~ (+ i 1) l)))))

(define (vector->list v)
 (let ~ ((i 0)
         (l (vector-length v)))
  (if (= i l) '()
      (cons (vector-ref v i) (~ (+ i 1) l)))))

; Returns a pair containing the string split on the first delimeter character.
;   (strtok "abc-123" #\-)  =>  ("abc" . "123")
;   (strtok "abc=123" #\-)  =>  ("abc=123")
(define (strtok str delim)
 (let ((len (string-length str)))
 (let ~ ((i 0))
   (if (= i len) (cons str "")
   (if (eq? (string-ref str i) delim)
       (cons (substring str 0 i)
             (substring str (+ i 1) len))
   (~ (+ i 1)))))))

; Reverse strtok
;   (rstrtok "a-bc-123" #\-) => ("a-bc" . "123")
(define (rstrtok str delim)
 (let ((len (string-length str)))
 (let ~ ((i (- len 1)))
   (if (= i -1) (cons "" str)
   (if (eq? (string-ref str i) delim)
       (cons (substring str 0 i)
             (substring str (+ i 1) len))
   (~ (- i 1)))))))

; Split's a string into a list of substring separated by delim
(define (split str delim)
 (let ((len (string-length str)))
 (let ~ ((i 0))
   (if (= i len) (cons str ())
   (if (eq? (string-ref str i) delim)
       (cons (substring str 0 i)
             (split (substring str (+ i 1) len) delim))
   (~ (+ i 1)))))))

(define (string-downcase s)
 (letrec ((len (string-length s))
          (newStr (make-string len)))
   (let ~ ((i (- len 1)))
    (if (<= 0 i)
      (begin
        (string-set! newStr i (char-downcase (string-ref s i)))
        (~ (- i 1)))))
   newStr))
    
(define (string-upcase s)
 (letrec ((len (string-length s))
          (newStr (make-string len)))
   (let ~ ((i (- len 1)))
    (if (<= 0 i)
      (begin
        (string-set! newStr i (char-upcase (string-ref s i)))
        (~ (- i 1)))))
   newStr))
    
(define (make-vector-vector y x a)
 (let ((v (make-vector y ())))
  (let ~ ((i 0))
    (if (>= i y) v
        (begin (vector-set! v i (make-vector x a))
               (~ (+ i 1)))))))

(define (vector-vector-set! v x y o)
 (vector-set! (vector-ref v x) y o))

(define (vector-set-vector! v1 s v2 t)
 (let ~ ((a s)
         (b t))
   (if (and (< a (vector-length v1))
            (< b (vector-length v2)))
      (begin
        (vector-set! v1 a (vector-ref v2 b))
        (~ (+ a 1)
           (+ b 1))))))

(define (vector-set-list! v1 s lst)
 (let ~ ((i s)
         (l lst))
   (or (null? l) (begin
     (vector-set! v1 i (car l))
     (~ (+ i 1) (cdr l))))))

(define (filter p l)
 (if (null? l) ()
 (if (p (car l))
     (cons (car l) (filter p (cdr l)))
     (filter p (cdr l)))))

(define (filter-not p l)
 (if (null? l) ()
 (if (p (car l))
     (filter-not p (cdr l))
     (cons (car l) (filter-not p (cdr l))))))

(define (map f x)
 (if (null? x) ()
  (cons (f (car x))
        (map f (cdr x)))))

(define (for-each fn lst)
  (if (pair? lst)
      (begin (fn (car lst))
             (for-each fn (cdr lst)))))

; Same as for-each except arguments are switched
(define (each-for lst fn) (for-each fn lst))

(define (loop a fn)
 (let ~ ((i 0))
  (if (!= i a)
   (begin
     (fn i)
     (~ (+ i 1))))))

(define (loop1 a b fn)
 (let ~ ((a a))
  (if (!= a b)
   (begin
     (fn a)
     (~ (+ a 1))))))

; Call fn over the 2d coordinate range excluding y1 x1 values
(define (loop2 y0 y1 x0 x1 fn)
 (let ~ ((y y0) (x x0))
  (if (!= y y1)
    (if (= x x1)
      (~ (+ y 1) x0)
      (begin
        (fn y x)
        (~ y (+ x 1)))))))

(define (vector-vector-ref v i1 i2)
 (vector-ref (vector-ref v i1) i2))
 
(define (vector-for-each fn v)
 (let ~ ((i 0))
  (or (= i (vector-length v))
      (begin
        (fn (vector-ref v i))
        (~ (+ i 1))))))

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

(define (vector-vector-for-each fn vv)
 (let ~~ ((y 0)(x 0))
  (if (< y (vector-length vv)) ; Done with Y iteration?
    (let ((v (vector-ref vv y))) ; Consider row.
      (let ~ ((x 0))
        (if (< x (vector-length v)) ; Done with X iteration?
          (begin (fn (vector-ref v x))
                 (~ (+ x 1)))
          (~~ (+ y 1) 0))))
    vv)))                      ; Return vector-vector done.

(define (vector-random v)
 (vector-ref v (random (vector-length v))))

(define (make-list num . default)
 (set! default (if (pair? default) (car default) ()))
 (let ~ ((i 0))
   (if (= i num) ()
     (cons default (~ (+ i 1))))))

(define (list->vector l)
  (let ((v #()))
    (let ~ ((len 0)
            (lst l))
      (if (null? lst)
        (set! v (make-vector len))
        (begin
          (~ (+ len 1) (cdr lst))
          (vector-set! v len (car lst)))))
    v))

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

(define (vector-copy vec)
 (let ((newvec (make-vector (vector-length vec))))
   (vector-set-vector! newvec 0 vec 0)
   newvec))

(define (vector-reverse vec)
 (letrec ((len (vector-length vec))
          (v (make-vector len)))
  (let ~ ((i 0))
    (if (= i len) v ; return reversed vector
     (begin
      (vector-set! v (- len i 1) (vector-ref vec i))
      (~ (+ i 1)))))))

; Return new list omitting 1st occurance of e in l.
(define (list-delete l e)
 (if (pair? l)
   (if (equal? (car l) e)
       (cdr l)
       (cons (car l) (list-delete (cdr l) e)))
   l))
 


(define (writel . l)
 (let ~ ((l l))
 (if (not (null? l))
     (begin (write (car l))
            (~ (cdr l))))))
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

; TODO close file. Tail-call optimize.
(define (load file)
 ; Convert file to port if a string
 (if (string? file)
   (let ((fp (open-file file)))
     (if (port? fp)
         (load fp)
         (error "Can't load file" file)))
 (if (not (port? file))
     (error "Can't load argument" file)
     (let ~ ((expr ())
             (lastVal ()))
       (if (eof-object? expr)
         (begin
           (close file)
           lastVal) ; Load returns value of last expression
         (~ (read file)
            (eval expr)))))))
; Default error handlers for all thread IDs:  Shutdown the entire machine.
; It is a function of one argument because exception handler could be a
; continuation.
(define ERRORS (make-vector 1024 
  (lambda (o)
    (if (pair? o)
        (for-each (lambda (e) (display "\r\nEXCEPTION::") (write e))
                  o)
        (begin (display "\r\nEXCEPTION::") (write o)))
    (debugger)
    (unthread))))


; The read eval print loop along with an error/exception handler.
(define repl-input ())

(define (replloop)
  (display "\nwscm>")
  (set! repl-input (read stdin))
  (display (eval repl-input))
  (or (eof-object? repl-input) (replloop)))

(define (repl . WELCOME)
  (set! WELCOME (if (null? WELCOME) "World Scheme" (car WELCOME)))
  (let ((msg (call/cc (lambda (c) (vector-set! ERRORS (tid) c) WELCOME))))
    ; An exception during the REPL returns to this
    ; point with 'msg' assigned the error/info list
    (if (eq? WELCOME msg)
        (display WELCOME)
        (if (pair? msg)
            (for-each (lambda (e) (displayl "\nREPL-ERROR::" e))
                      msg)
            (display msg))))
  (replloop))

(define (signal-set num func)
 (vector-set! SIGNALHANDLERS num func)
 (signal num))

; If wscm is run with a command line argument that isn't a switch, then 
; assume it's a file to run otherwise begin the REPL.
(if (and (> (vector-length argv) 1)
         (not (string=? "-" (vector-ref argv 1))))
 (load (vector-ref argv 1))
 (repl "Welcome to \e[1;31mW\e[33mO\e[32mR\e[34mL\e[35mD\e[01;m.\e[?25l"))
