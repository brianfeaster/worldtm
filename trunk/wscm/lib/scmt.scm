(display "\n--Welcome to unit test scmt.scm----------------")
(define SCMTPASS #t)


;
; Inspired by a make-vector system call bug.  The code popped the fill
; value from the stack then called memNewVector which, if forced a garbage collection
; resulted in an invalid C pointer into a dead heap.
;
(display "\nMake-vector syscall with garbage collection")
(let ~ ((c 0))
  (if (< c 1000000)
   (begin
    (+ (vector-ref (make-vector 7 (random 2)) 0))
    (~ (+ c 1)))))

; Verify thread call returns
(display "\nSimple thread creation")
(display (thread 9))

; Verify too many threads won't crash wscheme
(display "\nVerify thread overflow exception caught")
(let ~ ((i 0))
 (cond ((= i 3000) (error "\nCan't verify thread overflow with " i " thread calls") (quit -1))
       ((thread (sleep 1000)) (~ (+ i 1)))))



;
; Call open-socket with an invalid parameter but catch the error, display message and return 'OK.
;
(display "\nOpen-socket parameter exception")
(display
  (call/cc (lambda (c)
   (vector-set! ERRORS (tid) c)
   (open-socket 'x)
   (display "\nDid not catch exception")
   (quit -1))))



;
; Write a series of binary numbers to a file then read them back.
;
(display "\ntestRecvSendByteWordLong")
(let testRecvSendByteWordLong ()
 (define step 15355377)
 (define p (open-new-file "z"))
 (define c 0)

 (loop 256 (lambda (n) (sendLong (* step n) p)))
 (close p)
 (set! p (open-file "z"))

 (loop 256 (lambda (n)
   (set! n (recvLong p))
   (if (!= n c) (begin
     (error "(testRecvSendByteWordLong): invalid match")))
   (set! c (+ step c)))))

(or (load "lib/adtt.scm") 
 (begin
   (error "\n\nadtt.scm test FAIL\n"))
   (set! SCMTPASS #f))


;
; Update a store object
;
(load "lib/store.scm")

; Open or create a new store
(define store (Store "storetest"))

; Create a new key/value if a new store
(or ((store 'exist?) 'val)
    ((store 'add) 'val 0))

; Get and increment value
(define val ((store 'get) 'val))
(set! val (+ 1 val))

; Save value back to store and shtudown
((store 'set) 'val val)
((store 'shutdown))

; Reopen store and verify store saved new value
(set! store (Store "storetest" displayl))

(or (eqv? val ((store 'get) 'val))
    (begin (displayl "\nERROR:  store's val = " ((store 'get) 'val))
           (set! SCMTPASS #f)))




(display "\n                                                            ")
(displayl (if SCMTPASS "PASS" "FAIL") "\n")
(displayl "argv=" argv)
