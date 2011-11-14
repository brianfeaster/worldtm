(display "\n--Welcome to unit test scmt.scm----------------")

; Inspired by a make-vector system call bug.  The code popped the fill
; value from the stack then called memNewVector which, if forced a garbage collection
; resulted in an invalid C pointer into a dead heap.
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

;(sleep 2000)


; Call open-socket with an invalid parameter but catch the error, display message and return 'OK.
(display "\nOpen-socket parameter exception")
(display
  (call/cc (lambda (c)
   (vector-set! ERRORS (tid) c)
   (open-socket 'x)
   (display "\nDid not catch exception")
   (quit -1))))

(display "\n                                                            PASS")

