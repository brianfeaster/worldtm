; Inspired by a make-vector system call bug.  The code popped the fill
; value from the stack then called memNewVector which, if forced a garbage collection
; resulted in an invalid C pointer into a dead heap.
(let ~ ((c 0))
  (if (< c 1000000)
   (begin
    (+ (vector-ref (make-vector 7 (random 2)) 0))
    (~ (+ c 1)))))

; Verify too many threads won't crash wscheme
(loop 1030 (lambda (t) (thread (sleep 1000) (displayl t #\ )))) (sleep 2000)

; Call open-socket with an invalid parameter but catch the error, display message and return 'OK.
(display
  (call/cc (lambda (c)
    (vector-set! ERRORS (tid) (lambda (m) (display m) (c 'OK)))
    (open-socket 'x)
    (display 'ERROR))))
