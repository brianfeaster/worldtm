; Inspired by a make-vector system call bug which poped the fill
; value then called memNewVector which, if forced a garbage collection
; resulted in an invalid C pointer into a dead heap.
(let ~ ((c 0))
  (if (< c 1000000)
   (begin
    (+ (vector-ref (make-vector 7 (random 2)) 0))
    (~ (+ c 1)))))
