;;
;; Queues and Lists
;;

; A list is a vector containing a semaphore and the header/list pair.
; #(SEM *-)--->(list . ())
(define (ListCreate)
  (vector (open-semaphore 1) (list 'list)))

(define (ListDestroy l)
  (close-semaphore (vector-ref l 0)))

(define (ListGet l)
  (cdr (vector-ref l 1)))

; #(SEM *-)--->(list . *-)--->(1 . ())
(define (ListAdd l e)
  (semaphore-down (vector-ref l 0)) ; Critical section
  (let ((head (vector-ref l 1)))
    (set-cdr! head (cons e (cdr head))))
  (semaphore-up (vector-ref l 0)))

; Delete first list element that is eqv to e
(define (ListDel l e)
  (semaphore-down (vector-ref l 0)) ; Critical section
  (let ~ ((lst (vector-ref l 1))
          (nxt (cdr (vector-ref l 1))))
    (if (pair? nxt)
        (if (eqv? (car nxt) e)
            (set-cdr! lst (cdr nxt))
            (~ nxt (cdr nxt)))))
  (semaphore-up (vector-ref l 0)))

; Delete first list element which passes the call to pred
(define (ListDelFn l pred)
  (semaphore-down (vector-ref l 0)) ; Critical section
  (let ~ ((lst (vector-ref l 1))
          (nxt (cdr (vector-ref l 1))))
    (if (pair? nxt)
        (if (pred (car nxt))
            (set-cdr! lst (cdr nxt))
            (~ nxt (cdr nxt)))))
  (semaphore-up (vector-ref l 0)))



; Initially a queue is a vector where the first item
; points to the last pair that make up the list.
; This is considered an empty queue.  The last read
; item will continue to exist as a placeholder.
;
; The first semaphore serializes internal access
; and the second counts waiting queued messages.
;
; #( *  SEM  SEM  *-)---->( queue . () )
;    `--------------^
(define (QueueCreate)
  (let ((q (vector () (open-semaphore 1) (open-semaphore 0) (list 'queue))))
    (vector-set! q 0 (vector-ref q 3))
    q))

(define (QueueDestroy q)
  (close-semaphore (vector-ref q 1))
  (close-semaphore (vector-ref q 2)))

; Adding an itme:  #( *  SEM  *-)---->( queue . *-)---->( e . () )
;                     `--------------------------------^
(define (QueueAdd q e)
  (semaphore-down (vector-ref q 1)) ; Critical section
  (let ((oldTail (vector-ref q 0))
        (newTail (cons e ())))
    (set-cdr! oldTail newTail)
    (vector-set! q 0 newTail))
  (semaphore-up (vector-ref q 1))
  (semaphore-up (vector-ref q 2))) ; Increment the message counter semaphore

; Getting an item will skip the current (last one returned) and return the next.
; #( *  SEM  *-)------------------------->( e . () )
;    `-----------------------------------^
(define (QueueGet q)
  (semaphore-down (vector-ref q 2)) ; Try to decrement the message counter semaphore.  Returns when a char is available
  (semaphore-down (vector-ref q 1)) ; Critical section
  (if (eq? (vector-ref q 0) (vector-ref q 3))
    (begin
      (display "QueueGet on an empty queue!!!")
      (quit)))
  (vector-set! q 3 (cdr (vector-ref q 3)))
  (semaphore-up (vector-ref q 1))
  (car (vector-ref q 3)))

; If the first item in the list points to the second
; then the queue is considered empty.
(define (QueueEmpty? q)
  (semaphore-down (vector-ref q 1))  ; Critical section
  (let ((ret (eq? (vector-ref q 0) (vector-ref q 3))))
    (semaphore-up (vector-ref q 1))
    ret))
