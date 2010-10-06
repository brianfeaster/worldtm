; Initiall a queue is a list where the first item
; points to the last pair that make up the list.
; This is considered an empty queue.  The last read
; item will continue to exist as a placeholder.
;
; [  *   |  *--]---->[ queue  |  ()  ]
;    `---------------^
;
(define QueueSemaphore (open-semaphore 1))

(define (QueueCreate)
  (let ((q (list () 'queue)))
    (set-car! q (cdr q))
    q))

; Adding an itme:
; [  *   |  *--]---->[ queue  |  *--]---->[ e  | () ]
;    `------------------------------------^
(define (QueueAdd q e)
  (semaphore-down QueueSemaphore)
  (let ((oldTail (car q))
        (newTail (cons e ())))
    (set-cdr! oldTail newTail)
    (set-car! q newTail))
  (semaphore-up QueueSemaphore))

; Getting an itme will skip the first one and return
; the last leaving it there.
; [  *   |  *--]------------------------->[ e  | () ]
;    `------------------------------------^
(define (QueueGet q)
  (semaphore-down QueueSemaphore)
  (if (eq? (car q) (cdr q)) (begin
   (display "QueueGet on an empty queue!!!")
   (quit)))
  (set-cdr! q (cddr q))
  (semaphore-up QueueSemaphore)
  (cadr q))

; If the first item in the list points to the second
; then the queue is considered empty.
(define (QueueEmpty? q)
  (semaphore-down QueueSemaphore)
  (let ((ret (eq? (car q) (cdr q))))
    (semaphore-up QueueSemaphore)
    ret))
