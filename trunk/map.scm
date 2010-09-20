;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Map Agent
;;   Avatar
;;   IPC
;;   Start_everything
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Avatar
;;
(load "entity.scm")
(define DNA 17749)
(define NAME "The Map Agent")
(define avatar (Entity DNA NAME 0 0 0 #(0 15 #\M 0 15 #\A)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IPC
;;
(load "ipc.scm")
(define ipc (Ipc displayl))

(define (who)
 ((ipc 'qwrite)
 `(entity ,DNA ,(avatar 'name) ,@((avatar 'gps)) ,(avatar 'glyph))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start_everything
;;
(thread 
 (let ((s (call/cc (lambda (c) (vector-set! ERRORS (tid) c) '*))))
    (or (eq? s '*) (displayl "\nIPC-REPL-ERROR::" s)))
 (let ~ () 
  (let ((e ((ipc 'qread))))
     (displayl "\nIPC::" e)
     (eval e)
     (~))))

(repl)
