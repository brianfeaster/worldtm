;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Map Agent
;;   IPC
;;   Avatar
;;   Incomming_IPC_messages
;;   Start_everything
;;

;(load "window.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IPC
;;
(load "ipc.scm")
(define ipc (Ipc displayl))

(define (who)
 ((ipc 'qwrite)
 `(entity ,DNA ,(avatar 'port) ,(avatar 'name) ,@((avatar 'gps)) ,(avatar 'glyph))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Avatar
;;
(load "entity.scm")
(define DNA 17749)
(define NAME "The Map Agent")
(define avatar (Entity DNA (ipc 'PrivatePort) NAME 0 0 0 #(0 15 #\M 0 15 #\A)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Incomming_IPC_messages
;;
(define EntityDB ())

(define (entityCreate dna port name z y x glyph)
  (vector dna port name z y x glyph))

(define (entityDna e)   (vector-ref e 0))
(define (entityPort e)  (vector-ref e 1))
(define (entityName e)  (vector-ref e 2))
(define (entityZ e)     (vector-ref e 3))
(define (entityY e)     (vector-ref e 4))
(define (entityX e)     (vector-ref e 5))
(define (entityGlyph e) (vector-ref e 6))

(define (entityDBAdd e)
  (set! EntityDB (cons (cons (entityDna e) e) EntityDB)))

(define (entityDBUpdate dna port name z y x glyph)
  (let ((e (assv dna EntityDB)))
    (or (null? e)
      (set-cdr! e (entityCreate dna port name z y x glyph)))))

(define (entityLookup dna)
  (let ((e (assv dna EntityDB)))
    (if (null? e) UnknownEntity (cdr e))))

; Create and add some default entities
(define SystemEntity (entityCreate 0 0 "SYSTEM" 0 0 0 'NOGLYPH))
(define UnknownEntity (entityCreate 1 0 "UNKNOWN" 0 0 0 'NOGLYPH))
(entityDBAdd SystemEntity)

(define (entity dna port name z y x glyph) 
 (let ((e (entityLookup dna)))
  (if (eq? e UnknownEntity)
    (begin
      (entityDBAdd (entityCreate dna port name z y x glyph))
      (displayl "\n\e[31m" name " registerd\e[0m"))
    (begin
      (entityDBUpdate dna port name z y x glyph)
      (displayl "\n\e[31m" name " updated\e[0m")))))

(define (move dna . loc)
 (displayl "\n\e[32m" (entityLookup dna) " moves to " loc "\e[0m"))

(define (voice dna level text)
 (let ((e (entityLookup dna)))
  (displayl "\n\e[1;34m" (entityName e) " says:"text "\e[0m")
  (display (string (entityName e) "\t" text "\n") log)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start_everything
;;
(define log (open-file "talk.log"))
(seek-end log 0)

((ipc 'qwrite) '(who))
(thread 
 (let ((s (call/cc (lambda (c) (vector-set! ERRORS (tid) c) '*))))
    (or (eq? s '*) (displayl "\nIPC-REPL-ERROR::" s)))
 (let ~ () 
  (let ((e ((ipc 'qread))))
     (displayl "\n\e[1;30mIPC::" e "\e[0m")
     (eval e)
     (~))))

(repl)
