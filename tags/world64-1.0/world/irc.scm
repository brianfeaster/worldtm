;(define CHANNEL "#not-world")
;(define NICK    "World")
;(define FRIENDS (list "firaelex" "erick" "timh" "hakk" "Shrewm" "strtok" "frisky"))
;(define FRIENDS ())
;(define (friend? x) (or (null? FRIENDS) (not (null? (memq from FRIENDS)))))
;(define LEADERS (list "ALTR" "QCOM" "GOOG" "XLNX" "BCSI" ))
; irc.choopa.net 6667
; irc.efnet.nl 6667
; irc.prison.net 6667
; irc.he.net 6667
; \x02 bold  \x16 inverse  \x1f underline
(load "world/world.scm")


(rem define (expandTabs line)
 (let ((split (strtok line "\t")))
   (if (null? (cdr split))
       (car split)
       (string (car split) "        " (expandTabs (cdr split))))))

; Parse "{nickname}? {command} {parameters}*" and return in a list.
(rem define (ircParseMessage msg)
 (let ~ ((nickname ())
         (command ())
         (parameters ()))
  ; Parse 'prefix' component to the message.
  (if (eq? #\: (string-ref msg 0))
    (let ((prefix&rest (strtok (cdr-string msg) #\ )))
      (set! nickname (car (strtok (car prefix&rest) #\!)))
      (set! msg (cdr prefix&rest))))
  ; Parse command component of message and parameters.
  (let ((cmd&par (strtok msg #\ )))
    (set! command (car cmd&par))
    (set! parameters (cdr cmd&par)))
  ; Debug output what we just parsed
  (Display "\c0;36m==> " nickname command parameters "\n")
  ; Return everything in a list
  (list nickname command parameters)))

; Given "{:.*} PRIVMSG {channel or my nick} ::content" eval the content
(rem define parsedMsg ())
(rem define (ircPrivmsg msg)
  (set! parsedMsg (ircParseMessage msg))
  (if (string=? "PRIVMSG" (cadr parsedMsg)) ; Is it a PRIVMSG 'command'?
    (let ((to ())
          (from ())
          (text ())
          (to&parameters (strtok (caddr parsedMsg) #\ )))
      (set! from (car parsedMsg))
      (set! to (car to&parameters))
      (set! text (cdr (strtok (cdr to&parameters) #\:)))
     (if (and (eq? (string-ref text 0) #\:)
              (friend? from))  ; Is it from a friend?
         ;(ircSend (string "PRIVMSG " CHANNEL " :" NICK " is trying to use a service he has not paid for!\r\n"))
         ; Does it begin with ":"?
         (begin
            (set! text (cdr-string text))   ; Remove the initial ":".
            (Display "\c0;37mEVAL " text)
            (Display "\c0;37mTO   " to " " NICK " " from "\n")
            (let ((ret (eval (read-string text)))) ; Evaluate the string.
              ; Don't wrie output if 'LEFIN sentinel.
              (if (!= ret 'LEFIN)
                  (ircSend (string "PRIVMSG "
                                   (if (string=? to NICK) from CHANNEL)
                                   " :" (write->string ret) "\r\n")))))))))

(rem define (ticker t)
 (let ((yahoo (open-stream (open-socket "finance.yahoo.com" 80))))
  (sleep 500)
  (if (not (eof-object? yahoo))
    (begin
      (send (string "GET /d/quotes.csv?s=" t "&f=nl1c1t1t7\n\n") yahoo)
      ; The above get request will get back from google a comma delimited
      ; list of strings, integers and floats.  How convenient.  That's
      ; practically a quasi-quote with a bunch of unquoted expression so
      ; we just slap on a `( and ) and evaluate.
      (sleep 500)
      (cons t (eval (read-string (return->space (string "`(" (recv 0 yahoo) ")" ) 0))))))))


(rem define (leader)
 (let ((victims (map (lambda (t) (display t)(cons (caddr t) (car t)))
                     (map ticker LEADERS))))
   (say (let ~ ((l (flattenList (sort victims))))
     (if (null? l) ""
       (string (serialize-display (cadr l)) "/" (serialize-display (car l))
               " " (~ (cddr l))))))
   LEFIN))
 
(rem define (sort lst)
 (if (null? lst) ()
 (let ((largest (largest lst () ())))
  (cons (car largest) (sort (cdr largest))))))

(rem define (largest lst big rest)
 (if (null? big) (largest (cdr lst) (car lst) ())
 (if (null? lst)     (cons big rest)
 (if (> (caar lst) (car big))
     (largest (cdr lst) (car lst) (cons big rest))
     (largest (cdr lst) big (cons (car lst) rest))))))

(rem define (flattenList l)
 (let ~ ((l l)
         (r ()))
  (if (pair? l) (~ (car l) (~ (cdr l) r))
  (if (null? l)
      r
      (cons l r)))))


(rem define (email person msg)
 (define e (open-stream (open-socket "dv8.org" 25)))
 (define (readall)
  (let ((c (recv 0 e)))
    (or (eq? "" c) (begin
     (write c)
     (readall)))))
 (sleep 500)
 (readall)
 (display "helo dv8.org\n" e)
 (readall)
 (display "mail from:world@dv8.org\n" e)
 (readall)
 (display (string "rcpt to:" person "@dv8.org\n") e)
 (readall)
 (display "data\n" e)
 (readall)
 (display msg e)
 (readall)
 (display msg) (display "\n" e)
 (readall)
 (display ".\n" e)
 (readall)
 (display "quit\n" e)
 (readall))

;(define (say x) (ircSend (string '"PRIVMSG " CHANNEL " :" (write->string x) "\r\n")) 'LEFIN)
;(define (love-me . x) (say (string "I lick " (car parsedMsg) "'s heart.")))


(define ipc (Ipc #f 7155))
(define irc (IrcAgent displayl "IRC" #x0 #xd8f #xae5 ipc))
(or irc (quit)) ; Quit if connection to irc server failed

(repl)
