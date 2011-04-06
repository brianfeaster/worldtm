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


(define (IrcAgent DisplayFunction)
 (define (self msg) (eval msg))
 ; Locals
 (define (Debug . l) (and Debug (for-each DisplayFunction l)))
 (define Server "irc.choopa.net")
 (define Port 6667)
 (define err_433_NicknameInUse #f)
 (define Nick "world")
 (define portIRC #f)
 (define channel "#worldtm")
 (define msgs (QueueCreate))
 (define avatar #f)
 ; Members
 (define (connectToIRCserver)
   (set! portIRC (open-stream (open-socket Server Port)))
   portIRC)
 (define (say . l)
   (display "PRIVMSG " portIRC)
   (display channel portIRC)
   (display " :" portIRC)
   (display (apply string (map (lambda (e) (apply string (display->strings e))) l)) portIRC)
   (display "\r\n" portIRC))
 (define (send . l)
    (Debug "\r\n" (apply string l))
    (map (lambda (s) (display s portIRC)) l)
    (display "\r\n" portIRC))
 ; Pushes a parsed IRC message, vector of strings, to queue.  Might also be #eof and a raw string.
 ; IRC message interface
 (define (msgPrefix m) (vector-ref m 0))
 (define (msgCommand m) (vector-ref m 1))
 (define (msgParameters m) (vector-ref m 2))
 (define (addToQueue s)
   ;(Debug "\r\n" s)
   (QueueAdd msgs s))
 ; Parse an IRC message, vector of strings, from a message string
 (define (parseMsgString ms)
   (let ((newMsg (vector #f #f #f)))
     ; Parse possible PREFIX part of message
     (if (eq? #\: (string-ref ms 0))
       (let ((toks (strtok (cdr-string ms) #\ )))
         (vector-set! newMsg 0 (car toks))
         (set! ms (cdr toks)))) ; Remove prefix from ms string
     ; Parse COMMAND and PARAMS parts of message
     (let ((toks (strtok ms #\ )))
         (vector-set! newMsg 1 (car toks))
         (vector-set! newMsg 2 (cdr toks)))
     newMsg))
 ; Scan IRC stream from portIRC and add strings to msgs queue
 (define (scanStream)
   (define buff (make-string 512)) ; IRC enforced 512 character limit (including trailing \r\n) for messages
   (let ~ ((c (read-char #f portIRC))
           (i 0))
     (cond ((eof-object? c)
            ; #EOF Connection lost/closed to IRC server.  Add remaining buffer as a raw string and #EOF to queue.
            (or (= i 0) (addToQueue (substring buff 0 i)))
            (addToQueue c)) ; Add #eof
           ((or (eq? c RETURN) (eq? c NEWLINE))
            ; "\r\n" Line terminator scanned.  Parse and queue if at least one character in buffer.
            (if (!= i 0) (addToQueue (parseMsgString (substring buff 0 i))))
            (~ (read-char #f portIRC) 0))
           (else
            ; Continue scanning the line
            (string-set! buff i c)
            (~ (read-char #f portIRC) (+ i 1))))))
 ; Dispatch on queued IRC messages
 (define (msgsDispatcher)
   (let ((ircMsg (QueueGet msgs)))
     (if (eof-object? ircMsg)
       (Debug "\r\nmsgsDispatcher: #eof from queue. halting")
     (begin
       (if (not (vector? ircMsg))
         (Debug "\r\nmsgsDispatcher: not a valid parsed IRC message: " ircMsg)
         (letrec ((prefix (vector-ref ircMsg 0)) ; Consider message components
                  (command (vector-ref ircMsg 1))
                  (parameters (vector-ref ircMsg 2)))
           (if prefix (Debug "\r\n" prefix " ") (Debug "\r\n")) ; Debug
           (Debug "" command " " parameters)
           ; Ping event TODO temporary
           (cond ((eqv? command "PING")
                  (send "PONG " parameters))
                 ((eqv? command "PRIVMSG")
                  ((avatar 'speak) (string (car (strtok prefix #\!))
                                           " "
                                           (cdr (strtok parameters #\:))))))))
       (msgsDispatcher)))))
 ; TODO
 ;  Implement error message dispatcher
 ;  Implement state machine which sets user only after a nick is set.  Use semaphore to block flow.
 (define (establishNickname nick)
   (set! err_433_NicknameInUse #f)
   (send "NICK " nick)
   (sleep 500)
   (if err_433_NicknameInUse
     (let ~ ((nick (string nick #\_))
             (next 101))
       (send "NICK " nick next)
       (sleep 500))))
 (define (IPCvoice dna level text)
    (if (= dna 0) ; System messages
      (say "WORLD " text)
      (letrec ((entity (((avatar 'myMap) 'entityDBGet) dna))
               (dist (if entity (distance ((entity 'gps)) ((avatar 'gps))))))
        (if (and entity (not (eq? entity avatar)) (< dist level))
            (say (entity 'name) " " text)))))
 (define (start)
   (if (connectToIRCserver) (begin ; Make the connection
     (Debug "\r\n::IrcAgent connected!  Starting scanStream loops")
     (set! avatar (Avatar "IRC" 1 3462 2770 ipc 'NOVIEWPORT)) ; Create the avatar
     (thread (scanStream))
     (thread (msgsDispatcher))
     ; Send nick until server doesn't respond with a nick error
     (establishNickname Nick)
     (sleep 500)
     (send "USER world 0 * :The World[tm] agent")
     (sleep 500)
     (send "JOIN " channel)
     ((avatar 'setIPCvoice) IPCvoice))))
 ; Main
 (start)
 self)

;(load "adt.scm")
;(define irc (IrcAgent display))
;(or irc (quit)) ; Quit if connection to irc server failed
;(repl)
