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
(load "world.scm")

(define IRCPRENAME "\x16(")
(define IRCPOSTNAME ")\x16")

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


(define (IrcAgent Debug name z y x ipc NOVIEWPORT . ChildStack)
 (apply Avatar name z y x ipc NOVIEWPORT
  (list Debug)
  (macro (parent Debug . ChildStack) ; Child
   (define (self msg) (eval msg))
   (define (info) (list 'IrcAgent name z y x 'hasChild (pair? ChildStack)))
   ; Locals
   (define portIRC #f)
   (define Server "irc.choopa.net")
   (define Port 6667)
   (define Nick "world")
   (define Nicks (BListCreate "w0rld" "worldtm" "world[tm]" "w0rld[tm]" "w0rldtm" "w[tm]rld" "w[]rld"))
   (define channel "#worldtm")
   (define msgs (QueueCreate))
   ; Members
   (define (connectToIRCserver)
     (set! portIRC (open-stream (open-socket Server Port)))
     portIRC)
   (define (send . l)
      (Debug "\r\n<" (serialize-write (apply string l)) ">")
      (map (lambda (s) (display s portIRC)) l)
      (display "\r\n" portIRC))
   (define (recvChar)
     (read-char #f portIRC))
   ; IRC message interface
   (define (msgNew) (vector #f #f #f))
   (define (msgPrefix m) (vector-ref m 0))
   (define (msgCommand m) (vector-ref m 1))
   (define (msgParameters m) (vector-ref m 2))
   (define (msgQueueAdd s) (QueueAdd msgs s)) ; Generally adds a parsed IRC message.  Could be a string or #eof.
   (define (msgQueueGet) (QueueGet msgs))
   (define (debugDumpMsg msg)
     (Debug (if (msgPrefix msg) (string "\r\n[" (msgPrefix msg) "]") "\r\n")
            "[" (msgCommand msg) "]"
            "[" (serialize-write (msgParameters msg)) "]"))
   ; Parse an IRC message, vector of strings, from a message string
   (define (parseMsgString ms)
     (let ((newMsg (msgNew))) ; Create new message container #(prefix command parameters)
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
   ; Scan IRC stream a character at a time and add each line to the queue
   (define (scanStream)
      (define buff (make-string 512)) ; IRC enforced 512 character limit (including trailing \r\n) for messages
      (let ~ ((ch (recvChar))
              (num 0))
        (cond ((eof-object? ch)
               ; #EOF-connection lost/closed to IRC server.  Add remaining buffer as a raw string and #EOF to queue.
               (or (= num 0) (msgQueueAdd (substring buff 0 num)))
               (msgQueueAdd ch)) ; Add #eof
              ((or (eq? ch RETURN) (eq? ch NEWLINE))
               ; "\r\n"-line terminator scanned.  Add to queue if at least one character scanned.
               (if (< 0 num) (msgQueueAdd (parseMsgString (substring buff 0 num))))
               (~ (recvChar) 0))
              (else
               ; Char-continue scanning a line
               (string-set! buff num ch)
               (~ (recvChar) (+ num 1))))))
   (define (cmdPING parameters)
      (send "PONG " parameters))
   (define (cmdTOPIC prefix parameters)
       (speak
        (string (car (strtok prefix #\!)) " changed the topic on "
                (car (strtok parameters #\:)) " to " (cdr (strtok parameters #\:)))))
   ; [Shrewm!~worlda@li54-107.members.linode.com][PRIVMSG]["#not-world :it's world!"]
   (define (cmdPRIVMSG prefix parameters)
        (speak (string (car (strtok prefix #\!))
                                 (let ((chan (car (strtok parameters #\ ))))
                                   (if (string=? channel chan) "" chan))
                                 " " (cdr (strtok parameters #\:)))))
   (define (cmdNICK prefix parameters)
      (if (string=? Nick (car (strtok prefix #\!))) ; "nick!.....com"
        (begin
         (set! Nick (cdr (strtok parameters #\:))) ; ":newnick"
         (speak (string "my new IRC nickname is " Nick)))))
   ; Received the error message that the nick I am assuming is invalid so try a new one
   (define (cmd433 parameters) ; ERR_NICKNAMEINUSE
     (letrec ((s (strtok parameters #\ )) ; ("toNick" . "desiredNick :Nickname is already in use.")
              (toNick (car s))
              (desiredNick (car (strtok (cdr s) #\ ))))
        (if (string=? Nick desiredNick)
          (begin
            (BListAddBack Nicks Nick)
            (set! Nick (BListDelFront Nicks))
            (Debug "\r\nTrying to register with next prefered nick="Nick " " (BListList Nicks))
            (send "NICK " Nick)))))
  
   ; Joining multiple channels.
   ;  <"JOIN #worldtm,#not-world">
   ;   [world!~world@li54-107.members.linode.com] [JOIN] [":#worldtm"]
   ;   [irc.choopa.net] [332] ["world #worldtm :The World[tm] >---< IRC[k] gateway"]
   ;   [irc.choopa.net] [333] ["world #worldtm shrewm!~shroom@li54-107.members.linode.com 1302287052"]
   ;   [irc.choopa.net] [353] ["world = #worldtm :world shrewm @strtok"]
   ;   [irc.choopa.net] [366] ["world #worldtm :End of /NAMES list."]
   ; 
   ;   [world!~world@li54-107.members.linode.com] [JOIN] [":#not-world"]
   ;   [irc.choopa.net] [332] ["world #not-world :World:  world.dv8.org [telnet-port 7154] [ssh-user world]"]
   ;   [irc.choopa.net] [333] ["world #not-world Shrewm!~shroom@li54-107.members.linode.com 1293489012"]
   ;   [irc.choopa.net] [353] ["world @ #not-world :world tangles__ tangles strtok @zumthing @eap sprocket"]
   ;   [irc.choopa.net] [366] ["world #not-world :End of /NAMES list."]
  
  
   ; The agent joins then parts a channel.  Shrewm joins the channel
   ;  [world!~world@li54-107.members.linode.com] [JOIN] [":#worldtm"]
   ;  [world!~world@li54-107.members.linode.com] [PART] ["#worldtm"]
   ;  [shrewm!~worlda@li54-107.members.linode.com] [JOIN] [":#worldtm"]
   ;  [shrewm!~worlda@li54-107.members.linode.com][PART]["#worldtm"]
  
   ; The topic for the channel.
   ;   [irc.choopa.net] [332] ["world #worldtm :The World[tm] >---< IRC[k] gateway"]
   ;   [irc.choopa.net] [333] ["world #worldtm shrewm!~shroom@li54-107.members.linode.com 1302287052"]
  
   ; The users in the channel.
   ;   [irc.choopa.net] [353] ["world = #worldtm :world shrewm @strtok"]
   ;   [irc.choopa.net] [366] ["world #worldtm :End of /NAMES list."]
  
   (define (cmdJOIN prefix parameters)
     (let ((joinee (car (strtok prefix #\!))))
       (if (string=? Nick joinee)
         ; I have joined an IRC channel for the first time
         (begin
           (sleep 1000)
           (speak "Joined channel " parameters)
           )
         (speak (string joinee " has joined " (cdr (strtok parameters #\#)))))))
   (define (cmdPART prefix parameters)
     (speak (string (car (strtok prefix #\!)) " has left " (cdr (strtok parameters #\#)))))
   ; [tangles_!~android@m630e36d0.tmodns.net][QUIT][":Ping timeout: 268 seconds"]
   (define (cmdQUIT prefix parameters)
     (speak (string (car (strtok prefix #\!)) " quits ")))
   ; Dispatch on queued IRC messages
   (define (msgsDispatcher)
     (let ((ircMsg (msgQueueGet)))
       (if (eof-object? ircMsg)
         (Debug "\r\nmsgsDispatcher: #eof from queue. halting")
       (begin
         (if (vector? ircMsg)
           (letrec ((prefix (vector-ref ircMsg 0)) ; Consider message components
                    (command (vector-ref ircMsg 1))
                    (parameters (vector-ref ircMsg 2)))
             (debugDumpMsg ircMsg)
             (cond ((eqv? command "PING")   (cmdPING           parameters))
                   ((eqv? command "TOPIC")  (cmdTOPIC   prefix parameters))
                   ((eqv? command "PRIVMSG")(cmdPRIVMSG prefix parameters))
                   ((eqv? command "NICK")   (cmdNICK    prefix parameters))
                   ((eqv? command "433")    (cmd433            parameters)) ; ERR_NICKNAMEINUSE
                   ((eqv? command "JOIN")   (cmdJOIN    prefix parameters))
                   ((eqv? command "PART")   (cmdPART    prefix parameters))
                   ((eqv? command "JOIN")   (cmdQUIT    prefix))))
           (Debug "\r\nmsgsDispatcher: not a valid parsed IRC message: " ircMsg))
         (msgsDispatcher)))))
   (define (IPCvoice adna level text)
    (displayl "::IPCvoice\r\n")
      (if (or (= adna 0) (= adna dna))
        () ; Ignore system messages
        (letrec ((entity ((myMap 'entityDBGet) adna))
                 (dist (if entity (distance ((entity 'gps)) (gps)))))
          (if (and entity (not (eq? entity self)) (< dist level))
              (say IRCPRENAME (entity 'name) IRCPOSTNAME " " text)))))
   (define (say . l)
     (apply send "PRIVMSG " channel " :" (map (lambda (e) (apply string (display->strings e))) l)))
   (define (main)
     (display "IrcAgent.main")
     (if (connectToIRCserver) (begin ; Make the connection
       (setIPCvoice IPCvoice)
       (Debug "\r\n::IrcAgent connected!  Starting scanStream loops")
       (thread (scanStream))
       (thread (msgsDispatcher))
       (send "USER world 0 * :The World[tm] agent")
       (send "NICK " Nick)
       (sleep 500)
       (send "JOIN " channel))))
   ; WOOEE
   (if (pair? ChildStack)
     ; childstack = ((child parameters) child-macro . reset of child stack)
     (apply (cadr ChildStack) self (append (car ChildStack) (cddr ChildStack)))
     (begin
       (let ~ ((obj self))
         (if (obj 'parent) (~ (obj 'parent)))
         ((obj 'main)))
       self)))
  ChildStack)) ; IrcAgent

(define ipc (Ipc #f 7155))
(define irc (IrcAgent displayl "IRC" #x0 #xd8f #xae5 ipc #t))
(or irc (quit)) ; Quit if connection to irc server failed

(repl)
