;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IPC
;;
;; Moving from star-topology to tree.  The ipc object will connect to the
;; lowest port number available and setup a listener for incomming child
;; connections.  It will also scan down from its port for a parent to connect
;; to.  The lowest port connection will never have a parent connection.  Any
;; other port will attempt to find a parent and if none found will try to grab
;; the 'parent' port.  If successful it will 'dis-own' its children and become
;; the new parent.  The disowned children will need to find new parents.
;;
;; Rather than a time stamp, clients will only connect to ports below it.
;;
;; Ipc Object:
;;
;; The Ipc object when instantiated will attempt to connect to existing
(define (Ipc Display . port)
 (define FirstPort (if (null? port) 7155 (car port)))
 (define PortCount 50) ; Only allow 50 Worldlians per server
 (define LastPort  (+ FirstPort PortCount -1))
 (define MyPort FirstPort)
 (define MyIncommingSocket
   (let findAvailablePort~ ()
    (let ((s (open-socket MyPort)))
     (if (eof-object? s)
         (begin
           (set! MyPort (+ 1 MyPort))
           (if (= MyPort LastPort)
               (begin (display "ERROR: Ipc: exceeded maximum port")
                      (quit))
               (findAvailablePort~)))
         s))))
 (define ParentSocket ())
 (define Peers ()) ; Peers are #(socket (queue) queue-semaphore)
 (define PeerCount 0)
 (define MsgQueueSemaphore (open-semaphore 0))
 (define MsgQueue (list 'empty))  ; Queues are (empty) (ignore 1st 2nd)

 (define (qwrite e) ; Append e to each peer's outgoing message queue.
  (map (lambda (p)
         (append! (vector-ref p 1) (list e))
         (semaphore-up (vector-ref p 2)))
        Peers)
  (msgQueueAdd e))

 (define (qread)    ; Read from my incomming message queue.
   (semaphore-down MsgQueueSemaphore)
   (let ((e (cadr MsgQueue)))
     (set! MsgQueue (cdr MsgQueue))
     e))

 (define (msgQueueAdd e)
   (append! MsgQueue (list e))
   (semaphore-up MsgQueueSemaphore))
  
 (define (peersAdd peer)
   (set! Peers (cons peer Peers)))

 (define PeersSemaphore (open-semaphore 1))

 (define (peersDelete peer)
   (semaphore-down PeersSemaphore)
   (set! Peers (list-delete Peers peer))
   (semaphore-up PeersSemaphore))

 ; Spawn two threads that propogate any incomming messages on this
 ; socket as well as send out any queued messages.
 (define (spawnCommunicators s)
  (Display "\r\n\aSpawning communicator on:" s "\r\n")
  (or (eof-object? s) (begin
   (display "'World1.0\r\n" s)
   ; Create a peer: #(socket '(queue) queue-semaphore)
   (let ((peer (vector s (list 'empty) (open-semaphore 0))))
     (peersAdd peer)
     ;(printl "Peers list now:" Peers)
     ; Read from peer and re-send to every other peer.
     ;(printl "spawning reader")
     (thread (let reader~ ((e (read (vector-ref peer 0))))
       ;(printl "Communicator: reader: received:" e)
       ;(printl "Appending original peers list:"Peers)
       ;(printl "Appending to peers:"(list-delete Peers peer))
       (if (eof-object? e)
         (begin
           (Display "\r\nRemoving communicator on:" s)
           (if (eq? ParentSocket (vector-ref peer 0))
             (begin (peersDelete peer)
                    (vector-set! peer 0 #f)
                    (lookForParent))
             (peersDelete peer)))
         (begin
           ; Relay message to every other peer
           (map (lambda (p)
                  (append! (vector-ref p 1) (list e))
                  (semaphore-up (vector-ref p 2)))
                (list-delete Peers peer))
           ; Add message to my local IO queue
           (msgQueueAdd e)
           (reader~ (read (vector-ref peer 0)))))))
     ; Write to peer anything in its queue.
     (thread (let writer~ ()
       (if (vector-ref peer 0)
         (begin
           (semaphore-down (vector-ref peer 2))
           (write (cadr (vector-ref peer 1))
                  (vector-ref peer 0))
           (send " " (vector-ref peer 0))
           (vector-set! peer 1 (cdr (vector-ref peer 1)))
           (writer~)))))))))

 ; Connect to first existing peer listening on a port below mine.
 (define (lookForParent)
  (let ~ ((p (- MyPort 1)))
   (if (= MyPort FirstPort)
     (Display "I'm the first peer and parent.\r\n")
   (if (< p FirstPort)
     (begin 
       (Display "No parents and I'm not the first port!  Attempting to move incomming socket...\r\n")
       (let ((s (open-socket FirstPort)))
         (Display "open-socket " FirstPort "=>" s "\r\n")
         (if (eof-object? s)
             (begin (Display "First port unavailable...attempting to find a parent again...\r\n")
                    (lookForParent))
             (begin (close MyIncommingSocket)
                    (set! ParentSocket ())
                    (set! MyIncommingSocket s)
                    (set! MyPort FirstPort)))))
     (begin
       (Display "Checking for parent client on port:" p "\r\n")
         (let ((s (open-stream (open-socket "localhost" p))))
           (if (eof-object? s)
             (~ (- p 1))
             (begin
               (Display "Connecting to parent peer on port:" p "\r\n")
               (set! ParentSocket s)
               (spawnCommunicators s)))))))))

 ; Have already created a local socket but not yet accepted any connections.
 ; Fire up thread that accepts incomming streams.
 (thread
   (let acceptChildren~ ()
     (Display "acceptChildren~\r\n")
     (spawnCommunicators (open-stream MyIncommingSocket))
     (acceptChildren~)))

 (Display "My IPC listener port:" MyPort "\r\n")

 (lookForParent)

(lambda (c) (eval c)))
