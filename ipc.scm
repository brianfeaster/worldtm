;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IPC II
;;
;; Star topology.  Hub is always listening on the first port 7155.
;;
(define (Ipc Display . port)
 (define HubPort (if (null? port) 7155 (car port))) ; IPC HUB port
 (define HubSocket ()) ; HUB socket
 (define MsgQueueSemaphore (open-semaphore 0))
 (define MsgQueue (list 'empty))  ; Queues are (empty) (ignore 1st 2nd)
 (define Peers ())     ; Peers are a list of #(socket (queue) queue-semaphore)
 (define PeersSemaphore (open-semaphore 1))

 (define (Displayl . l) (for-each Display l))

 (define (qwrite e) ; Append e to each peer's outgoing message queue.
  (map (lambda (p)
         (append! (vector-ref p 1) (list e))
         (semaphore-up (peerSemaphore p)))
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

 (define (peersDelete peer)
   (semaphore-down PeersSemaphore)
   (set! Peers (list-delete Peers peer)) ; Remove this peer from the global peer list.
   (semaphore-up PeersSemaphore))

 ; Peers are #(socket list-of-messages message-sempahore-counter)
 (define (peerCreate s)
   (vector s (list 'empty) (open-semaphore 0)))
 (define (peerSocket p) (vector-ref p 0))
 (define (peerMessages p) (vector-ref p 1))
 (define (peerSemaphore p) (vector-ref p 2))

 ; Spawn two threads that propogate any incomming messages on this
 ; socket as well as send out any queued messages.
 (define (spawnCommunicators stream)
   (Displayl "\r\n::(spawnCommunicators)  stream " stream)
   (display "'(World 5 2 0)" stream) ; Identify myself to peer.
   (let ((peer (peerCreate stream))) ; Create a peer: #(socket '(queue) queue-semaphore)
     (peersAdd peer)
     ; Read from peer and forward to every other peer's queue.
     (thread (let reader~ ((e (read (peerSocket peer))))
       (if (eof-object? e)
         (begin
           (Display "\r\n::(spawnCommunicators) Removing peer " peer)
           (peersDelete peer)
           (vector-set! peer 0 #f) ; BF TODO Required to remove socket from peer?
           (if (eof-object? HubSocket) (createHub)))
         (begin
           ; Relay message to every other peer
           (map (lambda (p)
                  (append! (peerMessages p) (list e))
                  (semaphore-up (peerSemaphore p)))
                (list-delete Peers peer))
           ; Add message to my local IO queue
           (msgQueueAdd e)
           (reader~ (read (peerSocket peer)))))))
     ; Write to peer anything in its queue.
     (thread (let writer~ ()
       (if (peerSocket peer)
         (begin
           (semaphore-down (peerSemaphore peer))
           (write (cadr (peerMessages peer))
                  (peerSocket peer))
           (send " " (peerSocket peer))
           (vector-set! peer 1 (cdr (peerMessages peer)))
           (writer~)))))))

 (define (createHub)
   (set! HubSocket (open-socket HubPort))
   (Displayl "\r\n::(createHub)  HubSocket=" HubSocket)
   (if (eof-object? HubSocket)
     ; Open communication to existing hub.  Eventually the threads
     ; handling I/O to the hub will close or fail and createHub will
     ; be called again.
     (spawnCommunicators (open-stream (open-socket "localhost" HubPort)))

     ; Acquired the hub socket port so act as the hub from now on.  Continusouly
     ; accept incomming peer connections and add to message queue.
     (begin
      (Displayl "\r\n::(createHub)  Accepting connections as hub port")
      (thread
       (let acceptPeers~ ()
        (spawnCommunicators (open-stream HubSocket))
        (acceptPeers~))))))

 ; DEBUG give a constant update of the object's state
 (rem thread (let ~ ()
  (Displayl "\n::Ipc peers")
  (map (lambda (s) (Displayl "\n  " s)) Peers)
  (sleep 1000)
  (~)))

 ; Start the engine
 (createHub)

(lambda (c) (eval c)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IPC
;;
;; Tree topology IPC network.  The ipc object will connect to the lowest
;; port number available and setup a listener for incomming child conections.
;; It will also scan down from its port for a parent to connect to.  The
;; lowest port connection will never have a parent connection.  Any other
;; port will attempt to find a parent and if none found will try to grab the
;; 'parent' port.  If successful it will 'dis-own' its children and become
;; the new parent.  The disowned children will need to find new parents.
;;
;; Rather than a time stamp, clients will only connect to ports below it.
;;
;; Ipc Object:
;;
;; The Ipc object when instantiated will attempt to connect to existing
(rem define (IpcTree Display . port)
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
  (Display "\r\nSpawning communicator on:" s)
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
     (Display "\r\nI'm the first peer and parent.")
   (if (< p FirstPort)
     (begin 
       (Display "\r\nNo parents and I'm not the first port!  Attempting to move incomming socket...")
       (let ((s (open-socket FirstPort)))
         (Display "\r\nopen-socket " FirstPort "=>" s "\r\n")
         (if (eof-object? s)
             (begin (Display "\r\nFirst port unavailable...attempting to find a parent again...")
                    (lookForParent))
             (begin (close MyIncommingSocket)
                    (set! ParentSocket ())
                    (set! MyIncommingSocket s)
                    (set! MyPort FirstPort)))))
     (begin
       (Display "\r\nChecking for parent client on port:" p)
         (let ((s (open-stream (open-socket "localhost" p))))
           (if (eof-object? s)
             (~ (- p 1))
             (begin
               (Display "\r\nConnecting to parent peer on port:" p)
               (set! ParentSocket s)
               (spawnCommunicators s)))))))))

 ; Have already created a local socket but not yet accepted any connections.
 ; Fire up thread that accepts incomming streams.
 (thread
   (let acceptChildren~ ()
     (Display "\r\nacceptChildren~")
     (spawnCommunicators (open-stream MyIncommingSocket))
     (acceptChildren~)))

 (Display "\r\nMy IPC listener port:" MyPort)

 (lookForParent)

(lambda (c) (eval c)))
