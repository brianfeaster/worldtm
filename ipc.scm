;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IPC Object
;;
;; Star topology.  Hub is always listening on the initial hub port.  Peers
;; will connect to an existing hub if they can not acquire the hub port.  Each
;; peer and hub will also have a "single connection, single read" listener.
;;
(load "queue.scm")

(define (Ipc DisplayFunction . portNum)
 (define Debug (and DisplayFunction)) ; Can pass in #f to disable any debug messages
 (define (Display . l) (and Debug (for-each DisplayFunction l)))
 (define HubPort (if (null? portNum) 7155 (car portNum)))
 (define HubSocket #eof) ; If not eof then this IPC instance is the hub socket.
 (define PrivatePort (+ 1 HubPort))
 (define PrivateSocket
   (let ~ ()
    (let ((s (open-socket PrivatePort)))
     (if (port? s) s
         (begin
           (set! PrivatePort (+ 1 PrivatePort))
           (if (< (+ 50 HubPort) PrivatePort)
               (begin (display "ERROR: Ipc: exceeded maximum port")
                      (quit))
               (~)))))))

 ; == Peer structure ==========================================================
 ; A peer is a  #(socket  message-queue  message-queue-semaphore)
 (define (peerCreate s) (vector s (QueueCreate) (open-semaphore 0)))
 (define (peerSocket p) (vector-ref p 0))
 (define (peerQueue p) (vector-ref p 1))
 (define (peerSemaphore p) (vector-ref p 2))

 ; == Peer lists ==============================================================
 (define PeerList ())
 (define PeersSemaphore (open-semaphore 1))

 (define (peersAdd peer)
   (semaphore-down PeersSemaphore)
   (set! PeerList (cons peer PeerList))
   (semaphore-up PeersSemaphore))

 (define (peersDelete peer)
   (semaphore-down PeersSemaphore)
   (set! PeerList (list-delete PeerList peer))
   (semaphore-up PeersSemaphore))

 ;=== Message_Queues ==========================================================
 (define MsgQueue (QueueCreate))
 (define MsgQueueSemaphore (open-semaphore 0))

 (define (qread)    ; Read from my incomming message queue.
   (semaphore-down MsgQueueSemaphore)
   (QueueGet MsgQueue))

 ; Append e to each peer's outgoing message queue as well as my own.
 (define (qwrite e)
  (map (lambda (p)
         (QueueAdd (peerQueue p) e)
         (semaphore-up (peerSemaphore p)))
       PeerList)
  (msgQueueAdd e))

 (define (msgQueueAdd e)
   (Display e)
   (QueueAdd MsgQueue e)
   (semaphore-up MsgQueueSemaphore))

 ; == Socket communication ====================================================

 ; Read from peer's socket and add to this object's I/O queue as well as every
 ; other queue in the peer list.
 (define (peerReaderLoop peer)
   (Display "\r\n::(peerReaderLoop) " peer "  ")
   (let ~ ((e (read (peerSocket peer))))
     (if (eof-object? e)
       (begin
         (peersDelete peer)
         (vector-set! peer 0 #f)) ; BF TODO Required to remove socket from peer?
       (begin
         ; Relay message to every other peer
         (map (lambda (p)
                (QueueAdd (peerQueue p) e)
                (semaphore-up (peerSemaphore p)))
              (list-delete PeerList peer))
         ; Add message to my local I/O queue
         (msgQueueAdd e)
         (~ (read (peerSocket peer)))))))

 ; Write to peer's socket anything in its queue.
 (define (peerWriterLoop peer)
   (Display "\r\n::(peerWriterLoop) " peer "  ")
   (let ~ ()
     (if (peerSocket peer) (begin
       (semaphore-down (peerSemaphore peer))
       (write (QueueGet (peerQueue peer))
              (peerSocket peer))
       (send " " (peerSocket peer))
       (~)))))

 (define (createHub)
   (set! HubSocket (open-socket HubPort))
   (if (eof-object? HubSocket)
     ; Open a connection to existing hub port.  Eventually the threads
     ; handling I/O to the hub will close or fail and createHub will
     ; be called again.
     (let ((hub (peerCreate (open-stream (open-socket "localhost" HubPort)))))
       (Display "\r\n::(createHub) Connected to hub " hub "  ")
       (display "'(World 5 3 0)" (peerSocket hub)) ; Identify myself to the hub.
       (peersAdd hub)
       (thread
         (peerWriterLoop hub))
       (thread
         (peerReaderLoop hub)
         (Display "\r\n::(createHub) Disconnected from hub " hub "  ")
         (createHub)))

     ; Acquired the hub socket port so act as the hub from now on.  Continusouly
     ; accept incomming peer connections and add to message queue.
     (thread
      (Display "\r\n::(createHub) Hub accepting peer connections  ")
      (let ~ ()
        (let ((peer (peerCreate (open-stream HubSocket))))
          (Display "\r\n::(createHub) Hub accepted peer " peer "  ")
          (display "'(World 5 3 0)" (peerSocket peer)) ; Identify myself to the hub.
          (peersAdd peer)
          (thread
            (peerWriterLoop peer))
          (thread
            (peerReaderLoop peer)
            (Display "\r\n::(createHub) Hub Disconnected from peer " peer "  "))
          (~))))))

 (define (private port msg)
   (letrec ((p (open-socket "localhost" port))
            (s (open-stream p)))
     (if (eof-object? s)
       (displayl "\r\nCan't send private message to " port ".  stream=" s)
       (begin
         (write msg s)
         (display " " s)
         (close s)))))

 ; Start the engine
 (createHub)

 ; The private local socket is continuously opened, read from once and queued then closed.
 (Display "\r\n::IPC Private socket " PrivateSocket "  ")
 (thread (let ~ ()
   (letrec ((s (open-stream PrivateSocket))
            (e (read s)))
     (Display "\r\n::IPC Private socket " PrivateSocket " received[" e "]")
     (or (eof-object? e) (msgQueueAdd e))
     (close s)
     (~))))

(lambda (c) (eval c)))
