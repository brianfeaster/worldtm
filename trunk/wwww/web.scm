#!wscm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Made-in-a-day web server for displaying the World[tm] Scheme's real time
;; heap usage information.
;;
;;
;; HTTP_stream_input_parsing
;; HTTP_stream_output_rendering
;; Request_handlers
;; Main

(define HTTPPortNum 7181)

; Global debug message object
(define WEB-DEBUG displayl)
(define (WEB-DB . r) (WEB-DEBUG "\n" (tid) " ") (apply WEB-DEBUG r))

; Return str with initial '/' removed
(define (chopSlash str)
  (if (eq? #\/ (string-ref str 0))
    (substring str 1 (string-length str))
    str))

;------------------------------------------------------------------------------
; HTTP_stream_output_rendering
;------------------------------------------------------------------------------
(define (outl wp . r)
 ;(or (and (pair? r) (eq? "" (car r))) (WEB-DB  " -->"))
 (for-each (lambda (o)
             (display o wp))
           r)
 (display "\r\n" wp))


(define (sendHeader404 wp len)
 (WEB-DB  "(sendHeader404)")
 (outl wp "HTTP/1.1 404 Not Found")
 (outl wp "Content-Type: text/html")
 (outl wp "Connection: keep-alive")
 (outl wp "Server: World[tm]")
 (if len (outl wp "Content-Length: " len))
 (outl wp ""))

(define (sendHeaderXML wp len)
 (WEB-DB  "(sendHeaderXML)")
 (outl wp "HTTP/1.1 200 OK")
 (outl wp "Content-Type: text/xml")
 (outl wp "Server: World[tm]")
 (outl wp "Connection: keep-alive")
 (if len (outl wp "Content-Length: " len))
 (outl wp ""))
 
(define (sendHeaderCss wp len)
 (WEB-DB  "(sendHeaderCss)")
 (outl wp "HTTP/1.1 200 OK")
 (outl wp "Content-Type: text/css")
 (outl wp "Server: World[tm]")
 (outl wp "Connection: keep-alive")
 (if len (outl wp "Content-Length: " len))
 (outl wp ""))

(define (sendHeaderJavascript wp len)
 (WEB-DB  "(sendHeaderJavascript)")
 (outl wp "HTTP/1.1 200 OK")
 (outl wp "Content-Type: text/javascript")
 (outl wp "Server: World[tm]")
 (outl wp "Connection: keep-alive")
 (if len (outl wp "Content-Length: " len))
 (outl wp ""))

(define (sendHeaderHtml wp len)
 (WEB-DB  "(sendHeaderHtml)")
 (outl wp "HTTP/1.1 200 OK")
 (outl wp "Server: World[tm]")
 (outl wp "Connection: keep-alive")
 (outl wp "Content-Type: text/html")
 (if len (outl wp "Content-Length: " len))
 (outl wp ""))

(define (sendHeaderText wp len)
 (WEB-DB  "(sendHeaderText)")
 (outl wp "HTTP/1.1 200 OK")
 (outl wp "Content-Type: text/plain")
 (outl wp "Server: World[tm]")
 (outl wp "Connection: keep-alive")
 (if len (outl wp "Content-Length: " len))
 (outl wp ""))


(define (sendHeaderIcon wp p)
 (WEB-DB  "::(sendHeaderIcon)")
 (outl wp "HTTP/1.1 200 OK")
 (outl wp "Content-Type: image/x-icon")
 (outl wp "Content-Length: " (vector-ref (file-stat p) 7))
 ;(outl wp "Last-Modified: 1320261216") ; st_mtime to string?
 (outl wp "Server: World[tm]")
 (outl wp "Connection: keep-alive")
 (outl wp ""))


(define (sendHeaderBinary wp p)
 (WEB-DB  "::(sendHeaderBinary)")
 (outl wp "HTTP/1.1 200 OK")
 (outl wp "Content-Type: application/octet-stream")
 (outl wp "Content-Length: " (vector-ref (file-stat p) 7))
 ;(outl wp "Last-Modified: 22 Mar 2006") ; st_mtime to string?
 (outl wp "Server: World[tm]")
 (outl wp "Connection: keep-alive")
 (outl wp ""))


(define (sendFileXml wp filename)
 (define fp (open-file filename 1))
 (define buff "")
 (WEB-DB  "::(sendFileXml)")
 (if fp (begin ; Did the file open successfully?
   (sendHeaderXML wp (vector-ref (file-stat fp) 7))
   (let ~ ()
     (set! buff (recv 1 0 fp))
     (or (not buff) (eof-object? buff)
         (begin
           (send buff wp)
           (~)))))))

(define (sendFileCss wp filename)
 (define fp (open-file filename))
 (define buff "")
 (define len (vector-ref (file-stat fp) 7))
 (WEB-DB  "(sendFileCss)")
 (if fp (begin ; Did the file open successfully?
   (sendHeaderCss wp len)
   (let ~ ()
     (set! buff (recv 1 0 fp))
     (or (not buff) (eof-object? buff)
         (begin
           (send buff wp)
           (~)))))))

(define (sendFileJavascript wp filename)
 (define fp (open-file filename))
 (define buff "")
 (define len (vector-ref (file-stat fp) 7))
 (WEB-DB  "(sendFileJavascript)")
 (if fp (begin ; Did the file open successfully?
   (sendHeaderJavascript wp len)
   (let ~ ()
     (set! buff (recv 1 0 fp))
     (or (not buff) (eof-object? buff)
         (begin
           (send buff wp)
           (~)))))))

(define (sendFileHtml wp filename)
 (define fp (open-file filename))
 (define buff "")
 (define len (vector-ref (file-stat fp) 7))
 (WEB-DB  "(sendFileHtml)")
 (sendHeaderHtml wp len)
 (let ~ ()
   (set! buff (recv 1 0 fp))
   (or (not buff) (eof-object? buff)
       (begin
         (send buff wp)
         (~)))))

(define (sendFileText wp filename)
 (define fp (open-file filename))
 (define len (vector-ref (file-stat fp) 7))
 (define buff "")
 (WEB-DB  "(sendFileText)")
 (sendHeaderText wp len)
 (let ~ ()
   (set! buff (recv 1 0 fp))
   (or (not buff) (eof-object? buff)
       (begin
         (send buff wp)
         (~)))))


(define (sendFileIcon wp filename)
 (define fp (open-file filename))
 (define buff "")
 (WEB-DB  "(sendFileIcon)")
 (sendHeaderIcon wp fp)
 (let ~ ()
   (set! buff (recv 1 0 fp))
   (or (not buff) (eof-object? buff)
       (begin
         (send buff wp)
         (~)))))

(define (sendFileBinary wp filename)
 (define fp (open-file filename))
 (define buff "")
 (WEB-DB  "(sendFileBinary)")
 (sendHeaderBinary wp fp)
 (let ~ ()
   (set! buff (recv 1 0 fp))
   (or (not buff) (eof-object? buff)
       (begin
         (send buff wp)
         (~)))))

(define (Send404 request)
 (define wp (request 'Stream))
 (define path (chopSlash (request 'URI)))
 (define str "")
 (WEB-DB  "::(Send404)")
 (set! str (string "(\"" path "\" Not Found)"))
 (sendHeader404 wp (string-length str))
 (send str wp))


; Send request for the "unicorn" object
(define (unicorn wp)
 (WEB-DB  "::(unicorn)")
 (sendHeaderXML wp 19)
 (send "<b>OMG unicorns</b>" wp))



;------------------------------------------------------------------------------
; Request_handlers
;------------------------------------------------------------------------------

; Get string handler function DB.  Initialized with the
; not found function.
(define WWWGetHandlers (list Send404 ))
(define (WWWRegisterGetHandler h)
  (WEB-DB "::(WWWRegisterGetHandler)  registering GET handler " h)
  (set! WWWGetHandlers (cons h WWWGetHandlers)))


; Handle special url requests.  Return #f if no matching request.
(define (WWWGetHandlerSpecial request)
 (define wp (request 'Stream))
 (define path (chopSlash (request 'URI)))
 (WEB-DB "(WWWGetHandlerSpecial) " path)
 (cond ((eqv? path "") (sendFileHtml wp "index.html") #t)
       ((eqv? path "unicorn") (unicorn wp) #t)
       (else #f)))

; Handle text file.  Return #f if no match.
(define (WWWGetHandlerFile request) ;wp path)
 (define wp (request 'Stream))
 (define path (chopSlash (request 'URI)))
 (define fp (open-file path 1)) ; Open file, #f if failure
 (WEB-DB "(WWWGetHandlerFile) " path)
 (and fp ; Port to file must be open
      (file-r-other? fp) ; File must be o+r
      (let ((toks (rstrtok path #\.))) ; Must be a valid extension
        (cond ((string=? (cdr toks) "ico") (sendFileIcon wp path))
              ((string=? (cdr toks) "jpg") (sendFileIcon wp path))
              ((string=? (cdr toks) "cur") (sendFileIcon wp path))
              ((string=? (cdr toks) "xml") (sendFileXml wp path))
              ((string=? (cdr toks) "html") (sendFileHtml wp path))
              ((string=? (cdr toks) "css") (sendFileCss wp path))
              ((string=? (cdr toks) "js") (sendFileJavascript wp path))
              (else (sendFileText wp path))))))



; Create's an HTTP request object given an HTTP stream object
; Returns an object with specific vars and an association list of
; HTTP header strings pairs. ("Host:" . "hostname")
;
; HTTP Request message:
;   Request line "GET / HTTP/1.1"
;   Headers*     "Accept-Language: en"
;   empty line   ""
;   body/post    "stuff" ...
;
; First line from HTTP request stream should be (rfc2616§5.1):
;   Method SPACE Request-URI SPACE HTTP-Version CRLF
(define (HTTPRequest Stream)
  (define (self msg) (eval msg))
  (define Method #f)
  (define URI #f)
  (define Version #f)
  (define HeaderDB ()) ; Association list of (header . value)
  (define Post #f)
  (define Valid #f)
  (define line "")
  ;; Methods
  (define (add n v) (set! HeaderDB (cons (cons n v) HeaderDB)))
  (define (get n) (let ((p (assv n HeaderDB))) (if (pair? p) (cdr p) #f)))
  ; Read one line from the stream delimted by a return/newline/eof
  ; TODO improve by reading as much as possible at a time rather than one char at a time.
  (define (readLine)
   (define ch #f)
   (set! line "")
   (let ~ ()
     (set! ch (read-char #f Stream))
     (if (eq? ch NEWLINE) line
     (if (eq? ch RETURN) (~) 
     (if (eq? ch #eof)
       (if (eq? line "") (set! line #eof))
       (begin (set! line (string line ch))
              (~)))))))
  (define (ReadRequest)
   (let ~ ()
     (readLine)
     ; All Header's have been parsed.  Check for post data.
     (if (eq? line "")
       (begin
         (let ((len (and (eqv? Method "POST") ; and expr returns #f or the 'Content-length:' header string value
                         (get "Content-Length:"))))
           (if len (set! Post (recv (read-string len) #f Stream))))
         (set! Valid #t))
       (begin
         ;(WEB-DB  "<--" line)
         (if (not (eq? line #eof))
           ; Initial request line
           (if (not Method) ; Must be first line/HTTP request
             (let ((req (split line #\ )))
               (set! Method  (car req)) (set! req (cdr req))
               (set! URI     (car req)) (set! req (cdr req))
               (set! Version (car req))
               (~))
             (let ((header (strtok line #\ ))) ; HTTP header line
               (add (car header) (cdr header))
               (~))))))))
  (define (DebugDumpInfo)
    (WEB-DB "::HTTPRequest.DebugDumpInfo")
    (WEB-DB " " Method " [" URI "] " Version)
    (for-each (lambda (h) (WEB-DB " " (car h) "=" (cdr h))) HeaderDB)
    (if Post (WEB-DB " Post = " Post)))
  (WEB-DB "::(HTTPRequest)")
  (ReadRequest)
  self)



(define (HTTPServer Port) 
  (define (self msg) (eval msg))
  (define Socket #f)
  ;; Methods
  ; Handle a single HTTP request, then close the connection unless a keep-alive
  (define (HandleRequest Stream)
    (WEB-DB "HTTPServer.HandleRequest")
    (letrec ((request (HTTPRequest Stream))) ; Create a new request object
      ((request 'DebugDumpInfo))
      (if (request 'Valid)
          ; Call each handler until #t is returned.  Last handler is 404 so will eventually succeed.
          (let ~ ((h WWWGetHandlers))
            (or ((car h) request)
                (~ (cdr h)))))
      (if (eqv? ((request 'get) "Connection:") "keep-alive")
          (HandleRequest Stream)
          (begin
            (WEB-DB  "--(HandleRequest) closing " Stream)
            (close Stream)))))
  ; Wait for a new HTTP connection on the socket
  ; then spawn a new thread to handle the request(s)
  (define (AcceptConnectionLoop)
    (define a (WEB-DB "HTTPServer.AcceptConnectionLoop"))
    (define Stream (if (port? Socket) (open-stream Socket)))
    (if (port? Stream)
      (begin
        (thread (HandleRequest Stream))
        (AcceptConnectionLoop))
      (begin
        (WEB-DB "ERROR: Unable to acquire a stream:" Stream " on socket:" Socket ".\n"))))
  ; Start a new listener thread
  (define (Start)
    (set! Socket (open-socket Port))
    (if (port? Socket)
        (thread (AcceptConnectionLoop))
        (WEB-DB "ERROR: Unable to start HTTP server on invalid socket:" Socket ".\n")))
  ; TODO Doesn't work as expected.  Keep-alive connection keep going.
  (define (Stop) 
    (set! Socket #f))
  self)



;------------------------------------------------------------------------------
; Main
;------------------------------------------------------------------------------
(define (WWWDebugSet db) (set! WEB-DEBUG db))

(define www ()) ; the HTTP server object

(define (WWWDefaultServer portNum)
  (set! www (HTTPServer HTTPPortNum))
  (WWWRegisterGetHandler WWWGetHandlerSpecial)
  (WWWRegisterGetHandler WWWGetHandlerFile)
  ((www 'Start))
  (repl))

(if (and (= 2 (vector-length argv))
         (eqv? (cdr (rstrtok (vector-ref argv 1) #\/)) "web.scm"))
  (WWWDefaultServer HTTPPortNum))
