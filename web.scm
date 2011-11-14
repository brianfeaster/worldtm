;
; Made-in-a-day web server for displaying the World[tm] Scheme's real time
; heap usage information.
;

;  <meta http-equiv='refresh' content='1'>

;------------------------------------------------------------------------------
; HTTP stream input parsing
;------------------------------------------------------------------------------

; Read one line from the stream delimted by a return/newline/eof
(define (readLine stream)
 (define line "")
 (define ch #f)
 (let ~ ()
   (set! ch (read-char #f stream))
   (if (eq? ch NEWLINE) line
   (if (eq? ch RETURN) (~) 
   (if (eq? ch #eof)
     (if (eq? line "") #eof line)
     (begin (set! line (string line ch))
            (~)))))))


; Return the desired path as a string.  All other HTTP headers are ignored
(define (readAndSkipHTTPRequest stream)
 (define desiredPath "") ; Default desired path
 (define line "") ; Parsed line
 (let ~ ()
   (set! line (readLine stream))
   (displayl "[" line "]" NEWLINE)
   (if (eq? line #eof) #eof
   (if (eq? line "") desiredPath
     (let ((getStr (strtok line #\ ))) ; Match [GET][/ HTTP/1.1]
       (if (eqv? "GET" (car getStr))
           (set! desiredPath (car (strtok (cdr getStr) #\ )))) ; Match [/][HTTP/1.1]
       (~))))))


;------------------------------------------------------------------------------
; HTTP stream output rendering
;------------------------------------------------------------------------------
(define ClientStream #f) ; The HTTP outgoing connection stream


; Like 'displayl' but for the current http client connection 'ClientStream'
(define (out . l)
 (for-each (lambda (o)
   (displayl "{" o "}")
   (display o ClientStream)) l)
 (display "\r\n" ClientStream))


(define (renderNotFound desiredPath)
 (out "HTTP/1.1 404 Not Found")
 (out "Content-Type: text/html")
 (out "Connection: close")
 (out "Server: World[tm]")
 (out "")
 (out "(" desiredPath " Not Found)"))


(define (mainPage)
 ; Headers
 (out "HTTP/1.1 200 OK")
 (out "Content-Type text/html")
 (out "Server: World[tm]")
 (out "Connection: close")
 (out "")

 ; Content
 (out "<html><head>
<script language='javascript' type='text/javascript'>
  function outToPadId(data) { document.getElementById('padid').innerHTML = data; }
  function outToInfoId(data) { document.getElementById('infoid').innerHTML = data; }
  function outToChangeIconId(data) { document.getElementById('changeiconid').innerHTML = data; }
  var x=0;
  function stateChangeHandler () {
    if (this.readyState == 4  &&
        this.status == 200) {
      if (this.responseXML != null  &&
          this.responseXML.getElementById('test') &&
          this.responseXML.getElementById('test').firstChild.data) {
        outToPadId(this.responseXML.getElementById('test').firstChild.data);
        //outToPadId(this.responseXML.getElementsByTagName('test')[0].firstChild.value);
      }
    } else if (this.readyState == 4 && this.status != 200) {
      outToPadId('Failed');
    }
    outToInfoId(x++);
  }")

 (out "function HTTPrequestUnicorn () {
  outToPadId('[Please standby...]');

  var client = new XMLHttpRequest();
  client.onreadystatechange = stateChangeHandler;
  client.open('GET', 'unicorn', true);
  client.send();
}")

 (out "function changeIcon (iconURL) {
  head = document.getElementsByTagName('head')[0];
  if (head) {
    var links = head.getElementsByTagName('link');
    if (links.length) {
      outToChangeIconId('attempting to change to ' + iconURL);
      var link = links[0];
      if (links && link.type=='image/x-icon' && link.rel=='shortcut icon') {
          head.removeChild(link);
      }
    }
    outToChangeIconId(iconURL);
    link = document.createElement('link');
    link.type = 'image/x-icon';
    link.rel = 'shortcut icon';
    link.href = iconURL;
    head.appendChild(link);
  } else {
   outToChangeIconId('cant change icon');
  }}")

 (out "
  var c=0;
  var s;
  function acounter () {
   if (!s) s = document.getElementById('scroller').innerHTML;
   s = s.substr(1) + s.substr(0,1);
   document.getElementById('counter').innerHTML = ++c + s;
   setTimeout('acounter()', 100);
  }")
  

 (out "</script>
</head><body onLoad='acounter()'>
<p id='scroller'>Web Experiments vBeta.Beta.Beta (beta!)</p>

<div id='padid' onClick='HTTPrequestUnicorn()'>click me</div>
<div>Info:<span id='infoid' onClick='HTTPrequestUnicorn()'>X</span></div>

<br
<div>
<span onClick=\"changeIcon('shrewm.ico')\">changeIcon</span> &nbsp;
<span onClick=\"changeIcon('lambda16.ico')\">changeIcon</span> &nbsp;
<span onClick=\"changeIcon('lambda162.ico')\">changeIcon</span> &nbsp;
<span onClick=\"changeIcon('lambda16c.ico')\">changeIcon</span> &nbsp;
<span onClick=\"changeIcon('lambda256.ico')\">changeIcon</span> &nbsp;
</div>
<div id='changeiconid'>STATUS</div>
<p id='counter'>counter</p>
<p>eol</p>
</body></html>"))


; Send request for the "unicorn" object
(define (unicorn)
 ; Headers
 (out "HTTP/1.1 200 OK")
 (out "Content-Type text/html")
 (out "Server: World[tm]")
 (out "Connection: close")
 (out "")
 ; Content
 (out "<test id='test'>a unicorn</test>"))


(define (sendFile filename)
 (define s (open-file filename))
 (define filesize (vector-ref (file-stat s) 7))
 ; Headers
 (out "HTTP/1.1 200 OK")
 (out "Content-Type: application/octet-stream")
 (out "Content-Length: " filesize)
 (out "")
 ; Binary content
 (send (recv filesize 5 s) ClientStream))


;------------------------------------------------------------------------------
; Main
;------------------------------------------------------------------------------
(define HTTPPort (open-socket 8080)) ; This web server's listener socket port

(if (eof-object? HTTPPort) 
 (begin (display "ERROR:  Unable to open socket for web service")
        (unthread))) ; Silently die if socket unavailable

; Handle a single HTTP request then close the connection
(define (AWebServer z)
 (set! ClientStream (open-stream HTTPPort))
 (if (eof-object? ClientStream)
     (unthread)
     (let ((desiredPath (readAndSkipHTTPRequest ClientStream)))
       (cond ((eqv? "/" desiredPath) (mainPage))
             ((eqv? "/unicorn" desiredPath) (unicorn))
             ((eqv? "/favicon.ico" desiredPath) (sendFile "shrewm.ico"))
             ((eqv? "/shrewm.ico" desiredPath) (sendFile "shrewm.ico"))
             ((eqv? "/lambda16.ico" desiredPath) (sendFile "lambda16.ico"))
             ((eqv? "/lambda162.ico" desiredPath) (sendFile "lambda162.ico"))
             ((eqv? "/lambda16c.ico" desiredPath) (sendFile "lambda16c.ico"))
             ((eqv? "/lambda256.ico" desiredPath) (sendFile "lambda256.ico"))
             (else (renderNotFound desiredPath)))))
 (close ClientStream))

; Handle up to 1000 incoming HTTP requests one at a time
(thread (loop 1000 AWebServer) (unthread))
(repl)
