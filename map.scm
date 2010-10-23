;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Map Agent
;;   IPC
;;   Avatar_and_entities
;;   Map_blocks
;;   Incomming_IPC_messages
;;   Start_everything
;;
;; Mapping from Ultima4 map file byte index to (y x) World[tm] coordinates.
;;  y = (+ (modulo (/ i 32) 32) (* (/ i 8192) 32))
;;  x = (+ (modulo i 32) (* (modulo (/ i 1024) 8) 32))
;; Mapping from World[tm] (y x) corrdinates to Ultima4 map file byte index.
;; i = (+ (modulo x 32) (* (/ x 32) 1024) (* (modulo y 32) 32) (* (/ y 32) 8192))
;;
;; Lord British's castle (108 86)
;;

(define DNA 17749)
(define MYNAME "The Map Agent")
(define ActivityTime (time)) ; Used for the idle time with the 'who' expression
(define cellCOLUMN 48)
(define cellFLOOR 62)
(define cellBRICKC 127)
(define cellAIR 1023)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IPC
;;
(load "ipc.scm")
(define ipc (Ipc #f)) ; Instead of #f can pass in a serializer for debug messages

(define (makeCounter i)
 (lambda ()
   (set! i (+ i 1))
   i))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Avatar_and_entities
;;
(load "entity.scm")

(define (Avatar dna port name z y x glyph) ((Entity dna port name z y x glyph) '(let ()
   (define self (lambda (msg) (eval msg)))
   (define counter (makeCounter 0))
   ; Map block origin AKA the upper left hand corner of the map blocks sent to the peer
   (define mapBlockY 0)
   (define mapBlockX 0)
   (define (setMapBlockLoc my mx)
     (set! mapBlockY my)
     (set! mapBlockX mx))
   self)))

(define EntityDB ())

(define (entityCreate dna port name z y x glyph)
  (Avatar dna port name z y x glyph))

(define (entityDBAdd e)
  (set! EntityDB (cons (cons (e 'dna) e) EntityDB)))

(define (entityDBUpdate dna port name z y x glyph)
  (let ((e (assv dna EntityDB)))
    (or (null? e)
      (((cdr e) 'setAll) port name z y x glyph))))

(define (entityLookup dna)
  (let ((e (assv dna EntityDB)))
    (if (null? e) UnknownEntity (cdr e))))

; Create the map agent
(define avatar (Avatar DNA (ipc 'PrivatePort) MYNAME 0 0 0 #(1 11 #\M 2 4 #\A)))
(entityDBAdd avatar)

; Create and add some default entities
; Arguments: dna port name z y x glyph
(define SystemEntity (entityCreate 0 0 "SYSTEM" 0 0 0 'NOGLYPH))
(entityDBAdd SystemEntity)
(define UnknownEntity (entityCreate 1 0 "UNKNOWN" 0 0 0 'NOGLYPH)) ; Not added to DB otherwise contradiction



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Map_blocks
;;
;; Maps blocks will populate a peers field as if it were a local cache relative
;; to the avatar's position.
;;
;; Avatar movement will readjust the field's virtual position in the map
;; coordinate system as well as repopulate/cache the new block areas.
;;
;; Example:                          +--+--+    +--+--+
;;   Assume avatar moved from a to   | a| B|    | e| B|
;;   b in the field  area.  e and f  +--+--+ -> +--+--+
;;   are cached in the area modulo   | c| D|    | f| D|
;;   the field coordinates.          +--+--+    +--+--+
;;
(define MapBlockSize  32) ; Each field block is a 32x32 array of columns
(define MapBlockCount 4)       ; A field is made up of 2x2 field blocks
(define MapRangeSize (* MapBlockCount MapBlockSize)) ; For now resulting blocok range size is 96x96
(define MapBoundsSize (/ MapBlockSize 1)) ; Distance from the edge the valid range is

; The Ultima4 map file is 8x8 blocks of 32x32 cells
; so create a simpler 256x256 array of cell numbers.
(define U4MapVector
 (let ((fd (open-file "ultima4.map"))
       (vec (make-vector 65536)))
  (let ~ ((i 0))
     (if (= i 65536) (begin
       (close fd)
       vec) ; Return the vector
     (begin
        (vector-set! vec
                 (+ (* 256 (+ (modulo (/ i 32) 32) (* (/ i 8192) 32)))
                    (+ (modulo i 32) (* (modulo (/ i 1024) 8) 32)))
                 (+ 0 (read-char fd))) ; Hack to convert char to integer
        (~ (+ i 1)))))))

; Return cell number from U4 map
(define (U4MapCell y x)
  (vector-ref U4MapVector (+ (* 256 (modulo y 256)) (modulo x 256))))

; Create a column given a U4 map coordinate
(define (U4MapColumn y x)
  (vector -1 (U4MapCell y x) cellAIR))

; Return vector of cell numbers representing
; the U4 city files. lcb1.ult
(define (ultMapVector fn)
 (let ((fd (open-file fn))
       (ultVec (make-vector 1024)))
  (loop 1024 (lambda (i) (vector-set! ultVec i (+ 0 (read-char fd))))) ; Hack to convert char to integer
  (close fd)
  ultVec))

; These are like town files only 11x11 so pad rest with grass
(define (conMapVector fn)
 (let ((fd (open-file fn))
       (ultVec (make-vector 1024 4)))
  (loop2 10 21 10 21 (lambda (y x) (vector-set! ultVec (+ (* y 32) x) (+ 0 (read-char fd))))) ; Hack to convert char to integer
  (close fd)
  ultVec))

(define (getUltima4ULT by bx)
  (if (and (= by 107) (= bx 86)) (ultMapVector "lcb1.ult")
  (if (and (= by 107) (= bx 87)) (ultMapVector "lcb2.ult")
  (if (and (= by 106) (= bx 82)) (ultMapVector "britain.ult")
  (if (and (= by 145) (= bx 98)) (ultMapVector "paws.ult")
  (if (and (= by 128) (= bx 22)) (ultMapVector "skara.ult")
  (if (and (= by 90) (= bx 136)) (ultMapVector "cove.ult")
  (if (and (= by 92) (= bx 128)) (conMapVector "shrine.con")
  #f))))))))

(define doubleHeightCells (list cellBRICKC cellCOLUMN))

(define (ultMapColumn baseCell ultVec y x)
  (let ((cell (vector-ref ultVec (+ (* 32 (modulo y 32)) (modulo x 32)))))
    (if (pair? (memv cell doubleHeightCells))
      (vector -1 baseCell cell cell cellAIR)
    (if (= cell cellFLOOR)
      (vector -1 baseCell cell cellAIR cellBRICKC cellAIR)
    (vector -1 baseCell cell cellAIR)))))

(define (tryToOpenFile fileName)
  (let ((fp (open-new-file fileName)))
    (if (port? fp) fp (begin
      (displayl "\r\nCan't open " fileName ".  Trying again...")
      (sleep 10)
      (tryToOpenFile fileName)))))

(define (generateBlockColumns by bx)
 (letrec ((fn (string "mapU4-" (number->string by 16) "-" (number->string bx 16)))
          (fp (open-file fn)))
  (if fp
    (begin
      (displayl "\r\nReading and sending existing cached map " fn)
      (let ((l (cons 'list (read fp))))
        (close fp)
        l))
    (let ~ ((l ()) ; Create the list of columns (in reverse)
            (ultVec (getUltima4ULT by bx)))
     (loop2 0 MapBlockSize 0 MapBlockSize
      (if ultVec
        (lambda (y x) (set! l (cons (ultMapColumn (U4MapCell by bx) ultVec (- MapBlockSize y 1) (- MapBlockSize x 1)) l)))
        (lambda (y x) (set! l (cons (U4MapColumn by bx) l)))))
     (let ((fp (tryToOpenFile fn)))
       (write l fp)
       (close fp))
     (cons 'list l))))) ; return it as an expression (list columns ...)

(define (sendInitialBlocks e)
  (letrec ((ey (e 'y)) ; Entity's position
           (ex (e 'x))
           (by (/ (- ey (/ MapRangeSize 2)) MapBlockSize)) ; A new block range origin
           (bx (/ (- ex (/ MapRangeSize 2)) MapBlockSize)))
  ((e 'setMapBlockLoc) by bx) ; Update the entity's block range origin
  ;(displayl "\r\nSending initial map blocks to " (e 'name)) ; DEBUG
  (loop2 by (+ by MapBlockCount) bx (+ bx MapBlockCount) (lambda (y x)
    ;(displayl " (" y " " x ")") ; DEBUG
    (displayl "\r\nSending block " (list y x) " to " (e 'name) ":" (e 'port) ":" ((e 'counter))) ; DEBUG
    ((ipc 'private) (e 'port) ; Send the block to the peer
       `(mapUpdateColumns ,(* y MapBlockSize) ,(* x MapBlockSize) ,MapBlockSize ,(generateBlockColumns y x)))))
  ((ipc 'private) (e 'port)
    '((ipc 'qwrite) '(who))))) ; Force the peer to request roll call from everyone

; Determine if the coordinate is above or below the inner field block area.
; An avatar that moves outside of the inner field range will cause the
; field to load new blocks.
;
;  +-----+<---Field range
;  |+---+|
;  ||   |<---Inner field range 1/4 the MapBlockSize
;  |+---+|
;  +-----+
(define (entityWithinBounds e)
 (let ((y (e 'y))
       (x (e 'x))
       ; Consider valid boundary
       (eby0 (+ (* (e 'mapBlockY) MapBlockSize) MapBoundsSize))
       (ebx0 (+ (* (e 'mapBlockX) MapBlockSize) MapBoundsSize))
       (eby1 (- (* (+ (e 'mapBlockY) MapBlockCount) MapBlockSize) MapBoundsSize))
       (ebx1 (- (* (+ (e 'mapBlockX) MapBlockCount) MapBlockSize) MapBoundsSize)))
   (and (<= eby0 y) (< y eby1)
        (<= ebx0 x) (< x ebx1))))

; Determine if location is out of bound of block range
(define (notInRange y x ry rx)
 (or (< y ry) (<= (+ ry MapBlockCount) y)
     (< x rx) (<= (+ rx MapBlockCount) x)))

(define (sendNewBlocks e)
 (letrec ((y (e 'y)) ; Consider entity's location
          (x (e 'x))
          (ry (e 'mapBlockY)) ; Entity's current map block range origin
          (rx (e 'mapBlockX))
          ; Consider map block the entity is in accounting for negative coordinates
          (my (/ (if (< y 0) (- y MapBlockSize) y) MapBlockSize))
          (mx (/ (if (< x 0) (- x MapBlockSize) x) MapBlockSize))
          ; Precompute invalid range boundary axis
          (top    (+ (* (e 'mapBlockY) MapBlockSize) MapBoundsSize))
          (left   (+ (* (e 'mapBlockX) MapBlockSize) MapBoundsSize))
          (bottom (- (* (+ (e 'mapBlockY) MapBlockCount) MapBlockSize) MapBoundsSize))
          (right  (- (* (+ (e 'mapBlockX) MapBlockCount) MapBlockSize) MapBoundsSize)))

   (if (< y top)     (set! ry (- my 1)))
   (if (<= bottom y) (set! ry (- my (- MapBlockCount 2))))
   (if (< x left)    (set! rx (- mx 1)))
   (if (<= right x)  (set! rx (- mx (- MapBlockCount 2))))

   (loop2 ry (+ ry MapBlockCount) rx (+ rx MapBlockCount) (lambda (by bx)
     (if (notInRange by bx (e 'mapBlockY) (e 'mapBlockX)) (begin
       (displayl "\r\nSending block " (list by bx) " to " (e 'name) ":" (e 'port) ":" ((e 'counter))) ; DEBUG
       ((ipc 'private) (e 'port) ; Send updated block to peer
         `(mapUpdateColumns ,(* by MapBlockSize) ,(* bx MapBlockSize) ,MapBlockSize ,(generateBlockColumns by bx)))
       (displayl "\r\nDone sending block " (list by bx) " to " (e 'name) ":" (e 'port) ":" ((e 'counter))))))) ; DEBUG

   ((e 'setMapBlockLoc) ry rx)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Incomming_IPC_messages
;;
(define (who)
 ((ipc 'qwrite)
 `(entity ,DNA ,(avatar 'port) ,(avatar 'name) ,@((avatar 'gps)) ,(avatar 'glyph))))

(define (entity dna port name z y x glyph) 
 (let ((e (entityLookup dna)))
  (if (eq? e UnknownEntity)
    (begin
      (entityDBAdd (entityCreate dna port name z y x glyph))
      (displayl "\n\e[31m" name " registerd\e[0m")
      (sendInitialBlocks (entityLookup dna)))
    (begin
      (entityDBUpdate dna port name z y x glyph)
      (displayl "\n\e[31m" name " updated\e[0m")))))

(define (voice dna level text)
 (let ((e (entityLookup dna)))
  (displayl "\n\e[1;34m" (e 'name) " says:"text "\e[0m")
  (display (string (e 'name) "\t" text "\n") log)))

(define (move dna . loc)
 (letrec ((e (entityLookup dna)))
   (apply (e 'setLoc) loc) ; Update entity's location
   (displayl "\n\e[32m" (e 'name) " moves to " ((e 'gps))) ; DEBUG
   (or (entityWithinBounds e) (sendNewBlocks e))))

(define (die . x) ())



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start_everything
;;
(define log (open-file "talk.log"))
(seek-end log 0)

((ipc 'qwrite) '(who)) ; Force everyone, including this map agent, to identify themselves

(thread 
 (let ((s (call/cc (lambda (c) (vector-set! ERRORS (tid) c) '*))))
    (or (eq? s '*) (displayl "\nIPC-REPL-ERROR::" s)))
 (let ~ () 
  (let ((e ((ipc 'qread))))
     (displayl "\n\e[1;30mIPC::" e "\e[0m")
     (eval e)
     (~))))

(repl) ; Start up a REPL loop for fun
