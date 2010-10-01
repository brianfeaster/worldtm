;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Map Agent
;;   IPC
;;   Avatar_and_entities
;;   Mapblocks
;;   Incomming_IPC_messages
;;   Start_everything
;;

;(load "window.scm")
(define MapBlockSize 4)
(define cellBRICK 19)
(define cellAIR 1023)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IPC
;;
(load "ipc.scm")
(define ipc (Ipc displayl))

(define (who)
 ((ipc 'qwrite)
 `(entity ,DNA ,(avatar 'port) ,(avatar 'name) ,@((avatar 'gps)) ,(avatar 'glyph))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Avatar_and_entities
;;
(load "entity.scm")
(define DNA 17749)
(define NAME "The Map Agent")
(define (Avatar dna port name z y x glyph) ((Entity dna port name z y x glyph) '(let ()
   (define self (lambda (msg) (eval msg)))
   (define fieldY (/ y MapBlockSize))
   (define fieldX (/ x MapBlockSize))
   (define (setMapLoc fy fx) ; The upper left corner of cached map block set
     (set! fieldY fy)
     (set! fieldX fx))
   self)))

(define EntityDB ())

(define (entityCreate dna port name z y x glyph)
  (Avatar dna port name z y x glyph))

(define (entityDBAdd e)
  (set! EntityDB (cons (cons (e 'dna) e) EntityDB)))

(define (entityDBUpdate dna port name z y x glyph)
  (let ((e (assv dna EntityDB)))
    (or (null? e)
      (set-cdr! e (entityCreate dna port name z y x glyph)))))

(define (entityLookup dna)
  (let ((e (assv dna EntityDB)))
    (if (null? e) UnknownEntity (cdr e))))

; Create the map agent
(define avatar (Avatar DNA (ipc 'PrivatePort) NAME 0 0 0 #(1 11 #\M 2 4 #\A)))
(entityDBAdd avatar)

; Create and add some default entities
; Arguments: dna port name z y x glyph
(define SystemEntity (entityCreate 0 0 "SYSTEM" 0 0 0 'NOGLYPH))
(entityDBAdd SystemEntity)
(define UnknownEntity (entityCreate 1 0 "UNKNOWN" 0 0 0 'NOGLYPH)) ; Not added to DB otherwise contradiction



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mapblocks
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
;(define MapBlockSize  32) ; Each field block is 256x256 columns
(define MapBlockCount 3)       ; A field is made up of 2x2 field blocks
(define MapSize (* MapBlockCount MapBlockSize)) ; For now resulting field size is 512x512
(define MapBlockCoordinates ()) ; Canvas coordinates in block space.  (1 1) would be (256 256) map space.

; The first pair of field block coordinates is always the upper left corner.
(define (mapBlockY) (car (car MapBlockCoordinates)))
(define (mapBlockX) (cdr (car MapBlockCoordinates)))

(define (mapInside? y x)
 (and (pair? MapBlockCoordinates)
      (<= (mapBlockY) y)
      (< y (+ (mapBlockY) MadSize))
      (<= (mapBlockX) x)
      (< x (+ (mapBlockX) MapSize))))

; Generate a list of coordinate pairs of each block
; that make up the canvas at the specified location.
(define (mapCreateBlockList y x)
 (let ~ ((m 0) (n 0))
  (if (= m MapBlockCount) '()
  (if (= n MapBlockCount) (~ (+ m 1) 0)
  (cons (cons (+ y m) (+ x n))
        (~ m (+ n 1)))))))

; Return list of block coordinates which need to be updated
; if we were to move from the current field block to the new
; (y x) field blocks location.
(define (canvasBlockCoordinatesNew y x)
  (let ~ ((newBlocks (mapCreateBlockList y x)) ; Blocks associated with new field block origin.
          (currentBlocks MapBlockCoordinates)) ; Curent blocks associated with current field block coor.
    (if (null? currentBlocks)
      newBlocks
      (~ (list-delete newBlocks (car currentBlocks)) ; Remove current blocks from new block list.
         (cdr currentBlocks)))))

; Determine if the coordinate is above or below the inner field block area.
; An avatar that moves outside of the inner field range will cause the
; field to load new blocks.
; TODO Implement map to field block coordinates.
;
;  +-----+<---Field range
;  |+---+|
;  ||   |<---Inner field range 1/4 the FieldBlockSize
;  |+---+|
;  +-----+
(define (mapTopBottomBuffer y x)
 (let ((mapy (mapBlockY))
       (buffer (/ MapBlockSize 4)))
  (if (< y (+ (* MapBlockSize   mapy) buffer)) '(top)
  (if (<= (- (* MapBlockSize (+ mapy MapBlockCount)) buffer) y) '(bottom)
  ()))))

(define (mapLeftRightBuffer y x)
 (let ((mapx (mapBlockX))
       (buffer (/ MapBlockSize 4)))
  (if (< x (+ (* MapBlockSize   mapx) buffer)) '(left)
  (if (<= (- (* MapBlockSize (+ mapx MapBlockCount)) buffer) x) '(right)
  ()))))

; TODO start this which should lead to a generic map block updater
(define (sendInitialBlocks e)
  (let ((by (- (/ (e 'y) MapBlockSize) (/ MapBlockCount 2)))
        (bx (- (/ (e 'x) MapBlockSize) (/ MapBlockCount 2))))
  ((e 'setMapLoc ) y x) ; Update the entities cached block location (temporary)
  (loop2 by (+ by MapBlockCount) bx (+ bx MapBlockCount) (lambda (y x)
    (displayl "Sending initial " y " " x " to " (e 'name) "\r\n")
    ((ipc 'private) (e 'port) ; Send the block to the peer
       `(mapUpdateColumns ,(* y MapBlockSize) ,(* x MapBlockSize) ,MapBlockSize
          ,(let ((l ()))
             (loop (* MapBlockSize MapBlockSize) (lambda (i)
               (set! l (cons (vector -1 (U4MapCell y x) cellAIR) l)))) (cons 'list l))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Incomming_IPC_messages
;;
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

(define (die . x) ())

; The Ultima4 map file is 8x8 blocks of 32x32 cells
; so create a simpler 256x25 array of cell numbers.
(define U4MapVector
 (let ((fd (open-file "ultima4.map"))
       (vec (make-vector 65536)))
  (let ~ ((i 0))
     (if (= i 65536) vec ; Return the vector
     (begin
        (vector-set! vec
                 (+ (* 256 (+ (modulo (/ i 32) 32) (* (/ i 8192) 32)))
                    (+ (modulo i 32) (* (modulo (/ i 1024) 8) 32)))
                 (+ 0 (read-char fd))) ; Hack to convert char to integer
        (~ (+ i 1)))))))

(define (U4MapCell y x)
 (vector-ref U4MapVector (+ (* 256 (modulo y 256)) (modulo x 256))))

(define (move dna . loc)
 (letrec ((e (entityLookup dna))
          (ey (e 'fieldY))
          (ex (e 'fieldX))
          (y (/ (cadr loc) MapBlockSize))
          (x (/ (caddr loc)MapBlockSize)))
   (apply (e 'setLoc) loc) ; Update entity's location
   (displayl "\n\e[32m" (e 'name) " moves to " ((e 'gps))) ; Debug message
   (if (or (!= ey y) ; Determine if a new block is required
           (!= ex x))
     (begin
       ((e 'setMapLoc ) y x) ; Update the entities cached block location (temporary)
       ((ipc 'private) (e 'port) ; Send the block to the peer
          `(mapUpdateColumns ,(* y MapBlockSize) ,(* x MapBlockSize) ,MapBlockSize
             ,(let ((l ()))
                (loop (* MapBlockSize MapBlockSize) (lambda (i)
                  (set! l (cons (vector -1 (U4MapCell y x) cellAIR) l)))) (cons 'list l))))))))



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
