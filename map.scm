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
(define ipcReadQueue ((ipc 'newReader)))
(define ipcWrite (ipc 'qwrite))

(define (makeCounter i)
 (lambda ()
   (set! i (+ i 1))
   i))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Avatar_and_entities
;;
(load "entity.scm")

(define (Avatar dna port name sprite z y x) ((Entity dna port name sprite z y x) '(let ()
   (define self (lambda (msg) (eval msg)))
   (define counter (makeCounter 0))
   ; Map block origin AKA the upper left hand corner of the map blocks sent to the peer
   (define mapBlockY 0)
   (define mapBlockX 0)
   (define (setMapBlockLoc my mx)
     (set! mapBlockY my)
     (set! mapBlockX mx))
   (define (glyph) ((sprite 'glyphRef) 0 0))
   self)))

; Association list of entites to their DNA values.
(define EntityDB ())

(define (entitiesAdd entity)
 (set! EntityDB (cons (cons (entity 'dna) entity) EntityDB)))

; Lookup entity in database.
(define (entitiesGet dna)
  (let ((entity (assv dna EntityDB)))
    (if (null? entity) #f (cdr entity))))

(define (entitiesSet dna . args)
  (let ((entity (entitiesGet dna)))
    (if entity
      (for-each ; Update only name and glyph if entity already exists
        (lambda (a)
          (if (integer? a) ((entity 'setPort) a)
          (if (string? a)  ((entity 'setName) a)
          (if (vector? a)  ((entity 'setGlyph) a)))))
        args)
      (begin
        ; Create a new entity.  Massage the arguments (port name glyph (x y z))->(port name glyph x y z)
        (set! entity (apply Avatar dna (let ~ ((args args))
                                         (if (pair? (car args)) (car args)
                                             (cons (car args) (~ (cdr args)))))))
        (entitiesAdd entity)))
    ; Return the new or modified entity
    entity))
       
; Create the map agent
(define avatar (Avatar DNA (ipc 'PrivatePort) MYNAME (Sprite 1 1 (vector #(1 11 #\M 2 4 #\A))) 0 0 0))
(entitiesAdd avatar)
(entitiesAdd (Entity 0 0 "System" #(0 1 #\S 0 1 #\Y) 0 0 0))


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
(define MapBoundsSize MapBlockSize) ; Distance from the edge the valid range is

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
  (if (and (= by 43)  (= bx 58)) (ultMapVector "yew.ult")
  (if (and (= by 90)  (= bx 136))(ultMapVector "cove.ult")
  (if (and (= by 92)  (= bx 128))(conMapVector "shrine.con")
  (if (and (= by 106) (= bx 82)) (ultMapVector "britain.ult")
  (if (and (= by 107) (= bx 86)) (ultMapVector "lcb1.ult")
  (if (and (= by 107) (= bx 87)) (ultMapVector "lcb2.ult")
  (if (and (= by 128) (= bx 22)) (ultMapVector "skara.ult")
  (if (and (= by 145) (= bx 98)) (ultMapVector "paws.ult")
  #f)))))))))

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

(define (saveMapBlock by bx block)
 (letrec ((fn (string "mapU4-" (number->string by 16) "-" (number->string bx 16))))
    (displayl "\r\nSaving map block " (list by bx) fn)
   (let ((fp (tryToOpenFile fn)))
     (write block fp)
     (close fp))))

(define CurrentMapBlockName "")
(define CurrentMapBlock #())

(define (getMapBlock by bx shouldSave)
 (let ((fn (string "mapU4-" (number->string by 16) "-" (number->string bx 16))))
  (if (string=? fn CurrentMapBlockName)
    (begin
      (displayl "\r\nUsing cached map block " fn)
      CurrentMapBlock) ; Return currently cached map block
    (let ((fp (open-file fn)))
      (if fp
        (begin
          (displayl "\r\nReading existing map block " fn)
          (let ((v (read fp)))
            (close fp)
            (set! CurrentMapBlockName fn)
            (set! CurrentMapBlock v)
            v))
        (let ((v (make-vector-vector MapBlockSize MapBlockSize #f))
              (ultVec (getUltima4ULT by bx)))
          (displayl "\r\nGenerating map block " fn)
          (loop2 0 MapBlockSize 0 MapBlockSize
            (if ultVec
              (lambda (y x) (vector-vector-set! v y x (ultMapColumn (U4MapCell by bx) ultVec y x)))
              (lambda (y x) (vector-vector-set! v y x (U4MapColumn by bx)))))
          (if shouldSave (saveMapBlock by bx v))
          (set! CurrentMapBlockName fn)
          (set! CurrentMapBlock v)
          v))))))

(define (sendInitialBlocks e)
  (letrec ((ey (e 'y)) ; Entity's position
           (ex (e 'x))
           (by (/ (- ey (/ MapRangeSize 2)) MapBlockSize)) ; A new block range origin
           (bx (/ (- ex (/ MapRangeSize 2)) MapBlockSize)))
  ((e 'setMapBlockLoc) by bx) ; Update the entity's block range origin
  (loop2 by (+ by MapBlockCount)
         bx (+ bx MapBlockCount)
    (lambda (y x)
      (displayl "\r\nSending block " (list y x) " to " (e 'name) ":" (e 'port) ":" ((e 'counter))) ; DEBUG
      ((ipc 'private) (e 'port) ; Send the block to the peer
         `(mapUpdateColumns ,(* y MapBlockSize) ,(* x MapBlockSize) ,MapBlockSize ,(getMapBlock y x #t)))))
  ((ipc 'private) (e 'port) '(ipcWrite '(who))))) ; Force the peer to request roll call from everyone

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
         `(mapUpdateColumns ,(* by MapBlockSize) ,(* bx MapBlockSize) ,MapBlockSize ,(getMapBlock by bx #t)))
       (displayl "\r\nDone sending block " (list by bx) " to " (e 'name) ":" (e 'port) ":" ((e 'counter))))))) ; DEBUG

   ((e 'setMapBlockLoc) ry rx)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Incomming_IPC_messages
;;
(define (who . dna)
 (ipcWrite
 `(entity ,DNA ,(avatar 'port) ,(avatar 'name) ,(((avatar 'sprite) 'serialize)) ',((avatar 'gps)))))

(define (entity dna . args)
 (let ((e (entitiesGet dna)))
  (if e
    (begin
      (apply entitiesSet dna args)
      (displayl "\n\e[31m" (e 'name) " updated\e[0m"))
    (begin
      (set! e (apply entitiesSet dna args))
      (displayl "\n\e[31m" (e 'port) (e 'name) (e 'glyph) (list (e 'z) (e 'y) (e 'x)) " registerd\e[0m")
      (sendInitialBlocks e)))))

(define (voice dna level text)
 (let ((e (entitiesGet dna)))
   (displayl "\n\e[1;34m" (e 'name) " says:"text "\e[0m")
   (display (string (e 'name) "\t" text "\n") log)))

(define (move dna . loc)
 (letrec ((e (entitiesGet dna)))
   (apply (e 'setLoc) loc) ; Update entity's location
   (displayl "\n\e[32m" (e 'name) " moves to " ((e 'gps))) ; DEBUG
   (or (entityWithinBounds e) (sendNewBlocks e))))

(define (die . x) ())

;-----------------------------------------------------------

; Height of first default implicit bottom object
(define (columnHeightBottom c)
  (vector-ref c 0))

; Height of first default implicit top object
(define (columnHeightTop c)
  (+ (vector-ref c 0)
     (vector-length c)))

(define (columnRef column z)
 (vector-ref column (let ((i (- z (columnHeightBottom column)))) ; normalize z coordinate
                      (if (<= i 1) 1 ; default bottom cell
                      (if (<= (vector-length column) i) (- (vector-length column) 1) ; default top cell
                      i)))))

; Extend column below based on existing column. #(3  4 5 6) -> #(1  4 4 4 5 6)
(define (columnExtendBelow c bot)
 (let ((k (make-vector (- (columnHeightTop c) bot)             ; vector size
                       (columnRef c (columnHeightBottom c))))) ; default object
   ; Set bottom height
   (vector-set! k 0 bot)
   ; Copy old column here.
   (vector-set-vector! k (- (columnHeightBottom c) bot -1) c 1)
   k))

; Extend column above based on existing column. #(3  4 5 6) -> #(3  4 5 6 6 6)
(define (columnExtendAbove c top)
 (let ((k (make-vector (- top (columnHeightBottom c))       ; vector size
                       (columnRef c (columnHeightTop c))))) ; default object
   ; Copy old column here including bottom height
   (vector-set-vector! k 0 c 0)
   k))

; Returns column (or new column) with object set at specified height.
(define (columnSet c h o)
  (let ((i (- h (columnHeightBottom c)))) ; Column height -> vector index
    (if (<= i 1)
      (begin
        (set! c (columnExtendBelow c (- h 2)))
        (vector-set! c 2 o))
    (if (>= i (- (vector-length c) 1))
      (begin
        (set! c (columnExtendAbove c (+ h 2)))
        (vector-set! c i o))
    (vector-set! c i o)))
    c))

(define (setCell . x) ())

(define (setCellAgent z y x cell)
  (displayl "\n\e[1;31mSet cell " cell " at " (list z y x)) ; DEBUG
  (letrec ((block (getMapBlock (/ y MapBlockSize) (/ x MapBlockSize) #f))
           (by (modulo y MapBlockSize))
           (bx (modulo x MapBlockSize))
           (column (vector-vector-ref block by bx)))
    (vector-vector-set! block by bx (columnSet column z cell))
    (saveMapBlock (/ y MapBlockSize) (/ x MapBlockSize) block)
    (ipcWrite `(setCell ,z ,y ,x ,cell))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start_everything
;;
(define log (open-file "talk.log"))
(seek-end log 0)

(ipcWrite '(who)) ; Force everyone, including this map agent, to identify themselves

(thread 
 (let ((s (call/cc (lambda (c) (vector-set! ERRORS (tid) c) '*))))
    (or (eq? s '*) (displayl "\nIPC-REPL-ERROR::" s)))
 (let ~ () 
  (let ((e (QueueGet ipcReadQueue)))
     (display "\n\e[1;30mIPC::")
     (write e)
     (display "\e[0m")
     (eval e)
     (~))))

(repl) ; Start up a REPL loop for fun
