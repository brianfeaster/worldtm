(define QUIETLOGIN (not (null? (memv "silent" (vector->list argv)))))
(define CELLANIMATION #t) 
(define SCROLLINGMAP #t)
(define KITTEHBRAIN  #f)
(define VOICEDELIMETER " ")
(define DNA 0)

(load "ipc.scm")
(load "window.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create terminal windows

; Force just only one instance of the Terminal object
; by setting the object value to an instance of itself.
(define Terminal (Terminal))

; Chat window.
(define WinChat ((Terminal 'WindowNew)
  0 0
  (- (Terminal 'THeight) 1)  (Terminal 'TWidth)
  #x0f))
(define WinChatPutc (WinChat 'putc))
(define WinChatPuts (WinChat 'puts))
(define WinChatSetColor (WinChat 'set-color))
(define (WinChatDisplay . l)
  (for-each (lambda (o) (WinChatPuts (display->string o))) l))
(define (WinChatWrite o)
  ((WinChat 'puts) (write->string o)))
(WinChat '(set! ScrollbackHack #t))

; Console window
(define WinConsole ((Terminal 'WindowNew)
  0 0
  (- (Terminal 'THeight) 1)  (Terminal 'TWidth)
  #x02))
(define WinConsolePuts (WinConsole 'puts))
(define (WinConsoleDisplay . e) (for-each (lambda (x) (WinConsolePuts (display->string x))) e))
(define (WinConsoleWrite . e) (for-each (lambda (x) (WinConsolePuts (write->string x))) e))
((WinConsole 'toggle))

; Input Window
(define WinInput ((Terminal 'WindowNew)
  (- (Terminal 'THeight) 1) 0
  1 (Terminal 'TWidth)
  #x4a))
(define WinInputPutc (WinInput 'putc))
(define WinInputPuts (WinInput 'puts))
(define WinInputSetColor (WinInput 'set-color))

; Map Window. Initial map size is 20 or terminal width/height.
(define WinMap
 (let ((MapSize (min 28 (min (- (Terminal 'THeight) 1)
                             (/ (Terminal 'TWidth) 2)))))
  ((Terminal 'WindowNew)
    0 (- (Terminal 'TWidth) (* MapSize 2)) ; Position of the map window
    (+ MapSize 0) (* 2 MapSize)
    #x17 'NOREFRESH)))
((WinMap 'toggle))
((WinMap 'cursor-visible) #f) ; Disable cursor in map window

; Help Window.
(define WinHelpBorder ((Terminal 'WindowNew) 4 19 14 32 #x20))
(define WinHelp ((Terminal 'WindowNew) 5 20 12 30 #x0a))
((WinHelpBorder 'toggle))
((WinHelp 'toggle))

;; Stats window
(define WinStatus ((Terminal 'WindowNew)
   (WinMap 'Y0) (- (Terminal 'TWidth) 16)
   1            14
   #x4e))
((WinStatus 'toggle))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Glyphs - Two characters including their color.
;;
(define (glyphColorNew background foreground)
  (+ (* background 256) foreground))

(define glyphNew vector)
(define (glyph0bg cell) (vector-ref cell 0))
(define (glyph0fg cell) (vector-ref cell 1))
(define (glyph0ch cell) (vector-ref cell 2))
(define (glyph1bg cell) (vector-ref cell 3))
(define (glyph1fg cell) (vector-ref cell 4))
(define (glyph1ch cell) (vector-ref cell 5))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cells - For now cells are just glyphs...eventually smarter objects.
;;
(define MAXCELL 1023)

(define CELLS
 (let ((cells (make-vector (+ 1 MAXCELL))))
  (vector-map! (lambda (i) (cons 'NOTHING (glyphNew 0 7 #\x 0 7 #\x)))
               cells)
  cells))

(define (cellSet      i symb glyph) (vector-set! CELLS i (cons symb glyph)))
(define (cellSetGlyph i glyph)      (set-cdr! (vector-ref CELLS i) glyph))

; Get the cell index of this cell symbol
(define (cellIndex symb)
  (let ~ ((i 0)
          (cell (vector-ref CELLS 0)))
   (if (< MAXCELL i) #f
   (if (eq? symb (car cell)) i
   (~ (+ i 1) (vector-ref CELLS (+ i 1)))))))

; Get the cell glyph given its index or symbol
(define (cellGlyph i)
  (if (integer? i)
    (if (< MAXCELL i) ; An index larger than MAXCEL is assumbed to be an entity object.
      ((entitiesGet i) 'glyph)
      (cdr (vector-ref CELLS i)))
    (let ((index (cellIndex i)))
     (if index
        (cdr (vector-ref CELLS index))
        #f))))

(cellSet MAXCELL 'air (glyphNew 0 0  CHAR-CTRL-@ 0 0 CHAR-CTRL-@))
(load "ultima4.cells")

; Some often used cell indices.
(define cellXX (cellIndex 'xx))
(define cellAIR (cellIndex 'air))
(define cellWATER1 (cellIndex 'water1))
(define cellBRICK (cellIndex 'brick))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Column - Somewhat compact group of objects in one dimension.  Compressed
;;  by including all values within a range with the objects below and above
;;  the range the first and last values in the vector.
;;
;;  Z coordinate   -2 -1  0  1  2| 3  4  5  6  7| 8  9 10 11
;;                ---------------+--------------+-------------
;;                  0  0  0  0  0| 0  5  7  4  9| 9  9  9  9
;;  Default bottom objects       | Real objects |     Default top objects
;;                               |              |
;;          Vector data       #(2  0  5  7  4  9)
;;
;;  Is stored as #(2 0 5 5 3 9) where the first value in the vector is the
;;  position of the first default object. The lower and upper default object
;;  are stored in the 2nd and last position. The position of the top most
;;  default object is derived by adding the vector length to the first vector
;;  value.
;;
; Height of first default bottom object.
(define (columnHeightBottom c)
  (vector-ref c 0))

; Height of first default top object.
(define (columnHeightTop c)
  (+ (columnHeightBottom c) (vector-length c)))

(define (columnRef c h)
  (let ((i (- h (columnHeightBottom c))))
    (if (<= i 0)
      (vector-ref c 1)
    (if (>= i (vector-length c))
      (vector-ref c (- (vector-length c) 1))
    (vector-ref c i)))))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Field - Maps will populate the field as if it were a local cache relative
;;         to the avatar's position.
;;
;; A canvas entry is a vector consisting of a cell column.  The canvas itself
;; is a 2x2 block dynamically updated with cells from a cell field agent.
;; Avatar movement will readjust the field's virtual position in the map
;; coordinate system as well as repopulate/cache the new block areas.
;;
;; Example:                          +--+--+    +--+--+
;;   Assume avatar moved from a to   | a| b|    | e| b|
;;   b in the field  area.  e and f  +--+--+ -> +--+--+
;;   are cached in the area modulo   | c| d|    | f| c|
;;   the field coordinates.          +--+--+    +--+--+
;;
(define FieldBlockSize 64) ; Each field block is 256x256 columns
(define FieldBlockCount 2)       ; A field is made up of 2x2 field blocks
(define FieldSize (* FieldBlockCount FieldBlockSize)) ; For now resulting field size is 512x512
(define FieldBlockCoordinates ()) ; Canvas coordinates in block space.  (1 1) would be (256 256) map space.

; The first pair of field block coordinates is always the upper left corner.
(define (fieldBlockY) (car (car FieldBlockCoordinates)))
(define (fieldBlockX) (cdr (car FieldBlockCoordinates)))

(define (fieldInside? y x)
 (and (pair? FieldBlockCoordinates)
      (<= (fieldBlockY) y)
      (< y (+ (fieldBlockY) FieldSize))
      (<= (fieldBlockX) x)
      (< x (+ (fieldBlockX) FieldSize))))

; Generate a list of coordinate pairs of each block
; that make up the canvas at the specified location.
(define (fieldCreateBlockList y x)
 (let ~ ((m 0) (n 0))
  (if (= m FieldBlockCount) '()
  (if (= n FieldBlockCount) (~ (+ m 1) 0)
  (cons (cons (+ y m) (+ x n))
        (~ m (+ n 1)))))))

; Return list of block coordinates which need to be updated
; if we were to move the current field blocks to this new
; field blocks location.
(define (canvasBlockCoordinatesNew y x)
  (let ~ ((newBlocks (fieldCreateBlockList y x)) ; Blocks associated with new field block origin.
          (currentBlocks FieldBlockCoordinates)) ; Curent blocks associated with current field block coor.
    (if (null? currentBlocks)
      newBlocks
      (~ (list-delete newBlocks (car currentBlocks)) ; Remove current blocks from new block list.
         (cdr currentBlocks)))))

(define (fieldTopBottomBuffer y x)
 (let ((fieldy (fieldBlockY))
       (buffer (/ FieldBlockSize 4)))
  (if (< y (+ (* FieldBlockSize   fieldy) buffer)) '(top)
  (if (<= (- (* FieldBlockSize (+ fieldy FieldBlockCount)) buffer) y) '(bottom)
  ()))))

(define (fieldLeftRightBuffer y x)
 (let ((fieldx (fieldBlockX))
       (buffer (/ FieldBlockSize 4)))
  (if (< x (+ (* FieldBlockSize   fieldx) buffer)) '(left)
  (if (<= (- (* FieldBlockSize (+ fieldx FieldBlockCount)) buffer) x) '(right)
  ()))))

; Create default plane.
(define FIELD ())

(define (resetField)
 (set! FIELD
   (vector-vector-map!
     (lambda (i) (vector -1 cellXX cellAIR))
     (make-vector-vector FieldSize FieldSize ()))))

(resetField)


; Fields are 2d arrays of columns.  Columns are quasi-compressed stacks
; of cells that consist of a start height and specified stack of cells.
; #(3 1 1 2) would be cells (1 1 2) starting at z=4=(3+1). Cells below and
; above are assumed to be the lowest and higest specified cells in the vector.
; Setting a cell outside the explicit stack range expands the actual vector
; and adjusts the start-height value.
(define (fieldColumn y x)
 (vector-vector-ref FIELD (modulo y FieldSize)
                          (modulo x FieldSize)))

; Query the first cell at this location.  Mainly used as the object to display.
(define (field-ref z y x)
 (letrec ((column   (fieldColumn y x))
          (elements (columnRef column z)))
   (if (pair? elements) (car elements) elements)));1st in improper list of objs

; Query the last cell at this location.  Used to get the base/non-entity object.
(define (field-base-ref z y x)
 (letrec ((column   (fieldColumn y x))
          (elements (columnRef column z)))
   (last elements))) ; Last in improper list of objs


; Scan down map column starting at z for first visibile cell.  Return height.
; BF:  What should this return if z is below the bottom most explicit cell?
(define (field-ref-top z y x)
 (letrec ((column (fieldColumn y x))
          (top    (columnHeightTop column)) ;1st implicit top cell in column
          (bot    (columnHeightBottom column)));1st implicit bottom cell in col
 ; Adjust the z coor down to the first explicit cell in the column
 (let findNonAir ((z (if (>= z top) (set! z (- top 1)))))
   (if (or (not (eq? (columnRef column z) cellAIR))
           (<= z bot))
       (if (pair? z) (car z) z) ; Return first in improper list of objects.
       (findNonAir (- z 1))))))

; Replace all cells at this Z location with a single cell.
(define (field-set! z y x c)
 (letrec ((fy (modulo y FieldSize))
          (fx (modulo x FieldSize))
          (column (vector-vector-ref FIELD fy fx)))
  (vector-vector-set! FIELD fy fx
     (columnSet column z c ))))

; Insert the cell at this Z location.  Creates a malformed list.
(define (field-add! z y x c)
 (letrec ((fy (modulo y FieldSize))
          (fx (modulo x FieldSize))
          (column (vector-vector-ref FIELD fy fx)))
  (vector-vector-set! FIELD fy fx
     (columnSet column z (cons c (columnRef column z))))))

; Remove the cell from the malformed list at this Z location.
(define (field-delete! z y x e)
 (letrec ((fy (modulo y FieldSize))
          (fx (modulo x FieldSize))
          (column (vector-vector-ref FIELD fy fx)))
  (vector-vector-set! FIELD fy fx
     (columnSet column z (list-delete (columnRef column z) e)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Canvas
;;
;; A canvas entry is a vector consisting of a cell and it's Z coordinate (the
;; top most visible).  The canvas itself is a flattened map field.
(define CANVAS (make-vector-vector FieldSize FieldSize ()))

(define (canvasCell y x)
  (car (vector-vector-ref CANVAS
         (modulo y FieldSize)
         (modulo x FieldSize))))

(define (canvasHeight y x)
  (cdr (vector-vector-ref CANVAS
         (modulo y FieldSize)
         (modulo x FieldSize))))

(define (canvasCellSet y x c)
  (set-car! (vector-vector-ref CANVAS
              (modulo y FieldSize)
              (modulo x FieldSize))
            c))

(define (canvasHeightSet y x h)
 (set-cdr! (vector-vector-ref CANVAS
             (modulo y FieldSize)
             (modulo x FieldSize))
           h))

(define (canvasRender y x)
 (let ((top (field-ref-top 1000 y x)))
  (canvasCellSet y x (cellGlyph (field-ref top y x)))
  (canvasHeightSet y x top)))

; Initialize the canvas with the Construct[tm].
(define timeStart (time))

(define (resetCanvas)
 (let ((defaultCell (cellGlyph 'xx)))
  (let ~ ((y 0)(x 0))
   (if (!= y FieldSize) (if (= x FieldSize) (~ (+ y 1) 0) (begin
    ; Each canvas entry consists of a map cell and its height.
     (vector-vector-set! CANVAS y x (cons defaultCell 0))
     (~ y (+ x 1))))))))
(resetCanvas)

(WinConsoleDisplay "Initialized Canvas " (- (time) timeStart) " seconds")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Viewport
;;
(define MapWindowSemaphore (open-semaphore 1))

(define PortY 0)
(define PortX 0)
(define PortH 0)
(define PortW 0)

(define (viewportReset y x)
 (set! PortH (WinMap 'WHeight)) ; Adjust Viewport dimensions.
 (set! PortW (/ (WinMap 'WWidth) 2))
 (set! PortY (- y (/ PortH 2)))     ; Center Viewport around Avatar.
 (set! PortX (- x (/ PortW 2)))
 ;((WinMap 'home))
 (let ~dumpGlyphs ((y 0) (x 0))
  (if (!= y PortH) (if (= x PortW) (~dumpGlyphs (++ y) 0)
    (begin
     (WinMapPutGlyph (canvasCell (+ PortY y) (+ PortX x))
                     y
                     (* x 2))
     (~dumpGlyphs y (++ x))))))
 ; Plot column of cells at avatar's (y x) location.  For debugging.
 ((WinColumn 'home))
 (let ~ ((z 11))
  (let ((c (field-ref z y x)))
   (if (eq? cellAIR c)
    (begin (WinColumnSetColor 0 8)
           (WinColumnPuts "()  "))
    (begin (set! c (cellGlyph c))
           (WinColumnSetColor (glyph0bg c) (glyph0fg c))
           (WinColumnPutc (glyph0ch c))
           (WinColumnSetColor (glyph1bg c) (glyph1fg c))
           (WinColumnPutc (glyph1ch c))
           (WinColumnSetColor 0 7)
           (set! c (field-base-ref z y x))
           (if (and (<= 0 c) (< c 256))
            (begin
             (if (< c 16) (WinColumnPuts "0"))
             (WinColumnPuts (number->string c 16)))
            (WinColumnPuts "  ") ))))
  (if (> z -6) (~ (- z 1)))))

(define (viewportRender gy gx)
 ; The cell position and viewport position (upper left corner) are on a torus
 ; coordinate system (wrap along the two dimensions).  I want to render to
 ; the screen cells plotted within the viewport .  I can do this by shifting
 ; the viewport to the origin and the cell by the same amount then checking if
 ; the cell is between the origin and lower right corner of the viewport.
 ;
 ; IE: (Cell % FieldWidth - Viewport % FieldWidth) % Fieldwidth < ViewportWidth
 ; But it would seem modulo distributes:  (a%m - b%m)%m == (a-b)%m%m == (a-b)%m
 ; so the actual computation is a bit simpler.  Smokin.
 (let ((y (modulo (- gy PortY) FieldSize)) ; Normalize avatar position.
       (x (modulo (- gx PortX) FieldSize)))
  (and (< y PortH) (< x PortW)
    (WinMapPutGlyph (canvasCell gy gx) y (* x 2)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Map manipulation
;;

; Drop a cell on the map
(define (dropCell y x cell)
 (let ((z (+ 1 (field-ref-top 1000 y x))))
  (field-set! z y x cell)
  (canvasRender y x)
  (viewportRender y x)))

; Set map cell and force rendering of cell through entire pipeline.
(define (setCell z y x cell)
  (field-set! z y x cell)
  (canvasRender y x)
  (viewportRender y x))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entites - Simple objects more dynamic than just cells.
;;
(define (Entity dna name z y x glyph)
 (define (self msg) (eval msg))
 (define (setAll name0 z0 y0 x0 glyph0)
  (set! name name0)
  (set! z z0)
  (set! y y0)
  (set! x x0)
  (set! glyph glyph0))
 (define (setGlyph glyph0)
  (set! glyph glyph0))
 (define (setNameGlyph name0 glyph0)
  (set! name name0)
  (set! glyph glyph0))
 (define (setLoc z0 y0 x0)
  (set! z z0)
  (set! y y0)
  (set! x x0))
 (define (gps) (list z y x))
 self)

; Association list of entites to their DNA values.
(define EntityDB ())

; Create a default "nobody" entity.
(define nobody (Entity 42 "nobody" 0 0 0 (glyphNew 0 7 #\n 0 7 #\o)))

; Update or create and add a new entity to the entity database.
(define (entitiesSet dna name z y x glyph)
 (let ((ent (assv dna EntityDB)))
  (if (null? ent)
      (set! EntityDB (cons (cons dna (Entity dna name z y x glyph))
                           EntityDB))
      (((cdr ent) 'setAll) name z y x glyph)))) ; Clone the remote entitiy locally.

; Lookup entity, or null, in database.
(define (entitiesGet dna)
 (let ((e (assv dna EntityDB)))
   (if (null? e)
       nobody
       (cdr e))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Avatar
;;
(define (Avatar name) ; Inherits Entity
 ((Entity (random) name
   1 108 86
   (glyphNew 0 15 (string-ref name 0) 0 15 (string-ref name 1)))
  `(let ()
    (define (self msg) (eval msg))
    (define dir 0)
    (define (jump zz yy xx) (set! z zz) (set! y yy) (set! x xx))
    (define (face dirNew) (set! dir dirNew))
    (define (move)
      (if (= dir 0) (jump (+ z -1) y      x)
      (if (= dir 1) (jump z        (++ y) (-- x))
      (if (= dir 2) (jump z        (++ y) x)
      (if (= dir 3) (jump z        (++ y) (++ x))
      (if (= dir 4) (jump z        y      (-- x))
      (if (= dir 5) (jump (+ z 1)  y      x)
      (if (= dir 6) (jump z        y      (++ x))
      (if (= dir 7) (jump z        (-- y) (-- x))
      (if (= dir 8) (jump z        (-- y) x)
      (if (= dir 9) (jump z        (-- y) (++ x)))))))))))))
    (define (walk dir)
     (face dir)
     (move))
    (define (look)
      (let ((loc (gps)))
        (move) ; temporarily walk to this location
        (let ((c (field-ref z y x)))
          (apply setLoc loc) ; restore old location.
          c)))
    (define cell cellBRICK)
    (define (gpsLook)
      (if (= dir 0) (list (+ z -1) y      x)
      (if (= dir 1) (list z        (++ y) (-- x))
      (if (= dir 2) (list z        (++ y) x)
      (if (= dir 3) (list z        (++ y) (++ x))
      (if (= dir 4) (list z        y      (-- x))
      (if (= dir 5) (list (+ z 1)  y      x)
      (if (= dir 6) (list z        y      (++ x))
      (if (= dir 7) (list z        (-- y) (-- x))
      (if (= dir 8) (list z        (-- y) x)
      (if (= dir 9) (list z        (-- y) (++ x)))))))))))))
    self) ))

(define avatar (Avatar "Guest"))
(set! DNA (avatar 'dna))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard Input
;;
(define AvatarDirectionGlyphs
 (vector
   (glyphNew 0 7  #\? 0 7  #\?)
   (glyphNew 4 15 #\\ 4 15 #\_)
   (glyphNew 4 15 #\\ 4 15 #\/)
   (glyphNew 4 15 #\_ 4 15 #\/)
   (glyphNew 4 15 #\< 4 15 #\=)
   (glyphNew 4 15 #\o 4 15 #\o)
   (glyphNew 4 15 #\= 4 15 #\>)
   (glyphNew 4 15 #\/ 4 15 #\~)
   (glyphNew 4 15 #\/ 4 15 #\\)
   (glyphNew 4 15 #\~ 4 15 #\\)))

; BF: Should this be a thread?  At least move somewhere else.
(define (refreshIfBeyondViewport)
 (let ((y (modulo (- (avatar 'y) PortY) FieldSize));Normalize avatar position.
   (x (modulo (- (avatar 'x) PortX) FieldSize)))
   (if (or (<= PortW x) (<= PortH y))
     (viewportReset (avatar 'y) (avatar 'x)))))

; TODO: Implement coor+dir use new dir to field-ref location an dif > MAX_CELL ipc-write (force blah blah)

(define SOLIDCELLS (list
 (cellIndex 'xx)
 (cellIndex 'mnts)
 (cellIndex 'hills)
 (cellIndex 'water0)
 (cellIndex 'water1)
 (cellIndex 'water2)
 (cellIndex 'brickc)
 (cellIndex 'column)
 (cellIndex 'window)
 (cellIndex 'doorl)))

(define (walk dir)
 ((avatar 'face) dir) ; Turn avatar
 (let ((nextCell ((avatar 'look)))) ; Consider cell I'm walking into
   (if (< MAXCELL nextCell) ; Cell I'm walking into is probably an entity
    ; Push the entity that's in my way
    ((ipc 'qwrite) `(force ,@((avatar 'gpsLook)) ,dir 10))
    ; Walk normally
    (if (null? (memv nextCell SOLIDCELLS))
     (begin
      ((avatar 'move))
      ((ipc 'qwrite) `(move ,DNA ,@((avatar 'gps))))
      (if (eq? (cellIndex 'help) (field-ref (avatar 'z) (avatar 'y) (avatar 'x)))  (help))
      (if (eq? (cellIndex 'snake) (field-ref (avatar 'z) (avatar 'y) (avatar 'x))) (thread (snake-random)))
      (if (eqv? (cellIndex 'brit2) (field-base-ref (avatar 'z) (avatar 'y) (avatar 'x))) (thread (spawnKitty)))
      ;(WinConsoleWrite (fieldColumn (avatar 'y) (avatar 'x)) (field-ref (avatar 'z) (avatar 'y) (avatar 'x)))
      ; Dump our coordinates.
      ((WinStatus 'puts) "\r\n")
      ((WinStatus 'puts) (number->string (avatar 'z) 16))
      ((WinStatus 'puts) " ")
      ((WinStatus 'puts) (number->string (avatar 'y) 16))
      ((WinStatus 'puts) " ")
      ((WinStatus 'puts) (number->string (avatar 'x) 16)))))))

; Change avatar color.  Will just cycle through all 16 avatar colors.
(define (avatarColor)
 (let ((glyph (avatar 'glyph)))
  ((avatar 'setGlyph)
    (glyphNew (glyph0bg glyph)
              (modulo (+ (glyph0fg glyph) 1) 16)
              (glyph0ch glyph)
              (glyph1bg glyph)
              (modulo (+ (glyph1fg glyph) 1) 16)
              (glyph1ch glyph))))
  (who))

(define (rollcall)
 ((ipc 'qwrite) `(if (!= DNA ,DNA) ((ipc 'qwrite) `(if (= DNA ,,DNA) (voice 0 10 (string ,(avatar 'name)" is present in world")))))))
 ;(WinChatSetColor #x0e)
 ;;(WinChatDisplay name))
 ;(WinChatSetColor #x07)
 ;(WinChatDisplay " is present in World[tm] as "))
 ;(WinChatSetColor (glyphColor0 glyph))
 ;(WinChatDisplay (glyphChar0 glyph))
 ;(WinChatSetColor (glyphColor1 glyph))
 ;(WinChatDisplay (glyphChar1 glyph)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Incomming IPC messages 
;;
(define (entity dna name z y x glyph)
 (entitiesSet dna name z y x glyph)
 (move dna z y x))


(define (who)
 ((ipc 'qwrite)
 `(entity ,DNA ,(avatar 'name) ,@((avatar 'gps)) ,(avatar 'glyph))))

;; Call this 'move.  Entites move.  Back to the original ways?
;; was 'avatar
(define (move dna z y x)
 (letrec ((entity (entitiesGet dna))
          (thisIsMe (= dna DNA))
          (shouldScroll (and thisIsMe
           (or SCROLLINGMAP (< 10 (distance (list 0 (avatar 'y) (avatar 'x))
                                           (list 0 (+ PortY (/ PortH 2)) (+ PortX (/ PortW 2)))))))))
  (if (null? entity)
    (begin
      (entitiesSet dna "??" z y x (glyphNew 1 9 #\? 1 9 #\?))
      ((ipc 'qwrite) '(who)))
    (begin
      ; Move from here
      (field-delete! (entity 'z) (entity 'y) (entity 'x) dna)
      (if (>= (entity 'z) (canvasHeight (entity 'y) (entity 'x))) (begin
        (canvasRender (entity 'y) (entity 'x))
        (or shouldScroll (viewportRender (entity 'y) (entity 'x)))))
      ; Place here
      ((entity 'setLoc) z y x)
      (field-add! z y x dna)
      (if (>= (entity 'z) (canvasHeight y x)) (begin
        (canvasRender y x)
        (or shouldScroll (viewportRender y x))))
      (if shouldScroll
        (viewportReset (avatar 'y) (avatar 'x)))))))

(define (force z y x dir str)
 (if (and (= z (avatar 'z)) (= y (avatar 'y)) (= x (avatar 'x)))
   (walk dir)))

(define (die dna)
 (let ((entity (entitiesGet dna))
       (thisIsMe (= dna DNA)))
  (if (not (null? entity)) ; Ignore unknown entities
    (begin
      ; Remove from here
      (field-delete! (entity 'z) (entity 'y) (entity 'x) dna)
      (if (>= (entity 'z) (canvasHeight (entity 'y) (entity 'x))) (begin
        (canvasRender (entity 'y) (entity 'x))
        (or thisIsMe (viewportRender (entity 'y) (entity 'x)))))))))


(define (voice dna level text)
 (if (= dna 0)
  (begin ; Message from the system
    (WinChatDisplay "\r\n")
    (WinChatSetColor 0 9) (WinChatDisplay "W")
    (WinChatSetColor 0 11) (WinChatDisplay "O")
    (WinChatSetColor 0 10) (WinChatDisplay "R")
    (WinChatSetColor 0 12) (WinChatDisplay "L")
    (WinChatSetColor 0 13) (WinChatDisplay "D")
    (WinChatSetColor 0 8) (WinChatDisplay VOICEDELIMETER)
    (WinChatSetColor 0 7) (WinChatDisplay text))
  (let ((entity (entitiesGet dna)))
    (WinChatSetColor (glyph0bg (entity 'glyph)) (glyph0fg (entity 'glyph)))
    (WinChatDisplay "\r\n" (if (null? entity) "???" (entity 'name)) VOICEDELIMETER)
    (WinChatSetColor (glyph1bg (entity 'glyph)) (glyph0fg (entity 'glyph)))
    (WinChatDisplay text)))
 (if (and (!= dna DNA) (eqv? text "unatco")) (say "no Savage")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get the wheels in motion
;;

; Make Map window circular!
(define (circularize . val)
 (set! val (not (null? val))) ; Default to disabling circular corners of map.
 (let ~ ((y 0)(x 0))
  (if (< y (/ (WinMap 'WHeight) 2))
  (if (= x (/ (WinMap 'WWidth) 2)) (~ (+ y 1) 0)
   (begin
    (if (> (sqrt (+ (* 4 (^2 (- y (/ (WinMap 'WHeight) 2))))
                    (^2 (- x  (/ (WinMap 'WWidth) 2)))))
           (+ 0 (WinMap 'WHeight)))
        (begin 
          ((WinMap 'alpha) y x val)
          ((WinMap 'alpha) y (- (WinMap 'WWidth) x 1) val)
          ((WinMap 'alpha) (- (WinMap 'WHeight) y 1) x val)
          ((WinMap 'alpha) (- (WinMap 'WHeight) y 1)
                           (- (WinMap 'WWidth) x 1) val)
          ))
    (~ y (+ x 1)))))))

(circularize)


(define deltaMoveTime (+ 62 (utime))) ; Double click 1/16 sec.

(define (winMapBigger)
 (if (< (WinMap 'WHeight) (Terminal 'THeight)) (begin
  ;((WinMap 'toggle))
  ((WinMap 'home))
  (if (< (utime) deltaMoveTime)
    ((WinMap 'moveresize) ; Full resize.
       0 (- (Terminal 'TWidth) (WinMap 'WWidth) 2)
             (min (/ (Terminal 'TWidth) 2) (- (Terminal 'THeight) 1))
       (* 2  (min (/ (Terminal 'TWidth) 2) (- (Terminal 'THeight) 1)))))
    ((WinMap 'moveresize) ; Resize by one.
       0 (- (Terminal 'TWidth) (WinMap 'WWidth) 2)
       (+ 1 (WinMap 'WHeight))
       (+ 2 (WinMap 'WWidth)))
  (circularize)
  (viewportReset (avatar 'y) (avatar 'x))
  ;((WinMap 'toggle))
  (set! deltaMoveTime (+ 250 (utime))))))

(define (winMapSmaller)
 (if (< 5 (WinMap 'WHeight)) (begin
  ;((WinMap 'toggle))
  ((WinMap 'home))
  ((WinMap 'moveresize)
     0 (- (Terminal 'TWidth) (WinMap 'WWidth) -2)
     (+ -1 (WinMap 'WHeight))
     (+ -2 (WinMap 'WWidth)))
  (circularize)
  (viewportReset (avatar 'y) (avatar 'x))
  ;((WinMap 'toggle))
)))

(define (winMapUp)  ((WinMap 'move)  (+ -1 (WinMap 'Y0)) (WinMap 'X0)))
(define (winMapDown) ((WinMap 'move)  (+  1 (WinMap 'Y0)) (WinMap 'X0)))
(define (winMapLeft)    ((WinMap 'move) (WinMap 'Y0) (+ -1 (WinMap 'X0))))
(define (winMapRight)  ((WinMap 'move) (WinMap 'Y0) (+  1 (WinMap 'X0))))

 
(define WinMapSetColor (WinMap 'set-color))
(define WinMapPutc (WinMap 'putc))
(define (WinMapPutGlyph glyph y x)
  (semaphore-down MapWindowSemaphore)
  ((WinMap 'goto) y x)
  ; Animation hack.  Every second use the default glyph or each
  ; consecutive glyph (stored as consecutive vectors of simple
  ; glyphs).
  (let ((t (time)) ; capture the time
        (l (- (vector-length glyph) 5))) ; number of extra animation glyphs
    (if (> (modulo t l) 0) (set! glyph (vector-ref glyph (+ 5 (modulo t l)))))
    (WinMapSetColor (glyph0bg glyph) (glyph0fg glyph))
    (WinMapPutc     (glyph0ch glyph))
    (WinMapSetColor (glyph1bg glyph) (glyph1fg glyph))
    (WinMapPutc     (glyph1ch glyph)))
  (semaphore-up MapWindowSemaphore))


;; Map column debug window
(define WinColumn ((Terminal 'WindowNew) 1 (- (Terminal 'TWidth) 4) 18 4 #x5b))
((WinColumn 'toggle))
(define WinColumnPutc (WinColumn 'putc))
(define (WinColumnPuts . l) (for-each (WinColumn 'puts) l))
(define WinColumnSetColor (WinColumn 'set-color))



(define (welcome)
 (define WinMarquee
  ((Terminal 'WindowNew)
    (/ (Terminal 'THeight) 3)  (- (/ (Terminal 'TWidth) 2) 12)
    3  24
    #x0f))
 (define WinMarqueePuts (WinMarquee 'puts))
 (define WinMarqueePutc (WinMarquee 'putc))
 (define WinMarqueeSetColor (WinMarquee 'set-color))
 ((WinMarquee 'cursor-visible) #f) ; Disable cursor in map window.
 (WinMarqueePuts " +====================+ ")
 (WinMarqueePuts " |                    | ")
 (WinMarqueePuts " +====================+ ")
 (let ~~ ((i -5))
   (sleep 200) ; Delay
   ((WinMarquee 'goto) 0 0)
   (WinMarqueeSetColor 0 15)
   (if (= 0 (modulo i 4)) (begin
      (WinMarqueePuts "  +====================+")
      (WinMarqueePuts " /                    / ")
      (WinMarqueePuts "+====================+  "))
   (if (or (= 3 (modulo i 4)) (= 1 (modulo i 4))) (begin
      (WinMarqueePuts " +====================+ ")
      (WinMarqueePuts " |                    | ")
      (WinMarqueePuts " +====================+ "))
   (begin
      (WinMarqueePuts "+====================+  ")
      (WinMarqueePuts " \\                    \\ ")
      (WinMarqueePuts "  +====================+"))))
   ((WinMarquee 'goto) 1 2)
   (let ~ ((j 0))
     (WinMarqueeSetColor 0 (vector-ref #(07 07 07 07 07 07 07 07   07 07 07
                                       9 11 10 12 5  1  2  6  4 7)
                                     (modulo (+ i j) 21)))
     (WinMarqueePutc (vector-ref #(#\W #\e #\l #\c #\o #\m #\e #\   #\t #\o #\ 
                                   #\W #\o #\r #\l #\d #\[ #\t #\m #\] #\ )
                                 (modulo (+ i j) 21)))
     (if (< j 19) (~ (+ j 1)))) ; Marquee area width
   (if (< i 60) ; Msg scroll count
       (~~ (+ i 1))))
 ((WinMarquee 'delete)))

(define (makeProgressBar y x title)
 (let ((win ((Terminal 'WindowNew) y x 3 22 #x4a))
       (pos 0))
  ((win 'cursor-visible) #f)
  ((win 'puts) "+--------------------+")
  ((win 'puts) "|                    |")
  ((win 'puts) "+--------------------+")
  ((win 'goto) 0 2) ((win 'puts) title)
  ((win 'goto) 1 1) ((win 'set-color) 7 0)
  (lambda ()
   (if (= pos 20)
    (begin
     ((win 'delete))
     #f)
    (begin
     ((win 'putc) #\ )
     (set! pos (+ pos 1))
     #t)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ultima city detail map.
;;
;; Mapping from Ultima4 map file byte index to (y x) World[tm] coordinates.
;;  y = (+ (modulo (/ i 32) 32) (* (/ i 8192) 32))
;;  x = (+ (modulo i 32) (* (modulo (/ i 1024) 8) 32))
;; Mapping from World[tm] (y x) corrdinates to Ultima4 map file byte index.
;; i = (+ (modulo x 32) (* (/ x 32) 1024) (* (modulo y 32) 32) (* (/ y 32) 8192))
;;
;; Lord British's castle (108 86)

(define U4MapSize 256) ; Size of the map file.
(define U4MapCellSize 32) ; Size of each map file cell in the World map.
(define U4Size (* U4MapSize U4MapCellSize))

; The Ultima4 map file is 8x8 blocks of 32x32 cells
; so create a simpler 256x25 array of cell numbers.
(define U4MapVector
 (let ((fd (open "ultima4.map"))
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

(define lcbfd (open "lcb1.ult"))
(define (U4Lcb1MapCell y x)
 (and (< y 0) (<= 32 y) (< x 0) (<= 32 x) (WinChatDisplay "lcb1 coordinates out of range") (quit))
 (seek lcbfd (+ (* y 32) x))
 (+ 0 (read-char lcbfd))) ; Hack to convert char to integer

(define britainfd (open "britain.ult"))
(define (U4BritainMapCell y x)
 (and (< y 0) (<= 32 y) (< x 0) (<= 32 x) (WinChatDisplay "Britain coordinates out of range") (quit))
 (seek britainfd (+ (* y 32) x))
 (+ 0 (read-char britainfd))) ; Hack to convert char to integer

; Get the map cell in the direction from this location.
(define (U4MapCellDir y x d)
 (U4MapCell (+ y (vector-ref #(0 -1 -1 -1  0  1 1 1) d))
            (+ x (vector-ref #(1  1  0 -1 -1 -1 0 1) d))))

; I will load the ultima5 map some day.  The ovl is a 16x16
; array indexing the 16x16 cell arrays in the map file.
(define (load-ultima-world5)
 (let ((fdm (open "ultima5.map"))
       (fdi (open "ultima5.ovl"))
       (bar (makeProgressBar 1 30 "Britannia 4")))
 (let ~ ((y 0) (x 0)) (if (< y 256) (if (= x 256) (~ (+ y 1) 0) (begin
   (if (and (= x 255) (= 0 (modulo y 12))) (bar)) ; Progress bar
   (let ((index (begin (seek fdi (+ (/ x 16) (* (/ y 16) 16)))
                       (+ 0 (read-char fdi)))))
    (setCell 2 y x 
      (if (= 255 index) cellWATER1 (begin
        (seek fdm (+ (* index 256) (modulo x 16) (* (modulo y 16) 16)))
        (+ 0 (read-char fdm))))))
   (~ y (+ x 1))))))))


; Debugging.  Update field blocks with numbered cells.
(define updateFieldBlocks (let ((nextCell 99)) (lambda (y0 x0)
  (letrec ((y1 (+ FieldBlockSize y0)) ; Lower left map coordinates of block
           (x1 (+ FieldBlockSize x0))
           (cellGlyph (set! nextCell (+ 1 nextCell)))
           (glyph (glyphNew 0 (+ 1 (modulo cell 7) 1) (integer->char (+ 48 (/ (- cell 100)      10)))
                            0 (+ 1 (modulo cell 7) 1) (integer->char (+ 48 (modulo (- cell 100) 10))))))
   (cellSet cell glyph)
   (loop2 y0 y1 x0 x1 (lambda (y x) (setCell 0 y x cell))) ))))

(define (inUltima4Range? y x) (and (<= 0 y) (< y U4Size) (<= 0 x) (< x U4Size)))

(define (inUltima4RangeLcb1? y x)
 (and (<= (* 107 U4MapCellSize) y)
      (< y (* 108 U4MapCellSize))
      (<= (* 86 U4MapCellSize) x)
      (< x (* 87 U4MapCellSize))))

(define (inUltima4RangeBritain? y x)
 (and (<= (* 106 U4MapCellSize) y)
      (< y (* 107 U4MapCellSize))
      (<= (* 82 U4MapCellSize) x)
      (< x (* 83 U4MapCellSize))))

; Outdated?
(define (load-ultima-world44)
 (resetField)
  (let ~ ((y 0)(x 0)) (or (= y 256) (if (= x 256) (~ (+ y 1) 0) (begin
      (setCell 1 y x
        (let ((yy (/ (+ y (* 108 (- U4MapCellSize 1)) ) U4MapCellSize))
              (xx (/ (+ x (*  86 (- U4MapCellSize 1)) ) U4MapCellSize)))
         (let ((dist (distance
                      (list 0 y x)
                      (list 0 (+ 20 (* (/ y U4MapCellSize) U4MapCellSize))
                              (+ 20 (* (/ x U4MapCellSize) U4MapCellSize))))))
          (if (< dist 100) (U4MapCell yy xx) (cellIndex 'help))
          )))
      (~ y (+ x 1)))))))

(define (updateFieldBlocksU4 y0 x0)
  (loop2 y0 (+ y0 FieldBlockSize)
         x0 (+ x0 FieldBlockSize)
     (lambda (y x)
        (if (inUltima4Range? y x)
          (begin
            (if (inUltima4RangeLcb1? y x)
              (setCell 1 y x (U4Lcb1MapCell (- y (* 107 U4MapCellSize)) (- x (* 86 U4MapCellSize)))) ; LB castle
            (if (inUltima4RangeBritain? y x)
              (setCell 1 y x (U4BritainMapCell (- y (* 106 U4MapCellSize)) (- x (* 82 U4MapCellSize)))) ; Britain
            (letrec ((ty (- y (* (/ y U4MapCellSize) U4MapCellSize) (/ U4MapCellSize 2)))
                     (tx (- x (* (/ x U4MapCellSize) U4MapCellSize) (/ U4MapCellSize 2)))
                     (dist (sqrt (+ (* ty ty) (* tx tx)))))
               (setCell 1 y x
                 (if (and #f (= 0 (random 6)) (> dist 14)) ; border of cells get random adjacent glyphs.
                     79
                     (U4MapCell (/ y U4MapCellSize) (/ x U4MapCellSize)))))))) ; Regular cell
          (setCell 1 y x NOTHING)))))

; Field block updater.
(thread (let ~ ()
 (sleep 500)
 (letrec ((avty (avatar 'y))
          (avtx (avatar 'x))
          (blky (+ (/ (if (< avty 0) (- avty FieldBlockSize) avty) FieldBlockSize) -0)) ; TODO Field block synchronization is off.  On startup loads twice.  If ofset is -1, doesn't cache correctly with avatar movement.
          (blkx (+ (/ (if (< avtx 0) (- avtx FieldBlockSize) avtx) FieldBlockSize) -0)))
  ; Create the first block list and load each block.  Occurs once.
  (if (null? FieldBlockCoordinates ) (begin
     (set! FieldBlockCoordinates (fieldCreateBlockList blky blkx))
     (map (lambda (b) (updateFieldBlocksU4 (* FieldBlockSize (car b)) (* FieldBlockSize (cdr b))))
          FieldBlockCoordinates)))
  ; Forever updated the field blocks.
  (let ((bounds (append (fieldTopBottomBuffer avty avtx)
                        (fieldLeftRightBuffer avty avtx))))
   (if (pair? bounds) (begin
     (let ((yb (fieldBlockY))  ; Assume new block origin at same place
           (xb (fieldBlockX))) ; then adjust based on new position.
      (if (pair? (memq 'bottom bounds)) (set! yb (- blky (- FieldBlockCount 2))))
      (if (pair? (memq 'top bounds))    (set! yb (- blky 1)))
      (if (pair? (memq 'right bounds))  (set! xb (- blkx (- FieldBlockCount 2))))
      (if (pair? (memq 'left bounds))   (set! xb (- blkx 1)))
      (map (lambda (b) (updateFieldBlocksU4 (* FieldBlockSize (car b)) (* FieldBlockSize (cdr b))))
           (canvasBlockCoordinatesNew yb xb))
      ; Set block coordinate list after the unloaded blocks have been loaded.
      (set! FieldBlockCoordinates (fieldCreateBlockList yb xb)))))))
 (~)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help window.  A bunch of button commands for now.

; Give the windows an 'artistic' border.
(map (lambda (x) ((WinHelp 'alpha) 0 x #f)) '(0 1 2 3 4 5 6 7 8 21 22 23 24 25 26 27 28 29))
(map (lambda (x) ((WinHelpBorder 'alpha) 0 x #f)) '(0 1 2 3 4 5 6 7 8 23 24 25 26 27 28 29 30 31))

;((WinHelp 'goto) 0 0)
;((WinHelp 'set-color) 0 10)
((WinHelp 'puts) "          !! Help !!")
((WinHelp 'set-color) 0 15)
((WinHelp 'puts) "\r\n?  toggle help window")
((WinHelp 'puts) "\r\nt  talk mode (esc to exit)")
((WinHelp 'puts) "\r\nc  talk color")
((WinHelp 'puts) "\r\nm  toggle map")
((WinHelp 'puts) "\r\nW  toggle animation")
((WinHelp 'puts) "\r\n>  increase map size")
((WinHelp 'puts) "\r\n<  decrease map size")
((WinHelp 'puts) "\r\np  present user list")
((WinHelp 'puts) "\r\nq  quit World[tm]")

(define (help)
 ((WinHelp 'toggle))
 ((WinHelpBorder 'toggle)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buttons
(define Buttons (make-vector 257 (lambda())))

(define (setButton ch exp)
 (vector-set! Buttons ch exp)) ; For now characters are also integer constants.

(define ShowButtons #f)
(define (showButtons) (set! ShowButtons (not ShowButtons)))

; Consider the button value.  If data just return the expression.  Otherwise
; assume a closure.  Consider the closure -> closure's code -> the code's pre-compiled
; expression and return it (hack).
(define (getButton ch)
 (let ((val (vector-ref Buttons ch)))
   (if (pair? val)  val
   (if (procedure? val) (cons 'lambda (vector-ref (vector-ref (vector-ref Buttons ch) 0) 2))
   val))))

(setButton #\p '(rollcall))
(setButton #\c '(avatarColor))
(setButton #\j '(walk 2))
(setButton #\B '(walk 2))
(setButton #\k '(walk 8))
(setButton #\A '(walk 8))
(setButton #\h '(walk 4))
(setButton #\D '(walk 4))
(setButton #\l '(walk 6))
(setButton #\C '(walk 6))
(setButton #\b '(walk 1))
(setButton #\n '(walk 3))
(setButton #\y '(walk 7))
(setButton #\u '(walk 9))
(setButton #\+ '(walk 5))
(setButton #\- '(walk 0))
(setButton #\H '(winMapLeft))
(setButton #\J '(winMapDown))
(setButton #\K '(winMapUp))
(setButton #\L '(winMapRight))
(setButton #\W '(begin
  (set! CELLANIMATION (not CELLANIMATION))
  (WinChatDisplay "\r\nCell animation " CELLANIMATION)))
(setButton #\s '(begin
  (set! SCROLLINGMAP (not SCROLLINGMAP))
  (WinChatDisplay "\r\nalwaysScroll " SCROLLINGMAP)))
(setButton #\m '((WinMap 'toggle)))
(setButton #\t '(begin
  (WinInputPuts (string ">" (replTalk 'getBuffer)))
  (set! state 'talk)))
(setButton CHAR-CTRL-F '(walkForever))
(setButton CHAR-CTRL-L '(begin (viewportReset (avatar 'y) (avatar 'x)) ((WinChat 'repaint))))
(setButton CHAR-CTRL-M '(begin ((WinStatus 'toggle)) ((WinColumn 'toggle))))
(setButton CHAR-CTRL-Q '(set! state 'done))
(setButton #\d '((ipc 'qwrite) `(dropCell ,(avatar 'y) ,(avatar 'x) (cellIndex 'brick))))
(setButton #\g 
   '(let ((o (field-ref (avatar 'z) (avatar 'y) (avatar 'x))))
     (field-delete!  (avatar 'z) (avatar 'y) (avatar 'x) o)
     (avatar `(set! cell ,o))))
(setButton #\? '(help))
(setButton #\< '(winMapSmaller))
(setButton #\> '(winMapBigger))
(setButton #\z '(circularize))
(setButton #\Z '(circularize #t))
(setButton #\q '(set! state 'done))
(setButton #\Q '(set! state 'done))
(setButton eof '(set! state 'done))
(setButton CHAR-CTRL-C '((WinConsole 'toggle)))
;(setButton CHAR-CTRL-K '((ipc 'qwrite) `(set! FIELD ,FIELD))) ; Send my plane out to IPC.
;(setButton #\1 '(thread (sigwinch)))
;(setButton CHAR-CTRL-_ '(walk 4)) ; Sent by backspace?



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typing and talking

(define replTalk
 (let ((talkInput ""))
  (lambda (c)
   (if (eq? c 'getBuffer) talkInput ; Return input buffer contents.
   ; backspace
   (if (or (eq? c CHAR-CTRL-H)
           (eq? c CHAR-CTRL-_)
           (eq? c CHAR-CTRL-?))
       (begin
        (if (not (eq? "" talkInput))
         (begin ((WinInput 'backspace) #\ )
                (set! talkInput (substring talkInput 0 (- (string-length talkInput) 1)))))
        'talk)
   ; Send Chatter
   (if (or (eq? c RETURN) 
           (eq? c NEWLINE))
     (begin
       ; Perform actions based on talk phrases.
       (tankTheOperator talkInput)
       ; Toggle help window if certain phrase entered
       (if (string=? "?" talkInput) (help))
       ; Send talk chatter to IPC or evaluate expression
       (if (and (not (eq? "" talkInput))
                (eq? #\: (string-ref talkInput 0)))
           (begin (WinChatDisplay "\r\n")
                  (WinChatDisplay talkInput)
                  (WinChatDisplay "=>")
                  (WinChatDisplay 
                    (call/cc (lambda (c) ; Return here if an error occurs
                       (vector-set! ERRORS (tid) c)
                       (eval (read-string (cdr-string talkInput)))))))
           (say talkInput))
       (WinInputPuts "\r\n>")
       (set! talkInput "")
       'talk)
   ; Quit chat mode.
   (if (or (eq? c CHAR-ESC) ; Escape char
           (eq? c CHAR-CTRL-I)) ; Tab char
     (begin (WinInputPuts "\r\n")
            'cmd)
   (if (and (>= c #\ )(<= c #\~))
       (begin (WinInputPutc c)
              (set! talkInput (string talkInput c))
              'talk)
   'talk))))))))

(define replTalk
 (let ((talkInput ""))
  (lambda (c)
   (if (eq? c 'getBuffer) talkInput ; Return input buffer contents.
   ; backspace
   (if (or (eq? c CHAR-CTRL-H)
           (eq? c CHAR-CTRL-_)
           (eq? c CHAR-CTRL-?))
       (begin
        (if (not (eq? "" talkInput))
         (begin ((WinInput 'backspace) #\ )
                (set! talkInput (substring talkInput 0 (- (string-length talkInput) 1)))))
        'talk)
   ; Send Chatter
   (if (or (eq? c RETURN) 
           (eq? c NEWLINE))
     (begin
       ; Perform actions based on talk phrases.
       (tankTheOperator talkInput)
       ; Toggle help window if certain phrase entered
       (if (string=? "?" talkInput) (help))
       ; Send talk chatter to IPC or evaluate expression
       (if (and (not (eq? "" talkInput))
                (eq? #\: (string-ref talkInput 0)))
           (begin (WinChatDisplay "\r\n")
                  (WinChatDisplay talkInput)
                  (WinChatDisplay "=>")
                  (WinChatDisplay 
                    (call/cc (lambda (c) ; Return here if an error occurs
                       (vector-set! ERRORS (tid) c)
                       (eval (read-string (cdr-string talkInput)))))))
           (say talkInput))
       (WinInputPuts "\r\n>")
       (set! talkInput "")
       'talk)
   ; Quit chat mode.
   (if (or (eq? c CHAR-ESC) ; Escape char
           (eq? c CHAR-CTRL-I)) ; Tab char
     (begin (WinInputPuts "\r\n")
            'cmd)
   (if (and (>= c #\ )(<= c #\~))
       (begin (WinInputPutc c)
              (set! talkInput (string talkInput c))
              'talk)
   'talk))))))))

(define (replCmd c)
 (define state 'cmd) ; state might be changed to 'done or 'talk.
 (let ((button (vector-ref Buttons c)))
   (if ShowButtons (WinChatDisplay "\r\n" c " " (getButton c)))
   (if (pair? button) (eval button)
    (if (procedure? button) (button)
     (WinChatDisplay "\r\nButton " c " undefined " button))))
 state)

(define wrepl
 (let ((state 'cmd))
  (lambda ()
   (if (not (eq? state 'done)) ; Exit if state is done.
     (let ((c (read-char stdin)))
       (if (eq? state 'talk) (set! state (replTalk c))
        (if (eq? state 'cmd) (set! state (replCmd c))))
       (wrepl))))))

(define (boxInput title)
 (define box
  ((Terminal 'WindowNew)
    (/ (Terminal 'THeight) 2)  (- (/ (Terminal 'TWidth) 2) 10)
    3  20
    #x0f))
 (define puts (box 'puts))
 (define putc (box 'putc))
 (define setcolor (box 'set-color))
 (define str "")
 ;((box 'cursor-visible) #f)
 (puts "+------------------+")
 (puts "|                  |")
 (puts "+------------------+")
 ((box 'goto) 0 2) (puts title)
 ((box 'goto) 1 1)
 (let ~ ((c (read-char stdin)))
   (or (eq? c RETURN) (eq? c NEWLINE)
    (begin
     (if (or (eq? c CHAR-CTRL-H) ; Rubout characters
             (eq? c CHAR-CTRL-_)
             (eq? c CHAR-CTRL-?))
       (if (eq? str "") "" (begin
             ((box 'backspace) #\ )
             (set! str (substring str 0 (- (string-length str) 1)))))
       (if (and (< (string-length str) 18) ; Name length limit
                (<= #\! c) (<= c #\~))     ; Name character restriction
            (begin
             (putc c)
             (set! str (string str c)))))
     (~ (read-char stdin)))))
 ((box 'delete))
 str)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tank.  The first interactive user agent.

(define (tankTalk str) (thread (begin
  (sleep 700)
  (WinChatSetColor 0 15)
  (WinChatDisplay "\r\nTank ")
  (WinChatSetColor 0 7)
  (WinChatDisplay str))))

(define tankHangupTime 0)
(define tankIsListening #f)

(define (tankStartListening)
  (set! tankHangupTime (+ 10 (time)))
  (tankTalk "Operator")
  (if (not tankIsListening) (begin (thread (let ~ ()
    (set! tankIsListening #t)
    (sleep 12000)
    (if (< (time) tankHangupTime)
      (~)
      (begin
       (tankTalk "*CLICK*")
       (set! tankIsListening #f))))))))

(define (tankTheOperator talkInput)
 (if (string=? talkInput "tank") (tankStartListening)
 (let ((strLen (string-length talkInput)))
  (if (and (> strLen 11)
           (string=? "my name is " (substring talkInput 0 11)))
     (begin
      ((avatar `setNameGlyph)
         (substring talkInput 11 strLen)
         (glyphNew (glyph0bg (avatar 'glyph))
                   (glyph0fg (avatar 'glyph))
                   (string-ref talkInput 11)
                   (glyph1bg (avatar 'glyph))
                   (glyph1fg (avatar 'glyph))
                   (string-ref talkInput (if (> strLen 12) 12 11))))
      (thread (begin (sleep 500) (who)))))))
 (if tankIsListening (begin
   (if (string=? "who" talkInput) ((ipc 'qwrite) '(say "I'm here!")))
   (if (string=? "load the jump program" talkInput) (tankTalk "I can't find the disk")
   (if (string=? "load underworld" talkInput) (thread (load-ultima-underworld))
   (if (string=? "load ultima5" talkInput) (thread (load-ultima-world5))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Screen redraw signal handler
(define (handleTerminalResize)
  (WinChatDisplay "\r\n" (terminal-size))
  ((Terminal 'ResetTerminal))
  ((WinChat 'resize)  (- (Terminal 'THeight) 1) (Terminal 'TWidth))
  ((WinMap 'move)     0 (- (Terminal 'TWidth) (WinMap 'WWidth) 2))
  ((WinColumn 'move)  1 (- (Terminal 'TWidth) 2) )
  ((WinStatus 'move)  (WinMap 'Y0) (- (Terminal 'TWidth) 12))
  ((WinInput 'resize) 1 (Terminal 'TWidth))
  ((WinInput 'move)   (- (Terminal 'THeight) 1) 0))

(define sigwinch
 (let ((sig28Semaphore (open-semaphore 1)))
  (lambda ()
   (semaphore-down sig28Semaphore)
   (handleTerminalResize)
   (semaphore-up sig28Semaphore))))

(vector-set! SIGNALHANDLERS 28 (lambda () (sigwinch) (unthread)))
(signal 28)



(define (say talkInput . level)
 ((ipc 'qwrite) (list 'voice DNA (if (null? level) 10 (car level)) talkInput)))

(define (saySystem talkInput)
 ((ipc 'qwrite) (list 'voice 0 10 talkInput)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prototypes and fun things.

;; Walking kitty soldier
(define (spawnKitty . cycles)
 (set! cycles (if (null? cycles) 128 (car cycles))) ; Set max cycles
 (letrec ((kitty (Avatar "Kat"))
          (dir->card (lambda (d) (vector-ref #(6 9 8 7 4 1 2 3) d)))
          (card->dir (lambda (c) (vector-ref #(0 5 6 7 4 0 0 3 2 1) c)))
          (happyVector (vector 0 0 0 0 0 0 0 0))
          (dist 0))
 ; Tell everyone who this kitteh is.
 ((ipc 'qwrite) `(entity ,(kitty 'dna) "kitty" ,@((kitty 'gps)) ,(glyphNew 0 7 #\K 0 15 #\a)))
 (let ~ ((i 0)) ; Main loop
   ; Distance from parent avatar
   (set! dist (distance ((kitty 'gps)) ((avatar 'gps))))
   ; Neuron depletion.
   (if (= 0 (modulo i 10)) (vector-map! (lambda (x) (/ x 2)) happyVector))
   ; Walk kitty quasi-randomly.
   ((kitty 'walk)
       (dir->card (letrec ((dir (card->dir (kitty 'dir)))
                           (dir1 (modulo (+ dir (random 3) -1) 8))
                           (dir2 (modulo (+ dir (random 3) -1) 8)))
                   (if (> (vector-ref happyVector dir1)
                          (vector-ref happyVector dir2))
                       dir1 dir2))))
   (vector-set! happyVector (card->dir (kitty 'dir))
       (+ (let ((kd (distance ((kitty 'gps))
                              ((avatar 'gps)))))
            (if (< kd dist) 1
            (if (= kd dist) -1 -2)))
          (vector-ref happyVector (card->dir (kitty 'dir)))))
   ((ipc 'qwrite) `(move ,(kitty 'dna) ,@((kitty 'gps))))
   (if KITTEHBRAIN (begin
     ((WinChat 'goto) 10 5) (WinChatDisplay (vector-ref happyVector 3) "  ")
     ((WinChat 'goto) 10 8) (WinChatDisplay (vector-ref happyVector 2) "  ")
     ((WinChat 'goto) 10 11)(WinChatDisplay (vector-ref happyVector 1) "  ")
     ((WinChat 'goto) 11 5) (WinChatDisplay (vector-ref happyVector 4) "  ")
     ((WinChat 'goto) 11 11)(WinChatDisplay (vector-ref happyVector 0) "  ")
     ((WinChat 'goto) 12 5) (WinChatDisplay (vector-ref happyVector 5) "  ")
     ((WinChat 'goto) 12 8) (WinChatDisplay (vector-ref happyVector 6) "  ")
     ((WinChat 'goto) 12 11)(WinChatDisplay (vector-ref happyVector 7) "  ")))
   (sleep 200)
   ;(if (equal? ((kitty 'gps))
   ;            ((avatar 'gps)))
   ;    ((ipc 'qwrite) `(voice ,(kitty 'dna) 10 "Mrrreeeooowww!")))
   (if (> i (+ cycles (random 30)))
       ((ipc 'qwrite) `(die ,(kitty 'dna))) ; kill entity
       (~ (+ i 1))))))


(define walkForever (let ((walkForeverFlag #f)) (lambda ()
 (if walkForeverFlag
  (begin
   (set! walkForeverFlag #f)
   (WinChatSetColor 0 10)
   (WinChatDisplay "\r\nThus ends the journey"))
  (begin
   (WinChatSetColor 0 10)
   (WinChatDisplay "\r\nThe journey begins")
   (set! walkForeverFlag #t)
   (thread (let ~ ()
     (for-each
       (lambda (x) (or walkForeverFlag (unthread)) (walk x) (sleep 400))
       '(6 6 6 6 8 8 8 8 4 4 4 4 2 2 2 2))
     (sleep 500)
     (~))))))))


(define (sayDesperate)
  (let ((cmd `(voice ,DNA 10 "desperate")))
   (sleep (random 500))
   ((ipc 'qwrite) cmd) (sleep 500)
   ((ipc 'qwrite) cmd) (sleep 500)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start everything

; Welcome marquee and login box
(if (not QUIETLOGIN ) (begin
 (thread (welcome))
 (let ((name (boxInput "Enter your name")))
   (if (eq? name "") (set! name "Guest"))
   ((avatar 'setNameGlyph)
      name
      (glyphNew 0 15 (string-ref name 0)
                0 15 (string-ref name 1))))))

; Display some initial information
(WinChatSetColor 0 11)
;((WinChat 'goto) 0 0)
(WinChatDisplay "Welcome to World\r\n") ; "   *** Welcome to the construct. ***"
(WinChatDisplay "See http://code.google.com/p/worldtm\r\n")
(WinChatDisplay "Hit ? to toggle the help window\r\n")
(WinChatDisplay "Your name is " (avatar 'name))

; Move avatar to entrance of Lord British's castle
((avatar 'jump) (avatar 'z) (* 108 U4MapCellSize) (* 86 U4MapCellSize))

; Create ipc object.  Pass in a debug message output port.
; for production world an 'empty' port is passed.
(define ipc (Ipc WinConsoleDisplay))
;(define ipc (Ipc WinChatDisplay))

; Always read and evaluate everything from IPC.
(thread  (let ~ () 
 (let ((sexp ((ipc 'qread))))
    (WinConsoleWrite sexp)
    (eval sexp)
    (~))))

((ipc 'qwrite) '(who))

(viewportReset (avatar 'y) (avatar 'x))
((WinMap 'toggle))

; Redraw map resulting in animated cells.
(thread (let ~ ()
   (sleep 1000)
   (if CELLANIMATION (viewportReset (avatar 'y) (avatar 'x)))
   (~)))

; Catch the terminal hangup signal so that normal shutdown can occur.
(define (registerSignalHandler sig msg)
 (vector-set! SIGNALHANDLERS sig (lambda () (say msg) (shutdown)))
 (signal sig))

(vector-set! SIGNALHANDLERS 1 (lambda () (say "signal 1 HUP")  (shutdown)))
(signal 1)

(vector-set! SIGNALHANDLERS 2 (lambda () (say "signal 2 INT")  (shutdown)))
(signal 2)

(vector-set! SIGNALHANDLERS 3 (lambda () (say "signal 3 QUIT")  (shutdown)))
(signal 3)

(vector-set! SIGNALHANDLERS 6 (lambda () (say "signal 6 ABRT")  (shutdown)))
(signal 6)

;(vector-set! SIGNALHANDLERS 13 (lambda () (say "signal 13 PIPE")  (shutdown)))
;(signal 13)

(vector-set! SIGNALHANDLERS 15 (lambda () (say "signal 15 TERM")  (shutdown)))
(signal 15)


(define (sayHelloWorld)
 (saySystem (string
     (avatar 'name)
     " says "
     (vector-random #("*PUSH* *SQUIRT* *SPANK* *WAAAAAAAAA*" "*All Worldlians Want to Get Borned*" "*Happy Birthday*" "*I thought you were in Hong Kong*")))))

(define (sayByeBye)
  (saySystem (string (avatar 'name) " exits")))

(define (shutdown)
  (or QUIETLOGIN (sayByeBye))
  ((ipc 'qwrite) `(die ,DNA)) ; Kill avatar's entity
  (sleep 1000) ; wait for ipc to flush
  (displayl "\e[" (Terminal 'THeight) "H\r\n\e[0m\e[?25h")
  (quit))

(or QUIETLOGIN (sayHelloWorld))
(wrepl)
(shutdown)
