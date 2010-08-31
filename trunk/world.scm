(define QUIETLOGIN (not (null? (memv "silent" (vector->list argv)))))
(define CELLANIMATION #t) 
(define SCROLLINGMAP #t)
(define KITTEHBRAIN  #f)
(define VOICEDELIMETER " ")
(define DNA 0)

(if QUIETLOGIN (load "ipc.scm" )(load "ipc.scm"))
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Glyphs - Two characters including their color.
;;
(define (glyphColorNew b f) (+ (* b 256) f))

(define glyphNew vector)
(define (glyph0bg cell) (vector-ref cell 0))
(define (glyph0fg cell) (vector-ref cell 1))
(define (glyph0ch cell) (vector-ref cell 2))
(define (glyph1bg cell) (vector-ref cell 3))
(define (glyph1fg cell) (vector-ref cell 4))
(define (glyph1ch cell) (vector-ref cell 5))

;(define (colorForeground color) (modulo color 256))
;(define (colorBackground color) (/ color 256))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cells - For now cells are just glyphs...eventually smarter objects.
;;
(define MAXCELL 1023)
(define CELLS (make-vector (+ 1 MAXCELL) (glyphNew 1 11 #\? 3 1 #\?)))

(define (cell-set! i c) (vector-set! CELLS i c))

(define (cell-ref i)
  (if (< MAXCELL i) ; An index larger than MAXCEL is assumbed to be an entity object.
    ((entitiesGet i) 'glyph)
    (vector-ref CELLS i)))

(define AIR   MAXCELL)(cell-set! AIR        (glyphNew 0 0  CHAR-CTRL-@ 0 0 CHAR-CTRL-@))
(define WATER0 0)     (cell-set! WATER0     (glyphNew 0 4  #\\ 0 4  #\/ #(0 4  #\/ 0 4 #\\)))
(define WATER1 1)     (cell-set! WATER1     (glyphNew 0 4  #\~ 0 4  #\~ #(0 4  #\- 0 4 #\-) #(0 4  #\_ 0 4  #\_)))
(define WATER2 2)     (cell-set! WATER2     (glyphNew 0 4  #\~ 0 12 #\~ #(0 12 #\~ 0 4 #\~)))
(define POISON 3)     (cell-set! POISON     (glyphNew 0 4  #\. 0 2  #\.))
(define GRASS 4)      (cell-set! GRASS      (glyphNew 0 2  #\. 0 2  #\.))
(define BUSHES 5)     (cell-set! BUSHES     (glyphNew 0 10 #\o 0 10 #\o))
(define FOREST 6)     (cell-set! FOREST     (glyphNew 0 2  #\O 0 2  #\O))
(define HILLS 7)      (cell-set! HILLS      (glyphNew 0 7  #\^ 0 7  #\^))
(define MNTS 8)       (cell-set! MNTS       (glyphNew 0 15 #\/ 0 15 #\\))
(define DUNGEON 9)    (cell-set! DUNGEON    (glyphNew 0 8  #\[ 0 8  #\]))
(define TOWN 10)      (cell-set! TOWN       (glyphNew 2 9  #\[ 2 9  #\]))
(define CONSTRUCT 11) (cell-set! CONSTRUCT  (glyphNew 1 11 #\? 3 1  #\a))
(define TOWN2 12)     (cell-set! TOWN2      (glyphNew 2 3  #\[ 2 3  #\]))
(define BRIT1 13)     (cell-set! BRIT1      (glyphNew 0 9  #\I 0 9  #\I))
(define BRIT2 14)     (cell-set! BRIT2      (glyphNew 0 7  #\[ 0 7  #\]))
(define BRIT3 15)     (cell-set! BRIT3      (glyphNew 0 9  #\I 0 9  #\I))
(define SAND 17)      (cell-set! SAND       (glyphNew 1 11 #\? 3 1  #\b))
(define STONE 18)     (cell-set! STONE      (glyphNew 0 7  #\[ 0 7  #\]))
(define BRICK 19)     (cell-set! BRICK      (glyphNew 1 9  #\[ 1 9  #\]))
(define DIRT  20)     (cell-set! DIRT       (glyphNew 0 3  #\, 0 3  #\,))
(define XX 21)        (cell-set! XX         (glyphNew 7 0  #\  7 0  #\ ))
(define BRIDGE 23)    (cell-set! BRIDGE     (glyphNew 0 3  #\= 0 3  #\=))
(define CELLB 29)     (cell-set! CELLB      (glyphNew 1 11 #\? 3 1  #\c))
(define SHRINE 30)    (cell-set! SHRINE     (glyphNew 0 6  #\[ 0 6  #\]))
(define SAND2 37)     (cell-set! SAND2      (glyphNew 0 3  #\, 0 3  #\,))
(define SAND3 50)     (cell-set! SAND3      (glyphNew 0 3  #\. 0 3  #\.))
(define CELLD 61)     (cell-set! CELLD      (glyphNew 1 11 #\? 3 1  #\d))
(define FIRE2 70)     (cell-set! FIRE2      (glyphNew 0 1  #\^ 0 1  #\^))
(define FIRE  76)     (cell-set! FIRE       (glyphNew 0 9  #\^ 0 9  #\^))
(define HELP  77)     (cell-set! HELP       (glyphNew 1 11 #\? 1 11 #\?))
(define CHAIR 78)     (cell-set! CHAIR      (glyphNew 1 9  #\P 1 9  #\o))
(define SNAKE 79)     (cell-set! SNAKE      (glyphNew 6 11 #\O 6 11 #\o #(6 11 #\o 6 11 #\O)))
(define KITTY 80)     (cell-set! KITTY      (glyphNew 0 7  #\M 0 7  #\e #(0 7  #\o 0 7  #\w)))
(define TV 90)        (cell-set! TV         (glyphNew 7 1  #\[ 7 1  #\]))


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
;;         to the avatar's position.  Plan on creating a 2x2 cache.
;;
(define FieldDimension 256) ; Field dimension

; Create default plane.
(define FIELD ())

(define (resetField)
 (set! FIELD
  (vector-vector-map! 
    (lambda (i)
     ;(if (random-bool 2) (vector 2 XX GRASS AIR) (vector 2 XX DIRT  AIR))
     ;(vector 2 XX  WATER  AIR AIR))
     (vector -1 XX AIR))
   (make-vector-vector FieldDimension FieldDimension ()))))
(resetField)

;(vector-vector-map! (lambda (i) (display "\r\n") (display i) i) FIELD)

; Fields are 2d arrays of columns.  Columns are quasi-compressed stacks
; of cells that consist of a start height and specified stack of cells.
; #(3 1 1 2) would be cells (1 1 2) starting at z=4=(3+1). Cells below and
; above are assumed to be the lowest and higest specified cells in the vector.
; Setting a cell outside the explicit stack range expands the actual vector
; and adjusts the start-height value.
(define (fieldColumn y x)
 (vector-vector-ref FIELD (modulo y FieldDimension)
                          (modulo x FieldDimension)))

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
   (if (or (not (eq? (columnRef column z) AIR))
           (<= z bot))
       (if (pair? z) (car z) z) ; Return first in improper list of objects.
       (findNonAir (- z 1))))))

; Replace all cells at this Z location with a single cell.
(define (field-set! z y x c)
 (letrec ((fy (modulo y FieldDimension))
          (fx (modulo x FieldDimension))
          (column (vector-vector-ref FIELD fy fx)))
  (vector-vector-set! FIELD fy fx
     (columnSet column z c ))))

; Insert the cell at this Z location.  Creates a malformed list.
(define (field-add! z y x c)
 (letrec ((fy (modulo y FieldDimension))
          (fx (modulo x FieldDimension))
          (column (vector-vector-ref FIELD fy fx)))
  (vector-vector-set! FIELD fy fx
     (columnSet column z (cons c (columnRef column z))))))

; Remove the cell from themalformed list at this Z location.
(define (field-delete! z y x e)
 (letrec ((fy (modulo y FieldDimension))
          (fx (modulo x FieldDimension))
          (column (vector-vector-ref FIELD fy fx)))
  (vector-vector-set! FIELD fy fx
     (columnSet column z (list-delete (columnRef column z) e)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Canvas
;;
;; A canvas entry is a vector consisting of a cell and it's Z coordinate (the
;; top most visible).  The canvas itself is a 2x2 block dynamically updated
;; with cells from the cell field agent.  Avatar movement will readjust the
;; canvas' virtual position in the field's coordinate system as well as
;; repopulate/cache the new block areas in the canvas.  It will be either
;; two or three (or four if the avatar 'warps' to a new area).
;;
;; Example:                         +--+--+    +--+--+
;;   Assume avatar moved from a to  | a| b|    | e| b|
;;   b in the canvas area.  e and f +--+--+ -> +--+--+
;;   are cached in the area modulo  | c| d|    | f| c|
;;   the field coordinates.         +--+--+    +--+--+

(define CanvasBlockDimension 256) ; Each canvas block is 256x256 cells.
(define CanvasBlockCount 2)       ; Canvas is made up of 2x2 blocks.
(define CanvasDimension (* CanvasBlockCount CanvasBlockDimension)) ; 512x512
(define CanvasCoordinates (cons 0 0)) ; Canvas coordinate in block space.  (1 1) would be (256 256) map space.

; Get list of coordinate pairs of each block that make up the canvas.
(define (canvasBlockCoordinates)
 (let ((y (car CanvasCoordinates))
       (x (cdr CanvasCoordinates)))
 (list (cons y x) (cons y (+ x 1)) (cons (+ y 1) x) (cons (+ y 1) (+ x 1)))))

; Return list of block coordinates which need to be updated
; if we were to move the canvas to this new block location.
(define (canvasMove y x)
  (let ~ ((newBlocks (canvasBlockCoordinates)) ; Assume we need to replace all current blocks.
          (current (list (cons y x) (cons y (+ x 1)) (cons (+ y 1) x) (cons (+ y 1) (+ x 1))))) ; Remove the blocks we already have from new block list.
    (if (null? current)
      newBlocks
      (~ (list-delete newBlocks (car current)) (cdr current)))))

(define CANVAS (make-vector-vector CanvasDimension CanvasDimension ()))

(define (canvasCell y x)
  (car (vector-vector-ref CANVAS
         (modulo y CanvasDimension)
         (modulo x CanvasDimension))))

(define (canvasHeight y x)
  (cdr (vector-vector-ref CANVAS
         (modulo y CanvasDimension)
         (modulo x CanvasDimension))))

(define (canvasCellSet y x c)
  (set-car! (vector-vector-ref CANVAS
              (modulo y CanvasDimension)
              (modulo x CanvasDimension))
            c))

(define (canvasHeightSet y x h)
 (set-cdr! (vector-vector-ref CANVAS
             (modulo y CanvasDimension)
             (modulo x CanvasDimension))
           h))

(define (canvasRender y x)
 (let ((top (field-ref-top 1000 y x)))
  (canvasCellSet y x (cell-ref (field-ref top y x)))
  (canvasHeightSet y x top)))

; Initialize the canvas with the Construct[tm].
(define timeStart (time))

(let ((defaultCell (cell-ref XX)))
 (let ~ ((y 0)(x 0))
  (if (!= y CanvasDimension) (if (= x CanvasDimension) (~ (+ y 1) 0) (begin
   ; Each canvas entry consists of a map cell and its height.
    (vector-vector-set! CANVAS y x (cons defaultCell 0))
    (~ y (+ x 1)))))))

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
   (if (eq? AIR c)
    (begin (WinColumnSetColor 0 8)
           (WinColumnPuts "()"))
    (begin (set! c (cell-ref c))
           (WinColumnSetColor (glyph0bg c) (glyph0fg c))
           (WinColumnPutc (glyph0ch c))
           (WinColumnSetColor (glyph1bg c) (glyph1fg c))
           (WinColumnPutc (glyph1ch c)))))
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
 (let ((y (modulo (- gy PortY) FieldDimension)) ; Normalize avatar position.
       (x (modulo (- gx PortX) FieldDimension)))
  (and (< y PortH) (< x PortW)
    (WinMapPutGlyph (canvasCell gy gx) y (* x 2)))))



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
    (define cell BRICK)
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
 (let ((y (modulo (- (avatar 'y) PortY) FieldDimension));Normalize avatar position.
   (x (modulo (- (avatar 'x) PortX) FieldDimension)))
   (if (or (<= PortW x) (<= PortH y))
     (viewportReset (avatar 'y) (avatar 'x)))))

; TODO: Implement coor+dir use new dir to field-ref location an dif > MAX_CELL ipc-write (force blah blah)

(define (walk dir)
 ((avatar 'face) dir) ; Turn avatar
 (let ((nextCell ((avatar 'look)))) ; Consider cell I'm walking into
   (if (< MAXCELL nextCell) ; Cell I'm walking into is probably an entity
    (begin
     ((ipc 'qwrite) `(force ,@((avatar 'gpsLook)) ,dir 10))) ; Push the entity that's in my way.
    (if (null? (memv nextCell (list XX MNTS HILLS WATER0 WATER1 WATER2)))
     (begin
      ((avatar 'move))
      ((ipc 'qwrite) `(move ,DNA ,@((avatar 'gps))))
      (if (eq? HELP (field-ref (avatar 'z) (avatar 'y) (avatar 'x)))  (help))
      (if (eq? SNAKE (field-ref (avatar 'z) (avatar 'y) (avatar 'x))) (thread (snake-random)))
      (if (eqv? BRIT2 (field-base-ref (avatar 'z) (avatar 'y) (avatar 'x))) (thread (spawnKitty)))
      ;(WinConsoleWrite (fieldColumn (avatar 'y) (avatar 'x)) (field-ref (avatar 'z) (avatar 'y) (avatar 'x)))
      ; Dump our coordinates.
      ((WinStatus 'puts) "\r\n")
      ((WinStatus 'puts) (number->string (avatar 'z)))
      ((WinStatus 'puts) " ")
      ((WinStatus 'puts) (number->string (avatar 'y)))
      ((WinStatus 'puts) " ")
      ((WinStatus 'puts) (number->string (avatar 'x))))))))

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




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get the wheels in motion
;;

;; Map window

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
(define WinColumn ((Terminal 'WindowNew) 1 (- (Terminal 'TWidth) 2) 18 2 #x5b))
((WinColumn 'toggle))
(define WinColumnPutc (WinColumn 'putc))
(define WinColumnPuts (WinColumn 'puts))
(define WinColumnSetColor (WinColumn 'set-color))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stuff
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


; Help window.  A bunch of button commands for now.
; Give the help window an artistic border.

; Give the window an 'artistic' border.
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


; Drop some cells on the map
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

; Mapping from Ultima4 map file byte index to (y x) World[tm] coordinates.
;  y = (+ (modulo (/ i 32) 32) (* (/ i 8192) 32))
;  x = (+ (modulo i 32) (* (modulo (/ i 1024) 8) 32))

; Mapping from World[tm] (y x) corrdinates to Ultima4 map file byte index.
; i = (+ (modulo x 32) (* (/ x 32) 1024) (* (modulo y 32) 32) (* (/ y 32) 8192))

; The ultima map file is an 8x8 array of 32x32 cells
; so create a simpler 256x25 array of cells.
(define U44MapVector
 (let ((fd (open "ultima4.map"))
       (timeStart (time))
       (vec (make-vector 65536 DIRT)))
  (let ~ ((i 0))
     (if (= i 65536)
      (begin ; return the vector
       (WinConsoleDisplay "\r\nInitialized U44MapVector " (- (time) timeStart) "s.")
       vec)
      (begin
        (vector-set! vec
                 (+ (* 256 (+ (modulo (/ i 32) 32) (* (/ i 8192) 32)))
                    (+ (modulo i 32) (* (modulo (/ i 1024) 8) 32)))
                 (+ 0 (read-char fd))) ; Hack to convert char to integer
        (~ (+ i 1)))))))

(define (loadUltimaWorld4)
 (resetField)
 (let ((bar (makeProgressBar 5 30 "Britannia 4"))
       (timeStart (time)))
  (let ~ ((y 0)(x 0)) (if (< y 256) (if (= 256 x) (~ (+ y 1) 0) (begin
      (if (= 0 (modulo (+ x (* 256 y)) (/ 65536 20))) (bar))
      (let ((c (+ 0 (U44MapVectorRef y x)))) ; Hack to convert char to integer
        (if (not (null? (memv c (list FOREST BUSHES)))) (setCell 2 y x c)) ; Forest trees are tall.
        (setCell 1 y x c))
      (~ y (+ x 1))))))
  (WinChatDisplay "\r\nInitialized UltimaIV Map " (- (time) timeStart) "s.")
  ((ipc 'qwrite) '(who))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ultima city detail map.
;;
;; u4mapsize 256
;; u44cellsize 8 (But will be 32 the U4 town dimension)
;; FieldDimension 256
;; Lord British's castle (108 86)
(define u4mapsize 256)
(define u44cellsize 20)
(define u44height (* u44cellsize u4mapsize))
(define u44width  (* u44cellsize u4mapsize))
(define (U44MapVectorRef y x) (vector-ref U44MapVector (+ (* 256 (modulo y 256)) (modulo x 256))))
(define U44MapVectorRefDir
 (let ((yd (vector 0 -1 -1 -1  0  1 1 1)) (xd (vector 1  1  0 -1 -1 -1 0 1)))
  (lambda (y x d)
    (set! d (modulo d 8))
    (vector-ref U44MapVector
                 (+ (* 256 (modulo (+ y (vector-ref yd d)) 256))
                    (modulo (+ x (vector-ref xd d)) 256))))))

(define (load-ultima-world44)
 (resetField)
 (let ((bar (makeProgressBar 5 30 "Britannia 4"))
       (timeStart (time)))
  (let ~ ((y 0)(x 0)) (or (= y 256) (if (= x 256) (~ (+ y 1) 0) (begin
      (if (= 0 (modulo (+ (* 256 y) x) (/ 65536 20))) (bar)) ; Update progress bar.
      (setCell 1 y x
        (let ((yy (/ (+ y (* 108 (- u44cellsize 1)) ) u44cellsize))
              (xx (/ (+ x (*  86 (- u44cellsize 1)) ) u44cellsize)))
         (let ((dist (distance
                      (list 0 y x)
                      (list 0 (+ 20 (* (/ y u44cellsize) u44cellsize))
                              (+ 20 (* (/ x u44cellsize) u44cellsize))))))
          (if (< dist 100) (U44MapVectorRef yy xx) HELP) ; BF TODO WORKING
          )))
      (~ y (+ x 1))))))
  (WinConsoleDisplay "\r\nInitialized UltimaIV Map " (- (time) timeStart) "s.")))


(define (load-ultima-world5)
 (let ((fdm (open "ultima5.map"))
       (fdi (open "ultima5.ovl"))
       (bar (makeProgressBar 1 30 "Britannia 4")))
 (let ~ ((y 0) (x 0)) (if (< y 256) (if (= x 256) (~ (+ y 1) 0) (begin
   (if (and (= x 255) (= 0 (modulo y 12))) (bar)) ; Progress bar
   (let ((index (begin (seek fdi (+ (/ x 16) (* (/ y 16) 16)))
                       (+ 0 (read-char fdi)))))
    (setCell 2 y x 
      (if (= 255 index) WATER1 (begin
        (seek fdm (+ (* index 256) (modulo x 16) (* (modulo y 16) 16)))
        (+ 0 (read-char fdm))))))
   (~ y (+ x 1))))))))


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
   (if (string=? "load ultima4" talkInput) (thread (loadUltimaWorld4))
   (if (string=? "load underworld" talkInput) (thread (load-ultima-underworld))
   (if (string=? "load ultima5" talkInput) (thread (load-ultima-world5)))))))))

(define (say talkInput . level)
 ((ipc 'qwrite) (list 'voice DNA (if (null? level) 10 (car level)) talkInput)))

(define (saySystem talkInput)
 ((ipc 'qwrite) (list 'voice 0 10 talkInput)))

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
;; Buttons
(define Buttons (make-vector 257 (lambda())))

(define (setButton ch exp)
 (vector-set! Buttons ch exp)) ; For now characters are also integer constants.

(define ShowButtons #f)
(define (showButtons) (set! ShowButtons (not ShowButtons)))

; Consider the button value.  If data just return the expression.  Otherwise
; assume a closure.  Consider the closure, closure's code, the code's pre-compiled
; expression and return it.
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
(setButton #\d '((ipc 'qwrite) `(dropCell ,(avatar 'y) ,(avatar 'x) BRICK)))
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
(setButton #\4 '(loadUltimaWorld4))
;(setButton CHAR-CTRL-K '((ipc 'qwrite) `(set! FIELD ,FIELD))) ; Send my plane out to IPC.
;(setButton #\1 '(thread (sigwinch)))
;(setButton CHAR-CTRL-_ '(walk 4)) ; Sent by backspace?

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
       (if (eq? state 'talk)
           (set! state (replTalk c))
           (if (eq? state 'cmd)  (set! state (replCmd c))))
       (wrepl))))))

;; Stats window
(define WinStatus ((Terminal 'WindowNew)
   (WinMap 'Y0) (- (Terminal 'TWidth) 12)
   1            12
   #x43))
((WinStatus 'toggle))


;; Walking kitty soldier
(define (spawnKitty . cycles)
 (set! cycles (if (null? cycles) 128 (car cycles))) ; Set max cycles
 (letrec ((kitty (Avatar "Kat"))
          (dir->card (lambda (d) (vector-ref #(6 9 8 7 4 1 2 3) d)))
          (card->dir (lambda (c) (vector-ref #(0 5 6 7 4 0 0 3 2 1) c)))
          (happyVector (vector 0 0 0 0 0 0 0 0))
          (dist 0))
 ; Tell everyone who this kitteh is.
 ((ipc 'qwrite) `(entity ,(kitty 'dna) "kitty" ,@((kitty 'gps)) ,(glyphNew 0 7 #\K 0 f #\a)))
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Screen redraw signal handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

; Create ipc object.  Pass in a debug message output port.
; for production world an 'empty' port is passed.
(define ipc (Ipc WinConsoleDisplay)) ; WinChatDisplay

; Always read and evaluate everything from IPC.
(thread  (let ~ () 
 (let ((sexp ((ipc 'qread))))
    (WinConsoleWrite sexp)
    (eval sexp)
    (~))))

((ipc 'qwrite) '(who))

(viewportReset (avatar 'y) (avatar 'x))
((WinMap 'toggle))
(thread (if QUIETLOGIN (loadUltimaWorld4) (loadUltimaWorld4)))

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

(vector-set! SIGNALHANDLERS 13 (lambda () (say "signal 13 PIPE")  (shutdown)))
(signal 13)

(vector-set! SIGNALHANDLERS 15 (lambda () (say "signal 15 TERM")  (shutdown)))
(signal 15)


(define (sayHelloWorld)
 (saySystem (string
     (avatar 'name)
     " says "
     (vector-random #("*PUSH* *SQUIRT* *SPANK* *WAAAAAAAAA*" "*All Worldlians Want to Be Borned*" "*Happy Birthday*" "*I thought you were in Hong Kong*")))))

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
