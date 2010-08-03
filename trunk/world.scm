(define QUIETLOGIN (not (null? (memv "silent" (vector->list argv)))))
(define CELLANIMATION #t) 
(define SCROLLINGMAP #t)
(define KITTEHBRAIN  #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IPC stuff
(load "ipc.scm")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window stuff
(load "window.scm")

; Force just only one instance of the Terminal object
; by setting the object value to an instance of itself.
(define Terminal (Terminal))

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

(define WinConsole ((Terminal 'WindowNew) 0 0 (- (Terminal 'THeight) 1)  (Terminal 'TWidth) #x74))
(define WinConsolePuts (WinConsole 'puts))
((WinConsole 'toggle))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Glyphs - Two characters including their color.
;;
(define glyphNew vector)
(define (glyphColor0 cell) (vector-ref cell 0))
(define (glyphChar0  cell) (vector-ref cell 1))
(define (glyphColor1 cell) (vector-ref cell 2))
(define (glyphChar1  cell) (vector-ref cell 3))

(define (colorForeground color) (modulo color 16))
(define (colorBackground color) (/ color 16))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cells - For now cells are just glyphs...eventually smarter objects.
;;
(define CELLS (make-vector 256 (glyphNew #x1b #\? #x31 #\?)))

(define (cell-set! i c) (vector-set! CELLS i c))

(define (cell-ref i)
  (if (< 256 i) ; An index larger than 255 is assumbed to be an entity object.
    ((entitiesGet i) 'glyph)
    (vector-ref CELLS i)))

(define WATER0 0)     (cell-set! WATER0     (glyphNew #x04 #\\ #x04 #\/ #(#x04 #\/ #x04 #\\)))
(define WATER1 1)     (cell-set! WATER1     (glyphNew #x04 #\~ #x04 #\~ #(#x04 #\- #x04 #\-) #(#x04 #\_ #x04 #\_)))
(define WATER2 2)     (cell-set! WATER2     (glyphNew #x04 #\~ #x0c #\~ #(#x0c #\~ #x04 #\~)))
(define POISON 3)     (cell-set! POISON     (glyphNew #x04 #\. #x02 #\.))
(define GRASS 4)      (cell-set! GRASS      (glyphNew #x02 #\. #x02 #\.))
(define BUSHES 5)     (cell-set! BUSHES     (glyphNew #x0a #\o #x0a #\o))
(define FOREST 6)     (cell-set! FOREST     (glyphNew #x02 #\O #x02 #\O))
(define HILLS 7)      (cell-set! HILLS      (glyphNew #x07 #\^ #x07 #\^))
(define MNTS 8)       (cell-set! MNTS       (glyphNew #x0f #\/ #x0f #\\))
(define DUNGEON 9)    (cell-set! DUNGEON    (glyphNew #x08 #\[ #x08 #\]))
(define TOWN 10)      (cell-set! TOWN       (glyphNew #x29 #\[ #x29 #\]))
(define CONSTRUCT 11) (cell-set! CONSTRUCT  (glyphNew #x1b #\? #x31 #\a))
(define TOWN2 12)     (cell-set! TOWN2      (glyphNew #x23 #\[ #x23 #\]))
(define BRIT1 13)     (cell-set! BRIT1      (glyphNew #x09 #\I #x09 #\I))
(define BRIT2 14)     (cell-set! BRIT2      (glyphNew #x07 #\[ #x07 #\]))
(define BRIT3 15)     (cell-set! BRIT3      (glyphNew #x09 #\I #x09 #\I))
(define SAND 17)      (cell-set! SAND       (glyphNew #x1b #\? #x31 #\b))
(define STONE 18)     (cell-set! STONE      (glyphNew #x07 #\[ #x07 #\]))
(define BRICK 19)     (cell-set! BRICK      (glyphNew #x19 #\[ #x19 #\]))
(define DIRT  20)     (cell-set! DIRT       (glyphNew #x03 #\, #x03 #\,))
(define XX 21)        (cell-set! XX         (glyphNew #x70 #\  #x70 #\ ))
(define BRIDGE 23)    (cell-set! BRIDGE     (glyphNew #x03 #\= #x03 #\=))
(define CELLB 29)     (cell-set! CELLB      (glyphNew #x1b #\? #x31 #\c))
(define SHRINE 30)    (cell-set! SHRINE     (glyphNew #x06 #\[ #x06 #\]))
(define SAND2 37)     (cell-set! SAND2      (glyphNew #x03 #\, #x03 #\,))
(define SAND3 50)     (cell-set! SAND3      (glyphNew #x03 #\. #x03 #\.))
(define CELLD 61)     (cell-set! CELLD      (glyphNew #x1b #\? #x31 #\d))
(define FIRE2 70)     (cell-set! FIRE2      (glyphNew #x01 #\^ #x01 #\^))
(define FIRE  76)     (cell-set! FIRE       (glyphNew #x09 #\^ #x09 #\^))
(define HELP  77)     (cell-set! HELP       (glyphNew #x1b #\? #x1b #\?))
(define CHAIR 78)     (cell-set! CHAIR      (glyphNew #x19 #\P #x19 #\o))
(define SNAKE 79)     (cell-set! SNAKE      (glyphNew #x6b #\O #x6b #\o #(#x6b #\o #x6b #\O)))
(define KITTY 80)     (cell-set! KITTY      (glyphNew #x07 #\M #x07 #\e #(#x07 #\o #x07 #\w)))
(define TV 90)        (cell-set! TV         (glyphNew #x71 #\[ #x71 #\]))
(define AIR 127)      (cell-set! AIR        (glyphNew #x00 CHAR-CTRL-@ #x00 CHAR-CTRL-@))

; Ultime cells
;(0:34014 1:10336 2:1933 3:666 4:7159 5:4924 6:2304 7:2328 8:1745 9:7 10:7 11:3 12:4 13:1 14:1 15:1 23:17 29:1 30:7 61:1 70:7 76:70)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Column - Somewhat compact group of objects in one dimension.  Compressed
;;  by including all values within a range with the objects below and above
;;  the range the first and last values in the vector.
;;
;; IE: Position -2 -1 0 1 2|3 4 5 6 7|8...
;;                         |         |
;;     Objects   0  0 0 0 0|0 1 2 2 5|5 5...
;;       default           |  real   |        default
;;    bottom objects_______| objects |_____ top objects
;;
;;  Is stored as #(2  0 1 2 2 5) where the first value in the vector is the
;;  position of the first default object.  The lower and upper default object
;;  are stored in the 2nd and last position.
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
     (vector 0 XX AIR))
   (make-vector-vector FieldDimension FieldDimension ()))))
(resetField)

;(vector-vector-map! (lambda (i) (display "\r\n") (display i) i) FIELD)

; Fields are 2d arrays of columns.  Columns are quasi-compressed stacks
; of cells that consist of a start height and specified stack of cells.
; #(3 1 1 2) would be cells (1 1 2) starting at z=4=(3+1). Cells below and
; above are assumed to be the lowest and higest specified cells in the vector.
; Setting a cell outside the explicit stack range expands the actual vector
; and adjusts the start-height value.
(define (field-column y x)
 (vector-vector-ref FIELD (modulo y FieldDimension)
                          (modulo x FieldDimension)))

(define (field-ref z y x)
 (letrec ((column   (field-column y x))
          (elements (columnRef column z)))
   (if (pair? elements) (car elements) elements)));1st in improper list of objs


; Scan down map column starting at z for first visibile cell.  Return height.
; BF:  What should this return if z is below the bottom most explicit cell?
(define (field-ref-top z y x)
 (letrec ((column (field-column y x))
          (top    (columnHeightTop column)) ;1st implicit top cell in column
          (bot    (columnHeightBottom column)));1st implicit bottom cell in col
 ; Adjust the z coor down to the first explicit cell in the column
 (let findNonAir ((z (if (>= z top) (set! z (- top 1)))))
   (if (or (not (eq? (columnRef column z) AIR))
           (<= z bot))
       (if (pair? z) (car z) z) ; Return first in improper list of objects.
       (findNonAir (- z 1))))))

(define (field-set! z y x c)
 (letrec ((fy (modulo y FieldDimension))
          (fx (modulo x FieldDimension))
          (column (vector-vector-ref FIELD fy fx)))
  (vector-vector-set! FIELD fy fx
     (columnSet column z (cons c (columnRef column z))))))

(define (field-delete! z y x e)
 (letrec ((fy (modulo y FieldDimension))
          (fx (modulo x FieldDimension))
          (column (vector-vector-ref FIELD fy fx)))
  (vector-vector-set! FIELD fy fx
     (columnSet column z (list-delete (columnRef column z) e)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Canvas
;;
(define CanvasDimension FieldDimension)

(define CANVAS (make-vector-vector CanvasDimension CanvasDimension ()))

; TODO this takes a long time.  Why?
(define timeStart (time))
(let ~ ((y 0)(x 0))
 (if (!= y CanvasDimension) (if (= x CanvasDimension) (~ (+ y 1) 0) (begin
  ; Each canvas entry consists of a map cell, the cell height and
  ; top most visible height.
   (vector-vector-set! CANVAS y x 
     (let ((top (field-ref-top 1000 y x)));What'll be higher than 1k?
       (vector (cell-ref (field-ref top y x))
               top
               1000)))
   (~ y (+ x 1))))))
;(WinChatDisplay "Initialized Canvas " (- (time) timeStart) " seconds")

(define (canvasRender y x)
 (let ((top (field-ref-top 1000 y x)))
  (canvasCellSet! y x (cell-ref (field-ref top y x)))
  (canvasCellHeightSet! y x top)))

(define (canvasCellRef y x)
  (vector-ref
     (vector-vector-ref CANVAS
       (modulo y CanvasDimension)
       (modulo x CanvasDimension))
     0))

(define (canvasCellSet! y x c)
  (vector-set! (vector-vector-ref CANVAS
                     (modulo y CanvasDimension)
                     (modulo x CanvasDimension))
               0 c))

(define (canvasCellHeightRef y x)
  (vector-ref (vector-vector-ref CANVAS
                   (modulo y CanvasDimension)
                   (modulo x CanvasDimension))
              1))
(define (canvasCellHeightSet! y x h)
 (vector-set! (vector-vector-ref CANVAS
                 (modulo y CanvasDimension)
                 (modulo x CanvasDimension))
              1 h))



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
     (WinMapPutGlyph (canvasCellRef (+ PortY y) (+ PortX x)) y (* x 2))
     (~dumpGlyphs y (++ x))))))
 ; DEBUG: Plot column
 ((WinColumn 'home))
 (let ~ ((z 11))
  (let ((c (field-ref z y x)))
   (if (eq? AIR c)
    (begin (WinColumnSetColor #x08)
           (WinColumnPuts "()"))
    (begin (set! c (cell-ref c))
           (WinColumnSetColor (glyphColor0 c))
           (WinColumnPutc (glyphChar0 c))
           (WinColumnSetColor (glyphColor1 c))
           (WinColumnPutc (glyphChar1 c)))))
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
    (WinMapPutGlyph (canvasCellRef gy gx) y (* x 2)))))



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
(define nobody (Entity 42 "nobody" 0 0 0 (glyphNew #x07 #\n #x07 #\o)))

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
   4 107 86
   ;0 0 0 
   (glyphNew #x0f (string-ref name 0)
             #x0f (string-ref name 1)))
  `(let ()
    (define (self msg) (eval msg))
    (define dir 0)
    (define (jump s r q) (set! z s) (set! y r) (set! x q))
    (define (face dirNew) (set! dir dirNew))
    (define (move)
      (if (= dir 0) (jump (+ z -1) y      x))
      (if (= dir 1) (jump z        (++ y) (-- x))
      (if (= dir 2) (jump z        (++ y) x)
      (if (= dir 3) (jump z        (++ y) (++ x))
      (if (= dir 4) (jump z        y      (-- x))
      (if (= dir 5) (jump (+ z 1)  y      x)
      (if (= dir 6) (jump z        y      (++ x))
      (if (= dir 7) (jump z        (-- y) (-- x))
      (if (= dir 8) (jump z        (-- y) x)
      (if (= dir 9) (jump z        (-- y) (++ x))))))))))))
    (define (walk dir)
     (face dir)
     (move))
    (define (look)
      (let ((loc (gps)))
        (move)
        (let ((c (cell-ref (field-ref z y x))))
          (apply setLoc loc)
          c)))
    (define cell BRICK)
    self) ))

(define avatar (Avatar "Guest"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Incomming IPC messages 
;;
(define (entity dna name z y x glyph)
 (entitiesSet dna name z y x glyph)
 (move dna z y x))
 ;(WinChatSetColor #x0e)
 ;;(WinChatDisplay name))
 ;(WinChatSetColor #x07)
 ;(WinChatDisplay " is present in World[tm] as "))
 ;(WinChatSetColor (glyphColor0 glyph))
 ;(WinChatDisplay (glyphChar0 glyph))
 ;(WinChatSetColor (glyphColor1 glyph))
 ;(WinChatDisplay (glyphChar1 glyph)))


(define (who)
 ((ipc 'qwrite)
 `(entity ,(avatar 'dna) ,(avatar 'name) ,@((avatar 'gps)) ,(avatar 'glyph))))

;; Call this 'move.  Entites move.  Back to the original ways?
;; was 'avatar
(define (move dna z y x)
 ;(WinChatDisplay "\r" (list z y x (distance (list 0 (avatar 'y) (avatar 'x)) (list 0 (+ PortY (/ PortH 2)) (+ PortX (/ PortW 2))))))
 (letrec ((entity (entitiesGet dna))
          (thisIsMe (= dna (avatar 'dna)))
          (shouldScroll (and thisIsMe
           (or SCROLLINGMAP (< 10 (distance (list 0 (avatar 'y) (avatar 'x))
                                           (list 0 (+ PortY (/ PortH 2)) (+ PortX (/ PortW 2)))))))))
  (if (null? entity)
    (begin
      ;(WinChatDisplay "\r\nERROR: move: unknown avatar:" dna)
      (entitiesSet dna "??" z y x (glyphNew #x19 #\? #x19 #\?))
      ((ipc 'qwrite) '(who)))
    (begin
      ; Move from here
      (field-delete! (entity 'z) (entity 'y) (entity 'x) dna)
      (if (>= (entity 'z) (canvasCellHeightRef (entity 'y) (entity 'x))) (begin
        (canvasRender (entity 'y) (entity 'x))
        (or shouldScroll (viewportRender (entity 'y) (entity 'x)))))
      ; Place here
      ((entity 'setLoc) z y x)
      (field-set! z y x dna)
      (if (>= (entity 'z) (canvasCellHeightRef y x)) (begin
        (canvasRender y x)
        (or shouldScroll (viewportRender y x))))
      (if shouldScroll (begin
        (viewportReset (avatar 'y) (avatar 'x))
        ;(WinChatDisplay (display->string ((avatar 'look))))
        ;(WinChatDisplay "\r")
        ;(displayl "\c28H\c0m" (field-column (avatar 'y) (avatar 'x)) "\cK" )
        ))))))

(define (die dna)
 (let ((entity (entitiesGet dna))
       (thisIsMe (= dna (avatar 'dna))))
  (if (not (null? entity)) ; Ignore unknown entities
    (begin
      ; Remove from here
      (field-delete! (entity 'z) (entity 'y) (entity 'x) dna)
      (if (>= (entity 'z) (canvasCellHeightRef (entity 'y) (entity 'x))) (begin
        (canvasRender (entity 'y) (entity 'x))
        (or thisIsMe (viewportRender (entity 'y) (entity 'x)))))))))


(define (voice dna z y x level text)
 (let ((entity (entitiesGet dna)))
   (WinChatDisplay "\r\n")
   (WinChatSetColor (glyphColor0 (entity 'glyph)))
   (if (null? entity)
     (WinChatDisplay "???>")
     (WinChatDisplay (entity 'name)))
   (WinChatSetColor (glyphColor1 (entity 'glyph)))
   (WinChatDisplay " ")
   (if (= level 100) (WinChatSetColor #x09))
   (WinChatDisplay text))
 (if (eqv? text "your turn") (thread (sayDesperate)))
 (if (and (!= dna (avatar 'dna)) (eqv? text "unatco"))
     ((ipc 'qwrite)
      `(voice ,(avatar 'dna)
              ,(avatar 'z) ,(avatar 'y) ,(avatar 'x)
              10 "no Savage"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard Input
;;
(define AvatarDirectionGlyphs
 (vector
   (glyphNew #x07 #\? #x07 #\?)
   (glyphNew #x4f #\\ #x4f #\_)
   (glyphNew #x4f #\\ #x4f #\/)
   (glyphNew #x4f #\_ #x4f #\/)
   (glyphNew #x4f #\< #x4f #\=)
   (glyphNew #x4f #\o #x4f #\o)
   (glyphNew #x4f #\= #x4f #\>)
   (glyphNew #x4f #\/ #x4f #\~)
   (glyphNew #x4f #\/ #x4f #\\)
   (glyphNew #x4f #\~ #x4f #\\)))

; BF: Should this be a thread?  At least move somewhere else.
(define (refreshIfBeyondViewport)
 (let ((y (modulo (- (avatar 'y) PortY) FieldDimension));Normalize avatar position.
   (x (modulo (- (avatar 'x) PortX) FieldDimension)))
   (if (or (<= PortW x) (<= PortH y))
     (viewportReset (avatar 'y) (avatar 'x)))))

(define (walk dir)
; (if (not (eq? BRICK (field-ref (avatar 'z) (avatar 'y) (avatar 'x))))
;   (begin
     ;(WinChatDisplay (list (avatar 'z)":" (avatar 'y)":"(avatar 'x)"="))
     ;(WinChatDisplay (list (field-ref 4 15 15) "," (field-ref 4 14 18)))
     ;(WinChatDisplay (field-ref (avatar 'z) (avatar 'y) (avatar 'x)))
     ;(WinChatDisplay "\r\n")))
 ((avatar 'walk) dir)
 ((ipc 'qwrite) `(move ,(avatar 'dna) ,@((avatar 'gps))))
 ; Popup a message to a new window temporarily.
 (if (eq? BUSHES   (field-ref (avatar 'z) (avatar 'y) (avatar 'x)))
  ((lambda ()
     (define WinTemp ((Terminal 'WindowNew) 1 29 1 22 #x1b))
     (define WinTempPutc (WinTemp 'putc))
     (map WinTempPutc (string->list "Are we having fun yet?"))
     (read-char stdin)
     ((WinTemp 'delete)))))
 (if (eq? HELP (field-ref (avatar 'z) (avatar 'y) (avatar 'x)))
     (help))
 (if (eq? SNAKE (field-ref (avatar 'z) (avatar 'y) (avatar 'x)))
     (thread (snake-random)))
 (if (eq? KITTY (field-ref (avatar 'z) (avatar 'y) (avatar 'x)))
     (thread (spawnKittehs)))
 ; Dump our coordinates.
 ((WinStatus 'puts) "\r\n")
 ((WinStatus 'puts) (number->string (avatar 'z)))
 ((WinStatus 'puts) " ")
 ((WinStatus 'puts) (number->string (avatar 'y)))
 ((WinStatus 'puts) " ")
 ((WinStatus 'puts) (number->string (avatar 'x))))

; Change avatar color.  Will just cycle through all 16 avatar colors.
(define (avatarColor)
 (letrec ((glyph (avatar 'glyph))
          (color0 (glyphColor0 glyph))
          (color1 (glyphColor1 glyph)))
  ((avatar 'setGlyph)
    (glyphNew (+ (* (colorBackground color0) 16)
                 (modulo (+ (colorForeground color0) 1) 16))
              (glyphChar0 glyph)
              (+ (* (colorBackground color1) 16)
                 (modulo (+ (colorForeground color1) 1) 16))
              (glyphChar1 glyph))))
  (who))

;(define (fortune)
; (let ~ (( dv8 (open-socket "dv8.org" 80)))
;  (sleep 10)
;  (if (not (eof-object? dv8))
;    (begin
;      (send "GET /~shroom/fortune.cgi\n\n" dv8) (sleep 10)
;      (let ~ ((c (read-char dv8)))
;           (if (eof-object? c)
;            (close dv8)
;            (begin (speak c)
;                   (~ (read-char dv8)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get the wheels in motion
;;

;; Input Window
(define WinInput
 ((Terminal 'WindowNew)
  (- (Terminal 'THeight) 1) 0
  1 (Terminal 'TWidth) #x4a))
(define WinInputPutc (WinInput 'putc))
(define WinInputPuts (WinInput 'puts))
(define WinInputSetColor (WinInput 'set-color))

;; Map window

; Initial map size is 20 or terminal width/height.
(define WinMap
 (let ((MapSize (min 28 (min (- (Terminal 'THeight) 1)
                             (/ (Terminal 'TWidth) 2)))))
  ((Terminal 'WindowNew)
    0 (- (Terminal 'TWidth) (* MapSize 2)) ; Position of the map window
    (+ MapSize 0) (* 2 MapSize)
    #x17 'NOREFRESH)))
((WinMap 'toggle))
((WinMap 'cursor-visible) #f) ; Disable cursor in map window.

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
        (l (- (vector-length glyph) 3))) ; number of extra animation glyphs
    (if (> (modulo t l) 0) (set! glyph (vector-ref glyph (+ 3 (modulo t l)))))
    (WinMapSetColor (glyphColor0 glyph))
    (WinMapPutc     (glyphChar0 glyph))
    (WinMapSetColor (glyphColor1 glyph))
    (WinMapPutc     (glyphChar1 glyph)))
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
    (/ (Terminal 'THeight) 3)  (- (/ (Terminal 'TWidth) 2) 11)
    3  22
    #x0f))
 (define WinMarqueePuts (WinMarquee 'puts))
 (define WinMarqueePutc (WinMarquee 'putc))
 (define WinMarqueeSetColor (WinMarquee 'set-color))
 ((WinMarquee 'cursor-visible) #f) ; Disable cursor in map window.
 (WinMarqueePuts "+--------------------+")
 (WinMarqueePuts "|                    |")
 (WinMarqueePuts "+--------------------+")
 (let ~~ ((i -5))
   ((WinMarquee 'goto) 1 1)
   (sleep 200) ; Delay
   (let ~ ((j 0))
     (WinMarqueeSetColor (vector-ref #(07 07 07 07 07 07 07 07   07 07 07
                                       9  11 10 12 13 15 15 15 15 15)
                                     (modulo (+ i j) 21)))
     (WinMarqueePutc (vector-ref #(#\W #\e #\l #\c #\o #\m #\e #\   #\t #\o #\ 
                                   #\W #\o #\r #\l #\d #\  #\  #\  #\  #\ )
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
  ((win 'goto) 1 1) ((win 'set-color) #x70)
  (lambda ()
   (if (= pos 20)
    (begin
     ((win 'delete))
     #f)
    (begin
     ((win 'putc) #\ )
     (set! pos (+ pos 1))
     #t)))))

(define (loading y x title)
 (let ((loadingbar (makeProgressBar y x "P R O G R E S S")))
   (let ~ ()
     (sleep 200)
     (if (loadingbar) (~)))))

(define (snake y x delay)
 (define Window3 ((Terminal 'WindowNew) y x 3 3 #x6b))
 (define Window3Puts (Window3 'puts))
 ((Window3 'alpha) 1 1 #f) ; Transparent window location.
 ((Window3 'cursor-visible) #f)
 (let ~ ((m 0))
   ((Window3 'home)) (Window3Puts "oO ") (Window3Puts ".  ") (Window3Puts ".  ")
   (sleep delay)
   ((Window3 'home)) (Window3Puts ".oO") (Window3Puts ".  ") (Window3Puts "   ")
   (sleep delay)
   ((Window3 'home)) (Window3Puts "..o") (Window3Puts "  O") (Window3Puts "   ")
   (sleep delay)
   ((Window3 'home)) (Window3Puts " ..") (Window3Puts "  o") (Window3Puts "  O")
   (sleep delay)
   ((Window3 'home)) (Window3Puts "  .") (Window3Puts "  .") (Window3Puts " Oo")
   (sleep delay)
   ((Window3 'home)) (Window3Puts "   ") (Window3Puts "  .") (Window3Puts "Oo.")
   (sleep delay)
   ((Window3 'home)) (Window3Puts "   ") (Window3Puts "O  ") (Window3Puts "o..")
   (sleep delay)
   ((Window3 'home)) (Window3Puts "O  ") (Window3Puts "o  ") (Window3Puts ".. ")
   (sleep delay)
   (if (< m 10) (~ (+ m 1)))) ; Loop 10 times
 ((Window3 'delete)))

(define (snake-random)
 (snake
   (random (- (Terminal 'THeight) 3))
   (random (- (Terminal 'TWidth) 3))
   200))

(define WinHelp ((Terminal 'WindowNew) 5 20 12 30 #x21))

; Give the help window an artistic border.
(map (lambda (x) ((WinHelp 'alpha) 0 x #f))
     '(0 1 2 3 4 5 6 7 8 20 21 22 23 24 25 26 27 28 29))

((WinHelp 'goto) 0 0)
((WinHelp 'puts) "         )) Help! ((")
((WinHelp 'set-color) #x20)
((WinHelp 'puts) "\r\n? = toggle help window\r\nt = talk\r\nesc = exit talk\r\nm = toggle map\r\nW = toggle animation\r\n> = increase map size\r\n< = decrease map size\r\nq = quit\r\nhjkl = move\r\n^A = change color\r\nc = spawn kitteh")
((WinHelp 'toggle))

(define (help)
 ; (WinHelp '(set! Y0 2))
 ; (WinHelp '(set! Y1 (+ Y0 WHeight)))
  ((WinHelp 'toggle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drop some cells on the map
(define (dropCell y x cell)
 (let ((z (+ 1 (field-ref-top 1000 y x))))
  (field-set! z y x cell)
  (canvasRender y x)
  (viewportRender y x)))

(define (setCell z y x cell)
  (field-set! z y x cell)
  (canvasRender y x)
  (viewportRender y x))

(define (build-brick-room yy xx)
 (let ~ ((i 0))
    ;(sleep 1000)
    (dropCell yy         (+ xx i)        BRICK)
    (dropCell (+ yy 10)  (+ 10 xx (- i)) BRICK)
    (dropCell (+ 10 yy (- i)) xx         BRICK)
    (dropCell (+ yy i)   (+ xx 10)       BRICK)
    (if (< i 9) (~ (+ i 1)))))


(define (build-island yy xx radius)
 (let ~ ((y (- radius))(x (- radius)))
  (if (< y radius)
  (if (= x radius) (~ (+ y 1) (- radius))
   (begin
    (if (> radius (sqrt (+ (^2 y) (^2 x))))
        (dropCell (+ y yy) (+ x xx) SAND))
    (~ y (+ x 1)) )))))

; Mapping from Ultima4 map file byte index to (y x) World[tm] coordinates.
;  y = (+ (modulo (/ i 32) 32) (* (/ i 8192) 32))
;  x = (+ (modulo i 32) (* (modulo (/ i 1024) 8) 32))

; Mapping from World[tm] (y x) corrdinates to Ultima4 map file byte index.
; i = (+ (modulo x 32) (* (/ x 32) 1024) (* (modulo y 32) 32) (* (/ y 32) 8192))

(define (load-ultima-world4)
 (resetField)
 (let ((fd (open "ultima4.map"))
       (bar (makeProgressBar 5 30 "Britannia 4"))
       (timeStart (time)))
  (let ~ ((i 0))
     (or (= i 65536) (begin
      (if (= 0 (modulo i (/ 65536 20))) (bar))
      (setCell 2
               (+ (modulo (/ i 32) 32) (* (/ i 8192) 32))
               (+ (modulo i 32) (* (modulo (/ i 1024) 8) 32))
               (+ 0 (read-char fd))) ; Hack to convert char to integer
      (~ (+ i 1)))))
  ;(WinChatDisplay "\r\nInitialized UltimaIV Map " (- (time) timeStart) " seconds")
))

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
  (WinChatSetColor #x0f)
  (WinChatDisplay "\r\nTank ")
  (WinChatSetColor #x07)
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
         (glyphNew (glyphColor0 (avatar 'glyph)) (string-ref talkInput 11)
                   (glyphColor1 (avatar 'glyph)) (string-ref talkInput (if (> strLen 12) 12 11))))
      (thread (begin (sleep 500) (who)))))))
 (if tankIsListening (begin
   (if (string=? "load the jump program" talkInput) (tankTalk "I can't find the disk")
   (if (string=? "load ultima4" talkInput) (thread (load-ultima-world4))
   (if (string=? "load underworld" talkInput) (thread (load-ultima-underworld))
   (if (string=? "load ultima5" talkInput) (thread (load-ultima-world5)))))))))

(define (say talkInput)
 ((ipc 'qwrite) (list 'voice (avatar 'dna) (avatar 'z) (avatar 'y) (avatar 'x) 10 talkInput)))

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
   (if (or (eq? c CHAR-CTRL-ESC) ; Escape char
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
 ;((box 'cursor-visible) #f) ; Disable cursor in map window.
 (puts "+------------------+")
 (puts "|                  |")
 (puts "+------------------+")
 ((box 'goto) 0 2) (puts title)
 ((box 'goto) 1 1)
 (let ~ ((c (read-char stdin)))
   (or (eq? c RETURN) (eq? c NEWLINE)
    (begin
     (if (or (eq? c CHAR-CTRL-H)
             (eq? c CHAR-CTRL-_)
             (eq? c CHAR-CTRL-?)
             (eq? c #\%))
       (if (eq? str "") "" (begin
             ((box 'backspace) #\ )
             (set! str (substring str 0 (- (string-length str) 1)))))
       (if (< (string-length str) 18) (begin
             (putc c)
             (set! str (string str c)))))
     (~ (read-char stdin)))))
 ((box 'delete))
 str)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buttons
; 157 crashes
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

(setButton #\1 '(thread (sigwinch)))
(setButton #\4 '(load-ultima-world4))
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
(setButton CHAR-CTRL-A '(avatarColor))
(setButton CHAR-CTRL-F '(walkForever))
(setButton CHAR-CTRL-K '((ipc 'qwrite) `(set! FIELD ,FIELD))) ; Send my plane out to IPC.
(setButton CHAR-CTRL-L '(begin (viewportReset (avatar 'y) (avatar 'x)) ((WinChat 'repaint))))
(setButton CHAR-CTRL-M '(begin ((WinStatus 'toggle)) ((WinColumn 'toggle))))
(setButton CHAR-CTRL-Q '(set! state 'done))
(setButton CHAR-CTRL-_ '(walk 4))
(setButton #\d '((ipc 'qwrite) `(dropCell ,(avatar 'y) ,(avatar 'x) BRICK))) ; Send my plane out to IPC.
(setButton #\g 
   '(let ((o (field-ref (avatar 'z) (avatar 'y) (avatar 'x))))
     (field-delete!  (avatar 'z) (avatar 'y) (avatar 'x) o)
     (avatar `(set! cell ,o))))
(setButton #\? '(help))
(setButton #\< '(winMapSmaller))
(setButton #\> '(winMapBigger))
(setButton #\z '(circularize))
(setButton #\Z '(circularize #t))
(setButton #\c '(thread (spawnKitty)))
(setButton #\q '(set! state 'done))
(setButton #\Q '(set! state 'done))
(setButton eof '(set! state 'done))
(setButton CHAR-CTRL-C '((WinConsole 'toggle)))

(define (replCmd c)
 (define state 'cmd) ; state might be changed to 'done or 'talk.
 (let ((button (vector-ref Buttons c)))
   (if ShowButtons (WinChatDisplay "\r\n" c " " (getButton c)))
   (if (pair? button) (eval button)
    (if (procedure? button) (button)
     (WinChatDisplay "\r\nButton " c " undefined " button))))
 state)

(define wrepl (let ((state 'cmd))
 (lambda ()
  (if (not (eq? state 'done))
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
 ((ipc 'qwrite) `(entity ,(kitty 'dna) "kitty" ,@((kitty 'gps)) ,(glyphNew #x07 #\K #x0f #\a)))
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
   ;    ((ipc 'qwrite) `(voice ,(kitty 'dna) ,@((kitty 'gps)) 10 "Mrrreeeooowww!")))
   (if (> i (+ cycles (random 30)))
       ((ipc 'qwrite) `(die ,(kitty 'dna))) ; kill entity
       (~ (+ i 1)))))) ; spawnKitty

(define (spawnKittehs)
 (loading 10 35)
 (vector-map! (lambda (x) (thread (x))) (make-vector (random 30) spawnKitty)))
 


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

(define sig28Semaphore (open-semaphore 1))

(define (sigwinch)
  (semaphore-down sig28Semaphore)
  (handleTerminalResize)
  (semaphore-up sig28Semaphore))

(vector-set! SIGNALHANDLERS 28 (lambda () (sigwinch) (unthread)))
(signal 28)


;; Example on how to implement a signal handler.
;; This will capture control C spaning a thread which just displays "hi".
;(define (catch) (display 'hi) (unthread))
;(vector-set! SIGNAL-HANDLERS 2 catch)
;(signal 2)

(define walkForever (let ((walkForeverFlag #f)) (lambda ()
 (if walkForeverFlag
  (begin
   (set! walkForeverFlag #f)
   (WinChatSetColor #x0a)
   (WinChatDisplay "\r\nThus ends the journey"))
  (begin
   (WinChatSetColor #x0a)
   (WinChatDisplay "\r\nThe journey begins")
   (set! walkForeverFlag #t)
   (thread (let ~ ()
     (for-each
       (lambda (x) (or walkForeverFlag (unthread)) (walk x) (sleep 400))
       '(6 6 6 6 8 8 8 8 4 4 4 4 2 2 2 2))
     (sleep 500)
     (~))))))))

(define (sayDesperate)
  (let ((cmd `(voice ,(avatar 'dna) ,(avatar 'z) ,(avatar 'y) ,(avatar 'x)
                     10
                     "desperate")))
   (sleep (random 500))
   ((ipc 'qwrite) cmd) (sleep 500)
   ((ipc 'qwrite) cmd) (sleep 500)))

(define (sayHelloWorld)
 ((ipc 'qwrite)
   (list 'voice (avatar 'dna) (avatar 'z) (avatar 'y) (avatar 'x) 100
     (vector-random
       #("*PUSH* *SQUIRT* *SPANK* *WAAAAAAAAA*"
         "*All Worldlians Want to Be Borned*"
         "*Happy Birthday*")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start everything

(thread (welcome))

(let ((name (boxInput "Enter your name")))
 (if (eq? name "") (set! name "Guest"))
 ((avatar 'setNameGlyph) name (glyphNew #x0f (string-ref name 0) #x0f (string-ref name 1))))

(WinChatSetColor #x0b)
;(WinChatDisplay "\r\n\r\n   *** Welcome to the construct. ***")
((WinChat 'goto) 0 0) ;(WinChat 'WHeight)
(WinChatDisplay "\r\nWelcome to World")
(WinChatDisplay "\r\nSee http://code.google.com/p/worldtm")
(WinChatDisplay "\r\nHit ? to toggle the help window")
(WinChatDisplay "\r\nYour name is " (avatar 'name))

; Create ipc object.  Pass in a debug message output port.
; for production world an 'empty' port is passed.
; (define ipc (Ipc WinChatDisplay))
(define ipc (Ipc (lambda x ())))

; Always read and evaluate everything from IPC.
(thread  (let ~ () 
 (let ((sexp ((ipc 'qread))))
    (WinConsolePuts (display->string sexp))
    (eval sexp)
    (~))))

((ipc 'qwrite) '(who))
;((ipc 'qwrite) `(voice ,(avatar 'dna) ,@((avatar 'gps)) 10 "Hello World[tm]!"))


;(build-island 15 15 7) 
;(dropCell 13 17 HELP)
;(dropCell 16 18 SNAKE)
;(dropCell 19 16 KITTY)
;(dropCell 12 13 TV)
;(dropCell 17 13 CHAIR)
;(build-island 4 28 4) 
;(build-island 20 30 5)
;(build-brick-room 15 35)
(viewportReset (avatar 'y) (avatar 'x))
((WinMap 'toggle))
(thread (load-ultima-world4))

;(thread (snake 18 38 500))

; Redraw map resulting in animated cells.
(thread (let ~ ()
   (sleep 1000)
   (if CELLANIMATION (viewportReset (avatar 'y) (avatar 'x)))
   (~)))

(define (sayByeBye)
 ((ipc 'qwrite) (list 'voice (avatar 'dna) (avatar 'z) (avatar 'y) (avatar 'x) 100 "*POOF*")))

; Catch the terminal hangup signal so that normal shutdown can occur.
(vector-set! SIGNALHANDLERS 1 (lambda () (sleep 1000)))
(signal 1)

; Catch SIGPIPE which occurs when an IPC connection is broken.
;(vector-set! SIGNALHANDLERS 13 (lambda () (WinChatDisplay  "\r\nSIGPIPE IGNORED")))
;(signal 13)

(or QUIETLOGIN (sayHelloWorld))
(wrepl)
(or QUIETLOGIN (sayByeBye))

((ipc 'qwrite) `(die ,(avatar 'dna))) ; Kill avatar's entity
(sleep 500) ; wait for ipc to flush

(displayl "\e[" (Terminal 'THeight) "H\r\n\e[0m\e[?25h")
(quit)
