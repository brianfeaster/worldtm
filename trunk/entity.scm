;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A sprite object which is a static grid of glyphs and #f
; TODO list of glyph arrays as animation frames
(define (Sprite height width glyphArray) ; height, width, vector of glyphs/#f
  (define (self msg) (eval msg))
  (define (serialize) (list 'Sprite height width glyphArray))
  (define coordinates ()) ; Eventually a list of non-false glyph coordinates
  (define glyphCount (vector-length glyphArray))

  (define (glyphRef y x)
    (vector-ref glyphArray (modulo (+ (* y width) x) glyphCount)))

  ; Initialize the list of relative glyph coordinates
  (loop2 0 height 0 width (lambda (y x)
   (let ((g (glyphRef y x)))
     (if g (set! coordinates (cons (cons y x) coordinates))))))
  (set! coordinates (reverse coordinates))

  self)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; An entity
;
(define (Entity dna port name sprite z y x)
 (define (self msg) (eval msg))
 (define oz z)
 (define oy y)
 (define ox x)
 (define (setPort port0)
   (set! port port0))
 (define (setName name0)
   (set! name name0))
 (define (setSprite sprite0)
   (set! sprite sprite0))
 (define (setLoc z0 y0 x0)
   (set! oz z) (set! oy y) (set! ox x)
   (set! z z0) (set! y y0) (set! x x0))
 (define (glyph) ((sprite 'glyphRef) 0 0))
 (define (gps)
   (list z y x))
 self)
