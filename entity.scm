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