;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Hw11) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)


(define-struct pipe [top bot left right])
;; A Pipe is a (make-pipe Boolean Boolean Boolean Boolean)
;; Interpretation: a pipe with openings in the given directions. A  #true for 
;; one of top, bot, left, right indicates an opening in that direction.

(define PIPE-TL (make-pipe #true #false #true #false))
(define PIPE-TR (make-pipe #t #f #f #t))
(define PIPE-BR (make-pipe #f #t #f #t))
(define PIPE-BL (make-pipe #f #t #t #f))
(define PIPE-LR (make-pipe #f #f #t #t))
(define PIPE-TB (make-pipe #t #t #f #f))
(define PIPE-TBLR (make-pipe #t #t #t #t))

(define (pipe-templ pipe)
  (...(pipe-top pipe)...
    (pipe-bot pipe)...
    (pipe-left pipe)...
    (pipe-right pipe)...))

;; A StartingPipe is a (make-pipe Boolean Boolean Boolean Boolean)
;; Interpretation: A StartingPipe is a Pipe where only one of top, bot, left, right are true.
;; StartingPipes cannot be placed; one StartingPipe's location is set when the game begins.

(define PIPE-T (make-pipe #t #f #f #f))
(define PIPE-B (make-pipe #f #t #f #f))
(define PIPE-L (make-pipe #f #f #t #f))
(define PIPE-R (make-pipe #f #f #f #t))

(define (starting-pipe-templ pipe)
  (...(pipe-top pipe)...
    (pipe-bot pipe)...
    (pipe-left pipe)...
    (pipe-right pipe)...))


(define ALL-PIPES (list PIPE-TL PIPE-TR PIPE-BR PIPE-BL PIPE-TB PIPE-LR PIPE-TBLR))


;; pipe->image: Pipe Integer Integer Boolean Direction -> Image
;; Draws the given pipe on a square tile with length tile-side-length. The width
;; of the pipe is pipe-width. Pipe-width should be less than tile-side-length.
;; If filled? then draw the pipe with goo.

;; add parameter to track if vertical or horizontal rectangles are already filled
;; can't place any more pipes after goo stops
;; fix hw8 errors

(define (pipe->image pipe tile-side-length pipe-width filled? dir)
  (local [;pipe-w/o-goo: n n string ->image
          ;creates a pipe without goo given the tile side length and pipe-width
          (define (pipe-w/o-goo l w)
            (rectangle (+ (/ tile-side-length 2) (/ pipe-width 2)) pipe-width "solid" "black"))
          ;pipe-w/-goo: n n ->image 
          ;creates a pipe with goo given the tile side length and pipe-width 
          (define (pipe-w/-goo length width)
            (rectangle (+ (/ tile-side-length 2) (/ pipe-width 2)) pipe-width "solid" "hot pink"))
          ;rotate-w/o-goo: n n -> image
          ;creates a pipe given the tile side length and pipe-width, then rotates it 90 degrees
          (define (rotate-w/o-goo length width)
            (rotate 90 (rectangle (+ (/ tile-side-length 2) (/ pipe-width 2)) pipe-width "solid" "black")))
          ;rotate-w/goo: n n ->image
          ;creates a pipe with goo given the tile side length and pipe-width, then rotate it 90 degreses
           (define (rotate-w/-goo length width)
             (rotate 90 (rectangle (+ (/ tile-side-length 2) (/ pipe-width 2)) pipe-width "solid" "hot pink")))
          ;Transparent-square
          (define TRANSPERENT-SQUARE (square tile-side-length "solid" "transparent"))]
  (overlay
   (cond
     [(and (pipe-left pipe) (pipe-right pipe) filled? (or (string=? dir LEFT) (string=? dir RIGHT))) (overlay (rectangle tile-side-length pipe-width "solid" "hot pink") TRANSPERENT-SQUARE)]
     [(and (pipe-left pipe) (pipe-right pipe) filled? (or (string=? dir UP) (string=? dir DOWN))) (overlay (rectangle tile-side-length pipe-width "solid" "black") TRANSPERENT-SQUARE)]
     [(and (pipe-left pipe) (pipe-right pipe)) (overlay (rectangle tile-side-length pipe-width "solid" "black") TRANSPERENT-SQUARE)]
     [(and (pipe-left pipe) filled?) (overlay/align "left" "middle" (pipe-w/-goo tile-side-length pipe-width) TRANSPERENT-SQUARE)]
     [(and (pipe-right pipe) filled?) (overlay/align "right" "middle" (pipe-w/-goo tile-side-length pipe-width)  TRANSPERENT-SQUARE)]
     [(pipe-left pipe) (overlay/align "left" "middle" (pipe-w/o-goo tile-side-length pipe-width)  TRANSPERENT-SQUARE)]
     [(pipe-right pipe) (overlay/align "right" "middle" (pipe-w/o-goo tile-side-length pipe-width) TRANSPERENT-SQUARE)]
     [else empty-image])
   (cond
     [(and (pipe-top pipe) (pipe-bot pipe) filled? (or (string=? dir UP) (string=? dir DOWN))) (overlay (rotate 90 (rectangle tile-side-length pipe-width "solid" "hot pink")) TRANSPERENT-SQUARE)]
     [(and (pipe-top pipe) (pipe-bot pipe) filled? (or (string=? dir LEFT) (string=? dir RIGHT))) (overlay (rotate 90 (rectangle tile-side-length pipe-width "solid" "black")) TRANSPERENT-SQUARE)]
     [(and (pipe-top pipe) (pipe-bot pipe)) (overlay (rotate 90 (rectangle tile-side-length pipe-width "solid" "black")) TRANSPERENT-SQUARE)]
     [(and (pipe-top pipe) filled?) (overlay/align "middle" "top" (rotate-w/-goo tile-side-length pipe-width) TRANSPERENT-SQUARE)]
     [(and (pipe-bot pipe) filled?) (overlay/align "middle" "bottom" (rotate-w/-goo tile-side-length pipe-width) TRANSPERENT-SQUARE)]
     [(pipe-top pipe) (overlay/align "middle" "top" (rotate-w/o-goo tile-side-length pipe-width) TRANSPERENT-SQUARE)]
     [(pipe-bot pipe) (overlay/align "middle" "bottom" (rotate-w/o-goo tile-side-length pipe-width) TRANSPERENT-SQUARE)]
     [else empty-image])
   (square tile-side-length "solid" "pink"))))


(define-struct pipe-w-coords [pipe row col])
;; A PipeWithCoords is a (make-pipe Pipe Number Number)
;; Interpretation: Represents a Pipe and its row # and column # on the grid.
(define PIPE-1 (make-pipe-w-coords PIPE-TL 1 2))
(define PIPE-2 (make-pipe-w-coords PIPE-LR 3 0))
(define PIPE-3 (make-pipe-w-coords PIPE-TBLR 5 5))
(define PIPE-4 (make-pipe-w-coords PIPE-BR 8 1))
(define PIPE-5 (make-pipe-w-coords PIPE-TR 0 6))

(define SP-1 (make-pipe-w-coords PIPE-R 2 2))
(define SP-2 (make-pipe-w-coords PIPE-B 1 0))
(define SP-3 (make-pipe-w-coords PIPE-R 0 4))

;; pipe-with-coords-templ : PipeWithCoords -> ?
(define (pipe-with-coords-templ p)
  (cond
    [(pipe-w-coords-pipe p)...]
    [(pipe-w-coords-row p)...]
    [(pipe-w-coords-col p)...]))


;; A ListOfPipeWithCoords is one of:
;; empty
;; (cons PipeWithCoords ListOfPipeWithCoords)
;; Interpretation: Represents a list of PipeWithCoords where:
;; empty represents an empty list of PipeWithCoords
;; (cons first rest) represents a list with a _first_ PipeWithCoords and the _rest_ of the PipeWithCoords
(define LOPWC-1 (list PIPE-1 PIPE-2))
(define LOPWC-2 (list PIPE-3 PIPE-4 PIPE-5))
(define LOPWC-3 (list PIPE-3))

;; lopwc-templ : ListOfPipeWithCoords -> ?
(define (lopwc-templ lopwc)
  (cond
    [(empty? lopwc)...]
    [(cons? lopwc) (...(pipe-with-coords-templ (first lopwc))...
                       (lopwc-templ (rest lopwc))...)]))


;; A Direction is one of:
;; "left"
;; "right"
;; "up"
;; "down"
;; Represents the direction that a pipe may open towards.
(define LEFT "left")
(define RIGHT "right")
(define UP "up")
(define DOWN "down")
;; dir-templ : Direction -> ?
(define (dir-templ d)
  (cond
    [(string=? LEFT d)...]
    [(string=? RIGHT d)...]
    [(string=? UP d)...]
    [(string=? DOWN d)...]))
    

(define-struct pp/dir [pp dir])
;; A PlacedPipeWithDirection is a (make-pp/dir PipeWithCoords Direction)
;; Represents a placed PipeWithCoords and the direction that the goo is flowing out of the pipe.
(define PP/DIR-1 (make-pp/dir PIPE-1 UP))
(define PP/DIR-2 (make-pp/dir SP-1 RIGHT))
(define PP/DIR-3 (make-pp/dir PIPE-2 LEFT))
;; pp/dir-templ : PlacedPipeWithDirection -> ?
(define (pp/dir-templ p)
  (...(pp/dir-pp p)...
      (pp/dir-dir p)...))


;; A GooFlow is a [List-of PlacedPipeWithDirection]
;; Interpretation: Represents a [List-of PlacedPipeWithDirection] where the PipeWithCoords in the list
;; are only pipes that contain goo and the first pipe of PlacedPipeWithDirection must be a StartingPipe, and the direction
;; of the last PlacedPipeWithDirection is where the goo will flow next (or try to). All of the PlacedPipeWithDirections in the GooFlow
;; must fit with the next one in the GooFlow.
(define GOOFLOW-1 (list (make-pp/dir SP-2 DOWN) (make-pp/dir (make-pipe-w-coords PIPE-TB 2 0) DOWN)))
(define GOOFLOW-2 (list PP/DIR-2 (make-pp/dir (make-pipe-w-coords PIPE-BL 2 3) DOWN)))
(define GOOFLOW-3 (list (make-pp/dir SP-3 RIGHT)))

;; gooflow-temp : GooFlow -> ?
(define (gooflow-templ gf)
  (cond
    [(empty? gf)...]
    [(cons? gf) (...(first gf)...
                       (gooflow-templ (rest gf))...)]))


;; fit? : PipeWithCoords PipeWithCoords -> Boolean
;; Do the two PipeWithCoords fit with each other? i.e. p1's opening meets p2's opening
(check-expect (fit? PIPE-1 (make-pipe-w-coords PIPE-LR 1 1)) #t)
(check-expect (fit? PIPE-2 (make-pipe-w-coords PIPE-TBLR 3 1)) #t)
(check-expect (fit? PIPE-3 (make-pipe-w-coords PIPE-LR 5 1)) #f)

(define (fit? p1 p2)
  (local [;; came-from : PipeWithCoords PipeWithCoords -> (Optional-String)
          ;; Returns "left" if pi1 is directly to the left of pi2, returns "right" if pi1 is directly to the right of pi2,
          ;; returns "above" if pi1 is directly above pi2, returns "below" if pi1 is directly below pi2, returns #false otherwise
          ;; read: p2 came from " ", where p1 was OR p1 is on the " " of p2
          (define (came-from pi1 pi2)
            (cond
              [(= (- (pipe-w-coords-col pi1) 1) (pipe-w-coords-col pi2)) "right"]
              [(= (+ (pipe-w-coords-col pi1) 1) (pipe-w-coords-col pi2)) "left"]
              [(= (- (pipe-w-coords-row pi1) 1) (pipe-w-coords-row pi2)) "below"]
              [(= (+ (pipe-w-coords-row pi1) 1) (pipe-w-coords-row pi2)) "above"]
              [else "nope"]))]
    (cond
      [(string=? (came-from p1 p2) "right") (and (pipe-left (pipe-w-coords-pipe p1)) (pipe-right (pipe-w-coords-pipe p2)))]
      [(string=? (came-from p1 p2) "left") (and (pipe-right (pipe-w-coords-pipe p1)) (pipe-left (pipe-w-coords-pipe p2)))]
      [(string=? (came-from p1 p2) "below") (and (pipe-top (pipe-w-coords-pipe p1)) (pipe-bot (pipe-w-coords-pipe p2)))]
      [(string=? (came-from p1 p2) "above") (and (pipe-bot (pipe-w-coords-pipe p1)) (pipe-top (pipe-w-coords-pipe p2)))]
      [else #f])))

;; last : [List-of X] -> X
;; returns the last item in a list
(check-expect (last (list 1 2 3)) 3)
(check-expect (last (list "a" "b" "c")) "c")
(check-expect (last empty) empty)

(define (last l)
  (cond
    [(empty? l) l]
    [(empty? (rest l)) (first l)]
    [(cons? (rest l)) (last (rest l))]))

;; grid-goo-propagate : GooFlow Grid -> GooFlow
;; Moves the goo forward by one tile. If not possible (reaches an empty cell, the boundary of the grid,
;; or a pipe that does not have an opening towards the direction of the flow), produces the same goo.
(check-expect (grid-goo-propagate GOOFLOW-1 STARTING-GRID) GOOFLOW-1)
(check-expect (grid-goo-propagate GOOFLOW-1 (place-pipe STARTING-GRID PIPE-LR 4 4)) GOOFLOW-1)
(check-expect (grid-goo-propagate GOOFLOW-2 (place-pipe STARTING-GRID PIPE-TBLR 3 3))
              (list PP/DIR-2 (make-pp/dir (make-pipe-w-coords PIPE-BL 2 3) DOWN) (make-pp/dir (make-pipe-w-coords PIPE-TBLR 3 3) DOWN)))
(check-expect (grid-goo-propagate GOOFLOW-3 (place-pipe STARTING-GRID PIPE-BL 0 5))
              (list (make-pp/dir SP-3 RIGHT) (make-pp/dir (make-pipe-w-coords PIPE-BL 0 5) DOWN)))


(define (grid-goo-propagate gf g)
  (local [;; next-gooflow : Direction -> Pipe
          ;; produces the Pipe at the Direction of the last GooFlow.
          (define (next-gooflow d)
            (cond
              [(string=? d RIGHT) (pipe-at g (pipe-w-coords-row (pp/dir-pp (last gf))) (+ 1 (pipe-w-coords-col (pp/dir-pp (last gf)))))]
              [(string=? d LEFT) (pipe-at g (pipe-w-coords-row (pp/dir-pp (last gf))) (- (pipe-w-coords-col (pp/dir-pp (last gf))) 1))]
              [(string=? d UP) (pipe-at g (- (pipe-w-coords-row (pp/dir-pp (last gf))) 1) (pipe-w-coords-col (pp/dir-pp (last gf))))]
              [(string=? d DOWN) (pipe-at g (+ 1 (pipe-w-coords-row (pp/dir-pp (last gf)))) (pipe-w-coords-col (pp/dir-pp (last gf))))]))
          ;; pwc-at-next : Direction -> GooFlow
          ;; locates the next PipeWithCoords to potentially be added to GooFlow
          (define (pwc-at-next d)
            (cond
              [(string=? d RIGHT) (make-pipe-w-coords
                                                (next-gooflow RIGHT)
                                                (pipe-w-coords-row (pp/dir-pp (last gf)))
                                                (+ 1 (pipe-w-coords-col (pp/dir-pp (last gf)))))]
              [(string=? d LEFT) (make-pipe-w-coords
                                                (next-gooflow LEFT)
                                                (pipe-w-coords-row (pp/dir-pp (last gf)))
                                                (- (pipe-w-coords-col (pp/dir-pp (last gf))) 1))]
              [(string=? d UP) (make-pipe-w-coords
                                                (next-gooflow UP)
                                                (- (pipe-w-coords-row (pp/dir-pp (last gf))) 1)
                                                (pipe-w-coords-col (pp/dir-pp (last gf))))]
              [(string=? d DOWN) (make-pipe-w-coords
                                                (next-gooflow DOWN)
                                                (+ 1 (pipe-w-coords-row (pp/dir-pp (last gf))))
                                                (pipe-w-coords-col (pp/dir-pp (last gf))))]))]

    (cond
      [(and (string=? RIGHT (pp/dir-dir (last gf))) 
            (fit? (pp/dir-pp (last gf)) (pwc-at-next RIGHT)))
       (cond
         [(pipe-right (next-gooflow RIGHT))
          (append gf (cons (make-pp/dir (pwc-at-next RIGHT) RIGHT) empty))]
         
         [(pipe-top (next-gooflow RIGHT))
          (append gf (cons (make-pp/dir (pwc-at-next RIGHT) UP) empty))]
         
         [(pipe-bot (next-gooflow RIGHT))
          (append gf (cons (make-pp/dir (pwc-at-next RIGHT) DOWN) empty))])]
      
      [(and (string=? LEFT (pp/dir-dir (last gf))) 
            (fit? (pp/dir-pp (last gf)) (pwc-at-next LEFT)))
       (cond
         [(pipe-left (next-gooflow LEFT))
          (append gf (cons (make-pp/dir (pwc-at-next LEFT) LEFT) empty))]
         
         [(pipe-top (next-gooflow LEFT))
          (append gf (cons (make-pp/dir (pwc-at-next LEFT) UP) empty))]
         
         [(pipe-bot (next-gooflow LEFT))
          (append gf (cons (make-pp/dir (pwc-at-next LEFT) DOWN) empty))])]
      
      [(and (string=? UP (pp/dir-dir (last gf))) 
            (fit? (pp/dir-pp (last gf)) (pwc-at-next UP)))
       (cond
         [(pipe-top (next-gooflow UP))
          (append gf (cons (make-pp/dir (pwc-at-next UP) UP) empty))]
         
         [(pipe-right (next-gooflow UP))
          (append gf (cons (make-pp/dir (pwc-at-next UP) RIGHT) empty))]                
         
         [(pipe-left (next-gooflow UP))
          (append gf (cons (make-pp/dir (pwc-at-next UP) LEFT) empty))])]
      
      [(and (string=? DOWN (pp/dir-dir (last gf))) 
            (fit? (pp/dir-pp (last gf)) (pwc-at-next DOWN)))
       (cond
         [(pipe-bot (next-gooflow DOWN))
          (append gf (cons (make-pp/dir (pwc-at-next DOWN) DOWN) empty))]
         
         [(pipe-right (next-gooflow DOWN))
          (append gf (cons (make-pp/dir (pwc-at-next DOWN) RIGHT) empty))]
         
         [(pipe-left (next-gooflow DOWN))
          (append gf (cons (make-pp/dir (pwc-at-next DOWN) LEFT) empty))])]
      [else gf])))


                                                            
(define-struct grid [n lopwc])
;; A Grid is a (make-grid Number ListOfPipeWithCoords)
;; Represents an n x n grid and the list of all PipeWithCoords on the grid, where n is greater than every row and col of the PipeWithCoords in the list.
(define GRID-1 (make-grid 5 LOPWC-1))
(define GRID-2 (make-grid 10 LOPWC-2))
(define GRID-3 (make-grid 7 LOPWC-3))

;; grid-templ : Grid -> ?
(define (grid-templ g)
  (...(grid-n g)
  ...(lopwc-templ (grid-lopwc g)...)))
              

(define STARTING-GRID (make-grid 7 empty))

;; pipe=? : Pipe Pipe -> Boolean
;; Are the two pipes equal?
(check-expect (pipe=? PIPE-TB PIPE-TB) #t)
(check-expect (pipe=? PIPE-TB PIPE-TL) #f)
(check-expect (pipe=? PIPE-TBLR PIPE-TB) #f)
               
(define (pipe=? pipe1 pipe2)
  (and (boolean=? (pipe-top pipe1)(pipe-top pipe2))
       (boolean=? (pipe-bot pipe1)(pipe-bot pipe2))
       (boolean=? (pipe-left pipe1)(pipe-left pipe2))
       (boolean=? (pipe-right pipe1)(pipe-right pipe2))))

;; place-pipe: Grid Pipe Integer Integer -> Grid
;; Places the pipe on the grid at the given row and column. We assume that the
;; row and column are valid positions on the grid.                            
(check-expect (place-pipe GRID-1 PIPE-TR 4 4) (make-grid 5 (list PIPE-1 PIPE-2 (make-pipe-w-coords (make-pipe #true #false #false #true) 4 4))))
(check-expect (place-pipe GRID-2 PIPE-TBLR 1 2) (make-grid 10 (list PIPE-3 PIPE-4 PIPE-5 (make-pipe-w-coords (make-pipe #true #true #true #true) 1 2))))
(check-expect (place-pipe GRID-3 PIPE-BL 6 4) (make-grid 7 (list PIPE-3 (make-pipe-w-coords (make-pipe #false #true #true #false) 6 4))))
(check-expect (place-pipe GRID-1 PIPE-TBLR 1 2) (make-grid 5 (list (make-pipe-w-coords PIPE-TBLR 1 2) PIPE-2)))


(define (place-pipe grid pipe row col)
  (make-grid (grid-n grid) (local [;; remove-old-pipe : Pipe -> ListOfPipeWithCoords
                                   ;; removes old pipe from lopwc if there's already a pipe there
                                   (define (remove-old-pipe p)
                                     (cons (make-pipe-w-coords p row col)
                                           (remove (make-pipe-w-coords (pipe-at grid row col) row col) (grid-lopwc grid))))
                                   ;; add-pipe : Grid Pipe Number Number -> Grid
                                   ;; adds the given pipe at the row and col to the given Grid
                                   (define (add-pipe g p r c)
                                       (cond
                                         [(empty? (grid-lopwc g)) (cons (make-pipe-w-coords p r c) empty)]
                                         [(and (cons? (grid-lopwc grid)) (not (pipe=? (make-pipe #f #f #f #f) (pipe-at grid row col))))
                                          (remove-old-pipe p)]
                                         [(cons? (grid-lopwc g))
                                          (cons (first (grid-lopwc g)) (add-pipe (make-grid (grid-n g) (rest (grid-lopwc g))) p r c))]))]
                             (add-pipe grid pipe row col))))



;; pipe-at: Grid Integer Integer -> [Optional Pipe]
;; Produces the pipe at the given row and column, or (make-pipe #f #f #f #f) if that position is
;; is blank. We assume that the row and column are valid positions on the grid.
(check-expect (pipe-at GRID-1 3 0) (make-pipe #f #f #t #t))
(check-expect (pipe-at GRID-2 8 1) (make-pipe #f #t #f #t))
(check-expect (pipe-at GRID-3 0 0) (make-pipe #f #f #f #f))


(define (pipe-at grid row col)
  (cond
    [(empty? (grid-lopwc grid)) (make-pipe #f #f #f #f)]
    [(cons? (grid-lopwc grid)) (cond
                               [(and (= row (pipe-w-coords-row (first (grid-lopwc grid))))
                                     (= col (pipe-w-coords-col (first (grid-lopwc grid)))))
                                (pipe-w-coords-pipe (first (grid-lopwc grid)))]
                               [else (pipe-at (make-grid (grid-n grid) (rest (grid-lopwc grid))) row col)])]))



;; grid->image: Grid Integer Integer GooFlow -> Image
;; Draws the grid of pipes. Every tile should be a square with side length
;; tile-side-length and every pipe should have width pipe-width.
(define (grid->image grid tile-side-length pipe-width gf)
  (local [;; make-empty-row : Number Number -> Image
          ;; draws a row of n tiles with length and width sl.
          (define (make-empty-row n sl)
            (if (= n 1) (square sl "outline" "black") (beside (square sl "outline" "black") (make-empty-row (- n 1) sl))))
          ;; draw-empty-grid : Number Number Number -> Image
          ;; draws a n x also-n grid of tiles with length and width sl. n and also-n must be the same value.
          (define (draw-empty-grid n also-n sl)
            (if (= also-n 1) (make-empty-row n sl) (above (make-empty-row n sl) (draw-empty-grid n (- also-n 1) sl))))        
          ;; grid-img-so-far : Number PipeWithCoords Number Number -> Image
          ;; draws the pipe in the given PipeWithCoords at its row and col on an empty grid of size n x n.
          (define (grid-img-so-far n pwc sl pw)
            (overlay/xy (draw-empty-grid n n sl)
                        (* (pipe-w-coords-col pwc) sl)
                        (* (pipe-w-coords-row pwc) sl)
                        (pipe->image (pipe-w-coords-pipe pwc) sl pw #f "")))
          ;; grid-img : Number ListOfPipeWithCoords Number Number -> Image
          ;; iterates grid-img-so-far over the given ListOfPipeWithCoords
          (define (grid-img n lopwc sl pw)
            (cond
              [(empty? lopwc) (draw-empty-grid n n sl)]
              [(cons? lopwc) (overlay (grid-img n (rest lopwc) sl pw) (grid-img-so-far n (first lopwc) sl pw))]))
          ;; draw-gooflow : Number PlacedPipeWithDirection Number Number -> Image
          ;; draws the grid with the given GooFlow
          (define (draw-gooflow n ppdir sl pw)
            (overlay/xy
             (draw-empty-grid n n sl)
             (* (pipe-w-coords-col (pp/dir-pp ppdir)) sl)
             (* (pipe-w-coords-row (pp/dir-pp ppdir)) sl)
             (pipe->image (pipe-w-coords-pipe (pp/dir-pp ppdir)) sl pw #t (pp/dir-dir ppdir))))
          ;; draw-all-gooflow : Number GooFlow Number Number -> Image
          ;; iterates draw-gooflow over all of gooflow
          (define (draw-all-gooflow n goo sl pw)
            (cond
              [(empty? goo) (draw-empty-grid n n sl)]
              [(cons? goo) (overlay (draw-all-gooflow n (rest goo) sl pw) (draw-gooflow n (first goo) sl pw))]))]
    (overlay (draw-all-gooflow (grid-n grid) gf tile-side-length pipe-width)
             (grid-img (grid-n grid) (grid-lopwc grid) tile-side-length pipe-width))))



(define ALL-PIPES-2 (list PIPE-TL PIPE-TR PIPE-BR PIPE-BL PIPE-TB PIPE-LR PIPE-TBLR
                          PIPE-TL PIPE-TR PIPE-BR PIPE-BL PIPE-TB PIPE-LR PIPE-TBLR
                          PIPE-TL PIPE-TR PIPE-BR PIPE-BL PIPE-TB PIPE-LR PIPE-TBLR
                          PIPE-TL PIPE-TR PIPE-BR PIPE-BL PIPE-TB PIPE-LR PIPE-TBLR
                          PIPE-TL PIPE-TR PIPE-BR PIPE-BL PIPE-TB PIPE-LR PIPE-TBLR
                          PIPE-TL PIPE-TR PIPE-BR PIPE-BL PIPE-TB PIPE-LR PIPE-TBLR
                          PIPE-TL PIPE-TR PIPE-BR PIPE-BL PIPE-TB PIPE-LR PIPE-TBLR
                          PIPE-TL PIPE-TR PIPE-BR PIPE-BL PIPE-TB PIPE-LR PIPE-TBLR))


(define-struct game-state [grid incoming tsl pw sp gf replaced time])
;; A GameState is a (make-game-state Grid [List-of Pipe] Number Number StartingPipe GooFlow)
;; Interpretation: Represents a game-state with fields:
;; grid: The Grid being used for the game.
;; incoming: the list of incoming pipes that appears when a player clicks on a cell.
;; tsl: The length and width of each of the tiles of the grid.
;; pw: The width of the pipes.
;; sp: the starting pipe of the game
;; gf: the gooflow of the game
;; replaced: the number of pipes that were replaced
;; time: ticks until the next goo propagation


;; OLD GAMESTATE DEFINITIONS FROM HW 6
(define GS-1 (make-game-state GRID-1 ALL-PIPES-2 33 11 SP-1 GOOFLOW-1 0 140))
(define GS-2 (make-game-state GRID-2 ALL-PIPES-2 50 15 SP-2 GOOFLOW-2 0 140))
(define GS-3 (make-game-state GRID-3 ALL-PIPES-2 40 10 SP-3 GOOFLOW-3 0 140))


;; game-state-templ : GameState -> ?
(define (game-state-templ gs)
  (...(grid-templ (game-state-grid gs))
      (game-state-incoming gs)...
      (game-state-tsl gs)...
      (game-state-pw gs)...
      (game-state-sp gs)...
      (gooflow-templ (game-state-gf gs)...)
      (game-state-replaced gf)...))


;; gamestate-init : Number Number Number Direction Number Number [List-of Pipe] -> GameState
;; initializes the gamestate given:
;; n : dimensions of grid
;; x : the x-coordinate of the starting pipe
;; y : the y-coordinate of the starting pipe
;; dir: the direction of the starting pipe
;; tsl : the tile-side-length of the game
;; pw : the pipe-width of the game
;; incoming : the list of incoming pipes
(check-expect (gamestate-init 5 2 2 RIGHT 33 11 ALL-PIPES-2)
              (make-game-state (make-grid 5 empty) ALL-PIPES-2 33 11 PIPE-R (list (make-pp/dir (make-pipe-w-coords PIPE-R 2 2) RIGHT)) 0 140))
(check-expect (gamestate-init 7 0 0 DOWN 33 11 ALL-PIPES-2)
              (make-game-state (make-grid 7 empty) ALL-PIPES-2 33 11 PIPE-B (list (make-pp/dir (make-pipe-w-coords PIPE-B 0 0) DOWN)) 0 140))
(check-expect (gamestate-init 10 6 8 LEFT 33 11 ALL-PIPES-2)
              (make-game-state (make-grid 10 empty) ALL-PIPES-2 33 11 PIPE-L (list (make-pp/dir (make-pipe-w-coords PIPE-L 6 8) LEFT)) 0 140))

                               
(define (gamestate-init n x y dir tsl pw incoming)
  (make-game-state
   (make-grid n empty)
   incoming
   tsl
   pw
   (cond
     [(string=? dir UP) PIPE-T]
     [(string=? dir DOWN) PIPE-B]
     [(string=? dir LEFT) PIPE-L]
     [(string=? dir RIGHT) PIPE-R])
   (list (cond
           [(string=? dir UP) (make-pp/dir (make-pipe-w-coords PIPE-T x y) UP)]
           [(string=? dir DOWN) (make-pp/dir (make-pipe-w-coords PIPE-B x y) DOWN)]
           [(string=? dir LEFT) (make-pp/dir (make-pipe-w-coords PIPE-L x y) LEFT)]
           [(string=? dir RIGHT) (make-pp/dir (make-pipe-w-coords PIPE-R x y) RIGHT)]))
   0
   140))


;; NEW GAMESTATE DEFINITIONS FROM HW 8
(define GS-4 (gamestate-init 7 2 2 RIGHT 50 17 ALL-PIPES))
(define GS-5 (gamestate-init 10 0 0 DOWN 50 17 ALL-PIPES-2))

;; place-pipe-on-click : GameState Integer Integer MouseEvent -> GameState
;; If the user clicks on a tile and there are incoming pipes available, places
;; the next incoming pipe on that tile. If no pipes are available, does nothing.
(check-expect (place-pipe-on-click GS-1 10 10 "button-down") (make-game-state (make-grid 5 (list PIPE-1 PIPE-2 (make-pipe-w-coords PIPE-TL 0 0)))
                                                                              (list PIPE-TR PIPE-BR PIPE-BL PIPE-TB PIPE-LR PIPE-TBLR
                          PIPE-TL PIPE-TR PIPE-BR PIPE-BL PIPE-TB PIPE-LR PIPE-TBLR
                          PIPE-TL PIPE-TR PIPE-BR PIPE-BL PIPE-TB PIPE-LR PIPE-TBLR
                          PIPE-TL PIPE-TR PIPE-BR PIPE-BL PIPE-TB PIPE-LR PIPE-TBLR
                          PIPE-TL PIPE-TR PIPE-BR PIPE-BL PIPE-TB PIPE-LR PIPE-TBLR
                          PIPE-TL PIPE-TR PIPE-BR PIPE-BL PIPE-TB PIPE-LR PIPE-TBLR
                          PIPE-TL PIPE-TR PIPE-BR PIPE-BL PIPE-TB PIPE-LR PIPE-TBLR
                          PIPE-TL PIPE-TR PIPE-BR PIPE-BL PIPE-TB PIPE-LR PIPE-TBLR) 33 11 SP-1 GOOFLOW-1 0 140))
(check-expect (place-pipe-on-click GS-2 101 51 "button-down") (make-game-state (make-grid 10 (list PIPE-3 PIPE-4 PIPE-5 (make-pipe-w-coords PIPE-TL 1 2)))
                                                                              (list PIPE-TR PIPE-BR PIPE-BL PIPE-TB PIPE-LR PIPE-TBLR
                          PIPE-TL PIPE-TR PIPE-BR PIPE-BL PIPE-TB PIPE-LR PIPE-TBLR
                          PIPE-TL PIPE-TR PIPE-BR PIPE-BL PIPE-TB PIPE-LR PIPE-TBLR
                          PIPE-TL PIPE-TR PIPE-BR PIPE-BL PIPE-TB PIPE-LR PIPE-TBLR
                          PIPE-TL PIPE-TR PIPE-BR PIPE-BL PIPE-TB PIPE-LR PIPE-TBLR
                          PIPE-TL PIPE-TR PIPE-BR PIPE-BL PIPE-TB PIPE-LR PIPE-TBLR
                          PIPE-TL PIPE-TR PIPE-BR PIPE-BL PIPE-TB PIPE-LR PIPE-TBLR
                          PIPE-TL PIPE-TR PIPE-BR PIPE-BL PIPE-TB PIPE-LR PIPE-TBLR) 50 15 SP-2 GOOFLOW-2 0 140))
(check-expect (place-pipe-on-click GS-3 125 125 "button-down") (make-game-state (make-grid 7 (list PIPE-3 (make-pipe-w-coords PIPE-TL 3 3)))
                                                                              (list PIPE-TR PIPE-BR PIPE-BL PIPE-TB PIPE-LR PIPE-TBLR
                          PIPE-TL PIPE-TR PIPE-BR PIPE-BL PIPE-TB PIPE-LR PIPE-TBLR
                          PIPE-TL PIPE-TR PIPE-BR PIPE-BL PIPE-TB PIPE-LR PIPE-TBLR
                          PIPE-TL PIPE-TR PIPE-BR PIPE-BL PIPE-TB PIPE-LR PIPE-TBLR
                          PIPE-TL PIPE-TR PIPE-BR PIPE-BL PIPE-TB PIPE-LR PIPE-TBLR
                          PIPE-TL PIPE-TR PIPE-BR PIPE-BL PIPE-TB PIPE-LR PIPE-TBLR
                          PIPE-TL PIPE-TR PIPE-BR PIPE-BL PIPE-TB PIPE-LR PIPE-TBLR
                          PIPE-TL PIPE-TR PIPE-BR PIPE-BL PIPE-TB PIPE-LR PIPE-TBLR) 40 10 SP-3 GOOFLOW-3 0 140))
(check-expect (place-pipe-on-click GS-1 170 170 "button-down") GS-1)
(check-expect (place-pipe-on-click (gamestate-init 5 0 0 RIGHT 33 11 empty) 50 50 "button-down")
              (make-game-state (make-grid 5 empty)
                               empty
                               33
                               11
                               PIPE-R
                               (list (make-pp/dir
                                      (make-pipe-w-coords
                                       PIPE-R
                                       0
                                       0)
                                       RIGHT)) 0 140))


;; goo-there? : PipeWithCoordinates GooFlow -> Boolean
;; is there a goo-filled pipe at the given PipeWithCoordinate's coordinates?
(check-expect (goo-there? (make-pipe-w-coords PIPE-TB 2 0) GOOFLOW-1) #t)
(check-expect (goo-there? (make-pipe-w-coords PIPE-TB 2 0) GOOFLOW-2) #f)
(check-expect (goo-there? (make-pipe-w-coords PIPE-TB 2 0) GOOFLOW-3) #f)

(define (goo-there? pwc gf)
  (cond
    [(empty? gf) #f]                           
    [(cons? gf) (if (and (= (pipe-w-coords-row pwc) (pipe-w-coords-row (pp/dir-pp (first gf))))
                         (= (pipe-w-coords-col pwc) (pipe-w-coords-col (pp/dir-pp (first gf)))))
                    #t (goo-there? pwc (rest gf)))]))

(define (place-pipe-on-click gs x y mouse-event)
  (if (and (string=? mouse-event "button-down")
           (< x (* (game-state-tsl gs) (grid-n (game-state-grid gs))))
           (< y (* (game-state-tsl gs) (grid-n (game-state-grid gs)))))
      (cond
        [(empty? (game-state-incoming gs))
         (make-game-state
          (game-state-grid gs)
          empty
          (game-state-tsl gs)
          (game-state-pw gs)
          (game-state-sp gs)
          (grid-goo-propagate (game-state-gf gs) (game-state-grid gs))
          (game-state-replaced gs)
          (game-state-time gs))]
        [(cons? (game-state-incoming gs))
         (if (goo-there? (make-pipe-w-coords (first (game-state-incoming gs))
                                           (floor (/ y (game-state-tsl gs)))
                                           (floor (/ x (game-state-tsl gs))))
                       (game-state-gf gs))
             gs
             (make-game-state (place-pipe
                               (game-state-grid gs)
                               (first (game-state-incoming gs))
                               (floor (/ y (game-state-tsl gs)))
                               (floor (/ x (game-state-tsl gs))))
                              (rest (game-state-incoming gs))
                              (game-state-tsl gs)
                              (game-state-pw gs)
                              (game-state-sp gs)
                              (game-state-gf gs)
                              (cond
                                [(pipe=? (pipe-at (game-state-grid gs) (floor (/ y (game-state-tsl gs))) (floor (/ x (game-state-tsl gs)))) (make-pipe #f #f #f #f))
                                 0]
                                [else (+ 1 (game-state-replaced gs))])
                              (game-state-time gs)))])
      gs))

;; get-score: GameState -> Integer
;; Computes the current score of the game
(check-expect (get-score (make-game-state
                          (make-grid
                           7
                           (list
                            (make-pipe-w-coords
                             (make-pipe #true #true #true #true) 4 3)
                            (make-pipe-w-coords
                             (make-pipe #false #true #true #false) 2 3)
                            (make-pipe-w-coords
                             (make-pipe #true #true #false #false) 3 3)))
                          '()
                          50
                          17
                          (make-pipe #false #false #false #true)
                          (list
                           (make-pp/dir
                            (make-pipe-w-coords
                             (make-pipe #false #false #false #true) 2 2) "right")
                           (make-pp/dir
                            (make-pipe-w-coords (make-pipe #false #true #true #false) 2 3) "down")
                           (make-pp/dir
                            (make-pipe-w-coords
                             (make-pipe #true #true #false #false) 3 3) "down")
                           (make-pp/dir
                            (make-pipe-w-coords
                             (make-pipe #true #true #true #true) 4 3) "down"))
                          1
                          140)) 150)
(check-expect (get-score (make-game-state
                          (make-grid 7
                           (list
                            (make-pipe-w-coords
                             (make-pipe #true #true #true #true) 3 4)
                            (make-pipe-w-coords
                             (make-pipe #false #true #false #true) 1 3)
                            (make-pipe-w-coords
                             (make-pipe #true #false #true #false) 2 3)
                            (make-pipe-w-coords
                             (make-pipe #false #true #true #false) 1 4)
                            (make-pipe-w-coords
                             (make-pipe #true #true #false #false) 2 4)))
                          '()
                          50
                          17
                          (make-pipe #false #false #false #true)
                          (list
                           (make-pp/dir
                            (make-pipe-w-coords
                             (make-pipe #false #false #false #true) 2 2) "right")
                           (make-pp/dir
                            (make-pipe-w-coords
                             (make-pipe #true #false #true #false) 2 3) "up")
                           (make-pp/dir
                            (make-pipe-w-coords
                             (make-pipe #false #true #false #true) 1 3) "right")
                           (make-pp/dir
                            (make-pipe-w-coords
                             (make-pipe #false #true #true #false) 1 4) "down")
                           (make-pp/dir
                            (make-pipe-w-coords
                             (make-pipe #true #true #false #false) 2 4) "down")
                           (make-pp/dir
                            (make-pipe-w-coords
                             (make-pipe #true #true #true #true) 3 4) "down"))
                          1
                          140)) 250)
(check-expect (get-score (make-game-state
                          (make-grid 7
                           (list
                            (make-pipe-w-coords
                             (make-pipe #true #true #true #true) 1 1)
                            (make-pipe-w-coords
                             (make-pipe #false #true #true #false) 1 3)
                            (make-pipe-w-coords
                             (make-pipe #true #false #true #false) 2 3)
                            (make-pipe-w-coords
                             (make-pipe #true #true #false #false) 1 2)))
                          '()
                          50
                          17
                          (make-pipe #false #false #false #true)
                          (list
                           (make-pp/dir
                            (make-pipe-w-coords
                             (make-pipe #false #false #false #true) 2 2) "right")
                           (make-pp/dir
                            (make-pipe-w-coords
                             (make-pipe #true #false #true #false) 2 3) "up")
                           (make-pp/dir
                            (make-pipe-w-coords
                             (make-pipe #false #true #true #false) 1 3) "left"))
                          1
                          140)) 100)


(define (get-score gs)
  (* 50 (- (length (game-state-gf gs)) (game-state-replaced gs))))

;; pipe-fantasy: GameState -> GameState
(define (pipe-fantasy initial-game-state)
  (big-bang initial-game-state
    [to-draw draw-gamestate]
    [on-mouse place-pipe-on-click]
    [on-tick tick]))


;; draw-grid : GameState -> GameState
;; Applies grid->image to the given GameState
(define (draw-grid gs)
  (grid->image (game-state-grid gs) (game-state-tsl gs) (game-state-pw gs) (game-state-gf gs)))


;; draw-gamestate : GameState -> GameState
;; draws the grid and the incoming list of pipes
(define (draw-gamestate gs)
  (local [;; draw-incoming : Number [List-of Pipe] -> Image
          ;; draws the incoming list of pipes
          (define (draw-incoming num lop)
            (if (or (= num 0) (empty? lop)) empty-image
                (above (pipe->image (first lop) (game-state-tsl gs) (game-state-pw gs) #f "")
                       (draw-incoming (- num 1) (rest lop)))))]
  (above (beside (draw-grid gs) (draw-incoming (grid-n (game-state-grid gs)) (game-state-incoming gs)))
         (text (string-append "Score: " (number->string (get-score gs))) 24 "pink"))))

;; tick : GameState -> GameState
;; Automatically propagates goo [grid incoming tsl pw sp gf replaced time]
(define (tick gs)
  (cond
    [(> (game-state-time gs) 0)
     (make-game-state (game-state-grid gs) (game-state-incoming gs) (game-state-tsl gs) (game-state-pw gs) (game-state-sp gs) (game-state-gf gs) (game-state-replaced gs) (- (game-state-time gs) 1))]
    [(zero? (game-state-time gs))
     (make-game-state (game-state-grid gs) (game-state-incoming gs) (game-state-tsl gs) (game-state-pw gs) (game-state-sp gs) (grid-goo-propagate (game-state-gf gs) (game-state-grid gs)) (game-state-replaced gs) 28)]))
    

      
(pipe-fantasy GS-5)


;; we need to fill the whole cross-pipe when the goo revisits it (or do we)

