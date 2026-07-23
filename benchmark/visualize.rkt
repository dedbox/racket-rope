#lang racket/base

;;; visualize.rkt -- renders a benchmark JSON run (as produced by report.rkt's
;;; `save-run!` / main.rkt's `--save`) as a single SVG chart.
;;;
;;;   racket benchmark/visualize.rkt --in results.json --out results.svg
;;;
;;; Design: one horizontal row per (group, name) operation, one dot per input
;;; size it was benchmarked at, every dot placed on one shared log-scale time
;;; axis, rows sorted fastest-to-slowest by geometric mean. The single shared
;;; scale makes both absolute operation speed (dot's absolute x position,
;;; readable off the axis) and relative speed (row's position relative to
;;; every other row) directly legible in the same image. A thin whisker per
;;; dot spans the operation's observed [min, max] sample range, and the
;;; fastest/slowest dot in each row is labeled with its exact duration in an
;;; adaptive unit (ns/us/ms/s) at 3 significant figures.

(require json
         racket/cmdline
         racket/date
         racket/format
         racket/list
         racket/match
         racket/math
         racket/string)

(define (clamp lo v hi) (max lo (min v hi)))
(define (log10 x) (/ (log x) (log 10)))

;; ---------------------------------------------------------------------------
;; Data model
;; ---------------------------------------------------------------------------

(struct point (size mean min max) #:transparent)
(struct series (group name points) #:transparent) ; points sorted by size

;; Groups the flat list of per-(group,name,size) JSON records into one
;; `series` per (group, name), each holding one `point` per size tested.
(define (records->series records)
  (define grouped
    (for/fold ([acc (hash)]) ([r (in-list records)])
      (define key (cons (hash-ref r 'group) (hash-ref r 'name)))
      (define pt (point (hash-ref r 'size) (hash-ref r 'mean) (hash-ref r 'min) (hash-ref r 'max)))
      (hash-update acc key (λ (pts) (cons pt pts)) '())))
  (for/list ([key (in-hash-keys grouped)])
    (series (car key) (cdr key) (sort (hash-ref grouped key) < #:key point-size))))

(struct delta-point (size pct noise-pct) #:transparent)
(struct delta-series (group name points) #:transparent)

;; Only rows present in *both* runs are drawn (NEW/REMOVED belong to
;; report.rkt's textual output, not this chart).
(define (records->delta-series before-records after-records)
  (define (index rs)
    (for/hash ([r (in-list rs)])
      (values (list (hash-ref r 'group) (hash-ref r 'name) (hash-ref r 'size)) r)))
  (define bi (index before-records))
  (define ai (index after-records))
  (define grouped
    (for/fold ([acc (hash)]) ([k (in-list (hash-keys bi))] #:when (hash-has-key? ai k))
      (match-define (list g n sz) k)
      (define b (hash-ref bi k)) (define a (hash-ref ai k))
      (define bm (hash-ref b 'mean)) (define am (hash-ref a 'mean))
      (define pct (* 100.0 (/ (- am bm) (max bm 1e-12))))
      ;; ~2 stddevs on each side, expressed as a % of the baseline mean: a
      ;; delta smaller than this is indistinguishable from noise given what
      ;; these two runs actually measured.
      (define noise-pct
        (* 100.0 (/ (+ (* 2 (hash-ref b 'stddev)) (* 2 (hash-ref a 'stddev))) (max bm 1e-12))))
      (hash-update acc (cons g n) (λ (pts) (cons (delta-point sz pct noise-pct) pts)) '())))
  (for/list ([key (in-hash-keys grouped)])
    (delta-series (car key) (cdr key) (sort (hash-ref grouped key) < #:key delta-point-size))))

;; Geometric mean of a series' per-size means -- appropriate for ranking
;; log-scale data by overall speed, since it weighs each order of magnitude
;; equally rather than letting the largest size dominate the way an arithmetic
;; mean over highly skewed timings would.
(define (series-geomean s)
  (define floored (map (λ (p) (max (point-mean p) 1e-9)) (series-points s)))
  (exp (/ (apply + (map log floored)) (length floored))))

;; The trailing "/"-separated component of a name, e.g. "differ-at-end" from
;; "hash-code.warm/string/differ-at-end", or "append1" from "string/append1".
;; Used as a cross-family, cross-type grouping key.
(define (trailing-keyword name)
  (define parts (string-split name "/"))
  (if (null? parts) name (last parts)))

(define (name<?      a b) (string<? (series-name a) (series-name b)))
(define (group<?     a b) (string<? (series-group a) (series-group b)))
(define (geomean<?   a b) (< (series-geomean a) (series-geomean b)))
(define (keyword<?   a b) (string<? (trailing-keyword (series-name a))
                                    (trailing-keyword (series-name b))))

;; Lexicographic comparator combinator: try each in turn, falling through
;; to the next only on an exact tie. name<? is included last in every
;; mode below so no two distinct series can ever tie completely — that's
;; what makes every non-"speed" mode 100% run-invariant.
(define ((chain . cmps) a b)
  (cond [(null? cmps) #f]
        [((car cmps) a b) #t]
        [((car cmps) b a) #f]
        [else ((apply chain (cdr cmps)) a b)]))

(define (order-series all-series mode)
  (sort all-series
        (case mode
          ;; current default, now tie-broken deterministically
          [(speed) (chain geomean<? group<? name<?)]
          ;; family+type together, alphabetically
          [(alpha) (chain group<? name<?)]
          ;; color blocks fully contiguous, fastest-first within each
          [(group) (chain group<? geomean<? name<?)]
          ;; same trailing keyword clustered across all families/types
          [(scenario) (chain keyword<? group<? name<?)]
          [else
           (raise-user-error
            'visualize
            "unknown --order mode ~a (expected speed, alpha, group, or scenario)" mode)])))

(define (delta-severity s)
  (apply max (map (λ (p) (abs (delta-point-pct p))) (delta-series-points s))))

(define (dname<?     a b) (string<? (delta-series-name a) (delta-series-name b)))
(define (dgroup<?    a b) (string<? (delta-series-group a) (delta-series-group b)))
(define (dseverity<? a b) (< (delta-severity a) (delta-severity b)))
(define (dkeyword<?  a b) (string<? (trailing-keyword (delta-series-name a))
                                    (trailing-keyword (delta-series-name b))))

(define (order-delta-series all-series mode)
  (sort all-series
        (case mode
          [(speed)    (chain dseverity<? dgroup<? dname<?)]  ; biggest |Δ| first
          [(alpha)    (chain dgroup<? dname<?)]
          [(group)    (chain dgroup<? dseverity<? dname<?)]
          [(scenario) (chain dkeyword<? dgroup<? dname<?)]
          [else
           (raise-user-error
            'visualize "unknown --order mode ~a (expected speed, alpha, group, or scenario)" mode)])))

;; ---------------------------------------------------------------------------
;; Formatting
;; ---------------------------------------------------------------------------

;; Renders a positive real with exactly 3 significant figures.
(define (sig3 v)
  (if (<= v 0)
      "0"
      (let* ([expo (inexact->exact (floor (log10 v)))]
             [decimals (max 0 (- 2 expo))])
        (~r v #:precision (list '= decimals)))))

;; Adaptive-unit rendering of a millisecond duration: 1.07e-5 -> "10.7ns",
;; 0.067 -> "67.0us", 506.3 -> "506ms", 2531.0 -> "2.53s".
(define (format-ms t)
  (define t* (max t 0.0))
  (cond
    [(< t* 1e-3)   (string-append (sig3 (* t* 1e6)) "ns")]
    [(< t* 1.0)    (string-append (sig3 (* t* 1e3)) "us")]
    [(< t* 1000.0) (string-append (sig3 t*) "ms")]
    [else          (string-append (sig3 (/ t* 1000.0)) "s")]))

;; ---------------------------------------------------------------------------
;; Color
;; ---------------------------------------------------------------------------

(define (byte->hex n) (~r n #:base 16 #:min-width 2 #:pad-string "0"))

;; h in [0,360), s and l in [0,1]. Standard HSL -> RGB conversion, done here
;; (rather than emitting `hsl(...)` directly) so the output is plain #rrggbb
;; and renders identically in every SVG consumer, not just browsers.
(define (hsl->hex h s l)
  (define c (* (- 1 (abs (- (* 2 l) 1))) s))
  (define hp (/ h 60.0))
  (define hp-mod2 (- hp (* 2 (floor (/ hp 2)))))
  (define x (* c (- 1 (abs (- hp-mod2 1)))))
  (define-values (r1 g1 b1)
    (cond [(< hp 1) (values c x 0)]
          [(< hp 2) (values x c 0)]
          [(< hp 3) (values 0 c x)]
          [(< hp 4) (values 0 x c)]
          [(< hp 5) (values x 0 c)]
          [else     (values c 0 x)]))
  (define m (- l (/ c 2)))
  (define (chan v) (byte->hex (inexact->exact (round (* 255 (+ v m))))))
  (string-append "#" (chan r1) (chan g1) (chan b1)))

(define (palette n)
  (for/list ([i (in-range n)])
    (hsl->hex (* i (/ 360.0 (max n 1))) 0.62 0.46)))

;; ---------------------------------------------------------------------------
;; SVG primitives
;; ---------------------------------------------------------------------------

(define (n2 v) (~r v #:precision 2))
(define (n0 v) (~r v #:precision 0))

(define (xml-escape s)
  (define s1 (string-replace s "&" "&amp;"))
  (define s2 (string-replace s1 "<" "&lt;"))
  (string-replace s2 ">" "&gt;"))

(define (svg-line x1 y1 x2 y2 #:stroke stroke #:width [width 1] #:opacity [opacity 1.0])
  (format "<line x1=\"~a\" y1=\"~a\" x2=\"~a\" y2=\"~a\" stroke=\"~a\" stroke-width=\"~a\" stroke-opacity=\"~a\" />\n"
          (n2 x1) (n2 y1) (n2 x2) (n2 y2) stroke (n2 width) (n2 opacity)))

(define (svg-circle cx cy r #:fill fill #:opacity [opacity 1.0])
  (format "<circle cx=\"~a\" cy=\"~a\" r=\"~a\" fill=\"~a\" fill-opacity=\"~a\" />\n"
          (n2 cx) (n2 cy) (n2 r) fill (n2 opacity)))

(define (svg-rect x y w h #:fill fill #:opacity [opacity 1.0])
  (format "<rect x=\"~a\" y=\"~a\" width=\"~a\" height=\"~a\" fill=\"~a\" fill-opacity=\"~a\" />\n"
          (n2 x) (n2 y) (n2 w) (n2 h) fill (n2 opacity)))

(define (svg-frame x y w h #:stroke stroke #:width [width 1])
  (format "<rect x=\"~a\" y=\"~a\" width=\"~a\" height=\"~a\" fill=\"none\" stroke=\"~a\" stroke-width=\"~a\" />\n"
          (n2 x) (n2 y) (n2 w) (n2 h) stroke (n2 width)))

(define (svg-text x y str #:size [size 12] #:anchor [anchor "start"]
                  #:fill [fill "#1a1a1a"] #:weight [weight "normal"])
  (format "<text x=\"~a\" y=\"~a\" font-size=\"~a\" text-anchor=\"~a\" fill=\"~a\" font-weight=\"~a\" font-family=\"Helvetica, Arial, sans-serif\">~a</text>\n"
          (n2 x) (n2 y) size anchor fill weight (xml-escape str)))

;; ---------------------------------------------------------------------------
;; Layout & rendering
;; ---------------------------------------------------------------------------

(define (render-svg run-jsexpr
                    #:canvas-width [canvas-width 1400]
                    #:row-height [row-height 20]
                    #:order [order 'speed])
  (define records (hash-ref run-jsexpr 'records))
  (when (null? records) (error 'visualize "no records in this run"))

  (define all-series (order-series (records->series records) order))
  (define n-rows (length all-series))
  (define groups (remove-duplicates (map series-group all-series)))
  (define colors
    (for/hash ([g (in-list groups)] [c (in-list (palette (length groups)))]) (values g c)))

  ;; global log-scale time domain, padded a third of a decade on each side so
  ;; the extreme dots and their labels don't clip the plot edge
  (define all-times
    (apply append
           (for/list ([s (in-list all-series)])
             (apply append
                    (for/list ([p (in-list (series-points s))])
                      (list (max (point-mean p) 1e-9)
                            (max (point-min p) 1e-9)
                            (max (point-max p) 1e-9)))))))
  (define dom-lo (inexact->exact (floor (log10 (apply min all-times)))))
  (define dom-hi (inexact->exact (ceiling (log10 (apply max all-times)))))
  (define lo (- dom-lo 0.3))
  (define hi (+ dom-hi 0.3))

  ;; layout constants
  (define label-w
    (clamp 220 (+ 40 (* 6.4 (apply max (map (λ (s) (string-length (series-name s))) all-series)))) 560))
  (define label-x0 16)
  (define plot-x0 (+ label-x0 14 label-w))
  (define right-margin 90)
  (define plot-x1 (- canvas-width right-margin))
  (define plot-w (- plot-x1 plot-x0))
  (define items-per-legend-row (max 1 (quotient (exact-round plot-w) 160)))
  (define legend-rows (ceiling (/ (length groups) items-per-legend-row)))
  (define title-h 40)
  (define legend-h (+ (* legend-rows 18) 8))
  (define top-margin (+ title-h legend-h 14))
  (define bottom-margin 46)
  (define plot-h (* n-rows row-height))
  (define canvas-height (+ top-margin plot-h bottom-margin))

  (define (xpos t) (+ plot-x0 (* (/ (- (log10 (max t 1e-9)) lo) (- hi lo)) plot-w)))

  ;; -- rows: one per operation, one dot per size, whisker = observed [min,max]
  (define row-elems
    (for/list ([s (in-list all-series)] [i (in-range n-rows)])
      (define row-y (+ top-margin (* i row-height)))
      (define cy (+ row-y (/ row-height 2)))
      (define color (hash-ref colors (series-group s)))
      (define pts (series-points s))
      (define zebra
        (if (even? i) (svg-rect plot-x0 row-y plot-w row-height #:fill "#f4f5f7") ""))
      (define whiskers
        (apply string-append
               (for/list ([p (in-list pts)])
                 (svg-line (xpos (point-min p)) cy (xpos (point-max p)) cy
                           #:stroke color #:width 1 #:opacity 0.30))))
      (define connector
        (if (> (length pts) 1)
            (apply string-append
                   (for/list ([a (in-list pts)] [b (in-list (cdr pts))])
                     (svg-line (xpos (point-mean a)) cy (xpos (point-mean b)) cy
                               #:stroke color #:width 1.2 #:opacity 0.55)))
            ""))
      (define dots
        (apply string-append
               (for/list ([p (in-list pts)])
                 (svg-circle (xpos (point-mean p)) cy
                             (clamp 2.5 (+ 2.5 (* 1.3 (log10 (add1 (point-size p))))) 8)
                             #:fill color #:opacity 0.92))))
      (define swatch (svg-rect label-x0 (- cy 5) 10 10 #:fill color))
      (define label (svg-text (- plot-x0 12) (+ cy 4) (series-name s) #:size 11 #:anchor "end"))
      (define fastest (argmin point-mean pts))
      (define slowest (argmax point-mean pts))
      (define fastest-label
        (svg-text (- (xpos (point-mean fastest)) 6) (+ cy -6) (format-ms (point-mean fastest))
                  #:size 9 #:anchor "end" #:fill "#555555"))
      (define slowest-label
        (if (eq? fastest slowest)
            ""
            (svg-text (+ (xpos (point-mean slowest)) 6) (+ cy -6) (format-ms (point-mean slowest))
                      #:size 9 #:anchor "start" #:fill "#555555")))
      (apply string-append (list zebra whiskers connector dots swatch label fastest-label slowest-label))))

  ;; -- x-axis: one gridline + adaptive-unit label per decade
  (define axis-elems
    (append
     (for/list ([e (in-range dom-lo (add1 dom-hi))])
       (define t (expt 10.0 e))
       (define x (xpos t))
       (string-append
        (svg-line x top-margin x (+ top-margin plot-h) #:stroke "#d8dadd" #:width 1)
        (svg-text x (+ top-margin plot-h 18) (format-ms t) #:size 10 #:anchor "middle" #:fill "#555555")))
     (list (svg-line plot-x0 (+ top-margin plot-h) plot-x1 (+ top-margin plot-h) #:stroke "#9aa0a6" #:width 1.2)
           (svg-text (/ (+ plot-x0 plot-x1) 2) (+ top-margin plot-h 34) "mean time per call (log scale)"
                     #:size 10 #:anchor "middle" #:fill "#555555"))))

  ;; -- legend: one swatch + label per group, wrapped to fit the plot width
  (define legend-elems
    (for/list ([g (in-list groups)] [i (in-naturals)])
      (define col (remainder i items-per-legend-row))
      (define row (quotient i items-per-legend-row))
      (define lx (+ plot-x0 (* col 160)))
      (define ly (+ title-h 6 (* row 18)))
      (string-append (svg-rect lx ly 11 11 #:fill (hash-ref colors g))
                     (svg-text (+ lx 16) (+ ly 10) g #:size 10 #:anchor "start" #:fill "#333333"))))

  ;; -- title block
  (define run-label (hash-ref run-jsexpr 'label "run"))
  (define run-ts (hash-ref run-jsexpr 'timestamp #f))
  (define run-rv (hash-ref run-jsexpr 'racket-version "?"))
  (define ts-str (if run-ts (date->string (seconds->date run-ts) #t) "unknown time"))
  (define title-elems
    (list
     (svg-text 16 24 (format "Rope Benchmark Results - ~a" run-label) #:size 17 #:anchor "start" #:weight "bold")
     (svg-text 16 40 (format "~a records, ~a operations - Racket ~a - ~a"
                             (length records) n-rows run-rv ts-str)
               #:size 11 #:anchor "start" #:fill "#555555")))

  (define fmt
    "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"~a\" height=\"~a\" viewBox=\"0 0 ~a ~a\">\n")
  (apply string-append
         (list (format fmt (n0 canvas-width) (n0 canvas-height) (n0 canvas-width) (n0 canvas-height))
               (svg-rect 0 0 canvas-width canvas-height #:fill "#ffffff")
               (apply string-append title-elems)
               (apply string-append legend-elems)
               (apply string-append axis-elems)
               (apply string-append row-elems)
               (svg-frame plot-x0 top-margin plot-w plot-h #:stroke "#c7cacf")
               "</svg>\n")))

(define (format-pct p)
  (string-append (if (>= p 0) "+" "") (~r p #:precision (list '= 1)) "%"))

(define (delta-color pct threshold noise)
  (cond [(and (> pct 0) (> pct threshold) (> pct noise)) "#c0392b"]   ; regression
        [(and (< pct 0) (< pct (- threshold)) (< pct (- noise))) "#1e8449"] ; improvement
        [else "#9aa0a6"]))                                            ; not significant

(define (render-delta-svg after-jsexpr before-jsexpr
                          #:canvas-width [canvas-width 1400]
                          #:row-height [row-height 20]
                          #:order [order 'scenario]
                          #:threshold [threshold 10])
  (define after-records (hash-ref after-jsexpr 'records))
  (define before-records (hash-ref before-jsexpr 'records))
  (define all-series (order-delta-series (records->delta-series before-records after-records) order))
  (when (null? all-series) (error 'visualize "no (group,name,size) rows in common between the two runs"))
  (define n-rows (length all-series))
  (define groups (remove-duplicates (map delta-series-group all-series)))
  (define colors
    (for/hash ([g (in-list groups)] [c (in-list (palette (length groups)))]) (values g c)))

  ;; linear domain, symmetric around 0%, padded 15%
  (define all-mags
    (append* (for/list ([s (in-list all-series)])
               (for/list ([p (in-list (delta-series-points s))])
                 (max (abs (delta-point-pct p)) (delta-point-noise-pct p))))))
  (define span (* 1.15 (apply max 1.0 all-mags)))

  (define label-w
    (clamp 220 (+ 40 (* 6.4 (apply max (map (λ (s) (string-length (delta-series-name s))) all-series)))) 560))
  (define label-x0 16)
  (define plot-x0 (+ label-x0 14 label-w))
  (define right-margin 90)
  (define plot-x1 (- canvas-width right-margin))
  (define plot-w (- plot-x1 plot-x0))
  (define items-per-legend-row (max 1 (quotient (exact-round plot-w) 160)))
  (define legend-rows (ceiling (/ (length groups) items-per-legend-row)))
  (define title-h 40)
  (define legend-h (+ (* legend-rows 18) 8))
  (define top-margin (+ title-h legend-h 14))
  (define bottom-margin 46)
  (define plot-h (* n-rows row-height))
  (define canvas-height (+ top-margin plot-h bottom-margin))

  (define (xpos pct) (+ plot-x0 (* (/ (+ pct span) (* 2 span)) plot-w)))

  (define row-elems
    (for/list ([s (in-list all-series)] [i (in-range n-rows)])
      (define row-y (+ top-margin (* i row-height)))
      (define cy (+ row-y (/ row-height 2)))
      (define color (hash-ref colors (delta-series-group s)))
      (define pts (delta-series-points s))
      (define zebra
        (if (even? i) (svg-rect plot-x0 row-y plot-w row-height #:fill "#f4f5f7") ""))
      (define noise-ticks
        (apply string-append
               (for/list ([p (in-list pts)])
                 (svg-line (xpos (- (delta-point-pct p) (delta-point-noise-pct p))) cy
                           (xpos (+ (delta-point-pct p) (delta-point-noise-pct p))) cy
                           #:stroke "#9aa0a6" #:width 1 #:opacity 0.35))))
      (define connector
        (if (> (length pts) 1)
            (apply string-append
                   (for/list ([a (in-list pts)] [b (in-list (cdr pts))])
                     (svg-line (xpos (delta-point-pct a)) cy (xpos (delta-point-pct b)) cy
                               #:stroke color #:width 1.2 #:opacity 0.45)))
            ""))
      (define dots
        (apply string-append
               (for/list ([p (in-list pts)])
                 (svg-circle (xpos (delta-point-pct p)) cy
                             (clamp 2.5 (+ 2.5 (* 1.3 (log10 (add1 (delta-point-size p))))) 8)
                             #:fill (delta-color (delta-point-pct p) threshold (delta-point-noise-pct p))
                             #:opacity 0.92))))
      (define swatch (svg-rect label-x0 (- cy 5) 10 10 #:fill color))
      (define label (svg-text (- plot-x0 12) (+ cy 4) (delta-series-name s) #:size 11 #:anchor "end"))
      (define worst (argmax (λ (p) (abs (delta-point-pct p))) pts))
      (define worst-label
        (svg-text (+ (xpos (delta-point-pct worst))
                     (if (>= (delta-point-pct worst) 0) 6 -6))
                  (+ cy -6) (format-pct (delta-point-pct worst))
                  #:size 9 #:anchor (if (>= (delta-point-pct worst) 0) "start" "end") #:fill "#555555"))
      (apply string-append (list zebra noise-ticks connector dots swatch label worst-label))))

  (define axis-elems
    (append
     (for/list ([frac (in-list '(-1.0 -0.5 0.0 0.5 1.0))])
       (define pct (* frac span))
       (define x (xpos pct))
       (string-append
        (svg-line x top-margin x (+ top-margin plot-h)
                  #:stroke (if (zero? frac) "#9aa0a6" "#d8dadd") #:width (if (zero? frac) 1.4 1))
        (svg-text x (+ top-margin plot-h 18) (format-pct pct) #:size 10 #:anchor "middle" #:fill "#555555")))
     (list (svg-line plot-x0 (+ top-margin plot-h) plot-x1 (+ top-margin plot-h) #:stroke "#9aa0a6" #:width 1.2)
           (svg-text (/ (+ plot-x0 plot-x1) 2) (+ top-margin plot-h 34)
                     "% change in mean time (after vs before); gray = not significant"
                     #:size 10 #:anchor "middle" #:fill "#555555"))))

  (define legend-elems
    (for/list ([g (in-list groups)] [i (in-naturals)])
      (define col (remainder i items-per-legend-row))
      (define row (quotient i items-per-legend-row))
      (define lx (+ plot-x0 (* col 160)))
      (define ly (+ title-h 6 (* row 18)))
      (string-append (svg-rect lx ly 11 11 #:fill (hash-ref colors g))
                     (svg-text (+ lx 16) (+ ly 10) g #:size 10 #:anchor "start" #:fill "#333333"))))

  (define after-label (hash-ref after-jsexpr 'label "after"))
  (define before-label (hash-ref before-jsexpr 'label "before"))
  (define title-elems
    (list
     (svg-text 16 24 (format "Rope Benchmark Delta - ~a vs ~a" after-label before-label)
               #:size 17 #:anchor "start" #:weight "bold")
     (svg-text 16 40 (format "~a rows in common - threshold ±~a% - gray band = within noise"
                             n-rows threshold)
               #:size 11 #:anchor "start" #:fill "#555555")))

  (define fmt
    "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"~a\" height=\"~a\" viewBox=\"0 0 ~a ~a\">\n")
  (apply string-append
         (list (format fmt (n0 canvas-width) (n0 canvas-height) (n0 canvas-width) (n0 canvas-height))
               (svg-rect 0 0 canvas-width canvas-height #:fill "#ffffff")
               (apply string-append title-elems)
               (apply string-append legend-elems)
               (apply string-append axis-elems)
               (apply string-append row-elems)
               (svg-frame plot-x0 top-margin plot-w plot-h #:stroke "#c7cacf")
               "</svg>\n")))

;; ---------------------------------------------------------------------------
;; CLI
;; ---------------------------------------------------------------------------

(define (default-out-path in-path-str)
  (if (string-suffix? in-path-str ".json")
      (string-append (substring in-path-str 0 (- (string-length in-path-str) 5)) ".svg")
      (string-append in-path-str ".svg")))

(module+ main
  (define in-path #f)
  (define out-path #f)
  (define baseline-path #f)
  (define canvas-width 1400)
  (define row-height 20)
  (define order-mode 'speed)
  (define threshold 10)

  (command-line
   #:program "rope-bench-visualize"
   #:once-each
   [("--in")
    path "Input benchmark JSON file (from main.rkt --save)"
    (set! in-path path)]
   [("--out")
    path "Output SVG path (default: <in>.svg)"
    (set! out-path path)]
   [("--baseline")
    path "Second JSON file; switches to delta-vs-baseline mode"
    (set! baseline-path path)]
   [("--order")
    mode "Row ordering: speed (default), alpha, group, or scenario"
    (set! order-mode (string->symbol mode))]
   [("--threshold")
    pct "Percent-change floor for coloring a delta significant (default 10)"
    (set! threshold (string->number pct))]
   [("--width")
    w "Canvas width in px (default: 1400)"
    (set! canvas-width (string->number w))]
   [("--row-height")
    h "Pixels per operation row (default: 20)"
    (set! row-height (string->number h))])

  (unless in-path (error 'visualize "missing required --in PATH"))
  (define run-jsexpr (call-with-input-file in-path read-json))
  (define svg
    (if baseline-path
        (render-delta-svg run-jsexpr (call-with-input-file baseline-path read-json)
                          #:canvas-width canvas-width
                          #:row-height row-height
                          #:order order-mode
                          #:threshold threshold)
        (render-svg run-jsexpr
                    #:canvas-width canvas-width
                    #:row-height row-height
                    #:order order-mode)))

  (define final-out
    (or out-path
        (default-out-path (if baseline-path (string-append in-path ".delta.svg") in-path))))
  (call-with-output-file final-out #:exists 'replace (λ (out) (display svg out)))
  (printf "Wrote ~a\n" final-out))
