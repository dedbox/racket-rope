#lang racket/base

(require racket/contract
         racket/match
         racket/port
         rope/define-rope-type
         rope/rope)

(provide
 in-bytes-rope
 (contract-out
  (struct bytes-rope-leaf  ([count exact-nonnegative-integer?]
                            [width exact-nonnegative-integer?]
                            [raw   bytes?]))
  (struct bytes-rope-node  ([count exact-nonnegative-integer?]
                            [width exact-nonnegative-integer?]
                            [left  bytes-rope?]
                            [right bytes-rope?]))
  [bytes-rope?             (any/c . -> . boolean?)]
  [bytes-raw?              (any/c . -> . boolean?)]
  [bytes-raw-limit         (-> exact-nonnegative-integer?)]
  [bytes-raw-empty         (-> bytes?)]
  [bytes-raw-count         (bytes? . -> . exact-nonnegative-integer?)]
  [bytes-raw-width         (bytes? . -> . exact-nonnegative-integer?)]
  [bytes-raw-slice         (bytes? exact-nonnegative-integer?
                                   exact-nonnegative-integer? . -> . bytes?)]
  [bytes-raw-ref           (bytes? exact-nonnegative-integer? . -> . byte?)]
  [bytes-raw-append        (bytes? ... . -> . bytes?)]
  [make-bytes-rope-leaf    (bytes? . -> . bytes-rope-leaf?)]
  [empty-bytes-rope        bytes-rope?]
  [bytes-rope-append1      (bytes-rope? bytes-rope? . -> . bytes-rope?)]
  [bytes-rope-append       (bytes-rope? ... . -> . bytes-rope?)]
  [bytes-rope-split        (bytes-rope? exact-nonnegative-integer? . -> .
                                        (values bytes-rope? bytes-rope?))]
  [bytes-rope-offset-index (bytes-rope? exact-nonnegative-integer? . -> . exact-nonnegative-integer?)]
  [bytes-rope-splice       (bytes-rope? exact-nonnegative-integer?
                                        exact-nonnegative-integer? bytes? . -> . bytes-rope?)]
  [bytes-rope-slice        (bytes-rope? exact-nonnegative-integer?
                                        exact-nonnegative-integer? . -> . bytes-rope?)]
  [bytes->rope             (bytes? . -> . bytes-rope?)]
  [rope->bytes             (bytes-rope? . -> . bytes?)]
  [bytes-cursor-at-end?    (cursor? . -> . boolean?)]
  [bytes-cursor-peek       (cursor? . -> . byte?)]
  [bytes-cursor-advance    (cursor? . -> . cursor?)]
  [bytes-cursor-drop       (cursor? exact-nonnegative-integer? . -> . cursor?)]
  [bytes-rope->cursor      (bytes-rope? . -> . cursor?)]
  [cursor->bytes-rope      (cursor? . -> . bytes-rope?)]
  [bytes-rope-foldl        (procedure? any/c bytes-rope? bytes-rope? ... . -> . any/c)]
  [bytes-rope-foldr        (procedure? any/c bytes-rope? bytes-rope? ... . -> . any/c)]
  [open-input-bytes-rope   (bytes-rope? . -> . input-port?)]))

(define-rope-type bytes
  bytes?
  (λ () 512)
  (λ () #"")
  bytes-length
  bytes-length
  subbytes
  (λ (raws) (apply bytes-append raws))
  bytes-ref)

(define bytes-limit (bytes-raw-limit))
(define empty-bytes-rope (make-empty-bytes-rope))

(define (bytes->rope text) (bytes-raw->bytes-rope text))
(define (rope->bytes rope) (bytes-rope->bytes-raw rope))

;; Per-read complexity: O(k), where k is the number of bytes transferred in that call.
;; 
;; Total complexity for reading all data: O(n * m), where m is the average number of bytes per
;; character.
;;
;; Given that m is typically a small constant (e.g., UTF-8 characters are 1 - 4 bytes), the per-call
;; complexity can be considered O(1), and the total compexity can be considered O(n).
(define (open-input-bytes-rope rope)
  (define active-cursor (box (bytes-rope->cursor rope)))
  (define pending-bytes (box #""))

  (define (read-in bstr)
    (when (and (zero? (bytes-length (unbox pending-bytes)))
               (not (bytes-cursor-at-end? (unbox active-cursor))))
      (define cur (unbox active-cursor))
      (define char (bytes-cursor-peek cur))
      (set-box! pending-bytes (bytes char))
      (set-box! active-cursor (bytes-cursor-advance cur)))

    (match (unbox pending-bytes)
      [#"" eof]
      [current-buffer
       (let* ([req-len (bytes-length bstr)]
              [buf-len (bytes-length current-buffer)]
              [transfer-len (min req-len buf-len)]
              [consumed (subbytes current-buffer 0 transfer-len)]
              [retained (subbytes current-buffer transfer-len)])
         (bytes-copy! bstr 0 consumed)
         (set-box! pending-bytes retained)
         transfer-len)]))

  (define (close)
    (set-box! active-cursor #f)
    (set-box! pending-bytes #""))

  (make-input-port/read-to-peek
   'bytes-rope-input-port
   read-in
   #f
   close))
