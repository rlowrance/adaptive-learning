#lang racket
;;;; main program for terminal-based spaced repetition (tsr)
;;;; copyright 2024 (c) Roy E Lowrance
;;;; license: MIT

(require gregor)
(require gregor/period)

;;; For now, the stored items are simple (no ID, no version)
(struct file-item (prompt response))
(struct file-history (item last-attempt-datetime current-delay-seconds))

(struct item (content learning-state delay last-attempt) #:transparent)

;; Returns the datetime of the next item for an item
(define (next-attempt item)
  (+period (item-last-attempt) (seconds (item-delay item))))
  
;; Return next learning state symbol and next delay (in seconds)
(define (next-state-delay current-learning-state current-delay user-rating)
  (printf "next-state-delay: ~v ~v ~v\n" current-learning-state current-delay user-rating)
  (define 1-minute 60)  ; seconds in time period
  (define 10-minutes (* 10 1-minute))
  (define 1-hour (* 60 1-minute))
  (define 1-day (* 24 1-hour))
  (define 4-days (* 4 1-day))
  (match (list current-learning-state user-rating)
    ('(learning1 again) (list 'learning1 1-minute))
    ('(learning1 hard) (list 'learning1 10-minutes))
    ('(learning1 good) (list 'learning1 1-day)) ; 1 day
    ('(learning1 easy) (list 'reviewing 4-days))
    ('(learning2 again) (list 'learning1 1-minute))
    ('(learning2 hard) (list 'learning2 10-minutes))
    ('(learning2 good) (list 'reviewing 1-day)) ;; Anki uses 4-days
    ('(learning2 easy) (list 'reviewing 4-days))
    ('(reviewing again) (list 'learning1 1-minute))
    ('(reviewing hard) (list 'reviewing (* 1.2 current-delay)))
    ('(reviewing good) (list 'reviewing (* 2.5 current-delay)))
    ('(reviewing easy) (list 'reviewing (* 1.3 2.5 current-delay)))
    (_ (error "match failed" current-learning-state user-rating))))

;; Returns a hash with key = item, value = next attempt datetime
(define (next-attempt-datetime items histories (sofar (hash)))
  (cond
    ((null? histories) (upsert-new-items items sofar))
    (#t (define history (car file-history))
        (define item (file-history-item history))
        (define last-attempt (file-history-last-attempt-datetime history))
        (define current-delay (file-histor-current-delay-seconds history))
        (define new-next-attempt-datetime (+period last-attempt (seconds current-delay)))
        (cond ((hash-has-key? sofar item) (define current-delay (hash-ref sofar item))
                                          (cond ((datetime<
        (next-attemt-datetime (cdr items) (hash-set sofar item (datetime<? 
(define (main)
  (define items (file->list "items.rkt"))
  (define histories (file->list "history.rkt"))
 )
  
(file->list "items.rkt")


;; ----------------------------------
;; Unit tests
;; ordered so that if A calls B, B is tested before A

(require racket/trace)

(require rackunit)
(require rackunit/text-ui)

;; Define test data


;; Tests
(trace next-state-delay)
(test-case
 "update-state"
 (define tests (list '((learning1 0 again) . (learning1 60))
                     `((reviewing 100 easy) . (reviewing ,(* 1.3 2.5 100)))))
 (for ((test tests))
   (define args (car test))
   (define expected (cdr test))
   (check-equal? (next-state-delay (first args) (second args) (third args)) expected)))

