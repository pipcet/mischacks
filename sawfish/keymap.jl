(define (current-stime)
  (if (boundp 'current-ntime)
      (/ (current-ntime) 1000000000)
      (/ (current-utime) 1000000)))

;; areas are specified as ((x0 . x1) . (y0 . y1))

(define (interval-union i1 i2)
  (let* ((p0 (min (car i1) (car i2)))
         (p1 (max (cdr i1) (cdr i2))))))

(define (area-union a1 a2)
  (let* ((x0 (min (caar a1) (caar a2)))
         (y0 (min (cadr a1) (cadr a2)))
         (x1 (max (cdar a1) (cdar a2)))
         (x2 (max (cddr a1) (cddr a2))))
    (cons (cons x0 x1)
          (cons y0 y1))))

(define (area-minus a1 a2)
  (let* ((x0 (min (caar a1) (caar a2)))
         (y0 (min (cadr a1) (cadr a2)))
         (x1 (max (cdar a1) (cdar a2)))
         (x2 (max (cddr a1) (cddr a2))))
    (cons (cons x0 x1)
          (cons y0 y1))))

(define (numpad-move w area)
  (let* ((scr (screen-dimensions))
         (ax (* (car scr) (caar area)))
         (aw (* (car scr) (- (cdar area) (caar area))))
         (ay (* (cdr scr) (cadr area)))
         (ah (* (cdr scr) (- (cddr area) (cadr area)))))
    (move-resize-window-and-frame-to w ax ay aw ah)))

(define numpad-tag-areas
   '(("1" ((0/2 . 2/2) . (1/2 . 2/2)))
     ("3" ((1/2 . 2/2) . (1/2 . 2/2)))
     ("7" ((0/2 . 1/2) . (0/2 . 1/2)))
     ("9" ((1/2 . 1/2) . (0/2 . 1/2)))

     ("2" ((1/2 . 1/2) . (1/2 . 2/2)))
     ("8" ((0/2 . 2/2) . (0/2 . 1/2)))
     ("4" ((0/2 . 1/2) . (0/2 . 2/2)))
     ("6" ((1/2 . 2/2) . (0/2 . 2/2)))

     ("5" ((0 . 1) . (0 . 1)))

     ("55" ((1/3 . 2/3) . (0/2 . 1)))
     ("52" ((1/3 . 2/3) . (1/2 . 1)))
     ("58" ((1/3 . 2/3) . (0 . 1/2)))
     ("54" ((0/3 . 1/3) . (0 . 1)))
     ("56" ((2/3 . 3/3) . (0 . 1)))

     ("51" ((0/3 . 1/3) . (1/2 . 1)))
     ("53" ((2/3 . 3/3) . (1/2 . 1)))
     ("57" ((0/3 . 1/3) . (0 . 1/2)))
     ("59" ((2/3 . 3/3) . (0 . 1/2)))))

(define (numpad-valid-tag-p tag)
  (not (null (assoc tag numpad-tag-areas))))

(define numpad-mode 'select)
(define numpad-mode-timeout 0)

(define (numpad-set-mode mode)
  (setq numpad-mode mode)
  (setq numpad-mode-timeout (+ 3/1 (current-stime))))

(define (numpad-get-mode)
  (if (< (current-stime) numpad-mode-timeout)
      numpad-mode
    'select))

(define numpad-old-tag ())
(define numpad-new-tag "")

(define (numpad-act w tag)
  (let* ((oldtag numpad-old-tag)
         (mode (numpad-get-mode))
         (area (cadr (assoc tag numpad-tag-areas)))
         (oldarea (cadr (assoc oldtag numpad-tag-areas))))
    (cond
     ((eq mode 'move)
      (numpad-move w area))
     ((eq mode 'grow)
      (numpad-move w (area-union area oldarea)))
     ((eq mode 'shrink)
      (numpad-move w (area-minus oldarea area)))
     ((eq mode 'select)
      (let* ((ws (cdr (assoc tag numpad-current-tags)))
             (nw ws))
        (when (eq (car ws) w)
          (setq ws (append (cdr ws) (list w)))
          (setcdr (assoc tag numpad-current-tags) ws))
        (when ws
          (popup-window (car ws))))))))

(define numpad-current-tags ())

(define (numpad-untag w tag)
  (let ((pair (assoc tag numpad-current-tags)))
    (unless (null pair)
      (setcdr pair (filter (lambda (w2) (not (eq w w2))) (cdr pair))))))

(define (numpad-tag w tag)
  (numpad-untag w (window-get w 'numpad-tag))
  (window-put w 'numpad-tag tag)
  (unless (assoc tag numpad-current-tags)
    (setq numpad-current-tags (cons (cons tag ()) numpad-current-tags)))
  (let ((pair (assoc tag numpad-current-tags)))
    (setcdr pair (cons w (cdr pair)))))

(define (numpad-tick)
  (when (>= (current-stime) numpad-mode-timeout)
    (numpad-clear)))

(define (numpad-clear)
  (setq numpad-mode 'select)
  (setq numpad-old-tag ())
  (setq numpad-new-tag ""))

(define (numpad-append w tag)
  (numpad-tick)
  (let ((new-tag (concat numpad-new-tag tag)))
    (when (numpad-valid-tag-p new-tag)
      (setq numpad-new-tag new-tag)
      (numpad-act w new-tag)
      (numpad-tag w new-tag))))


(define (numpad-bind-mode mode)
  (lambda (w)
    (numpad-clear)
    (numpad-set-mode mode)))

(define (numpad-bind-tag tag)
  (lambda (w)
    (numpad-append w tag)))

(define-command 'numpad-do-1 (numpad-bind-tag "1") #:spec "%W")
(define-command 'numpad-do-2 (numpad-bind-tag "2") #:spec "%W")
(define-command 'numpad-do-3 (numpad-bind-tag "3") #:spec "%W")
(define-command 'numpad-do-4 (numpad-bind-tag "4") #:spec "%W")
(define-command 'numpad-do-5 (numpad-bind-tag "5") #:spec "%W")
(define-command 'numpad-do-6 (numpad-bind-tag "6") #:spec "%W")
(define-command 'numpad-do-7 (numpad-bind-tag "7") #:spec "%W")
(define-command 'numpad-do-8 (numpad-bind-tag "8") #:spec "%W")
(define-command 'numpad-do-9 (numpad-bind-tag "9") #:spec "%W")

(define-command 'numpad-do-grow (numpad-bind-mode 'grow) #:spec "%W")
(define-command 'numpad-do-shrink (numpad-bind-mode 'shrink) #:spec "%W")
(define-command 'numpad-do-move (numpad-bind-mode 'move) #:spec "%W")
(define-command 'numpad-do-select (numpad-bind-mode 'select) #:spec "%W")


;; (define-command 'grow-left (lambda (w) (grow-window w '(-1 . 0))))
;; (define-command 'grow-right (lambda (w) (grow-window w '(1 . 0))) #:spec "%W")
;; (define-command 'grow-up (lambda (w) (grow-window w '(0 . -1))) #:spec "%W")
;; (define-command 'grow-down (lambda (w) (grow-window w '(0 . 1))) #:spec "%W")
;; (define-command 'grow-topleft (lambda (w) (grow-window-up w) (grow-window-left w)) #:spec "%W")
;; (define-command 'grow-topright (lambda (w) (grow-window-up w) (grow-window-right w)) #:spec "%W")
;; (define-command 'grow-botleft (lambda (w) (grow-window-down w) (grow-window-left w)) #:spec "%W")
;; (define-command 'grow-botright (lambda (w) (grow-window-down w) (grow-window-right w)) #:spec "%W")

;; (defmacro bind-keyseq (seq fun #!rest args)
;;   (eval `(define-command ,sym (lambda (w) (,fun w ,@args))))
;;   (eval `(bind-keys )

(bind-keys global-keymap
           "KP_Left" 'numpad-do-4
           "KP_Right" 'numpad-do-6
           "KP_Up" 'numpad-do-8
           "KP_Down" 'numpad-do-2
           "KP_Home" 'numpad-do-7
           "KP_End" 'numpad-do-1
           "KP_Prior" 'numpad-do-9
           "KP_Next" 'numpad-do-3
           "KP_Begin" 'numpad-do-5

           "KP_Add" 'numpad-do-grow
           "KP_Subtract" 'numpad-do-shrink
           "KP_Divide" 'numpad-do-move
           "KP_Enter" 'numpad-do-select)
