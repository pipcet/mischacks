(define (pointer-head-dimensions) (head-dimensions (pointer-head)))

(define (window-by-id id)
  (let* ((mw (managed-windows))
         (ws (filter (lambda (w) (equal (window-id w) id)) mw)))
    (if ws
        (car ws))))

(define (window-by-cli-tag tag)
  (let* ((mw (managed-windows))
         (ws (filter (lambda (w) (equal (window-get w 'cli-tag) tag)) mw)))
    (if ws
        (car ws))))

(define (cli-current-window)
  (cadr (stacking-order)))

(define cli-tag-areas-halves
  '((1 ((0/2 . 2/2) . (1/2 . 2/2)))
    (3 ((1/2 . 2/2) . (1/2 . 2/2)))
    (7 ((0/2 . 1/2) . (0/2 . 1/2)))
    (9 ((1/2 . 1/2) . (0/2 . 1/2)))

    (4 ((0/2 . 1/2) . (0/2 . 2/2)))
    (6 ((1/2 . 2/2) . (0/2 . 2/2)))

    (2 ((1/2 . 1) . (0/2 . 2/2)))
    (8 ((1/2 . 1) . (0/2 . 2/2)))
    (5 ((0 . 1) . (0 . 1)))))

(define cli-tag-areas-thirds
  '((5 ((1/3 . 2/3) . (0/2 . 1)))
    (2 ((1/3 . 2/3) . (0 . 1)))
    (8 ((1/3 . 2/3) . (0 . 1)))
    (4 ((0/3 . 1/3) . (0 . 1)))
    (6 ((2/3 . 3/3) . (0 . 1)))

    (1 ((0/3 . 1/3) . (1/2 . 1)))
    (3 ((2/3 . 3/3) . (1/2 . 1)))
    (7 ((0/3 . 1/3) . (0 . 1/2)))
    (9 ((2/3 . 3/3) . (0 . 1/2)))

    (45 ((0 . 1/2) . (0/2 . 1)))
    (56 ((1/2 . 1) . (0/2 . 1)))
    (85 ((0 . 1) . (0/2 . 1/2)))
    (52 ((0 . 1) . (1/2 . 2/2)))

    (55 ((0 . 1) . (0 . 1)))))

(define cli-tag-layouts
  `((2 ,cli-tag-areas-halves)
    (8 ,cli-tag-areas-thirds)))

(define (cli-tag-layout head)
  (if (> (car (head-dimensions head)) 1400)  cli-tag-areas-thirds cli-tag-areas-halves))

(define (cli-move-window w area-sexp head)
  (let* ((area (eval area-sexp))
         (scr (head-dimensions head))
         (off (head-offset head))
         (ax (+ (car off) (* (car scr) (caar area))))
         (aw (* (car scr) (- (cdar area) (caar area))))
         (ay (+ (cdr off) (* (cdr scr) (cadr area))))
         (ah (* (cdr scr) (- (cddr area) (cadr area)))))
    (window-put w 'cli-area area-sexp)
    (move-resize-window-and-frame-to w ax ay aw ah)))

(define (cli-change-layout layout)
  (let ((old-layout (cli-tag-layout (pointer-head))))
    (mapc (lambda (w)
            (let ((area-sexp (window-get w 'cli-area))
                  (head (window-get w 'cli-head)))
              (when area-sexp
                (cli-move-window w area-sexp head))))
          (managed-windows))))

(define (get-area digit)
  (let* ((layout (cadr (assoc digit cli-tag-layouts))))
    (let ((area (cadr (assoc digit (cli-tag-layout (pointer-head))))))
      area)))

(define (area-union a1 a2)
  (let* ((ax0 (min (caar a1) (caar a2)))
         (ax1 (max (cdar a1) (cdar a2)))
         (ay0 (min (cadr a1) (cadr a2)))
         (ay1 (max (cddr a1) (cddr a2))))
    (cons (cons ax0 ax1)
          (cons ay0 ay1))))

(define (cli-layout-switch)
  (cli-change-layout (if (eq (cli-tag-layout (pointer-head)) cli-tag-areas-halves)
                         cli-tag-areas-thirds
                       cli-tag-areas-halves)))

(define (pop-under win)
  (let* ((w (car (window-frame-dimensions win)))
         (h (cdr (window-frame-dimensions win)))
         (px (car (query-pointer)))
         (py (cdr (query-pointer)))
         (new-x (floor (- px (/ w 2))))
         (new-y (floor (- py (/ h 2))))
         (hw (car (pointer-head-dimensions)))
         (hh (cdr (pointer-head-dimensions))))
    (when (< new-x 0) (setq new-x 0))
    (when (< new-y 0) (setq new-y 0))
    (when (> (+ new-x w) hw) (setq new-x (- hw w)))
    (when (> (+ new-y h) hh) (setq new-y (- hh h)))
    (move-window-to win new-x new-y)))

(define (cli-window-info w)
  (let ((tag (window-get w 'cli-tag))
        (name (window-name w)))
    (concat
     (if tag (symbol-name tag) " ")
     " "
     name
     " "
     (prin1-to-string (window-get w 'cli-area)))))

(define (count i)
  (if (= i 0) () (cons (1- i) (count (1- i)))))

(define (window-list)
  (let ((ws (stacking-order)))
    (apply concat (mapcar (lambda (i) (concat (cli-window-info (nth i ws))
                                              " @"
                                              (number->string i)
					      " "
					      (number->string (window-id (nth i ws)))
                                              "\n"))
                          (reverse (count (length ws)))))))

(define (window-pid w)
  (aref (caddr (get-x-property w '_NET_WM_PID)) 0))

(define (free-all-the-windows)
  (mapc (lambda (w) (set-window-type w 'unframed)) (managed-windows)))


(define (window-area w)
  (let* ((pos (window-position w))
         (dim (window-dimensions w))
         (hw (car (pointer-head-dimensions)))
         (hh (cdr (pointer-head-dimensions)))
         (ax0 (/ (car pos) hw))
         (ax1 (/ (+ (car pos) (car dim)) hw))
         (ay0 (/ (cdr pos) hw))
         (ay1 (/ (+ (cdr pos) (cdr dim)) hw))
         (ax (cons ax0 ax1))
         (ay (cons ay0 ay1))
         (area (cons ax ay)))
    area))

(define (devscore a1 a2)
  (let* ((dx0 (- (caar a1) (caar a2)))
         (dx1 (- (cdar a1) (cdar a2)))
         (dy0 (- (cadr a1) (cadr a2)))
         (dy1 (- (cddr a1) (cddr a2))))
    (+ (* dx0 dx0) (* dx1 dx1) (* dy0 dy0) (* dy1 dy1))))

(define (windows-by-regexp regexp)
  (filter (lambda (w) (string-match regexp (window-name w)))
          (managed-windows)))

(define (match-best area windows)
  (let* ((scores (mapcar
                  (lambda (w) (cons (devscore area (window-area w))
                                    w))
                  windows))
         (sscores (sort scores
                        (lambda (a b) (< (- (car a) (car b))
                                         0))))
         (best (cdar sscores)))
    best))

(add-hook 'add-window-hook
          (lambda (w)
            (cond
             ((and
               (string-match "^Emacs" (window-name w))
               (not (window-by-cli-tag 'l)))
              (window-put w 'cli-tag 'l))
             ((and
               (string-match "^Emacs" (window-name w))
               (not (window-by-cli-tag 'r)))
              (window-put w 'cli-tag 'r))
             ((and
               (string-match "^Emacs" (window-name w))
               (not (window-by-cli-tag 'e)))
              (window-put w 'cli-tag 'e))
             ((and
               (string-match ".*Nightly$" (window-name w))
               (not (window-by-cli-tag 'b)))
              (window-put w 'cli-tag 'b))
             ((and
               (string-match ".*Nightly$" (window-name w))
               (not (window-by-cli-tag 'a)))
              (window-put w 'cli-tag 'a)))))
