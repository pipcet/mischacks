;; matchbox-window.jl -- match windows to properties
;; $$

;; Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>

;; sawfish is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; sawfish is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with sawfish; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; Commentary:

;; This module provides a general mechanism for setting properties on
;; matched windows as they're created. It removes much of the need for
;; manually creating functions to put in before-add-window-hook.

;; For example, doing:

;;	(add-window-matcher 'WM_CLASS "Term" '(place-mode . interactive))

;; makes all terminal windows be placed interactively. Or even better:

;;	(add-window-matcher 'WM_CLASS "Netscape/Navigator"
;;			    '(ignore-program-position . t))

;; makes netscape windows a lot easier to live with.

(define-structure sawfish.wm.ext.matchbox-window

    (export matchbox-window-grab-x-property
	    define-matchbox-window-group
	    define-matchbox-window-property
	    define-matchbox-window-setter
	    define-matchbox-window-formatter
	    add-window-matchboxer
	    remove-window-matchboxer
	    matchbox-window)

    (open rep
	  rep.system
	  rep.regexp
	  sawfish.wm
	  sawfish.wm.util.groups)

  (define-structure-alias matchbox-window sawfish.wm.ext.matchbox-window)

;;; configuration and customize stuff

  (i18n-defvar matchbox-window-x-properties
    '((WM_NAME . "Name")
      (WM_CLASS . "Class")
      (WM_ICON_NAME . "Icon Name")
      (WM_WINDOW_ROLE . "Role")
      (WM_CLIENT_MACHINE . "Host")
      (WM_COMMAND . "Command")
      (WM_LOCALE_NAME . "Locale")))

  (i18n-defvar matchbox-window-properties
    `((placement ,(_ "Placement")
       (ignore-program-position boolean)
       (place-mode ,(lambda () `(choice ,@placement-modes)))
       (position (pair number number))
       (dimensions (pair (number 1) (number 1)))
       (workspace (number 1))
       (viewport (pair (number 1) (number 1)))
       (depth (number -16 16))
       (placement-weight number)
       (fixed-position boolean)
       (maximized (choice all vertical horizontal)))
      (focus ,(_ "Focus")
       (raise-on-focus boolean)
       (focus-when-mapped boolean)
       (never-focus boolean)
       (focus-click-through boolean)
       (focus-mode ,(lambda () `(choice ,@focus-modes))))
      (appearance ,(_ "Appearance")
       (frame-type ,(lambda () `(choice ,@(mapcar car matchbox-window-types))))
       (frame-style ,(lambda () `(symbol ,@(find-all-frame-styles t)))))
      (state ,(_ "State")
       (avoid boolean)
       (ignored boolean)
       (iconified boolean)
       (shaded boolean)
       (sticky boolean)
       (sticky-viewport boolean)
       (group ,(lambda ()
		 `(symbol ,@(delete-if-not symbolp (window-group-ids)))))
       (ungrouped boolean)
       (cycle-skip boolean)
       (window-list-skip boolean)
       (task-list-skip boolean)
       (never-iconify boolean)
       (never-maximize boolean))
      (other ,(_ "Other")
       (unique-name boolean)
       (auto-gravity boolean)
       (shade-hover boolean)
       (transients-above (choice all parents none))
       (ignore-stacking-requests boolean))))

  ;; alist of (PROPERTY . FEATURE) mapping properties to the lisp
  ;; libraries implementing them
  (defvar matchbox-window-features
    '((raise-on-focus . auto-raise)
      (shade-hover . shade-hover)))

  (defvar matchbox-window-types
    '((normal . default)
      (title-only . shaped)
      (border-only . transient)
      (top-border . shaped-transient)
      (none . unframed)))

  (define (matchbox-window-widget)
    (let ((props (mapcar
		  (lambda (sub)
		    (cons (cadr sub)
			  (mapcar (lambda (prop)
				    (if (functionp (cadr prop))
					(list* (car prop)
					       ((cadr prop))
					       (cddr prop))
				      prop))
				  (cddr sub)))) matchbox-window-properties)))
      `(matchbox-window ,props ,matchbox-window-x-properties)))

  (put 'matchbox-window 'custom-widget matchbox-window-widget)

  ;;###autoload (defgroup match-window "Matched Windows" :layout single :require sawfish.wm.ext.match-window)
  (defgroup matchbox-window "Matched Windows"
    :layout single
    :require sawfish.wm.ext.matchbox-window)

  ;; List of (MATCH-ELTS . ACTION-ELTS)
  ;; Each MATCH-ELT is (PROP . REGEXP or NUMBER or SYMBOL)
  ;; Each ACTION-ELT is (PROP . VALUE)
  (defcustom matchbox-window-profile nil
    nil
    :type matchbox-window
    :group matchbox-window
    :require sawfish.wm.ext.matchbox-window)

  ;; used by sawfish-ui when grabbing property values
  (define (matchbox-window-grab-x-property real-prop)
    (let ((window (select-window))
	  prop)
      (when window
	(setq prop (get-x-property window real-prop))
	(if (and prop (eq (car prop) 'STRING))
	    (setq prop (get-x-text-property window real-prop))
	  (setq prop (nth 2 prop)))
	(when prop
	  (cond ((get real-prop 'matchbox-window-formatter)
		 (setq prop ((get real-prop 'matchbox-window-formatter) prop)))
		((vectorp prop)
		 (setq prop (aref prop 0))))))
      (when (stringp prop)
	(setq prop (concat #\^ (quote-regexp prop) #\$)))
      prop))

  (define (define-matchbox-window-group group name)
    (unless (assq group matchbox-window-properties)
      (setq matchbox-window-properties (nconc matchbox-window-properties
					   (list (list group name))))))

  (define (define-matchbox-window-property name group . def)
    (let* ((group-cell (or (assq group matchbox-window-properties)
			   (error "Unknown match-window group: %s" group)))
	   (item-cell (assq name (cddr group-cell))))
      (if item-cell
	  (rplacd item-cell def)
	(rplacd (cdr group-cell) (nconc (cddr group-cell)
					(list (cons name def)))))))

  (define (define-matchbox-window-setter name setter)
    (put name 'matchbox-window-setter setter))

  (define (define-matchbox-window-formatter name formatter)
    (put name 'matchbox-window-formatter formatter))

;;; main entry point

  (define (add-window-matchboxer prop value #!rest actions)
    (catch 'out
      (let
	  ((pair (cons prop value))
	   (add-to (lambda (slot)
		     (mapc (lambda (action)
			     (let
				 ((tem (assq (car action) (cdr slot))))
			       (if tem
				   (rplacd tem (cdr action))
				 (rplacd slot (cons action (cdr slot))))))
			   actions))))
	;; does the list already contain a (PROP . VALUE) pair?
	(mapc (lambda (cell)
		(when (member pair (car cell))
		  (add-to cell)
		  (throw 'out t)))
	      matchbox-window-profile)
	;; no
	(setq matchbox-window-profile (cons (list (cons pair nil))
					 matchbox-window-profile))
	(add-to (car matchbox-window-profile)))))

  (define (remove-window-matchboxer prop value #!rest props)
    (let
	((pair (cons prop value))
	 (remove-from (lambda (slot)
			(mapc (lambda (p)
				(let
				    ((tem (assq p (cdr slot))))
				  (when tem
				    (rplacd slot (delq tem (cdr slot))))))
			      props))))
      (mapc (lambda (cell)
	      (when (member pair (car cell))
		(remove-from cell)))
	    matchbox-window-profile)
      ;; remove any empty matchboxers
      (setq matchbox-window-profile
	    (delete-if (lambda (cell)
			 (null (cdr cell))) matchbox-window-profile))))

;;; matchboxer code

  (define (safe-string-matchbox re . args)
    (condition-case data
	(apply string-matchbox re args)
      (regexp-error
       (format standard-error
	       "regexp error in matchbox-window: %s, %s" re (car data)))))

  (define (matchbox-window w)
    (let ((prop-cache '()))

      ;; Get the X property P of window W, uses a cache, will
      ;; reformat properties with a matchbox-window-formatter property
      (define (get-prop p)
	(let
	    ((tem (assq p prop-cache)))
	  (if tem
	      (cdr tem)
	    (setq tem (copy-sequence (get-x-property w p)))
	    (when (and tem (eq (car tem) 'STRING))
	      (rplaca (cddr tem) (get-x-text-property w p)))
	    (when (and tem (get p 'matchbox-window-formatter))
	      (rplaca (cddr tem) ((get p 'matchbox-window-formatter)
				  (nth 2 tem))))
	    (setq prop-cache (cons (cons p tem) prop-cache))
	    tem)))

      ;; Return t if X property PROP of window W matchboxes INPUT (a
      ;; regexp, number, or symbol)
      (define (matchbox-prop prop input)
	(catch 'out
	  (cond ((and (stringp input)
		      (or (stringp prop)
			  (and (vectorp prop)
			       (> (length prop) 0)
			       (stringp (aref prop 0)))))
		 ;; regexp matchbox
		 (if (vectorp prop)
		     (do ((i 0 (1+ i)))
			 ((= i (length prop)) nil)
		       (when (safe-string-matchbox input (aref prop i))
			 (throw 'out t)))
		   (safe-string-matchbox input prop)))
		((and (numberp input) (numberp prop))
		 (= input prop))
		(t (equal input prop)))))

      ;; Execute the list of actions for window W
      (define (run-actions actions)
	(mapc (lambda (a) (a)) actions))

      (mapc (lambda (cell)
	      (when (catch 'out
		      (mapc (lambda (matchbox)
			      (let ((prop (and (symbolp (car matchbox))
					       (get-prop (car matchbox)))))
				(when (or (not prop)
					  (not (matchbox-prop (nth 2 prop)
							   (cdr matchbox))))
				  (throw 'out nil))))
			    (car cell))
		      t)
		(run-actions (cdr cell))))
	    matchbox-window-profile)))

;  (add-hook 'before-add-window-hook matchbox-window t)

;;; custom property formatters and setters

  (define-matchbox-window-formatter 'WM_CLASS
   (lambda (vec)
     (format nil "%s/%s" (and (> (length vec) 1) (aref vec 1)) (aref vec 0))))

  (define-matchbox-window-formatter 'WM_COMMAND
   (lambda (vec)
     (let ((i 0)
	   parts)
       (while (< i (length vec))
	 (when parts
	   (setq parts (cons ?  parts)))
	 (setq parts (cons (aref vec i) parts))
	 (setq i (1+ i)))
       (apply concat (nreverse parts)))))

  (define-matchbox-window-setter 'workspace
   (lambda (w prop value)
     (declare (unused prop))
     (unless (or (window-get w 'placed) (window-workspaces w))
       ;; translate from 1.. to 0..
       (set-window-workspaces w (list (1- value))))))

  (define-matchbox-window-setter 'position
   (lambda (w prop value)
     (declare (unused prop))
     (let ((x (car value))
	   (y (cdr value)))
       (when (< x 0)
	 ;; XXX should change placement gravity
	 (setq x (+ (screen-width) x)))
       (when (< y 0)
	 ;; XXX should change placement gravity
	 (setq y (+ (screen-height) y)))
       (move-window-to w x y))))

  (define-matchbox-window-setter 'dimensions
   (lambda (w prop value)
     (declare (unused prop))
     (resize-window-with-hints w (car value) (cdr value))))

  (define-matchbox-window-setter 'viewport
   (lambda (w prop value)
     (declare (unused prop))
     (unless (window-get w 'placed)
       (set-screen-viewport (1- (car value)) (1- (cdr value)))
       (set-window-viewport w (1- (car value)) (1- (cdr value))))))

  (define-matchbox-window-setter 'frame-type
   (lambda (w prop value)
     (declare (unused prop))
     (set-window-type w (or (cdr (assq value matchbox-window-types)) value))))

  (define-matchbox-window-setter 'ungrouped
   (lambda (w prop value)
     (declare (unused prop))
     (when value
       (add-window-to-new-group w))))

  (define-matchbox-window-setter 'unique-name
    (lambda (w prop value)
     (declare (unused prop))
      (when value
	(uniquify-window-name w))))

  (define-matchbox-window-setter 'focus-mode
   (lambda (w prop value)
     (declare (unused prop))
     (set-focus-mode w value)))

  (define-matchbox-window-setter 'maximized
   (lambda (w prop value)
     (declare (unused prop))
     (when (memq value '(all vertical))
       (window-put w 'queued-vertical-maximize t))
     (when (memq value '(all horizontal))
       (window-put w 'queued-horizontal-maximize t)))))
