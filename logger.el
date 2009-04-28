;; logger -- log work time and activities.
;; $Id: logger.el,v 1.8 1996/05/20 23:27:12 nickson Exp $

;; Copyright (C) 1994 by Ray Nickson <nickson@cs.uq.oz.au>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;;;;;;;;;;;;
;;; TO DO ;;;
;;;;;;;;;;;;;
;; Implement copy

(require 'assoc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customization Variables ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar logger-db-file (substitute-in-file-name "$HOME/.logger_db")
  "*The name of the file containing the logger data base.")

(defvar logger-db-backup (substitute-in-file-name "$HOME/.logger_db~")
  "*The name of the file containing the logger data base backup.")

(defvar logger-display-count-default 10
  "*How many logger lines to display by default.
This is the number by which logger-display-{more,less} adjust
logger-display-count.")

(defvar logger-display-count logger-display-count-default
  "*How many logger lines to display.
If you set this yourself, call `\\[logger]' to update the display.
Otherwise, adjust it with `\\[logger-display-more]' and `\\[logger-display-less]'.")

(defvar logger-mode-hooks nil
  "*Hooks run after setting up a *logger* buffer.")

;;;;;;;;;;;;;;;;;;;;;;;
;;; Other variables ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defvar logger-times-db nil
  "Logger data base of times and activities.
It is a list of (TIME ACTNO DESCR) where TIME is the time (current-time) at
the start of the activity, ACTNO is the number of the activity, and DESCR is a
string describing the activity, or nil.  The list is ordered by TIME, with
the most recent first.")

(defvar logger-activities-db nil
  "Logger data base of activities and their codes.
It is a list of pairs (ACTCODE . ACTDESC) whose Nth entry gives the one-letter
code (or nil) and the description of activity number N.")

(defvar logger-buffer nil
  "The buffer displaying the logger.")

(defvar logger-mode-map (make-keymap)
  "Keymap used in logger mode.")

(defvar logger-edit-time-mode nil
  "Are we currently editing a logger time field?
If not, this is nil.  Otherwise, it is a string defining which field we are
editing.")

(make-variable-buffer-local 'logger-edit-time-mode)

(defvar logger-edit-time-field nil
  "Which logger time field are we editing?")

(defvar logger-edit-time-increments nil
  "How much should we add or subtract for each increment in this field?")

(define-prefix-command 'logger-edit-time-mode-map)
(aput 'minor-mode-alist 'logger-edit-time-mode '(logger-edit-time-mode))
(aput 'minor-mode-map-alist 'logger-edit-time-mode 'logger-edit-time-mode-map)

(defconst logger-time-increments
  '((hour   0   3600)
    (minute 0   60)
    (second 0   1)
    (year   481 13184)
    (month  39  36096)
    (day    1   20864))
  "List of triples (UNIT HIGH LOW) giving high and low 16 bits of number of
seconds in a time UNIT.")

(defconst logger-time-format "%a %d %b %Y    %R")
(defconst logger-time-fields
  '((day    0 0)
    (month  1 7)
    (year   2 11)
    (hour   3 19)
    (minute 4 22))
  "List of positions of the mutable fields of a time string.
Each is a triple (FIELDNAME INDEX STARTPOS).")

(defconst logger-display-format
  '("%-25s  %-8s  %-14s  %s"
    (0 24 start-time) (26 34 duration) (36 50 activity) (52 999 description))
  "A list (FMT . FIELDS)
describing the logger display format.
FMT is a format string; other elements are triples (START POS FIELD) giving
start and end column positions where the field display beins and ends.")

(defvar logger-description-history nil)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User entry-points ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun logger ()
  "Make sure the logger is set up properly and visible."
  (interactive)
  (if logger-times-db
      nil
    (logger-load-db)
    (logger-write-db logger-db-backup))
  (or (and logger-buffer (buffer-name logger-buffer))
      (setq logger-buffer (get-buffer-create "*logger*")))
  (pop-to-buffer logger-buffer)
  (raise-frame (window-frame (get-buffer-window logger-buffer t)))
  (set-mouse-position (window-frame (get-buffer-window logger-buffer t)) 0 0)
  (setq buffer-modified-p nil
        buffer-read-only t)
  (logger-mode)
  (logger-display))

(defun logger-log-current-time ()
  "Log a new activity starting now."
  (interactive)
  (logger)
  (let ((time (current-time)))
    (setq logger-times-db
          (cons (logger-record time nil nil) logger-times-db))
    (let ((buffer-read-only nil))
      (goto-char 0)
      (insert "\n")
      (backward-char 1)
      (logger-redisplay-record (car logger-times-db) nil)
      (if (cdr logger-times-db)
          (progn
            (beginning-of-line 2)
            (logger-redisplay-record (car (cdr logger-times-db)) time)))
      (goto-char 0))
    (logger-write-db)))

;;;;;;;;;;;;;;;;;;;;;;
;;; Local bindings ;;;
;;;;;;;;;;;;;;;;;;;;;;

(suppress-keymap logger-mode-map)

(let ((char ?A))
  (while (<= char ?Z)
    (define-key logger-mode-map (char-to-string char)
      'logger-set-activity)
    (setq char (1+ char))))

(define-key logger-mode-map "t" 'logger-log-current-time)
(define-key logger-mode-map "a" 'logger-set-activity)
(define-key logger-mode-map "l" 'logger-list-activities)
(define-key logger-mode-map "+" 'logger-display-more)
(define-key logger-mode-map "-" 'logger-display-less)
(define-key logger-mode-map "n" 'logger-new-activity)
(define-key logger-mode-map "e" 'logger-edit-time)
(define-key logger-mode-map "d" 'logger-delete-entry)
(define-key logger-mode-map "c" 'logger-copy-entry)
(define-key logger-mode-map "r" 'logger-revert)
(define-key logger-mode-map "x" 'logger-set-activity-code)
(define-key logger-mode-map "s" 'logger-summarize-activities)
(define-key logger-mode-map "'" 'logger-edit-description)
(define-key logger-mode-map [down-mouse-1] 'logger-mouse-select)
(define-key logger-mode-map [mouse-1] 'ignore)

(defun logger-new-activity (desc code)
  "Register a new activity in the logger data base."
  (interactive "sDescription: \ncCode: ")
  (setq code (logger-normalize-code code))
  (logger-check-duplicate-code code)
  (setq logger-activities-db
        (append logger-activities-db
                (list (cons code desc))))
  (logger-write-db))

(defun logger-set-activity (actid)
  "Set the activity for the current record to ACTID.
ACTID may be an activity number or code written as a string.
Interactively, a numeric prefix arg specifies an activity number.  With no
prefix arg, if this function was invoked by a single capital letter, that
letter is the code.  Otherwise, prompt for a code."
  (interactive
   (list
    (if (numberp current-prefix-arg)
        current-prefix-arg
      (let ((keys (this-command-keys)))
           (if (and (stringp keys)
                    (= (length keys) 1)
                    (>= (string-to-char keys) ?A)
                    (<= (string-to-char keys) ?Z))
               keys
             (prog2 (message "Code: ")
                 (char-to-string (upcase (read-char)))
               (message nil)))))))
  (if (stringp actid)
      (setq actid (logger-get-activity-number (string-to-char actid))))
  (let ((rec (nth (current-line) logger-times-db)))
    (logger-set-record-actno rec actid)
    (logger-redisplay-record
        rec
        (car (nth (1- (current-line)) logger-times-db)))
    (logger-write-db)))

(defun logger-list-activities ()
  "Pop up a buffer listing logger activity numbers, codes and descriptions."
  (interactive)
  (set-buffer (get-buffer-create "*logger activities*"))
  (erase-buffer)
  (let ((acts logger-activities-db) (count 0))
    (while acts
      (insert (format "%4d     %s     %s\n"
                      count
                      (logger-code-string (car (car acts)))
                      (cdr (car acts))))
      (goto-char 0)
      (setq count (1+ count) acts (cdr acts))))
  (insert "  NUM  CODE  DESCRIPTION\n")
  (display-buffer (current-buffer)))

(defun logger-delete-entry (recno)
  "Delete logger record NUMBER.
Interactively, delete the record point is on.
The data base is NOT WRITTEN automatically after this operation."
  (interactive (list (current-line)))
  (if (= recno 0)
      (setq logger-times-db (cdr logger-times-db))
    (let ((recs (nthcdr (1- recno) logger-times-db)))
      (rplacd recs (cdr (cdr recs)))))
  (logger-display)
  (logger-write-db))

(defun logger-write-db (&optional file)
  "Write out the logger database.
This is done automatically after all normal logger commands, but not after the
editing commands."
  (interactive)
  (write-region (format "%S\n%S\n"
                        (list 'setq 'logger-times-db
                              (list 'quote logger-times-db))
                        (list 'setq 'logger-activities-db
                              (list 'quote logger-activities-db)))
     nil
     (or file logger-db-file))
  (if (null file)
      (set-buffer-modified-p nil)))

(defun logger-display-more (n)
  "Display N (logger-display-count-default) more logger records."
  (interactive "P")
  (setq logger-display-count
        (min (+ logger-display-count (if n
                                         (prefix-numeric-value n)
                                       logger-display-count-default))
             (length logger-times-db)))
  (logger-display)
  (message "Now displaying %s%d records."
           (if (= logger-display-count (length logger-times-db))
               "all "
             "")
           logger-display-count))

(defun logger-display-less (n)
  "Display N (logger-display-count-default) fewer logger records."
  (interactive "P")
  (setq logger-display-count
        (max (- logger-display-count (if n
                                         (prefix-numeric-value n)
                                       logger-display-count-default))
             0))
  (logger-display)
  (message "Now displaying %d records." logger-display-count))

(defun logger-set-activity-code (actid code)
  "Set code for ACTID to CODE.
ACTID is an activity number or (old) code as a string.  CODE is the new code
as a character, or nil.
Interactively, with a numeric prefix argument, we use that as ACTID,
otherwise we prompt for a code.  We always prompt for the new CODE."
  (interactive
   (list
    (if (numberp current-prefix-arg)
        current-prefix-arg
      (prog2 (message "Old code: ")
          (char-to-string (read-char))
        (message nil)))
    (prog2 (message "New code: ")
        (read-char)
      (message nil))))
  (if (stringp actid)
      (setq actid (logger-get-activity-number (string-to-char actid))))
  (setq code (logger-normalize-code code))
  (logger-check-duplicate-code code)
  (rplaca (nth actid logger-activities-db) code)
  (logger-list-activities)
  (logger-write-db))

(define-key logger-edit-time-mode-map "<" 'logger-time-previous-field)
(define-key logger-edit-time-mode-map ">" 'logger-time-next-field)
(define-key logger-edit-time-mode-map "+" 'logger-time-increment-field)
(define-key logger-edit-time-mode-map "-" 'logger-time-decrement-field)
(define-key logger-edit-time-mode-map "q" 'logger-time-quit)

(defun logger-edit-time ()
  "Edit the time string of the current record.
This is achieved via a minor mode in which <,> move backwards and forwards in
the time string, and +,- increment and decrement time fields.  The mode line
indicates which time field is currently being altered.  `q' finishes time
editing.
\\{logger-edit-time-map}"
  (interactive)
  (logger-edit-time-field 'hour))

(defun logger-time-previous-field (n)
  "Go to the time field N positions to the left of this one."
  (interactive "p")
  (let ((fnum logger-edit-time-field))
    (if (null fnum)
        (error "Not editing a time field.")
      (setq fnum (% (+ (length logger-time-fields) fnum (- n)) (length logger-time-fields)))
      (logger-edit-time-field (car (nth fnum logger-time-fields))))))

(defun logger-time-next-field (n)
  "Go to the time field N positions to the right of this one."
  (interactive "p")
  (let ((fnum logger-edit-time-field))
    (if (null fnum)
        (error "Not editing a time field.")
      (setq fnum (% (+ (length logger-time-fields) fnum n) (length logger-time-fields)))
      (logger-edit-time-field (car (nth fnum logger-time-fields))))))

(defun logger-time-increment-field (n)
  "Add N units to the current time field."
  (interactive "p")
  (or logger-edit-time-field
      (error "Not editing a time field."))
  (let* ((hi (* n (car logger-edit-time-increments)))
         (lo (* n (car (cdr logger-edit-time-increments))))
         (rec (nth (current-line) logger-times-db))
         (tim (car rec)))
    (logger-set-time tim (+ (car tim) hi) (+ (car (cdr tim)) lo))
    (logger-redisplay-record rec nil)))

(defun logger-time-decrement-field (n)
  "Subtract N units from the current time field."
  (interactive "p")
  (or logger-edit-time-field
      (error "Not editing a time field."))
  (let* ((hi (* n (car logger-edit-time-increments)))
         (lo (* n (car (cdr logger-edit-time-increments))))
         (rec (nth (current-line) logger-times-db))
         (tim (car rec)))
    (logger-set-time tim (- (car tim) hi) (- (car (cdr tim)) lo))
    (logger-redisplay-record rec nil)))

(defun logger-time-quit ()
  "Finish editing time fields."
  (interactive)
  (or logger-edit-time-field
      (error "Not editing a time field."))
  (setq logger-edit-time-field nil
        logger-edit-time-mode nil)
  (setq logger-times-db (sort logger-times-db 'logger-compare-times))
  (logger-display)
  (logger-write-db))

(defun logger-revert ()
  "Revert the logger from the backup data base.
This discards all edit commands since the last logger command."
  (interactive)
  (logger-load-db logger-db-backup)
  (logger-display)
  (logger-write-db))

(defun logger-edit-description (desc)
  "Edit the description field of current logger activity."
  (interactive (list (read-string "Description: " nil '
                                  logger-description-history)))
  (let ((rec (nth (current-line) logger-times-db)))
    (logger-set-record-description rec desc)
    (logger-redisplay-record rec nil)
    (logger-write-db)))

(defun logger-mouse-select (event)
  (interactive "@e")
  "Edit the field clicked on."
  (mouse-set-point event)
  (let ((flds (cdr logger-display-format)))
    (while (consp flds)
      (if (and (>= (current-column) (nth 0 (car flds)))
               (<= (current-column) (nth 1 (car flds))))
          (setq flds (nth 2 (car flds)))
        (setq flds (cdr flds))))
    (if flds
        (funcall (intern (format "logger-mouse-%s" flds))
                 event))))

(defun logger-mouse-start-time (event)
  (let ((flds logger-time-fields) fld)
    (while (and (consp flds) (>= (current-column) (nth 2 (car flds))))
      (setq fld (nth 0 (car flds))
            flds (cdr flds)))
    (logger-edit-time-field fld)))

(defun logger-mouse-activity (event)
  (let ((menu nil) act (n 0))
    (while (< n (length logger-activities-db))
      (setq menu (cons (cons (cdr (nth n logger-activities-db)) n) menu)
            n (1+ n)))
    (setq act
          (x-popup-menu event (list "Activities" (cons "Activities" menu))))
    (if act
        (logger-set-activity act))))

(defun logger-mouse-description (event)
  (call-interactively 'logger-edit-description))

(defun logger-summarize-activities (arg)
  "Make a summary of the activities currently displayed."
  (interactive "p")
  (let ((totals (make-vector (length logger-activities-db) 0))
        (subtotals (make-vector (length logger-activities-db) nil))
        (times (nthcdr (1- arg) logger-times-db))
        (num (- logger-display-count arg))
        st et etg diff str)
    (setq et (logger-record-time (car times))
          times (cdr times))
    (setq etg et)
    (while (> num 0)
      (setq st (logger-record-time (car times)))
      (setq diff (logger-time-diff et st))
      (aset totals (logger-record-actno (car times))
            (+ (aref totals (logger-record-actno (car times)))
               diff))
      (setq str (assoc (logger-record-descr (car times))
                       (aref subtotals (logger-record-actno (car times)))))
      (if str
          (rplacd str (+ (cdr str) diff))
        (aset subtotals (logger-record-actno (car times))
              (cons (cons (logger-record-descr (car times)) diff)
                    (aref subtotals (logger-record-actno (car times))))))
      (setq et st
            times (cdr times)
            num (1- num)))
    (set-buffer (get-buffer-create "*Logger Summary*"))
    (erase-buffer)
    (display-buffer (current-buffer))
    (insert "Activity summary for "
            (format-time-string logger-time-format st)
            " -- "
            (format-time-string logger-time-format etg)
            "\n\n")
    (let ((actno 0) sts)
      (while (< actno (length logger-activities-db))
        (if (= (aref totals actno) 0)
            nil
          (insert (format "%-30s%s\n"
                          (cdr (nth actno logger-activities-db))
                          (logger-duration-string (aref totals actno) t)))
          (setq sts (aref subtotals actno))
          (while sts
            (if (car (car sts))
                (insert (format "  %-20s%s\n"
                                (car (car sts))
                                (logger-duration-string (cdr (car sts)) t))))
            (setq sts (cdr sts))))
        (setq actno (1+ actno))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Major auxilliaries ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun logger-mode ()
  "Major mode for logging activities.
\\{logger-mode-map}"
  (setq major-mode 'logger-mode
        mode-name "Logger")
  (use-local-map logger-mode-map)
  (run-hooks 'logger-mode-hooks))

(defun logger-load-db (&optional file)
  (or file (setq file logger-db-file))
  (or (file-exists-p file)
      (if (y-or-n-p (format "Logger file %s does not exist.  Create it? "
                            file))
          (logger-write-db file)
        (error "Logger: data base not found.")))
  (load-file file))

(defun logger-display ()
  ; Display the logger in the current buffer.
  (let ((buffer-read-only nil) laststart)
    (erase-buffer)
    (let ((times logger-times-db) (count logger-display-count))
      (while (and times (> count 0))
        (insert "\n")
        (backward-char 1)
        (logger-redisplay-record (car times) laststart)
        (goto-char (point-max))
        (setq laststart (logger-record-time (car times))
              times (cdr times)
              count (1- count)))))
  (goto-char 0))

(defun logger-redisplay-record (rec endtime)
  ; Redisplay RECORD on the current line.  ENDTIME is the start time of the
  ; next activity (the one before this one in the list), or nil.
  (let ((buffer-read-only nil))
    (beginning-of-line nil)
    (delete-region (point) (progn (end-of-line nil) (point)))
    (insert (format (car logger-display-format)
                    (format-time-string logger-time-format
                                        (logger-record-time rec))
                    (logger-duration-string (logger-record-time rec)
                                            endtime)
                    (logger-activity-string (logger-record-actno rec))
                    (or (logger-record-descr rec) "")))
    (beginning-of-line nil)))

(defun logger-activity-string (activity)
  ; Return the activity string for the numbered ACTIVITY.
  (or (and activity
           (cdr-safe (nth activity logger-activities-db)))
      "<no activity>"))

(defun logger-duration-string (start end)
  ; Return a string giving the time (hh:mm:ss) between START and END,
  ; or if END is t, return string for START.
  (if (null end)
      " ??:??"
    (let (secs hours mins)
      (if (consp end)
          (setq secs (logger-time-diff end start))
        (setq secs start))
      (setq mins (/ secs 60)
            secs (% secs 60)
            hours (/ mins 60)
            mins (% mins 60))      
      (format "%3d:%02d" hours mins secs))))

(defun logger-get-activity-number (code)
  ; Return the activity number corresponding to CODE.
  (setq code (logger-normalize-code code))
  (let ((acts logger-activities-db) (count 0) actno)
    (while acts
      (if (eq (car (car acts)) code)
          (setq actno count acts nil)
        (setq count (1+ count) acts (cdr acts))))
    (or actno
        (error "No registered activity has code %s"
               (logger-code-string code)))))

(defun logger-normalize-code (code)
  ; Return an upcase letter for CODE, or nil.
  (and code
       (setq code (upcase code))
       (>= code ?A)
       (<= code ?Z)
       code))

(defun logger-code-string (code)
  ; Return a 1-character string describing CODE.
  (char-to-string (or code ?-)))

(defun logger-check-duplicate-code (code)
  ; check that CODE is not already an activity code.
  ; if it is, steal it or raise an error.
  (if code
      (let (oldcode)
        (if (setq oldcode (and code (assoc code logger-activities-db)))
            (if (y-or-n-p (format "Steal code %s from `%s'? "
                                  (logger-code-string code)
                                  (cdr oldcode)))
                (logger-set-activity-code (char-to-string (car oldcode)) nil)
              (error "Code %s is already in use."
                     (logger-code-string code)))))))

(defun logger-edit-time-field (f)
  (let ((fnum (nth 1 (assoc f logger-time-fields)))
        (allf (cdr logger-display-format))
        (buffer-read-only nil)
        spt ept scol ecol)
    (while (not (eq (nth 2 (car allf)) 'start-time))
      (setq allf (cdr allf)))
    (setq scol (nth 0 (car allf))
          ecol (nth 1 (car allf)))
    (setq logger-edit-time-mode (concat " Edit " (capitalize (symbol-name f)))
          logger-edit-time-field fnum
          logger-edit-time-increments (cdr (assoc f logger-time-increments)))
    (beginning-of-line nil)
    (setq spt (point))
    (end-of-line nil)
    (setq ept (point))
    (remove-text-properties spt ept '(face nil))
    (setq scol (or (nth 2 (nth fnum logger-time-fields)) scol)
          ecol (or (nth 2 (nth (1+ fnum) logger-time-fields)) ecol))
    (add-text-properties (+ spt scol) (+ spt ecol) '(face highlight))
))

(defun logger-record (time actno descr)
  "Return a logger record."
  (list time actno descr))

(defun logger-set-record-actno (rec actno)
  "Set (by side-effect) the activity number of REC to ACTNO."
  (if (consp (cdr rec))
      (rplaca (cdr rec) actno)
    ;; backward compatibility
    (rplacd rec (list actno))))

(defun logger-set-record-description (rec descr)
  "Set (by side-effect) the activity description of REC to DESCR."
  (if (consp (cdr rec))
      (rplacd (cdr rec) (list descr))
    ;; backward compatibility
    (rplacd rec (list (cdr rec) descr))))

(defun logger-record-time (rec)
  "Return the start time of REC."
  (nth 0 rec))

(defun logger-record-actno (rec)
  "Return the activity number of REC."
  (if (consp (cdr rec))
      (nth 1 rec)
    (cdr rec)))

(defun logger-record-descr (rec)
  "Return the activity description of REC."
  (if (consp (cdr rec))
      (nth 2 rec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Minor auxilliaries ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun logger-compare-times (tim1 tim2)
  (or (> (nth 0 (car tim1)) (nth 0 (car tim2)))
      (and (= (nth 0 (car tim1)) (nth 0 (car tim2)))
           (> (nth 1 (car tim1)) (nth 1 (car tim2))))
      (and (= (nth 0 (car tim1)) (nth 0 (car tim2)))
           (= (nth 1 (car tim1)) (nth 1 (car tim2)))
           (> (nth 1 (car tim1)) (nth 1 (car tim2))))))

(defun logger-time-diff (end start)
  "Return the difference in seconds between time structures END and START."
  (+ (* (lsh 1 16) (- (car end) (car start)))
     (- (car (cdr end)) (car (cdr start)))))

(defun logger-set-time (time high low)
  "Set high and low words of TIME structure to HIGH and LOW.
Normalize high and low first.  TIME is altered by side-effect."
  (let (xhi (fac (lsh 1 16)))
    (if (< low 0)
        (setq xhi  (1- (/ low fac))
              high (+ high xhi)
              low  (- low (* xhi fac)))
      (if (> low fac)
          (setq xhi  (/ low fac)
                high (+ high xhi)
                low  (- low (* xhi fac)))))
    (rplaca time high)
    (rplaca (cdr time) low)
    time))

(provide 'logger)

;;; end of file logger.el
