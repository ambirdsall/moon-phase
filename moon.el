;; (lunar-phases) logs its business with (message) and in scripts, (message) prints to stderr.
;; No thanks.
(setq inhibit-message t)

(defun stdout (string &optional newline)
  "Prints STRING to stdout, optionally appending a newline."
  (princ string)
  (and newline (princ "\n")))

;; TODO: port calendar-extract-month for emacs 22 compatibility
(require 'calendar)
(require 'solar)
(require 'cal-dst)

;; seq functions adapted from seq.el (seq-24.el, to be specific); this library isn't included in
;; emacs 22, which is the version that ships with macOS, so inlining the definitions makes this
;; script more portable
(defun seq-filter (predicate sequence)
  "Return a list of all the elements for which (PREDICATE element) is non-nil in SEQUENCE."
  (let ((exclude (make-symbol "exclude")))
    (delq exclude (mapcar (lambda (elt)
                             (if (funcall predicate elt)
                                 elt
                               exclude))
                           sequence))))

;; (defmacro seq-doseq (spec &rest body)
;;   "Loop over a sequence.
;; Similar to `dolist' but can be applied to lists, strings, and vectors.
;; Evaluate BODY with VAR bound to each element of SEQ, in turn.
;; \(fn (VAR SEQ) BODY...)"
;;   (declare (indent 1) (debug ((symbolp form &optional form) body)))
;;   (let ((length (make-symbol "length"))
;;         (seq (make-symbol "seq"))
;;         (index (make-symbol "index")))
;;     `(let* ((,seq ,(cadr spec))
;;             (,length (if (listp ,seq) nil (seq-length ,seq)))
;;             (,index (if ,length 0 ,seq)))
;;        (while (if ,length
;;                   (< ,index ,length)
;;                 (consp ,index))
;;          (let ((,(car spec) (if ,length
;;                                 (prog1 (seq-elt ,seq ,index)
;;                                   (setq ,index (+ ,index 1)))
;;                               (pop ,index))))
;;            ,@body)))))

(defun seq-find (predicate sequence &optional default)
  "Return the first element for which (PREDICATE element) is non-nil in SEQUENCE.
If no element is found, return DEFAULT.
Note that `seq-find' has an ambiguity if the found element is
identical to DEFAULT, as it cannot be known if an element was
found or not."
  (catch 'seq--break
    (dolist (elt sequence)
      (when (funcall predicate elt)
        (throw 'seq--break elt)))
    default))
;; calendar-astro-to-absolute and v versa are cal-autoloads.
;;;(require 'cal-julian)

(setq lunar-phase-names '("New Moon" "Waxing Crescent Moon" "First Quarter Moon" "Waxing Gibbous Moon" "Full Moon" "Waning Gibbous Moon" "Last Quarter Moon" "Waning Crescent Moon"))

(defun lunar-phase (index)
  "Local date and time of lunar phase INDEX.
Integer below INDEX/8 gives the lunation number, counting from
Jan 1, 1900; remainder mod 8 gives the phase: 0 new moon, 1
waxing crescent, 2 first quarter, 3 waxing gibbous, 4 full moon,
5 waning gibbous, 6 last quarter, 7 waning crescent. Returns a
list (DATE TIME PHASE)."
  (let* ((phase (mod index 8))
         (index (/ index 8.0))
         (time (/ index 1236.85))
         (date (+ (calendar-absolute-from-gregorian '(1 0.5 1900))
                  0.75933
                  (* 29.53058868 index) ; FIXME 29.530588853?
                  (* 0.0001178 time time)
                  (* -0.000000155 time time time)
                  (* 0.00033
                     (solar-sin-degrees (+ 166.56
                                           (* 132.87 time)
                                           (* -0.009173 time time))))))
         (sun-anomaly (mod
                       (+ 359.2242
                          (* 29.105356 index)
                          (* -0.0000333 time time)
                          (* -0.00000347 time time time))
                       360.0))
         (moon-anomaly (mod
                        (+ 306.0253
                           (* 385.81691806 index)
                           (* 0.0107306 time time)
                           (* 0.00001236 time time time))
                        360.0))
         (moon-lat (mod
                    (+ 21.2964
                       (* 390.67050646 index)
                       (* -0.0016528 time time)
                       (* -0.00000239 time time time))
                    360.0))
         (adjustment
          (if (memq phase '(0 2))
              (+ (* (- 0.1734 (* 0.000393 time))
                    (solar-sin-degrees sun-anomaly))
                 (* 0.0021 (solar-sin-degrees (* 2 sun-anomaly)))
                 (* -0.4068 (solar-sin-degrees moon-anomaly))
                 (* 0.0161 (solar-sin-degrees (* 2 moon-anomaly)))
                 (* -0.0004 (solar-sin-degrees (* 3 moon-anomaly)))
                 (* 0.0104 (solar-sin-degrees (* 2 moon-lat)))
                 (* -0.0051 (solar-sin-degrees (+ sun-anomaly moon-anomaly)))
                 (* -0.0074 (solar-sin-degrees (- sun-anomaly moon-anomaly)))
                 (* 0.0004 (solar-sin-degrees (+ (* 2 moon-lat) sun-anomaly)))
                 (* -0.0004 (solar-sin-degrees (- (* 2 moon-lat) sun-anomaly)))
                 (* -0.0006 (solar-sin-degrees
                             (+ (* 2 moon-lat) moon-anomaly)))
                 (* 0.0010 (solar-sin-degrees (- (* 2 moon-lat) moon-anomaly)))
                 (* 0.0005 (solar-sin-degrees
                            (+ (* 2 moon-anomaly) sun-anomaly))))
            (+ (* (- 0.1721 (* 0.0004 time))
                  (solar-sin-degrees sun-anomaly))
               (* 0.0021 (solar-sin-degrees (* 2 sun-anomaly)))
               (* -0.6280 (solar-sin-degrees moon-anomaly))
               (* 0.0089 (solar-sin-degrees (* 2 moon-anomaly)))
               (* -0.0004 (solar-sin-degrees (* 3 moon-anomaly)))
               (* 0.0079 (solar-sin-degrees (* 2 moon-lat)))
               (* -0.0119 (solar-sin-degrees (+ sun-anomaly moon-anomaly)))
               (* -0.0047 (solar-sin-degrees (- sun-anomaly moon-anomaly)))
               (* 0.0003 (solar-sin-degrees (+ (* 2 moon-lat) sun-anomaly)))
               (* -0.0004 (solar-sin-degrees (- (* 2 moon-lat) sun-anomaly)))
               (* -0.0006 (solar-sin-degrees (+ (* 2 moon-lat) moon-anomaly)))
               (* 0.0021 (solar-sin-degrees (- (* 2 moon-lat) moon-anomaly)))
               (* 0.0003 (solar-sin-degrees
                          (+ (* 2 moon-anomaly) sun-anomaly)))
               (* 0.0004 (solar-sin-degrees
                          (- sun-anomaly (* 2 moon-anomaly))))
               (* -0.0003 (solar-sin-degrees
                           (+ (* 2 sun-anomaly) moon-anomaly))))))
         (adj (+ 0.0028
                 (* -0.0004 (solar-cosine-degrees
                             sun-anomaly))
                 (* 0.0003 (solar-cosine-degrees
                            moon-anomaly))))
         (adjustment (cond ((= phase 1) (+ adjustment adj))
                           ((= phase 2) (- adjustment adj))
                           (t adjustment)))
         (date (+ date adjustment))
         (date (+ date (/ (- calendar-time-zone
                             (solar-ephemeris-correction
                              (calendar-extract-year
                               (calendar-gregorian-from-absolute
                                (truncate date)))))
                          60.0 24.0)))
         (time (* 24 (- date (truncate date))))
         (date (calendar-gregorian-from-absolute (truncate date)))
         (adj (dst-adjust-time date time)))
    (list (car adj) (apply 'solar-time-string (cdr adj)) phase)))

(defconst lunar-cycles-per-year 12.3685 ; 365.25/29.530588853
  "Mean number of lunar cycles per 365.25 day year.")

;; FIXME new-moon index; use in lunar-phase-list implies always below.
(defun lunar-index (date)
  "Return the lunar index for Gregorian date DATE.
This is 8 times the approximate number of new moons since 1 Jan 1900.
The factor of 8 allows (mod INDEX 8) to represent the eight phases."
  (* 8 (truncate
        (* lunar-cycles-per-year
           ;; Years since 1900, as a real.
           (+ (calendar-extract-year date)
              (/ (calendar-day-number date) 366.0)
              -1900)))))

;; TODO: create alternate fn for this which only generates current phase
(defun lunar-phase-list (month year)
  "List of lunar phases for three months starting with Gregorian MONTH, YEAR."
  (let* ((index (lunar-index (list month 1 year)))
         (new-moon (lunar-phase index))
         (end-date (let ((end-month month)
                         (end-year year))
                     (calendar-increment-month end-month end-year 3)
                     (list (list end-month 1 end-year))))
         ;; Alternative for start-date:
;;;         (calendar-gregorian-from-absolute
;;;          (1- (calendar-absolute-from-gregorian (list month 1 year))))
         (start-date (progn
                       (calendar-increment-month month year -1)
                       (list (list month
                                   (calendar-last-day-of-month month year)
                                   year))))
         list)
    (while (calendar-date-compare new-moon end-date)
      (if (calendar-date-compare start-date new-moon)
          (setq list (append list (list new-moon))))
      (setq index (1+ index)
            new-moon (lunar-phase index)))
    list))

(defun lunar-phase-name (phase)
  "Name of lunar PHASE.
0 = new moon, 1 = first quarter, 2 = full moon, 3 = last quarter."
  (message (concat "phase " (number-to-string phase) " in lunar-phase-name: " (nth phase lunar-phase-names)))
  (nth phase lunar-phase-names))

(defvar displayed-month)                ; from calendar-generate
(defvar displayed-year)

(defun calendar-lunar-phases (&optional event)
  "Create a buffer with the lunar phases for the current calendar window.
If EVENT is non-nil, it's an event indicating the buffer position to
use instead of point."
  (interactive (list last-nonmenu-event))
  ;; If called from a menu, with the calendar window not selected.
  (with-current-buffer
      (if event (window-buffer (posn-window (event-start event)))
        (current-buffer))
    (message "Computing phases of the moon...")
    (let ((m1 displayed-month)
          (y1 displayed-year)
          (m2 displayed-month)
          (y2 displayed-year))
      (calendar-increment-month m1 y1 -1)
      (calendar-increment-month m2 y2 1)
      (calendar-in-read-only-buffer lunar-phases-buffer
       (calendar-set-mode-line
         (if (= y1 y2)
             (format "Phases of the Moon from %s to %s, %d%%-"
                     (calendar-month-name m1) (calendar-month-name m2) y2)
           (format "Phases of the Moon from %s, %d to %s, %d%%-"
                   (calendar-month-name m1) y1 (calendar-month-name m2) y2)))
        (insert
         (mapconcat
          (lambda (x)
            (format "%s: %s %s" (calendar-date-string (car x))
                    (lunar-phase-name (nth 2 x))
                    (cadr x)))
          (lunar-phase-list m1 y1) "\n")))
      (message "Computing phases of the moon...done"))))

(defun lunar-phases (&optional arg)
  "Display the quarters of the moon for last month, this month, and next month.
If called with an optional prefix argument ARG, prompts for month and year.
This function is suitable for execution in an init file."
  (interactive "P")
  (save-excursion
    (let* ((date (if arg (calendar-read-date t)
                   (calendar-current-date)))
           (displayed-month (calendar-extract-month date))
           (displayed-year (calendar-extract-year date)))
      (calendar-lunar-phases))))

(defvar date)


(setq phase-emojis '(("New" . "🌑")
                     ("Waxing Crescent" . "🌒")
                     ("First Quarter" . "🌓")
                     ("Waxing Gibbous" . "🌔")
                     ("Full" . "🌕")
                     ("Waning Gibbous" . "🌖")
                     ("Last Quarter" . "🌗")
                     ("Waning Crescent" . "🌘")))

(lunar-phases)
(switch-to-buffer "*Phases of Moon*")

;; seems like it should work, but giving incorrect result:
;; (lunar-phase-name (nth 2 (lunar-phase (lunar-index (calendar-current-date)))))

(let* ((date (calendar-current-date))
       (year (number-to-string (calendar-extract-year date)))
       (current-month (calendar-month-name (calendar-extract-month date)))
       (last-month (calendar-month-name (1- (calendar-extract-month date))))
       (current-day (number-to-string (calendar-extract-day (calendar-current-date))))

       (phase-list (split-string (buffer-string) "\n"))
       ;; TODO interpolate dates to add in waxing/waning crescent/gibbous
       (current-month-phases (seq-filter (lambda (phase-string)
                                           (string-match current-month phase-string))
                                         phase-list))
       (current-phase-with-date (or
                                 ;; maybe today is one of the listed dates?
                                 (seq-find (lambda (phase)
                                             (let ((date-matcher (concat " " current-day ", " year)))
                                               (string-match date-matcher phase)))
                                           current-month-phases)

                                 ;; if not, maybe the last-specified phase was this month?
                                 (let ((earlier-phases-this-month
                                        (seq-filter (lambda (phase)
                                                      (let ((phase-date (string-to-number
                                                                         (and (string-match "[0-9]+" phase)
                                                                              (match-string 0 phase)))))
                                                        (< phase-date (string-to-number current-day))))
                                                    current-month-phases)))
                                   (car (last earlier-phases-this-month)))

                                 ;; if not, get last phase of last month
                                 (car (last (seq-filter (lambda (phase-string)
                                                          (string-match last-month phase-string))
                                                        phase-list)))
                                 ))

       (current-phase-name (substring (car (split-string
                                            (cadr (split-string current-phase-with-date ":"))
                                            " Moon"))
                                      1))
       (current-phase (cdr (assoc current-phase-name phase-emojis))))
  (stdout current-phase))
