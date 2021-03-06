;;; Program to format a buffer as verse, for PostScript
;;; Time-stamp: <2015-05-29 21:38:46 jcgs>

;; Copyright (C) 2007, 2009, 2015  John Sturdy

;; Author: John Sturdy <jcgs@hosea>
;; Keywords: multimedia

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;; Program to format a buffer as verse, for PostScript
;;; Simple troff-like text formatting, for non-filled non-justified text
;;; such as verses and short notices.

(setq verse-verbose nil)

(defvar verse-prologue-file "~/open-projects/ps-verse/ps-verse-el.pro"
  "The preamble.")

(defvar verse-font-area-marker "% Font loading area:"
  "Marker for where to put downloaded fonts")

(defvar verse-centred nil
  "Whether we are drawing the text centred.")

(defvar verse-x-centre 300
  "The centre position, for centred text.")

(defun verse-directive-title (title-string)
  "Print STRING centred as the verse title, possibly in a different size from
the rest of the text."
  (if (not hide-titles)
      (progn
        (princ (format "gsave /%s findfont %d scalefont setfont\n"
                       verse-title-font verse-title-font-size))
        (princ (format
                "%d %d moveto (%s) dup\nstringwidth pop 2 div neg 0 rmoveto show\n"
                (+ verse-left-margin
                   (/ (- verse-page-width verse-left-margin) 2))
                verse-current-line-place
                title-string))
        (princ "grestore\n")
        (setq verse-current-line-place (- verse-current-line-place
                                          (* 2 verse-line-separation))))))

(defun verse-line (line-text)
  "Print LINE-TEXT starting indented from the left margin as indicated
by any leading spaces and the current indent scaling, onto."
  (if (< verse-current-line-place 0)
      (verse-new-page ""))

  (if verse-centred
      (let ((line-text (buffer-substring-no-properties
			(line-beginning-position)
			(line-end-position))))
	(princ (format "%d %d moveto (%s) cs\n"
		       verse-x-centre
		       verse-current-line-place
		       line-body)))
    (let ((line-indent (string-match "[^ ]" line-text)))
      (if (null line-indent) (setq line-indent 0))
      (let ((line-body (substring line-text line-indent))
	    (x-pos (+ verse-left-margin
		      (* line-indent verse-indentation-step))))
	(if verse-verbose
	    (progn
	      (message "Reservation: bottom %d (current place %d) width %d"
		       (if verse-reservation-bottom verse-reservation-bottom 0)
		       verse-current-line-place
		       verse-reservation-width)
	      (sit-for 2)))
	(if (and verse-reservation-bottom
		 (> verse-current-line-place verse-reservation-bottom))
	    (setq x-pos (+ x-pos verse-reservation-width)))
	(princ (format "%d %d moveto (%s) s\n"
		       x-pos
		       verse-current-line-place
		       line-body)))))
  (setq verse-current-line-place
	(- verse-current-line-place verse-line-separation)))

(defun verse-set-output-font (fontname fontsize)
  "Set the output font to FONTNAME and size to FONTSIZE."
  (when (or (not (string= fontname verse-font))
	    (not (= fontsize verse-font-size)))
    (setq verse-font fontname
	  verse-font-size fontsize)
    (message "Looking for %S in %S" verse-font verse-loaded-fonts)
    (unless (member verse-font verse-loaded-fonts)
      (message "Loading %S" verse-font)
      (save-window-excursion		; load the font
	(set-buffer standard-output)
	(save-excursion
	  (goto-char (point-min))
	  (search-forward verse-font-area-marker)
	  (insert "\n\n%%BeginFont " verse-font "\n")

	  (let ((font-file (expand-file-name verse-font verse-font-directory)))
	    (if (not (file-exists-p font-file))
		(setq font-file (concat font-file ".ps")))
	    (insert-file font-file)
	    (goto-char (mark))
	    )
	  (insert "\n%%EndFont\n")
	  )
	(setq verse-loaded-fonts
	      (cons verse-font verse-loaded-fonts))))
    (princ (format "/%s findfont %d scalefont setfont\n"
		   verse-font verse-font-size))))

(defun verse-set-size (size)
  "Set the size for printing verse to SIZE, telling a PostScript eater on
STANDARD-OUTPUT about the change."
  (verse-set-output-font verse-font size)
  (setq verse-line-separation (/ (* verse-font-size 5) 4))
  (setq stave-height (* verse-font-size 3))
  (princ "\n/stave_lh ")
  (princ verse-font-size)
  (princ " 2 div def\n")
  (setq verse-indentation-step verse-font-size))

(defun verse-directive-size (string)
  "Set the size of verse text, from a directive string."
  (verse-set-size (car (read-from-string string))))

(defun verse-directive-title-size (string)
  "Set the size of verse titles, from a directive string."
  (setq verse-title-font-size (car (read-from-string string))))

(defun verse-directive-set-margin (string)
  "Set the left margin for printing verse, from a directive string."
  (setq verse-left-margin (car (read-from-string string))))

(defun verse-directive-set-indent (string)
  "Set the indentation step for printing verse to that given in STRING."
  (setq verse-indentation-step
        (car (read-from-string string))))

(defun verse-directive-font (string)
  "Set the font for printing verse, from a command string."
  (verse-set-output-font (symbol-name (car (read-from-string string)))
                         verse-font-size))

(defun verse-directive-title-font (string)
  "Set the font for printing titles, from a command string."
  (setq verse-title-font (symbol-name (car (read-from-string string)))))

(defun verse-new-page (string)
  "Go onto a new page of verse, using STRING for the title."
  (if (> verse-page-number 0)
      (progn
	(princ "showpage ")
	(princ "\npagesave restore\n")))
  (setq
   verse-current-line-place (- verse-page-top verse-top-margin)
   verse-page-number (1+ verse-page-number)
   verse-reservation-width 0
   verse-reservation-bottom nil
   )
  (princ (format "\n%%%%Page: verse-%s %d\n" string verse-page-number)
         )
  (princ "save /pagesave exch def\n")
  (princ (format "/fontscale %d def\n" verse-font-size))
  (princ (format "/%s findfont %d scalefont setfont\n"
                 verse-font verse-font-size)
         )
  (princ "/kernstring (; ")
  (princ verse-kern-string)
  (princ ") def\n")
  (if verse-kern
      (progn
        (princ "/s /ks load def\n"))
    (progn
      (princ "/s /show load def\n"))))

(defun verse-directive-new-page (string)
  (verse-new-page string))

(defun verse-directive-font-directory (string)
  "Set the directory for loading exotic fonts from."
  (setq verse-font-directory string))

(defun verse-yes-or-no (string)
  "Return whether STRING contains yes or no."
  (if (string-match "yes" string)
      t
    (if (string-match "no" string)
        nil
      (error "Neither yes nor no!"))))

(defun verse-directive-hide-titles (string)
  "Determine whether titles are to be shown."
  (setq hide-titles (verse-yes-or-no string)))

(defun verse-directive-hide-chars (string)
  "Determine whether chars are to be shown."
  (setq hide-chars (verse-yes-or-no string)))

(defun verse-box (string drawbox)
  "Reserve or draw a box as described by STRING and DRAWBOX.
If the string contains no parameters, no reservation or box
is made."
  (if (string-match " *\\([-0-9]+\\) \\([-0-9]+\\)" string)
      (progn
        (setq verse-reservation-width
              (string-to-int (substring string (match-beginning 1) (match-end 1)))
              verse-reservation-bottom
              (- verse-current-line-place
                 (string-to-int (substring string (match-beginning 2) (match-end 2)))))
        (if verse-verbose (progn (message "Set reservation: %d %d"
                                          verse-reservation-width
                                          verse-reservation-bottom) (sit-for 2)))

        ;; now draw the box
        (if drawbox
            (princ
             (format
              "newpath %d %d moveto %d %d lineto %d %d lineto %d %d lineto closepath stroke\n"

              (+ verse-left-margin verse-box-inwardness)
              (+ verse-reservation-bottom verse-line-separation
                 verse-box-inwardness)

              (+ verse-left-margin verse-box-inwardness)
              (- (+ verse-current-line-place verse-line-separation) verse-box-inwardness)

              (- (+ verse-left-margin verse-reservation-width) verse-box-inwardness)
              (- (+ verse-current-line-place verse-line-separation) verse-box-inwardness)

              (- (+ verse-left-margin verse-reservation-width) verse-box-inwardness)
              (+ verse-reservation-bottom verse-line-separation verse-box-inwardness)

              )))
        )
    (progn
      (setq verse-reservation-width 0
            verse-reservation-bottom nil)
      (if verse-verbose (progn (message "Cancelled reservation") (sit-for 2))))))

(defun verse-directive-reserve-box (string)
  "Reserve a box as described by STRING. If the string contains no
parameters, no reservation is made."
  (verse-box string nil))

(defun verse-directive-box (string)
  "Draw a box as described by STRING. If the string contains no
parameters, no drawing is made."
  (verse-box string t))

(defun verse-directive-char (string)
  "Set a character as described in the STRING."
  (if (string-match " *\\([^ ]\\) +\\([0-9]+\\)\\(.*\\)$" string)
      (let ((char-string (substring string
                                    (match-beginning 1)
                                    (match-end 1)))
            (char-size (string-to-int (substring string
                                                 (match-beginning 2)
                                                 (match-end 2))))
            (remaining-str (substring string
                                      (match-beginning 3)
                                      (match-end 3)))
            (char-x-offset 0)
            (char-y-offset 0)
            )
        (if (string-match " *\\([-0-9]+\\) +\\([-0-9]+\\)\\(.*\\)$" remaining-str)
            (progn
              (setq char-x-offset (string-to-int (substring remaining-str
                                                            (match-beginning 1)
                                                            (match-end 1)))
                    char-y-offset (string-to-int (substring remaining-str
                                                            (match-beginning 2)
                                                            (match-end 2))))
              (if verse-verbose (progn (message "Set char offset %d %d"
                                                char-x-offset
                                                char-y-offset) (sit-for 2)))
              (verse-box (substring remaining-str
                                    (match-beginning 3)
                                    (match-end 3))
                         verse-box-chars)))
        (if (not hide-chars)
            (princ (format
                    "gsave %d %d moveto /%s findfont %d scalefont setfont (%s) show grestore\n"
                    (- verse-left-margin char-x-offset)
                    (- verse-current-line-place char-y-offset)
                    verse-font
                    char-size
                    char-string))))))

(defun verse-directive-box-chars (string)
  "Set whether capital letters should have boxes drawn around them."
  (setq verse-box-chars (verse-yes-or-no string)))

(defun verse-directive-box-inwardness (string)
  "Set how far in boxes are drawn from their size's edge."
  (setq verse-box-inwardness (string-to-int string)))

(defun verse-directive-kerning (string)
  "Switch kerning on or off."
  (setq verse-kern (verse-yes-or-no string))
  (if verse-kern
      (princ "/s /ks load def\n")
    (princ "/s /show load def\n")))

(defun verse-directive-kerns (string)
  "Set the kerning table."
  (setq verse-kern-string (concat string " " verse-kern-string))
  (if (> verse-page-number 0)
      (progn
	(princ "/kernstring (; ")
	(princ verse-kern-string)
	(princ ") def\n"))))

(defun verse-directive-postscript-directory (string)
  "Set the directory name for included postscript files."
  (setq verse-postscript-directory string))

(defun verse-directive-postscript-file (string)
  "Insert a copy of a named file of postscript in the output.
Argument STRING names the file."
  (save-window-excursion
    (message "postscript-file \"%s\" in \"%s\"" string verse-postscript-directory)
    (find-file (expand-file-name string verse-postscript-directory))
    (princ
     (buffer-string))))

(defun verse-directive-postscript (string)
  "Insert the given postscript in the output,
within gsave and grestore."
  (princ (format "\ngsave %d %d translate\n"
                 verse-left-margin
                 verse-current-line-place))
  (princ string)
  (princ "\ngrestore\n"))

(defun verse-directive-unprotected-postscript (string)
  "Insert the given postscript in the output,
without gsave and grestore."
  (princ "\n")				; ensure not in comment
  (princ string)
  (princ "\n"))

(defun verse-directive-vspace (string)
  "Move down by the given number of points."
  (setq verse-current-line-place
        (- verse-current-line-place (car (read-from-string string)))))

(defun verse-directive-column (string)
  "Start a new column of text."
  (setq verse-current-line-place  (- verse-page-top verse-top-margin)
	verse-left-margin (string-to-number string)))

(defun verse-directive-landscape (string)
  "Switch to landscape layout.
STRING is ignored."
  (princ "\n90 rotate 0 -800 translate\n"))

(defun verse-directive-reference (string)
  "Insert reference, in appropriate font."
   (princ "(")
   (princ string)
   (princ ") reference\n"))

(defun verse-directive-stave (string)
  "Make a stave STRING long."
  (princ (format "%d %d moveto %s stave\n"
		 verse-left-margin
		 verse-current-line-place
		 string))
  (setq verse-current-line-place
	(- verse-current-line-place stave-height)))

(defun ps-verse-uncomment-string (string)
  "Remove comments from STRING."
  (substring string 0 (string-match ";" string)))

(defun verse-directive-note (string)
  "Draw a note as described by STRING."
  (princ (ps-verse-uncomment-string string))
  (princ " note\n"))

(defun verse-directive-falling (string)
  "Draw a falling pair of notes as described by STRING."
  (princ (ps-verse-uncomment-string string))
  (princ " falling\n"))

(defun verse-directive-rising (string)
  "Draw a rising pair of notes as described by STRING."
  (princ (ps-verse-uncomment-string string))
  (princ " rising\n"))

(defun verse-directive-cpitch (string)
  "Mark the pitch of C, according to STRING."
  (princ string)
  (princ " cpitch\n"))

(defun verse-directive-hspace (string)
  "Move across by the given number of points."
  (princ string)
  (princ " 0 rmoveto\n"))

(defun verse-directive-flat (string)
  "Draw a flat sign, as described by STRING."
  (princ string)
  (princ " flat\n"))

(defun verse-centring (string)
  "Start or stop centring, as specified by STRING."
  (let ((n (string-to-number string)))
    (if (zerop n)
	(setq verse-centred nil)
      (setq verse-centred t
	    verse-x-centre n))))

(makunbound 'verse-directive-alist)

(defvar verse-directive-alist
  '(
    (title . verse-directive-title)
    (size . verse-directive-size)
    (title-size . verse-directive-title-size)
    (font . verse-directive-font)
    (title-font . verse-directive-title-font)
    (margin . verse-directive-set-margin)
    (indent . verse-directive-set-indent)
    (page . verse-directive-new-page)
    (column . verse-directive-column)
    (landscape . verse-directive-landscape)
    (font-directory . verse-directive-font-directory)
    (hide-titles . verse-directive-hide-titles)
    (hide-chars . verse-directive-hide-chars)
    (reserve-box . verse-directive-reserve-box)
    (box . verse-directive-box)
    (char . verse-directive-char)
    (box-chars . verse-directive-box-chars)
    (box-inwardness . verse-directive-box-inwardness)
    (kerning . verse-directive-kerning)
    (kerns . verse-directive-kerns)
    (postscript-file . verse-directive-postscript-file)
    (postscript . verse-directive-postscript)
    (unprotected-postscript . verse-directive-unprotected-postscript)
    (postscript-directory . verse-directive-postscript-directory)
    (vspace . verse-directive-vspace)
    (reference . verse-directive-reference)
    (hspace . verse-directive-hspace)
    (stave . verse-directive-stave)
    (note . verse-directive-note)
    (falling . verse-directive-falling)
    (rising . verse-directive-rising)
    (cpitch . verse-directive-cpitch)
    (flat . verse-directive-flat)
    (centre . verse-centring)
    )
  "Verse directive names and functions alist.")

(defun do-verse-directive (line)
  "Interpret the verse directive in LINE.
LINE begins with a dot, the directive introduction character."
  (let* ((directive-name-and-length (read-from-string line 1))
         (directive-function (cdr (assoc (car directive-name-and-length)
                                         verse-directive-alist))))
    (if (null directive-function)
        (message "Bad directive: %s" line )
      (let* ((arg-start (string-match "\\S-" line (cdr directive-name-and-length)))
	     (arg (substring line arg-start)))
	(message "calling %S on %S" (symbol-function directive-function) arg)
	(funcall (symbol-function directive-function)
		 arg)))))

(defconst semicolon (aref ";" 0))

(defun format-verse ()
  "Format the verse text in the current buffer into PostScript.
Writes the output to another buffer, of the same name with .ps
appended.
Lines beginning with a dot are formatting commands, other lines are
printed with neither filling nor justification.
Dot commands are:
  .title TITLE-TEXT        print the rest of the line as the title
  .size SIZE               set the font size to SIZE points.
  .title-size SIZE         set the title font size to SIZE points.
  .leading SIZE            set the distance between successive
                           baselines to SIZE
  .font FONT-NAME          set the font to FONT-NAME
  .title-font FONT-NAME    set the title font to FONT-NAME
  .margin WIDTH            set the left margin to WIDTH points
  .indent INDENT           set the indent per leading space to INDENT
  .page                    begin a new page
  .landscape               switch to landscape format
  .column X                begin a new column at X
  .font-directory DIR      say where to load fonts from
  .hide-titles NOYES       indicate whether to hide titles
  .reserve-box W H         reserve a box in the margin of size W H
  .char CHAR SIZE [ X Y [ W H ] ]
                           draw letter CHAR at SIZE, in the margin,
                           offsetting left/down by X Y if given, and
                           reserving a box of W H if given
  .box-chars NOYES         indicate whether to box big characters
  .box-inwardness N        draw boxes N points in from their
                           reserved edges
  .hide-chars NOYES        indicate whether to hide the results of .char
  .kerning NOYES           indicate whether to kern
  .kerns KERNSTRING        set the kerning table
You can save the results into a file by going into the output buffer (with
\\[switch-to-buffer] <buffer-name>.ps and using \\[write-file], or send them directly to
the printer from there with \\[print-buffer] (for which lpr-switches must
be set up to point to a PostScript printer)."
  (interactive)
  (setq
   ;; current font name and size
   verse-font "ZapfChancery-MediumItalic"
   verse-font-size 20

   ;; title font name and size
   verse-title-font "ZapfChancery-MediumItalic"
   verse-title-font-size 40
   hide-titles nil
   hide-chars nil

   ;; keeping track of fonts specially loaded
   verse-font-directory "~/Dropbox/psfonts"
   verse-loaded-fonts nil

   ;; margins
   verse-left-margin 72
   verse-top-margin 72

   ;; reservations for illuminated characters etc
   verse-reservation-width 0
   verse-reservation-bottom nil
   verse-box-chars nil
   verse-box-inwardness 1

   ;; the page size
   verse-page-top (* 11 72)
   verse-page-width (* 72 7)

   ;; leading
   verse-line-separation (/ (* verse-font-size 5) 4)
   stave-height (* verse-font-size 2)

   ;; the vertical position -- works down from the top of the page
;   verse-current-line-place (- verse-page-top verse-top-margin)
   verse-current-line-place -1

   ;; horizontal indentation
   verse-indentation-step verse-font-size

   ;; page numbering
   verse-page-number 0

   ;; kerning
   verse-kern nil
   verse-kern-string ""

   ;; postscript inclusions
   verse-postscript-directory nil

   ;; the prologue
   verse-prologue-string (save-window-excursion
                           (find-file verse-prologue-file)
                           (buffer-string))

   )
  (save-excursion
    (let ((standard-output
           (save-window-excursion
             (find-file (concat (buffer-name (current-buffer)) ".ps"))
             (current-buffer)))
          (in-stream (current-buffer)))
      (set-buffer standard-output)
      (erase-buffer)
      (insert "%!PS\n"
              "%%Creator: ps-verse\n"
              "%%For: " (user-real-login-name) " on " (system-name) "\n"
              "%%Pages: (atend)\n"
              "%%EndComments\n"
              "save /versesave exch def\n"
              verse-font-area-marker "\n\n")
      (insert verse-prologue-string)
      (insert
              "/s /show load def\n"
              "%%EndProlog\n"
              "/" verse-font " findfont "
              (int-to-string verse-font-size) " scalefont setfont 0 setgray\n"
              "/fontscale " (int-to-string verse-font-size) " def\n"
              )
      (set-buffer in-stream)
      (untabify (point-min) (point-max))
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line-start (point))
              (line-body nil)
              (line-end nil))
          (end-of-line 1)
          (setq line-end (point))
          (setq line-body (buffer-substring line-start line-end))
	  (message "Processing %s" line-body)
          (if (not (= (string-to-char line-body) semicolon))
                      (if (= (string-to-char line-body) ?.)
                          (do-verse-directive line-body)
                        (verse-line line-body)))
                   (forward-char 1)))
          (princ "showpage\npagesave restore\n%%Trailer\nversesave restore\n")
          (princ (format "%%%%Pages: %d\n"
                         verse-page-number))
          (save-window-excursion
            (set-buffer standard-output)
            (basic-save-buffer)))))

(defvar ps-verse-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'format-verse)
    map))

(defun ps-verse-mode ()
  "Major mode for verse."
  (interactive)
  (fundamental-mode)
  (use-local-map ps-verse-mode-map))

(provide 'ps-verse)
