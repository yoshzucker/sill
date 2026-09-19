;;; sill.el --- One mode line at the foot of the frame  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/sill
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, frames

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A mode line costs one row of every window that has one.  Four windows,
;; four rows, and the same answer four times over -- because only one of
;; those lines is about the window being worked in, and the other three
;; describe places the eye is not.
;;
;; So this draws one, on a row of its own at the foot of the frame, about
;; whichever window is selected; and takes the per-window ones away.  With
;; four windows that is three rows back, with eight it is seven.  The row
;; it costs is the same row however many windows there are, which is the
;; whole of the idea.
;;
;; What it draws is not decided here.  `sill-format' is a mode-line format
;; like any other and is rendered against the selected window, so whatever
;; a configuration had built for `mode-line-format' arrives unchanged --
;; faces, glyphs and all.
;;
;; The row is an ordinary window, at the frame's bottom side, and the
;; reason it is not felt as one is a handful of window parameters: nothing
;; selects it, nothing deletes it, nothing displays into it, and nothing
;; resizes it.  The alternative -- the echo area -- costs no row at all,
;; and loses the line to every message and every minibuffer prompt.

;;; Code:

(require 'seq)
;; `make-glyph-code' and the display-table accessors live here.
(require 'disp-table)

(defgroup sill nil
  "One mode line at the foot of the frame."
  :group 'convenience
  :prefix "sill-")

(defcustom sill-format nil
  "What the sill shows, as a mode-line format.

Rendered against the selected window, so `mode-line-window-selected-p' and
anything else that asks about the window answer about that one.

Nil means the mode line the frame would otherwise have drawn -- the default
`mode-line-format' as it stood when `sill-mode' was turned on, which the
mode then empties.  That is what makes turning this on a change of place
rather than of content."
  :type 'sexp)

(defcustom sill-face 'mode-line
  "Face the sill's row is filled out to the frame's width with.

The row Emacs draws for a mode line is filled to the end by Emacs itself.
A window is not: what a buffer does not cover is the default background.
So the rest of the row is painted here, and this is what paints it."
  :type 'face)

(defconst sill--buffer-parameter 'sill-buffer
  "Frame parameter holding that frame's sill buffer.")

(defconst sill--window-parameter 'sill-window
  "Frame parameter holding the window that frame's sill was last about.")

(defvar sill--saved-default nil
  "The default `mode-line-format' from before `sill-mode' emptied it.")

;; Defined at the foot of the file by `define-minor-mode', and read above it.
(defvar sill-mode)

(defun sill--eligible-window-p (window)
  "Non-nil when WINDOW is one a sill can be about."
  (and (window-live-p window)
       (not (window-minibuffer-p window))
       (not (window-parameter window 'sill))))

(defun sill--subject (frame)
  "Return the window FRAME's sill should be about, or nil.

FRAME\='s own selected window and not the selected one, because every frame
has a sill and each is about its own.

Remembered, because the selected window is not always one worth reporting:
a prompt selects the minibuffer, and the answer while it is open is still
the window the reader came from."
  (let ((selected (frame-selected-window frame)))
    (when (sill--eligible-window-p selected)
      (set-frame-parameter frame sill--window-parameter selected))
    (let ((remembered (frame-parameter frame sill--window-parameter)))
      (and (sill--eligible-window-p remembered) remembered))))

(defun sill--buffer (frame)
  "Return FRAME's sill buffer, making it if there is none."
  (let ((buffer (frame-parameter frame sill--buffer-parameter)))
    (unless (buffer-live-p buffer)
      (setq buffer (generate-new-buffer " *sill*"))
      (with-current-buffer buffer
        ;; The line arrives carrying its own faces.  Font lock would paint
        ;; over them, and undo would keep every version of a line that is
        ;; rewritten on every keystroke.
        (font-lock-mode -1)
        (buffer-disable-undo)
        (setq-local mode-line-format nil
                    header-line-format nil
                    cursor-type nil
                    cursor-in-non-selected-windows nil
                    word-wrap nil
                    truncate-lines t
                    show-trailing-whitespace nil)
        ;; A line that does not fit is marked as cut off, and where that
        ;; mark goes depends on the fringes: in one when there is one, and
        ;; in the last column of the text when there is not.  This window
        ;; has none -- they would be a notch in the band -- so the mark
        ;; would be a `$' sitting in the corner of it, and staying there.
        ;;
        ;; The glyph is the display table's to decide.  A space in the
        ;; sill's own face is a mark that says the same thing about a row
        ;; nobody can read the end of anyway: nothing.
        (setq-local buffer-display-table (make-display-table))
        (set-display-table-slot buffer-display-table 'truncation
                                (make-glyph-code ?\s sill-face)))
      (set-frame-parameter frame sill--buffer-parameter buffer))
    buffer))

(defun sill--window-of (frame)
  "Return FRAME's sill window, or nil."
  (seq-find (lambda (window) (window-parameter window 'sill))
            (window-list frame 'never)))

(defun sill--make-window (frame)
  "Show FRAME's sill buffer on a row at the foot of FRAME.  Return the window."
  (let ((window (with-selected-frame frame
                  (display-buffer-in-side-window
                   (sill--buffer frame)
                   '((side . bottom)
                     (slot . 0)
                     (window-height . 1)
                     (preserve-size . (nil . t))
                     (window-parameters
                      ;; Nothing selects it, nothing deletes it, and it is
                      ;; not a place to put a mode line -- it is one.
                      (sill . t)
                      (no-other-window . t)
                      (no-delete-other-windows . t)
                      (mode-line-format . none)))))))
    (when (window-live-p window)
      ;; A row, and only ever a row.  Dedicated so `display-buffer' never
      ;; finds it; preserved so a resize elsewhere never takes from it.
      (set-window-dedicated-p window 'side)
      (set-window-parameter window 'quit-restore nil)
      (set-window-margins window 0 0)
      (set-window-fringes window 0 0)
      (set-window-scroll-bars window 0 nil 0 nil)
      (window-preserve-size window nil t))
    window))

(defun sill--render (window)
  "Return the line to show about WINDOW, reaching the right edge."
  (let ((format (or sill-format sill--saved-default)))
    (concat (format-mode-line format nil window (window-buffer window))
            ;; One space stretched to the edge, rather than as many spaces as
            ;; the width less what is already there.  Counting columns means
            ;; trusting `string-width' about every glyph in the line, and a
            ;; glyph a font draws wider than that says takes the line past
            ;; the edge -- where what appears is Emacs's continuation mark,
            ;; a `$' in the last column, and it stays for as long as the
            ;; line does.
            (propertize " " 'display '(space :align-to right)
                        'face sill-face))))

(defun sill--draw (frame)
  "Draw FRAME's sill."
  (let ((window (or (sill--window-of frame) (sill--make-window frame)))
        (subject (sill--subject frame)))
    (when (and (window-live-p window) subject)
      (with-current-buffer (window-buffer window)
        (let ((inhibit-read-only t)
              (line (sill--render subject)))
          ;; Only when it differs.  Writing into a buffer that is on the
          ;; screen is itself a change of window state, which is what asks
          ;; for the next update -- so a draw that always writes is a draw
          ;; that always asks to be done again.
          ;;
          ;; And properties count as a difference.  `equal' on strings
          ;; ignores them, so a line whose words are the same and whose
          ;; faces have changed would read as unchanged: the mode line of a
          ;; window that has just become the selected one, saying what it
          ;; said before in the colours of a window that is not.
          (unless (equal-including-properties line (buffer-string))
            (erase-buffer)
            (insert line))))
      ;; Held at the start, or a line longer than the frame would scroll the
      ;; row away and leave it blank.
      (set-window-point window (point-min))
      (set-window-start window (point-min)))))

(defun sill--hide-mode-lines (frame)
  "Take the mode line off every window of FRAME but the sill's own.

The default is already empty, which is what covers a window the moment it
is born: a window split off another does not inherit its parameters, so
anything done here would arrive a redisplay late and the new window would
show a mode line and then lose it.

This is for the rest -- a buffer that sets `mode-line-format' for itself
keeps its line whatever the default says, and one window in four still
drawing one is the arrangement at its worst."
  (dolist (window (window-list frame 'never))
    (unless (or (window-parameter window 'sill)
                ;; Only when it differs.  Setting the parameter asks for a
                ;; redisplay of that window, and this runs on every state
                ;; change there is.
                (eq 'none (window-parameter window 'mode-line-format)))
      (set-window-parameter window 'mode-line-format 'none))))

(defun sill--adopt (window &rest _)
  "Take the mode line off WINDOW as soon as it exists.  Return WINDOW.

Advice on `split-window', which every split goes through -- evil\='s, Org\='s,
`display-buffer\='s -- because a new window is the one case nothing else
reaches in time.  Window parameters are not inherited, so the window is
born with whatever its buffer says; and a buffer that sets
`mode-line-format' for itself says a mode line.  The update that would
take it off runs from a timer, so what the eye gets in between is a line
that appears on a fresh window and stays there until the next window is
selected.

Here instead, where it is one `set-window-parameter' in the command that
made the window, and never a frame late."
  (when (and sill-mode (window-live-p window))
    (set-window-parameter window 'mode-line-format 'none))
  window)

(defun sill--show-mode-lines (frame)
  "Give every window of FRAME its mode line back."
  (dolist (window (window-list frame 'never))
    (set-window-parameter window 'mode-line-format nil)))

(defvar sill--timer nil
  "The update waiting to happen, or nil.")

(defun sill--schedule (&optional _)
  "Ask for an update, and return at once.

What the update does -- make a window, take a mode line off another -- is
changing the window configuration.  The hooks that know a window changed
are run *by* redisplay, which is in the middle of doing exactly that, and
`window-size-change-functions' says in as many words that its functions
must not do it.  Done there it leaves the frame half drawn -- no tab bar,
no echo area, a minibuffer that never appears -- and then takes Emacs
down.

A timer runs in the command loop, where changing windows is the ordinary
thing to do.  So the hooks only ask, and the asking is one `run-at-time'."
  (unless sill--timer
    (setq sill--timer (run-at-time 0 nil #'sill--update))))

(defun sill--update ()
  "Bring every frame's sill up to date."
  (setq sill--timer nil)
  (dolist (frame (frame-list))
    (when (frame-live-p frame)
      (sill--hide-mode-lines frame)
      (sill--draw frame))))

(defun sill--teardown ()
  "Put every frame back the way it was."
  (dolist (frame (frame-list))
    (when-let ((window (sill--window-of frame)))
      (set-window-parameter window 'no-delete-other-windows nil)
      (delete-window window))
    (sill--show-mode-lines frame)
    (set-frame-parameter frame sill--window-parameter nil)
    (when-let ((buffer (frame-parameter frame sill--buffer-parameter)))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (set-frame-parameter frame sill--buffer-parameter nil))))

;;;###autoload
(define-minor-mode sill-mode
  "Draw one mode line at the foot of the frame, and none in the windows.

Both halves, because either alone is worse than neither: the windows keep
their rows and the frame grows one, or the frame says nothing about the
window being worked in."
  :global t
  :group 'sill
  (if sill-mode
      (progn
        ;; Emptied rather than overridden window by window, so that a window
        ;; has no mode line from the moment it exists.  A window parameter
        ;; can only be set on a window that is already there, and by then it
        ;; has been drawn once.
        (setq sill--saved-default (default-value 'mode-line-format))
        (setq-default mode-line-format nil)
        (advice-add 'split-window :filter-return #'sill--adopt)
        (add-hook 'window-state-change-hook #'sill--schedule)
        (add-hook 'after-make-frame-functions #'sill--schedule)
        (sill--update))
    (advice-remove 'split-window #'sill--adopt)
    (remove-hook 'window-state-change-hook #'sill--schedule)
    (remove-hook 'after-make-frame-functions #'sill--schedule)
    (when sill--timer
      (cancel-timer sill--timer)
      (setq sill--timer nil))
    (setq-default mode-line-format sill--saved-default)
    (sill--teardown)))

(provide 'sill)

;;; sill.el ends here
