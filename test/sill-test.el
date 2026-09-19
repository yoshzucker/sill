;;; sill-test.el --- Tests for sill  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run from the package root:
;;
;;   emacs --batch -Q -L . -l test/sill-test.el -f ert-run-tests-batch-and-exit
;;
;; Batch has no redisplay, so `format-mode-line' answers with the empty
;; string there.  What that function does is Emacs's business; what is
;; tested here is everything around it -- which window it is asked about,
;; and what becomes of the answer.

;;; Code:

(require 'ert)
(require 'seq)
(require 'sill)

(defmacro sill-test--with-windows (n &rest body)
  "Run BODY with N windows in the frame and sill turned off afterwards."
  (declare (indent 1))
  `(let ((sill-format '("")))
     (unwind-protect
         (progn
           (delete-other-windows)
           (dotimes (_ (1- ,n)) (split-window-below))
           (balance-windows)
           ,@body)
       (when sill-mode (sill-mode -1))
       (delete-other-windows))))

(defun sill-test--window ()
  "Return the sill's window, or nil."
  (seq-find (lambda (w) (window-parameter w 'sill)) (window-list nil 'never)))

(defun sill-test--others ()
  "Return every window but the sill's."
  (seq-remove (lambda (w) (window-parameter w 'sill)) (window-list nil 'never)))

(ert-deftest sill-test-one-row-and-no-others ()
  "Both halves of it: a row at the foot, and none in the windows.

Either alone is worse than neither.  The windows keeping their rows while
the frame grows one is a row lost, not gained; the frame saying nothing
about the window being worked in is the answer taken away."
  (sill-test--with-windows 3
    (sill-mode 1)
    (let ((sill (sill-test--window)))
      (should sill)
      (should (= 1 (window-height sill)))
      (should (eq 'bottom (window-parameter sill 'window-side)))
      ;; and the row is content: the sill has no mode line of its own
      (should (eq 'none (window-parameter sill 'mode-line-format)))
      (should (= 1 (window-body-height sill)))
      ;; every other window has lost its own
      (should (equal '(none none none)
                     (mapcar (lambda (w) (window-parameter w 'mode-line-format))
                             (sill-test--others)))))))

(ert-deftest sill-test-a-buffers-own-mode-line-goes-too ()
  "The window parameter and not the variable, so a buffer that sets
`mode-line-format' for itself loses its line like the rest.

One window in four still drawing a mode line is the arrangement at its
worst: the row is spent and the line it holds is about the wrong window."
  (sill-test--with-windows 2
    ;; A buffer of its own: a local value left on `*scratch*' is a value the
    ;; next test inherits, and a suite whose tests depend on their order is
    ;; a suite that passes for reasons nobody wrote down.
    (let ((buffer (generate-new-buffer "insistent")))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq-local mode-line-format '("insistent")))
            (set-window-buffer (frame-first-window) buffer)
            (sill-mode 1)
            (should (equal '(none none)
                           (mapcar (lambda (w)
                                     (window-parameter w 'mode-line-format))
                                   (sill-test--others)))))
        (kill-buffer buffer)))))

(ert-deftest sill-test-a-new-window-never-shows-one ()
  "A window split off another has no mode line from the moment it exists.

Window parameters are not inherited and the update runs from a timer, so
anything left to the update arrives after the window has been drawn once --
and a buffer that keeps its own `mode-line-format' is drawn with it.  What
that looks like is a line appearing on a fresh window and staying there
until some other window is selected, which is a fault to anyone watching.

Both halves are needed.  The default is empty, which covers the ordinary
buffer; the split itself takes the line off the new window, which covers
the buffer that insists."
  (sill-test--with-windows 1
    (sill-mode 1)
    ;; the ordinary buffer: nothing to draw, because the default is empty
    (let ((new (split-window-below)))
      (should (null (buffer-local-value 'mode-line-format (window-buffer new))))
      (should (= (window-total-height new) (window-body-height new))))
    ;; and one that keeps its own, split before any update can run
    (let ((buffer (generate-new-buffer "insistent")))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq-local mode-line-format '("insistent")))
            (set-window-buffer (frame-first-window) buffer)
            (let ((new (split-window (frame-first-window))))
              (should (eq 'none (window-parameter new 'mode-line-format)))
              (should (= (window-total-height new) (window-body-height new)))))
        (kill-buffer buffer)))))

(ert-deftest sill-test-turning-it-off-lets-go-of-split-window ()
  "A mode that is off is off everywhere, the advice included.

`split-window' is a function the whole of Emacs goes through, and a piece
of a disabled mode still sitting on it is the kind of thing that is found
months later by somebody wondering why their new windows have no mode
line."
  (sill-test--with-windows 1
    (sill-mode 1)
    (should (advice-member-p #'sill--adopt 'split-window))
    (sill-mode -1)
    (should-not (advice-member-p #'sill--adopt 'split-window))
    ;; and were it left on, it would still do nothing with the mode off
    (advice-add 'split-window :filter-return #'sill--adopt)
    (unwind-protect
        (let ((new (split-window-below)))
          (should-not (eq 'none (window-parameter new 'mode-line-format))))
      (advice-remove 'split-window #'sill--adopt))))

(ert-deftest sill-test-the-format-survives-the-emptying ()
  "What the sill draws is the line the frame would have drawn, and emptying
the default is how the windows stop drawing it -- so the sill has to have
kept a copy, or it empties itself along with them."
  (sill-test--with-windows 1
    (let ((sill-format nil)
          (before (default-value 'mode-line-format)))
      (unwind-protect
          (progn
      (setq-default mode-line-format '("the line"))
      (sill-mode 1)
      (should (equal '("the line") sill--saved-default))
      (should (null (default-value 'mode-line-format)))
      (cl-letf (((symbol-function 'format-mode-line)
                 (lambda (format &rest _) (format "%S" format))))
        (should (string-prefix-p "(\"the line\")"
                                 (sill--render (frame-first-window)))))
      (sill-mode -1)
      (should (equal '("the line") (default-value 'mode-line-format))))
        (setq-default mode-line-format before)))))

(ert-deftest sill-test-turning-it-off-puts-everything-back ()
  "A mode that cannot be turned off is a decision, not a setting."
  (sill-test--with-windows 3
    (let ((windows (length (window-list nil 'never)))
          (heights (mapcar #'window-body-height (window-list nil 'never))))
      (sill-mode 1)
      (should (sill-test--window))
      (sill-mode -1)
      (should-not (sill-test--window))
      (should (= windows (length (window-list nil 'never))))
      (should (equal heights (mapcar #'window-body-height
                                     (window-list nil 'never))))
      (should (equal (make-list windows nil)
                     (mapcar (lambda (w) (window-parameter w 'mode-line-format))
                             (window-list nil 'never))))
      ;; and nothing of it is left lying about
      (should-not (seq-find (lambda (b) (string-prefix-p " *sill" (buffer-name b)))
                            (buffer-list)))
      (should-not (frame-parameter nil 'sill-buffer)))))

(ert-deftest sill-test-the-subject-is-a-window-worth-reporting ()
  "Never the minibuffer, never the sill, and remembered while a prompt holds
the selection.

A prompt selects the minibuffer, and what the reader wants to know then is
still where they came from -- the sill going blank at the moment a command
is being typed would blank it for most of the typing there is."
  (sill-test--with-windows 2
    (sill-mode 1)
    (let ((real (frame-first-window))
          (sill (sill-test--window)))
      (should (sill--eligible-window-p real))
      (should-not (sill--eligible-window-p sill))
      (should-not (sill--eligible-window-p (minibuffer-window)))
      (select-window real)
      (should (eq real (sill--subject (selected-frame))))
      ;; the frame's own selected window, not the one selected anywhere
      (should (eq (frame-selected-window (selected-frame))
                  (sill--subject (selected-frame))))
      ;; and when what is selected is not worth reporting, the last one that
      ;; was still answers
      (with-selected-window sill
        (should (eq real (sill--subject (selected-frame))))))))

(ert-deftest sill-test-the-row-is-filled-to-the-frame ()
  "What the buffer does not cover, the sill paints -- to the edge, and by
asking for the edge rather than by counting columns.

Emacs fills out the row it draws for a mode line itself; a window is not a
mode line, so past the text is the default background, which is the one
place a line designed as a band stops looking like one.  Filling it with a
count of spaces means trusting `string-width' about every glyph in the
line, and one glyph drawn wider than it claims puts the line past the edge
-- where Emacs writes a `$' that stays as long as the line does."
  (sill-test--with-windows 2
    (cl-letf (((symbol-function 'format-mode-line) (lambda (&rest _) "abc")))
      (let ((line (sill--render (frame-first-window))))
        (should (string-prefix-p "abc" line))
        ;; one space, stretched by the display engine to the right edge
        (should (equal " " (substring-no-properties line 3)))
        (should (equal '(space :align-to right)
                       (get-text-property 3 'display line)))
        (should (eq sill-face (get-text-property 3 'face line)))
        ;; the text itself is left as it came, faces and all
        (should-not (get-text-property 0 'face line))))))

(ert-deftest sill-test-a-line-that-overruns-is-not-marked ()
  "The row carries no `$'.

Emacs marks a line it had to cut off, and puts the mark in the fringe when
there is one and in the last column of the text when there is not.  This
window has no fringes -- they would be a notch in the band -- so the mark
lands inside it, in the corner, and stays as long as the line does."
  (sill-test--with-windows 1
    (sill-mode 1)
    (with-current-buffer (window-buffer (sill-test--window))
      (let ((glyph (display-table-slot buffer-display-table 'truncation)))
        (should glyph)
        (should (= ?\s (glyph-char glyph)))
        (should (eq sill-face (glyph-face glyph)))))))

(ert-deftest sill-test-drawing-the-same-line-writes-nothing ()
  "Writing into a buffer that is on the screen is a change of window state,
and a change of window state is what asks for the next draw.

So a draw that always writes is a draw that always asks to be done again,
and the asking never stops."
  (sill-test--with-windows 1
    (sill-mode 1)
    (cl-letf (((symbol-function 'sill--render) (lambda (&rest _) "steady")))
      (sill--draw (selected-frame))
      (with-current-buffer (window-buffer (sill-test--window))
        (set-buffer-modified-p nil))
      (sill--draw (selected-frame))
      (should-not (buffer-modified-p (window-buffer (sill-test--window)))))))

(ert-deftest sill-test-a-changed-face-is-a-changed-line ()
  "Redrawing is skipped when nothing changed, and a face is something.

`equal' on strings ignores text properties.  A line whose words are the
same and whose faces are not therefore reads as unchanged -- which is the
mode line of a window that has just become the selected one, still in the
colours of a window that is not, until something makes the words differ
too."
  (sill-test--with-windows 1
    (sill-mode 1)
    (let* ((window (sill-test--window))
           (buffer (window-buffer window))
           (plain (propertize "same" 'face 'default))
           (fancy (propertize "same" 'face 'mode-line)))
      (cl-letf (((symbol-function 'sill--render) (lambda (&rest _) plain)))
        (sill--draw (selected-frame)))
      (should (equal-including-properties
               plain (with-current-buffer buffer (buffer-string))))
      (cl-letf (((symbol-function 'sill--render) (lambda (&rest _) fancy)))
        (sill--draw (selected-frame)))
      (should (equal-including-properties
               fancy (with-current-buffer buffer (buffer-string)))))))

(ert-deftest sill-test-the-row-is-not-a-window-to-work-in ()
  "It is a row that happens to be a window, and nothing should treat it as
one: not the key that balances, not the key that moves, not the key that
clears the frame."
  (sill-test--with-windows 3
    (sill-mode 1)
    (let ((sill (sill-test--window)))
      (balance-windows)
      (should (= 1 (window-height sill)))
      ;; Every step of the way round, not merely wherever a count of them
      ;; happens to stop -- landing on it once is landing on it.
      (select-window (frame-first-window))
      (dotimes (_ (* 2 (length (window-list nil 'never))))
        (other-window 1)
        (should-not (eq sill (selected-window))))
      (delete-other-windows (frame-first-window))
      (should (window-live-p sill))
      (should (= 1 (window-height sill)))
      ;; nothing displays into it either
      (should (window-dedicated-p sill)))))

(ert-deftest sill-test-startup-waits-for-the-frame ()
  "Switched on while Emacs is still loading, it makes no window yet.

There is no finished frame to make one into: the tab bar may not be on it,
and whatever configures the display later has still to change its shape.
A row taken from a frame in that state is a row taken from a guess, and
what comes of it is a frame drawn to halfway -- no tab bar, no echo area,
a minibuffer that never appears."
  (sill-test--with-windows 1
    (let ((after-init-time nil)
          (emacs-startup-hook nil))
      (sill-mode 1)
      (should-not (sill-test--window))
      (should (memq #'sill--update emacs-startup-hook))
      ;; and when the loading is over, it draws
      (run-hooks 'emacs-startup-hook)
      (should (sill-test--window))
      (sill-mode -1)
      (should-not (memq #'sill--update emacs-startup-hook)))))

(ert-deftest sill-test-the-hooks-only-ask ()
  "Nothing that changes windows runs from a hook that redisplay runs.

`window-state-change-hook' and its kind are run by redisplay, in the middle
of the very thing this does -- and what that buys is a frame drawn to
halfway: no tab bar, no echo area, a minibuffer that never comes up, and
then a crash.  So the hook arms a timer and returns, and the work happens
in the command loop."
  (sill-test--with-windows 2
    (sill-mode 1)
    (should (memq #'sill--schedule window-state-change-hook))
    (should-not (memq #'sill--update window-state-change-hook))
    ;; nothing else this package adds runs from redisplay either
    (should-not (seq-find (lambda (f) (string-prefix-p "sill" (format "%s" f)))
                          window-size-change-functions))
    ;; and asking is all the hook does: no window is made by the call itself
    (let ((before (length (window-list nil 'never))))
      (setq sill--timer nil)
      (delete-window (sill-test--window))
      (sill--schedule)
      (should (= (1- before) (length (window-list nil 'never))))
      (should (timerp sill--timer))
      ;; the timer is what makes it
      (sill--update)
      (should (= before (length (window-list nil 'never)))))))

(provide 'sill-test)

;;; sill-test.el ends here
