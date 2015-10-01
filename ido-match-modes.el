;;; ido-match-modes.el --- Cycle ido matching methods from in ido

;; Copyright (C) whoever, I don't care.

;; Author: Tom Hinton <t@larkery.com>
;; Version: 1.0.0
;; Package-Requires: ((ido) (cl))
;; Keywords: convenience
;; URL: https://github.com/larkery/ido-match-modes.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Rebinds C-S-SPC in `ido-mode' so that it rotates through a list of
;; ways to match user input. You can customize the list of ways
;; (`ido-match-modes-list'), and the display of the current method
;; indicator in the ido-match-modes custom group.

;; to enable, invoke `ido-match-modes-toggle'
;; (negative argument means disable no matter what, positive means enable no matter what)

;; Most of the modes are as normal in ido:
;; - typical 'entire substring' mode (what ido does by default)
;; - regex matching (what ido does if you set `ido-enable-regexp')
;; - flex matching (what ido does if you set `ido-enable-flex-matching')
;; - prefix matching (what ido does if you set `ido-enable-prefix')

;; there is also a 'words' or 'substrings' matching mode added, which
;; is like a slightly less sledgehammery version of flex
;; matching. Where flex matching inserts a wildcard between every
;; character of the input, so 'abcd' matches 'Any big sequenCe
;; Containing D', the substrings mode inserts wildcards between spaces
;; in the input, so the input 'customize group' matches
;; 'customize-group', 'customize-group-other-window',
;; 'customize-some-group' and so on. I prefer this to flex matching,
;; as I find I usually know words in the name of my thing, and type
;; those rather than knowing a sequence of characters.

;; For convenience, the substrings mode also passes through the start
;; and end anchors, so that "some file .el$" will match any input which
;; - contains the word "some" and the word "file", in that order
;; - ends with ".el"
;; i.e. it matches the regex ".+some.+file\.el$"

;; in substrings mode, SPC is re-bound to type space; normally SPC
;; does something else in ido.

;;; Code:

(defvar ido-match-modes-enabled nil)

(defcustom ido-match-modes-list '(substring words prefix)
  "Defines which modes are enabled for cycling"
  :type '(repeat (choice (const :tag "Normal: Entire input is substring of element" substring)
                         (const :tag "Regex: Input is regex matching element" regex)
                         (const :tag "Substrings: Words in input are substrings of element (in order), ^ and $ are anchors" words)
                         (const :tag "FLX: Chars of input are substrings of element (in order)" flex)
                         (const :tag "Prefix: Input is prefix of element" prefix)))
  :group 'ido-match-modes)

(defface ido-match-modes-indicator-face
  '((t))
  "Face used to indicate match mode in input"
  :group 'ido-match-modes)

(defvar ido-match-modes-mode nil)
(defvar ido-match-old-values nil)
(defvar ido-match-modes-lighter "")
(defvar ido-match-modes-old-space-command nil)
(defvar ido-match-modes-last-input nil)

(defvar ido-match-mode-bindings
  ;;        flx pre rx
  '((substring nil nil nil)
    (regex  nil nil t)
    (words  nil nil nil)
    (flex   t   nil nil)
    (prefix nil t   nil)))

(defun ido-match-remember (symbol new-value)
  (push (cons symbol (eval symbol)) ido-match-old-values)
  (eval `(setf ,symbol ,new-value)))

(defun ido-match-modes-hack-spacebar ()
  (if (eq ido-match-modes-mode 'words)
      (let ((existing-command  (lookup-key ido-completion-map " ")))
        (when (not (eq existing-command #'self-insert-command))
          ;; remember how to unhack space bar
          (define-key ido-completion-map " " #'self-insert-command)
          ;; hack space bar
          (setf ido-match-modes-old-space-command existing-command)))
    (when (and ido-match-modes-old-space-command
               (not (eq ido-match-modes-old-space-command #'self-insert-command)))
      (define-key ido-completion-map " " ido-match-modes-old-space-command))))

;; this could be neater
(defun ido-match-modes-cycle ()
  (interactive)

  (if (eq ido-match-modes-mode 'words)
      ;; unhack space bar
      (define-key ido-completion-map " "
        ido-match-modes-old-space-command))

  (setf ido-match-modes-mode
        (or
         (nth (1+ (or (position ido-match-modes-mode ido-match-modes-list) 0))
              ido-match-modes-list)
         (car ido-match-modes-list)
         'substring))

  (ido-match-modes-hack-spacebar)

  (setf ido-match-modes-lighter
        (case ido-match-modes-mode
          (substring " S")
          (regex  " R")
          (words  " W")
          (flex   " F")
          (prefix " P")
          (t " ???")))

  (add-face-text-property 1 (length ido-match-modes-lighter)
                          'ido-match-modes-indicator-face nil
                          ido-match-modes-lighter)

  ;; (setf ido-match-modes-lighter
  ;;       (format "%s %s %s %s" ido-match-modes-lighter
  ;;               ido-enable-flex-matching
  ;;               ido-enable-prefix
  ;;               ido-enable-regexp))

  (ido-match-mode--set-bindings))

(defun ido-match-mode--set-bindings ()
  (let ((bindings (cdr (assoc ido-match-modes-mode ido-match-mode-bindings))))
    (setf ido-enable-flex-matching (nth 0 bindings)
          ido-enable-prefix (nth 1 bindings)
          ido-enable-regexp (nth 2 bindings))))

(defun ido-match-modes--adv-completions (o &rest args)
  ;; this messes with the common match prefix
  (if (and (stringp ido-common-match-string)
           (> (length ido-common-match-string) (length (car args))))
      (apply o args)
    (concat ido-match-modes-lighter (apply o args))))

(defun ido-match-modes--words-to-rx (words)
  (if words
   (let* ((words (string-trim words))
          (hat   (string-match "^\\^" words))
          (dollar (string-match "\\$$" words))
          (words (substring words
                            (if hat 1 0)
                            (if dollar
                                (- (length words) 1)
                              (length words)))))
     (concat
      (if hat "^" "")
      (mapconcat
       (lambda (x) (if (zerop (length x)) "" (concat "\\(" (regexp-quote x) "\\)")))
       (split-string words " +")
       ".*")
      (if dollar "$" "")
      ))
   ""))

(defun ido-match-modes--adv-set-matches (o &rest args)
  ;; depending on mode, we fiddle various ido variables
  (ido-match-mode--set-bindings) ;; smex and stuff mess with these settings by turning on flx.

  (setf ido-match-modes-last-input ido-text)

  (case ido-match-modes-mode
    (words
     (let ((is-special (string-match "[\\^\\$ ]" ido-text))
           (original-text ido-text)
           result)

       (setf ido-enable-regexp is-special)
       (when is-special
         (setf ido-text (ido-match-modes--words-to-rx ido-text)))

       (setf result (apply o args))

       (when (and is-special (not ido-matches))
         ;; apparently need to undo here

         (setf ido-text original-text
               ido-enable-regexp nil))

       result
       ))

    (t (apply o args))))

(defun ido-match-modes--adv-exit-mb (o &rest args)
  (setf ido-text (or ido-match-modes-last-input ido-text))
  (apply o args))

(defun ido-match-modes--bind-keys ()
  (ido-match-modes-hack-spacebar)
  (define-key ido-completion-map (kbd "C-S-SPC") #'ido-match-modes-cycle))

(defun ido-match-modes-enable ()
  ;; enable
  (unless ido-match-modes-enabled
    (message "enabling ido-match-modes")
    (setf ido-match-modes-enabled t)
    (advice-add 'ido-completions :around #'ido-match-modes--adv-completions)
    (advice-add 'ido-set-matches :around #'ido-match-modes--adv-set-matches)
    (advice-add 'ido-exit-minibuffer :around #'ido-match-modes--adv-exit-mb)
    (advice-add 'ido-kill-buffer-at-head :around #'ido-match-modes--adv-exit-mb)
    (advice-add 'ido-delete-file-at-head :around #'ido-match-modes--adv-exit-mb)
    (add-hook 'ido-setup-hook #'ido-match-modes--bind-keys)
    (ido-match-remember 'ido-enable-flex-matching nil)
    (ido-match-remember 'ido-enable-regexp nil)
    (ido-match-remember 'ido-enable-prefix nil)
    (setf ido-match-modes-mode (or (car ido-match-modes-list) 'substring))))

(defun ido-match-modes-disable ()
  (when ido-match-modes-enabled
    (setf ido-match-modes-enabled nil)
    (message "disabling ido-match-modes")
    (advice-remove 'ido-completions #'ido-match-modes--adv-completions)
    (advice-remove 'ido-set-matches #'ido-match-modes--adv-set-matches)
    (advice-remove 'ido-exit-minibuffer #'ido-match-modes--adv-exit-mb)
    (advice-remove 'ido-kill-buffer-at-head #'ido-match-modes--adv-exit-mb)
    (advice-remove 'ido-delete-file-at-head #'ido-match-modes--adv-exit-mb)
    (remove-hook 'ido-setup-hook #'ido-match-modes--bind-keys)
    (dolist (old-value ido-match-old-values)
      (eval `(setf ,(car old-value) ,(cdr old-value))))
    (setf ido-match-old-values nil)
    (when ido-match-modes-old-space-command
      (define-key ido-completion-map " " ido-match-modes-old-space-command))))


;;;###autoload
(defun ido-match-modes-toggle (arg)
  "Switch ido-match-modes on if it's off or off if it's on.
With a negative prefix argument, make sure it's off, and with a
positive one make sure it's on."
  (interactive "P")
  (message "toggling ido-match-modes")
  (if (or (and arg (> (prefix-numeric-value arg) 0))
          (not ido-match-modes-enabled))
      (ido-match-modes-enable)
    (ido-match-modes-disable)))

(provide 'ido-match-modes)

;;; ido-match-modes.el ends here
