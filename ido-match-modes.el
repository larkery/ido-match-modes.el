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

(defun ido-match-modes-cycle ()
  (interactive)

  (if (eq ido-match-modes-mode 'words)
      ;; unhack space bar
      (define-key ido-completion-map " "
        ido-match-modes-old-space-commmand))

  (setf ido-match-modes-mode
        (or
         (nth (1+ (or (position ido-match-modes-mode ido-match-modes-list) 0))
              ido-match-modes-list)
         (car ido-match-modes-list)
         'substring))

  (if (eq ido-match-modes-mode 'words)
      ;; remember how to unhack space bar
      (setf ido-match-modes-old-space-commmand (lookup-key ido-completion-map " "))

      ;; hack space bar
      (define-key ido-completion-map " " #'self-insert-command))

  (setf ido-match-modes-lighter
        (case ido-match-modes-mode
          (substring " S")
          (regex  " R")
          (words  " W")
          (flex   " F")
          (prefix " P")))

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
     )))

(defun ido-match-modes--adv-set-matches (o &rest args)
  ;; depending on mode, we fiddle various ido variables
  (ido-match-mode--set-bindings) ;; smex and stuff mess with these settings by turning on flx. kill!
  (if (eq ido-match-modes-mode 'words)
      (let ((original-text ido-text))
        (if (string-match "[\\^\\$ ]" ido-text)
            (setf ido-enable-regexp t
                  ido-text  (ido-match-modes--words-to-rx ido-text))
          (setf ido-enable-regexp nil))

        (let ((result (apply o args)))
          (unless ido-matches
            (setf ido-text original-text
                  ido-enable-regexp nil))
          result))

    (apply o args)))

(defun ido-match-modes--bind-keys ()
  (define-key ido-completion-map (kbd "C-.") #'ido-match-modes-cycle))

(defun ido-match-modes-toggle ()
  (interactive)
  (if (not ido-match-modes-enabled)
      (progn
        ;; enable
        (advice-add 'ido-completions :around #'ido-match-modes--adv-completions)
        (advice-add 'ido-set-matches :around #'ido-match-modes--adv-set-matches)
        (add-hook 'ido-setup-hook #'ido-match-modes--bind-keys)
        (ido-match-remember 'ido-enable-flex-matching nil)
        (ido-match-remember 'ido-enable-regexp nil)
        (ido-match-remember 'ido-enable-prefix nil)
        (setf ido-match-modes-old-space-commmand (lookup-key ido-completion-map " "))
        (setf ido-match-modes-mode (or (car ido-match-modes-list) 'substring)))

    (progn
      ;; disable
      (advice-remove 'ido-completions #'ido-match-modes--adv-completions)
      (advice-remove 'ido-set-matches #'ido-match-modes--adv-set-matches)
      (remove-hook 'ido-setup-hook #'ido-match-modes--bind-keys)
      (dolist (old-value ido-match-old-values)
        (eval `(setf ,(car old-value) ,(cdr old-value))))
      (setf ido-match-old-values nil)
      (when ido-match-modes-old-space-command
        (define-key ido-completion-map " " ido-match-modes-old-space-command))))

  (setf ido-match-modes-enabled (not ido-match-modes-enabled)))
