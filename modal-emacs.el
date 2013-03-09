(defun modal-emacs-on () (interactive) (modal-emacs-mode 1))
(defun modal-emacs-off () (interactive) (modal-emacs-mode -1))

(defvar modal--normal-mode-map "The standard keymap that starts everything")

(setq modal--normal-mode-map (make-sparse-keymap))

(define-key modal--normal-mode-map "a" 'move-beginning-of-line)
(define-key modal--normal-mode-map "e" 'move-end-of-line)
(define-key modal--normal-mode-map "i" 'modal--insert-mode)
(define-key modal--normal-mode-map "n" 'next-line)
(define-key modal--normal-mode-map "p" 'previous-line)
(define-key modal--normal-mode-map "f" 'forward-char)
(define-key modal--normal-mode-map "b" 'backward-char)
(define-key modal--normal-mode-map "d" 'delete-char)
(define-key modal--normal-mode-map " " 'set-mark-command)
(define-key modal--normal-mode-map "o" 'other-window)
(define-key modal--normal-mode-map "/" 'isearch-forward-regexp)
(define-key modal--normal-mode-map "?" 'isearch-backward-regexp)
(define-key modal--normal-mode-map "k" 'paredit-forward)
(define-key modal--normal-mode-map "j" 'paredit-backward)
(define-key modal--normal-mode-map "u" 'universal-argument)
(define-key modal--normal-mode-map "<" 'beginning-of-buffer)
(define-key modal--normal-mode-map ">" 'end-of-buffer)
(define-key modal--normal-mode-map "v" 'scroll-up-command)
(define-key modal--normal-mode-map "V" 'scroll-down-command)


(defvar modal--normal-mode-map "emulates the spirit fo vims insert mode, retores functionality")
(setq modal--insert-mode-map (make-sparse-keymap))
(define-key modal--insert-mode-map (kbd "M-ESC") 'modal--normal-mode)
(define-key modal--insert-mode-map (kbd "C-z") 'modal--normal-mode)


(defun modal--insert-mode ()
  ""
  (interactive)
  (setq insert-mode t)
  (modal--switch-active-keymap 'insert-mode modal--insert-mode-map)
  (modal-emacs-update-mode-line "I")
  nil)

(defun modal--normal-mode ()
  ""
  (interactive)
  (setq normal-mode t)
  (modal--switch-active-keymap 'normal-mode modal--normal-mode-map)
  (modal-emacs-update-mode-line "N"))


(defvar modal--active-keymap-alist nil
  "contains the keymap that is currently active")

(defun modal--switch-active-keymap (mode-name new-keymap)
  ;; we only ever have  a single active keymap
  (setq modal--active-keymap-alist 
        `((,mode-name . ,new-keymap))))

(define-minor-mode modal-emacs-mode
  "minor mode that enables our semi-simplistic \"modal\" keymap system"
  :lighter " Modal"
  :group 'modal-emacs
  (if (not modal-emacs-mode)
      (setq emulation-mode-map-alists (delq 'modal--active-keymap-alist emulation-mode-map-alists))
    (add-to-ordered-list 'emulation-mode-map-alists 'modal--active-keymap-alist 400)
    (modal--insert-mode)))

(define-globalized-minor-mode modal-emacs-globalized-mode modal-emacs-mode
  modal-emacs-on)

(defun modal-emacs-update-mode-line (mode-indicator)
  ;; sets the structure directly
  ;; kinda sad
  (setcdr (assq 'modal-emacs-mode
                minor-mode-alist)
          (list (format " Modal[%s]" mode-indicator)))
  (force-mode-line-update))





(provide 'modal-emacs)
