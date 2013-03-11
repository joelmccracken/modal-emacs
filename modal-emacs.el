;; released into the public domain.
;; TODO add gpl header

(defun modal-emacs-on () (interactive) (modal-emacs-mode 1))
(defun modal-emacs-off () (interactive) (modal-emacs-mode -1))


(defvar modal--insert-mode-map "Map for typing characters.")
(setq modal--insert-mode-map (make-sparse-keymap))
(define-key modal--insert-mode-map (kbd "M-ESC") 'modal--normal-mode)

(defvar modal--normal-mode-map "The standard keymap for commanding Emacs.")
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


(defun modal--buffer-mode-exclusive-switch (mode-to-switch-on)
  (setq modal--insert-mode-enabled nil)
  (setq modal--normal-mode-enabled nil)
  (set mode-to-switch-on t))

(defun modal--insert-mode ()
  "Switches to insert mode"
  (interactive)
  (modal--buffer-mode-exclusive-switch 'modal--insert-mode-enabled)
  (modal-emacs-update-mode-line))

(defun modal--normal-mode ()
  "Switches to normal mode"
  (interactive)
  (modal--buffer-mode-exclusive-switch 'modal--normal-mode-enabled)
  (modal-emacs-update-mode-line))

(defvar modal--super-mode-enabling-map "Map which lets user get super mode" nil)
(setq modal--super-mode-enabling-map (make-sparse-keymap))
(define-key modal--super-mode-enabling-map (kbd "C-z") 'modal--super-mode)
(defvar modal--super-mode-enabling-enabled t
  "When non-nil, allows super-mode activation keybindings.")


(defvar modal--super-mode-map "The 'super' map for controlling Emacs accross buffers.")
(setq modal--super-mode-map (make-sparse-keymap))
(define-key modal--super-mode-map "o" 'other-window)
(define-key modal--super-mode-map "q" 'modal--super-mode-quit)
(define-key modal--super-mode-map (kbd "C-z") 'modal--super-mode-quit)
(defvar modal--super-mode-enabled nil
  "Should bindings for super-mode be active")

(defun modal--super-mode ()
  "Enables 'super' mode, which enables a few modal-like keybindings in all buffers"
  (interactive)
  (setq modal--super-mode-enabling-enabled nil
        modal--super-mode-enabled t)
  (modal-emacs-update-mode-line))

(defun modal--super-mode-quit ()
  "Disable 'super' mode"
  (interactive)
  (setq modal--super-mode-enabling-enabled t
        modal--super-mode-enabled nil)
  (modal-emacs-update-mode-line))

(defun modal--super-mode-exclusive-switch (mode-to-switch-on)
  (setq modal--insert-mode-enabled nil)
  (setq modal--normal-mode-enabled nil)
  (set mode-to-switch-on t))

(defvar modal--keymap-alist
  `((modal--normal-mode-enabled . ,modal--normal-mode-map)
    (modal--insert-mode-enabled . ,modal--insert-mode-map)
    (modal--super-mode-enabling-enabled . ,modal--super-mode-enabling-map)
    (modal--super-mode-enabled . ,modal--super-mode-map))
  "Keymaps and toggles that allow us to turn them on and off.")

(define-minor-mode modal-emacs-mode
  "minor mode that enables our semi-simplistic \"modal\" keymap system"
  :group 'modal-emacs
  :lighter (:eval (format " Modal[%s]" modal--current-indicator))

  (dolist (x '(modal--current-indicator
               modal--normal-mode-enabled
               modal--insert-mode-enabled))
    (make-local-variable x))

  (if (not modal-emacs-mode)
      (setq emulation-mode-map-alists (delq 'modal--keymap-alist emulation-mode-map-alists))
    (add-to-ordered-list 'emulation-mode-map-alists 'modal--keymap-alist 400)
    (modal--insert-mode)))

(define-globalized-minor-mode modal-emacs-globalized-mode modal-emacs-mode
  modal-emacs-on)

(defvar modal--current-indicator nil)

(defun modal-emacs-update-mode-line ()
  (setq modal--current-indicator
        (mapconcat
         'identity
         (delete nil (list
                      (when modal--super-mode-enabled "S")
                      (when modal--normal-mode-enabled "N")
                      (when modal--insert-mode-enabled "I")))
         ","))
  (force-mode-line-update))




(provide 'modal-emacs)
