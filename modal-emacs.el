;; Release into the public domain.
;; TODO add gpl header

(require 'eieio)


;;; global activation and control logic


(defun modal-emacs-on () (interactive) (modal-emacs-mode 1))
(defun modal-emacs-off () (interactive) (modal-emacs-mode -1))


;; modal system setup / support
(defclass modal-mode ()
  ((name :initarg :name
         :initform 'tmp-name
         :type symbol
         :documentation "The name of the keyboard mode")
   (keymap :initarg :keymap
           :initform (make-sparse-keymap)
           :documentation "The keyboard mode keymap")
   (is-default :initarg :default
               :initform nil
               :documentation "Should this mode be considered a default mode"))
  "A single keyboard mode.")


(defmacro modal-define-mode (name &rest args)
  `(let ((strname (symbol-name ',name))
         (args ',args)
         (mode-map (make-sparse-keymap))
         the-mode should-be-default)
     
     ,@(mapcar #'(lambda (binding)
                   `(define-key mode-map (kbd ,(car binding)) ,(cadr binding)))
               (cadr (member :map args)))

     (setq should-be-default
           (if (member :default args)
               (cadr (member :default args))))
     
     (setq the-mode
           (modal-mode strname
                       :name ',name
                       :keymap mode-map
                       :default should-be-default))
     
     (set (intern (concat "modal-" strname "-mode")) the-mode)
     (set (modal-enabled-symbol the-mode) nil)
     
     (fset (intern (concat "modal-" strname "-mode"))
           (lambda ()
             (interactive)
             (modal-activate the-mode)))
     (modal-mode-defined
      (or
       (plist-get args :emacs-instance)
       (modal-global-emacs-instance))
      the-mode)
     the-mode))

(defmethod modal-enabled-symbol ((mode modal-mode))
  (intern (concat "modal-" (symbol-name (oref mode name)) "-mode--enabled")))

(defmethod modal-activate ((mode modal-mode))
  "Activate this mode."
  (set (modal-enabled-symbol mode) t))

(defmethod modal-deactivate ((mode modal-mode))
  (set (modal-enabled-symbol mode) nil))

(defmethod me--enabled-p ((mode modal-mode))
  (eval (modal-enabled-symbol mode)))

(defmethod me--default-p ((mode modal-mode))
  (oref mode is-default))

(defclass modal-emacs-instance ()
  ((modes :initform '())) 
  "The emacs instance to work with.
Serves as a wrapper around emacs interfaces and provides
a place to hang functions that are relative in a global way.")


(defmethod modal-mode-defined ((emacs-instance modal-emacs-instance) mode)
  (object-add-to-list emacs-instance 'modes mode)
  (add-to-list 'modal--keymap-alist
               `(,(modal-enabled-symbol mode) . ,(oref mode keymap))))

(defmethod me--default-mode ((emacs modal-emacs-instance))
  (let ((modes (oref emacs modes)))
    (or (car (delete
              nil
              (mapcar (lambda (it)
                        (and (me--default-p it) it))
                      modes)))
        (car (last modes)))))

(defmethod modal-activate-default ((emacs modal-emacs-instance))
  (let ((default (me--default-mode emacs)))
    (when default
      (modal-activate-mode emacs
                           default))))


(defmethod modal-activate-mode ((emacs modal-emacs-instance) mode)
  (dolist (x (oref emacs modes))
    (modal-deactivate x))
  (modal-activate mode))


(defvar modal--keymap-alist nil
  "Keymaps and toggles that allow us to turn modes on and off.
Referenced from")


(defmethod modal-init ((inst modal-emacs-instance))
  (setq modal--keymap-alist '()))

(defun modal-emacs-init ()
  "Initializes the global emacs setup."
  (modal-global-emacs-instance))

(defvar modal-global-emacs-instance-value nil
  "The global `modal-emacs-instance'. This ought to be set and accessed
an assumed thorugh `modal-global-emacs-instance'")
(defun modal-global-emacs-instance ()
  (setq modal-global-emacs-instance-value
        (or modal-global-emacs-instance-value
            (let ((instance (modal-emacs-instance "Emacs Instance")))
              (modal-init instance)
              instance))))

(define-minor-mode modal-emacs-mode
  "minor mode that enables our semi-simplistic \"modal\" keymap system"
  :group 'modal-emacs
  :lighter (:eval (format " Modal[%s]" modal--current-indicator))

  (dolist (x '(modal--current-indicator
               modal-normal-mode--enabled
               modal-insert-mode--enabled))
    (make-local-variable x))

    (if (not modal-emacs-mode)
        (setq emulation-mode-map-alists (delq 'modal--keymap-alist emulation-mode-map-alists))
      (add-to-ordered-list 'emulation-mode-map-alists 'modal--keymap-alist 400)
      (modal-activate-default (modal-global-emacs-instance))
      (modal-emacs-update-mode-line)))



;; while this exists (and is possible), i woudln't recommend it
;; for now its seems better to activaate modal mode on a per-buffer basis
(define-globalized-minor-mode modal-emacs-globalized-mode modal-emacs-mode modal-emacs-on)


(defvar modal--current-indicator nil)
(defun modal-emacs-update-mode-line ()
  (setq modal--current-indicator
        (mapconcat
         'identity
         (delete nil (list
                      (when modal-normal-mode--enabled "N")
                      (when modal-insert-mode--enabled "I")))
         ","))
  (force-mode-line-update))

;; junk for development

(defun modal--reset-emacs-instance--! ()
  (interactive)
  (modal--reset-minor-mode-alist)
  (modal--remove-symbols--!))

(defun modal--reset-minor-mode-alist ()
  (setq minor-mode-alist
         (-remove (lambda (elt)
                    (string= (symbol-name (car elt)) "modal-emacs-mode"))
                  minor-mode-alist)))


(defun modal--remove-symbols--! ()
  (interactive)
  (dolist (it (modal--symbol-search--!))
    (unintern it)))


(defun modal--symbol-search--! ()
  "return a list of the symbols that we believe belong to modal-emacs.
Of course, we might be wrong, sine it uses a regular expression and is kinda fuzzy"
  (let (matching-symbols)
    (mapatoms
     (lambda (it)
       (when (string-match  "\\(^modal-\\|^modal--\\)" (symbol-name it))
         (push it matching-symbols)
         )))
    matching-symbols))



;;; normal mode
(modal-define-mode
 normal
 :doc "The standard keymap for commanding Emacs."
 :map (("a" 'move-beginning-of-line)
       ("e" 'move-end-of-line)
       ("i" 'modal--insert-mode)
       ("n" 'next-line)
       ("p" 'previous-line)
       ("f" 'forward-char)
       ("b" 'backward-char)
       ("d" 'delete-char)
       (" " 'set-mark-command)
       ("o" 'modal--other-window)
       ("s" 'isearch-forward-regexp)
       ("r" 'isearch-backward-regexp)
       ("k" 'paredit-forward)
       ("j" 'paredit-backward)
       ("u" 'universal-argument)
       ("<" 'beginning-of-buffer)
       (">" 'end-of-buffer)
       ("v" 'scroll-up-command)
       ("V" 'scroll-down-command)))

(defun modal--normal-mode ()
  "Switches to normal mode"
  (interactive)
  (modal--buffer-mode-exclusive-switch 'modal-normal-mode--enabled)
  (modal-emacs-update-mode-line))





;;; insert mode 
(modal-define-mode
 insert
 :doc "Map for typing characters. Really only contains a helper to switch to normal mode"
 :map (("M-ESC" 'modal--normal-mode)
       ("M-SPC" 'modal--normal-mode)
       ( "o" 'modal--other-window-or-self-insert))
 :default t)

(defun modal--insert-mode ()
  "Switches to insert mode"
  (interactive)
  (modal--buffer-mode-exclusive-switch 'modal-insert-mode--enabled)
  (modal-emacs-update-mode-line))

;;; global mode
(modal-define-mode
 global
 :doc "Global mode that brings support to other buffers to make the feel more natural"
 :map (("o" 'modal--other-window-or-self-insert)))



;; movement mode
(defun modal--movement-mode ()
  (interactive)
  (modal--buffer-mode-exclusive-switch 'modal--movement-mode-enabled)
  (modal-emacs-update-mode-line))


;; mode "stack" support

(defvar modal--mode-stack nil
  "list of modes; when current mode is popped, then switch to top")


(defun modal--pop-mode-stack ()
  "return to last mode on stack"
  (push )
  )




;; misc / library functions

(defun modal--other-window ()
  "wrapper around other-window"
  (interactive)
  (other-window 1))


(defvar modal--other-window-or-self-insert-acted-as-other-window nil)
(defun modal--other-window-or-self-insert ()
  "If the previous command was `other-window` or
`modal--other-window-or-self-insert`, run `other-window`.
Otherwise, insert the char as though `self-insert-command`.

The point of this command is to allow a user to flip from
a window with a buffer in normal mode to another window, and still
be able to continue moving between windows."
  (interactive)
  (if (or (eq last-command 'modal--other-window)
          (and (eq last-command 'modal--other-window-or-self-insert)
              modal--other-window-or-self-insert-acted-as-other-window))
      (progn
        (setq modal--other-window-or-self-insert-acted-as-other-window t)
        (other-window 1))
    (setq modal--other-window-or-self-insert-acted-as-other-window nil)
    (self-insert-command 1)))

(defun modal--buffer-mode-exclusive-switch (mode-to-switch-on)
  (setq modal-insert-mode--enabled nil)
  (setq modal-normal-mode--enabled nil)
  (setq modal-movement-mode--enabled nil)
  (set mode-to-switch-on t))

(defmacro modal-carry (things-to-carry  &rest body)
  `(let (,@(mapcar (lambda (item) (list item item))
                    things-to-carry))
     ,@body))

(provide 'modal-emacs)
