(require 'ert)

(defvar modal-test-testing-modes nil)
(defvar modal-test-testing-mode-counter 0)
(defun modal-test-define-testing-mode (&optional action &rest args)
  (incf modal-test-testing-mode-counter)
  (let ((default-opt (member :default args))
        (mode-name (intern (format "testing%d" modal-test-testing-mode-counter)))
        (default (plist-get args :default))
        (emacs-instance (plist-get args :emacs-instance))
        the-mode)
    (setq the-mode
          (eval  `(modal-define-mode
                   ,mode-name
                   :doc "a testing mode"
                   :map (( "a" (eval '(modal-carry
                                       (action)
                                       (lambda () (interactive)
                                         (funcall action)))
                                     t)))
                   :default ,default
                   :emacs-instance ,emacs-instance)))

    (add-to-list 'modal-test-testing-modes mode-name)
    the-mode))

(defun modal-remove-testing-modes-empty ()
  )



(defmacro modal-mock-fresh-environment (&rest body)
  `(let ((emulation-mode-map-alists nil)
         (modal-global-emacs-instance-value nil)
         (modal--keymap-alist nil)
         )
     ,@body))


(defvar modal-test-run-on-eval nil "should evaling a deftest run it immediately")
(setq modal-test-run-on-eval nil)
(defun modal-test--toggle-run-on-eval ()
  (interactive)
  (setq modal-test-run-on-eval (not modal-test-run-on-eval)))

(defmacro modal-test (docstring &rest body)
  (declare (indent defun))
  (let* ((docstring (eval docstring))
         (test-name (modal--testing-symbol-from-string docstring)))
    `(let (the-test
           modal-after-test-hook)
       (setq the-test
             (ert-deftest ,test-name ()
               ,docstring
               :tags '(modal)
               (unwind-protect
                   (modal-mock-fresh-environment
                    ,@body)
                 (run-hooks 'modal-after-test-hook))))
       (when modal-test-run-on-eval
         (ert-run-tests-interactively the-test))
       the-test)))

(defun modal--testing-symbol-from-string (str)
  (intern (replace-regexp-in-string "[^a-z0-9]" "-" str)))

(defmacro modal-test-with-buffer (&rest body)
  `(with-current-buffer (get-buffer-create "modal-test-buffer")
     (modal-emacs-mode)
     ,@body))

(defun modal-run-modal-tests ()
  (interactive)
  (ert-run-tests-interactively '(tag modal)))

(defun me--mock (&optional call-through)
  (eval '(let ((calls nil))
           (lambda (&rest args)
             (if (equal args '(call-history))
                 calls
               (push args calls))))
        t))

(modal-test "test support: modal mock has a recallable history"
  (let ((mock (me--mock)))
    (funcall mock 10)
    (should (equal '((10)) (funcall mock 'call-history)))))


;; defining a mode  / interface tests
(modal-test "defines a method which will activate the mode"
  (modal-test-define-testing-mode)
  (should (functionp 'modal-testing-mode)))


;; enabling the modal system
(modal-test "enabling modal mode adds the correct value to minor-mode-alist"
  (modal-test-with-buffer
    (let ((alist-entry (assoc 'modal-emacs-mode minor-mode-alist)))
      (should (string-match "Modal\\[.*\\]" (prin1-to-string alist-entry))))))

(modal-test "enabling modal mode adds the modal--minor-mode alist variable
to the emulation-mode-map-alists.
emulation-mode-map-alists is how we specify which keymap is active."
  (modal-test-with-buffer
    (should (member 'modal--keymap-alist emulation-mode-map-alists))))


(modal-test "enabling modal mode enables the default mode not dependent upon order"
  (let ((first-test-mode (modal-test-define-testing-mode))
        (second-test-mode (modal-test-define-testing-mode nil :default t))
        (third-test-mode (modal-test-define-testing-mode nil :default nil)))
    (modal-test-with-buffer
     (should (me--enabled-p second-test-mode)))))


;; feature-level stuff

(modal-test "feature: enabling modal as minor mode correctly initializes the mode. "
  (let ((mock (me--mock)))
    (modal-test-define-testing-mode mock)
    (modal-test-with-buffer
     (call-interactively (key-binding (kbd "a")))
     (should (eql (length (funcall mock 'call-history))
                  1)))))


;;(defclass modal-mock-emacs-instance ())
;;(defmethod )



;; unit tests
(modal-test (concat "modal-emacs-instance me--default-mode chooses its default mode based on "
                    "modes responding to is-default")
  (let* ((mock (me--mock))
         (inst (modal-emacs-instance "test-instance"))
         (m1   (modal-test-define-testing-mode nil :emacs-instance inst))
         (m2   (modal-test-define-testing-mode nil :emacs-instance inst :default t))
         (m3   (modal-test-define-testing-mode nil :emacs-instance inst)))
    (should (equal (me--default-mode inst)
                   m2))))

(modal-test (concat "modal-emacs-instance me--default-mode chooses the first instance if "
                    "there are no selected defaults")
  (let* ((mock (me--mock))
         (inst (modal-emacs-instance "test-instance"))
         (m1   (modal-test-define-testing-mode nil :emacs-instance inst))
         (m2   (modal-test-define-testing-mode nil :emacs-instance inst))
         (m3   (modal-test-define-testing-mode nil :emacs-instance inst)))
    (should (equal (me--default-mode inst)
                   m1))))

