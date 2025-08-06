;;; scheme-formal-verification.el --- Emacs configuration for Scheme Formal Verification project

;;; Commentary:
;; Project-specific Emacs configuration for Scheme development
;; with Geiser, Paredit, and Org-mode integration

;;; Code:

;; Package initialization
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Install required packages if not present
(defvar project-packages
  '(geiser
    geiser-guile
    paredit
    rainbow-delimiters
    company
    flycheck
    magit
    org))

(dolist (pkg project-packages)
  (unless (package-installed-p pkg)
    (package-refresh-contents)
    (package-install pkg)))

;; Project configuration
(defvar project-root (or (getenv "PROJECT_ROOT")
                         (file-name-directory load-file-name)))

;; Geiser configuration for Guile
(require 'geiser)
(require 'geiser-guile)
(setq geiser-guile-binary "guile3")
(setq geiser-active-implementations '(guile))
(setq geiser-default-implementation 'guile)
(setq geiser-guile-load-path (list (concat project-root "/src")))

;; Scheme mode configuration
(add-hook 'scheme-mode-hook 'geiser-mode)
(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)
(add-hook 'scheme-mode-hook 'company-mode)
(add-hook 'scheme-mode-hook 'show-paren-mode)

;; Paredit configuration
(require 'paredit)
(add-hook 'geiser-repl-mode-hook 'paredit-mode)

;; Company mode for auto-completion
(require 'company)
(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 2)
(add-to-list 'company-backends 'geiser-company-backend)

;; Org-mode configuration for literate programming
(require 'org)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((scheme . t)))
(setq org-confirm-babel-evaluate nil)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

;; File associations
(add-to-list 'auto-mode-alist '("\\.scm\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.ss\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . scheme-mode))

;; Tramp configuration for remote development
(require 'tramp)
(setq tramp-default-method "ssh")

;; Project-specific functions
(defun project-run-verification ()
  "Run verification examples in the project."
  (interactive)
  (compile "make verify-examples"))

(defun project-run-tests ()
  "Run project test suite."
  (interactive)
  (compile "make test"))

(defun project-repl ()
  "Start Geiser REPL with project configuration."
  (interactive)
  (run-geiser 'guile))

;; Key bindings
(global-set-key (kbd "C-c v") 'project-run-verification)
(global-set-key (kbd "C-c t") 'project-run-tests)
(global-set-key (kbd "C-c r") 'project-repl)
(global-set-key (kbd "C-c g") 'magit-status)

;; Display configuration
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq scheme-indent-offset 2)
(setq show-trailing-whitespace t)
(column-number-mode 1)
(line-number-mode 1)

;; Initial message
(message "Scheme Formal Verification project configuration loaded")
(message "Project root: %s" project-root)

;; Auto-start Geiser REPL
(when (and (boundp 'server-name)
           (string= server-name "scheme-formal-verification"))
  (project-repl))

(provide 'scheme-formal-verification)
;;; scheme-formal-verification.el ends here