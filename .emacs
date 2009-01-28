(setq inhibit-splash-screen t)
(setq default-major-mode 'text-mode)
(column-number-mode)
(mouse-wheel-mode t)
(setq case-fold-search t)
; auto delete selection on typing
(delete-selection-mode t)
; alt-left right up down to change split frames
(require 'windmove)
(windmove-default-keybindings 'meta)
; change yes-or-no to y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;; set ido mode
(ido-mode t)
(setq ido-enable-flex-matching t) ; fuzzy matching is a must have
(setq ido-confirm-unique-completion t)
(setq ido-default-buffer-method 'samewindow)
(setq ido-use-filename-at-point t)
(setq ido-enable-flex-matching t)
(ido-mode t)
(ido-everywhere t)
(icomplete-mode 1)
(set-face-background 'ido-first-match "aqua")
(set-face-foreground 'ido-subdir "lightBlue")

;; directory to put various el files into
(defvar home-dir (concat (expand-file-name "~") "/"))
(add-to-list 'load-path (concat home-dir ".site-lisp"))
(add-to-list 'load-path (concat home-dir ".site-lisp/color-theme-6.6.0"))


;; MODES

;; loads diff mode when git commit file loaded
(setq auto-mode-alist  (cons '("COMMIT_EDITMSG" . diff-mode) auto-mode-alist))
;; loads html mode when erb file load
(setq auto-mode-alist  (cons '(".html\.erb$" . html-mode) auto-mode-alist))

;; loads ruby mode when a .rb file is opened.
(autoload 'ruby-mode "ruby-mode" "Major mode for editing ruby scripts." t)
(setq auto-mode-alist  (cons '(".\.rb$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '(".rhtml$" . html-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '(".rake$" . ruby-mode) auto-mode-alist))
;; haml mode
(require 'haml-mode nil 't)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
;; sass mode
(require 'sass-mode nil 't)
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))
;; js mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
(autoload 'javascript-mode "javascript" nil t)
;; css mode
(autoload 'css-mode "css-mode-simple" nil t)
(add-to-list 'auto-mode-alist '(".css$" . css-mode))
;; yaml mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; Rinari (rails helpers)
(add-to-list 'load-path (concat home-dir ".site-lisp/rinari"))
(require 'rinari)
;; magit
(add-to-list 'load-path (concat home-dir ".site-lisp/magit"))
(require 'magit)
;; yasnippet
;(add-to-list 'load-path (concat home-dir ".site-lisp/yasnippet-0.5.7"))
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat home-dir ".site-lisp/snippets"))
;; closure mode
(add-to-list 'load-path (concat home-dir "/projects/resources/clojure/clojure-mode"))
(setq inferior-lisp-program "~/bin/clj")
(require 'clojure-auto)

;; KEY BINDINGS

;; Translate C-h to DEL
(keyboard-translate ?\C-h ?\C-?)
;; Define M-h to help  ---  please don't add an extra ' after help!
(global-set-key "\M-h" 'help)

;; BACKUP FILES

;; From http://snarfed.org/space/gnu%20emacs%20backup%20files
;; Put autosave files (ie #foo#) in one place, *not*
;; scattered all over the file system!
(defvar autosave-dir
 (concat "/tmp/emacs_autosaves/" (user-login-name) "/"))

(make-directory autosave-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
   (if buffer-file-name
      (concat "#" (file-name-nondirectory buffer-file-name) "#")
    (expand-file-name
     (concat "#%" (buffer-name) "#")))))

;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar backup-dir (concat "/tmp/emacs_backups/" (user-login-name) "/"))
(setq backup-directory-alist (list (cons "." backup-dir)))

;;; enables outlining for ruby
;;; You may also want to bind hide-body, hide-subtree, show-substree,
;;; show-all, show-children, ... to some keys easy folding and unfolding
(add-hook 'ruby-mode-hook
              '(lambda ()
                 (outline-minor-mode)
                 (setq outline-regexp " *\\(def \\|class\\|module\\)")))
(setq-default outline-minor-mode-prefix  "\C-c") 

;; load color themes
(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)
(load-file (concat home-dir ".site-lisp/zenburn.el"))
(load-file (concat home-dir ".site-lisp/twilight.el"))
(color-theme-twilight)

;; FUNCTIONS

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
 (filename (buffer-file-name)))
    (if (not filename)
 (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
   (message "A buffer named '%s' already exists!" new-name)
 (progn
   (rename-file name new-name 1)
   (rename-buffer new-name)
   (set-visited-file-name new-name)
   (set-buffer-modified-p nil))))))


(defun run-current-file ()
  "Execute or compile the current file.
For example, if the current buffer is the file x.pl,
then it'll call “perl x.pl” in a shell.
The file can be php, perl, python, bash, java.
File suffix is used to determine what program to run."
(interactive)
  (let (ext-map file-name file-ext prog-name cmd-str)
; get the file name
; get the program name
; run it
    (setq ext-map
          '(
            ("php" . "php")
            ("pl" . "perl")
            ("py" . "python")
            ("sh" . "bash")
            ("java" . "javac")
            ("rb" . "ruby")
            )
          )
    (setq file-name (buffer-file-name))
    (setq file-ext (file-name-extension file-name))
    (setq prog-name (cdr (assoc file-ext ext-map)))
    (setq cmd-str (concat prog-name " " file-name))
    (shell-command cmd-str)))
(global-set-key (kbd "<f7>") 'run-current-file)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(fringe-mode 0 nil (fringe))
 '(paren-match-face (quote paren-face-match-light))
 '(paren-sexp-mode t)
 '(transient-mark-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(defun swap-windows ()
 "If you have 2 windows, it swaps them." (interactive) (cond ((not (= (count-windows) 2)) (message "You need exactly 2 windows to do this."))
 (t
 (let* ((w1 (first (window-list)))
	 (w2 (second (window-list)))
	 (b1 (window-buffer w1))
	 (b2 (window-buffer w2))
	 (s1 (window-start w1))
	 (s2 (window-start w2)))
 (set-window-buffer w1 b2)
 (set-window-buffer w2 b1)
 (set-window-start w1 s2)
 (set-window-start w2 s1)))))