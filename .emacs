
(server-start)

(setq-default line-spacing 2)
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
(set-default 'truncate-lines t)
; show trailing whitespace
(setq-default show-trailing-whitespace t)
; don't popup a new frame for files dropped on dock
(setq ns-pop-up-frames nil)

;; set ido mode
(ido-mode t)
(setq ido-enable-flex-matching t) ; fuzzy matching is a must have
(setq ido-confirm-unique-completion t)
(setq ido-default-buffer-method 'samewindow)
(setq ido-use-filename-at-point nil)
(setq ido-enable-flex-matching t)
(ido-mode t)
(ido-everywhere t)
(icomplete-mode 1)
(set-face-background 'ido-first-match "aqua")
(set-face-foreground 'ido-subdir "lightBlue")
(add-to-list 'ido-ignore-files "\\.DS_Store")

;; directory to put various el files into
(defvar home-dir (concat (expand-file-name "~") "/"))
(add-to-list 'load-path (concat home-dir ".site-lisp"))
(add-to-list 'load-path (concat home-dir ".site-lisp/color-theme-6.6.0"))

;; two panes
(split-window-horizontally)

;; setup path
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-from-shell-PATH)

;; use default mac browser
(setq browse-url-browser-function 'browse-url-default-macosx-browser
      delete-by-moving-to-trash t)

;; set smex
(add-to-list 'load-path (concat home-dir ".site-lisp/smex/"))
(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'smex-update-and-run)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; MODES

;; loads diff mode when git commit file loaded
(setq auto-mode-alist  (cons '("COMMIT_EDITMSG" . diff-mode) auto-mode-alist))
;; loads ruby mode when a .rb file is opened.
(autoload 'ruby-mode "ruby-mode" "Major mode for editing ruby scripts." t)
(setq auto-mode-alist  (cons '(".rb$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '(".rjs$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '(".rjs.js$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '(".rake$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '("Rakefile" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '("Gemfile" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '(".irbrc" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '(".ru$" . ruby-mode) auto-mode-alist))
;; loads html mode when erb file load
(setq auto-mode-alist  (cons '(".html.erb$" . html-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '(".rhtml$" . html-mode) auto-mode-alist))
;;; (defun ruby-eval-buffer () (interactive)
;;; "Evaluate the buffer with ruby."
;;; (shell-command-on-region (point-min) (point-max) "ruby"))

;; haml mode
(add-to-list 'load-path (concat home-dir ".site-lisp/haml"))
(require 'haml-mode nil 't)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
;; sass mode
(add-to-list 'load-path (concat home-dir ".site-lisp/sass"))
(require 'sass-mode nil 't)
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))
;; js mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.as$" . javascript-mode))
(autoload 'javascript-mode "javascript" nil t)
;; css mode
(autoload 'css-mode "css-mode-simple" nil t)
(add-to-list 'auto-mode-alist '(".css$" . css-mode))
;; yaml mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; cucumber mode
(add-to-list 'load-path "~/.site-lisp/cucumber.el")
;; and load it
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))
;; Rinari (rails helpers)
(add-to-list 'load-path (concat home-dir ".site-lisp/rinari"))
(require 'rinari)
(setq rinari-rgrep-file-endings "*.rb *.css *.rhtml *.sass *.haml *.rake *.js *.yml *.csv *.feature")
;; php mode
(add-to-list 'load-path (concat home-dir ".site-lisp/php-mode"))
(require 'php-mode)
;; csv mode
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)
;; Markdown mode
;;(add-to-list 'load-path (concat home-dir ".site-lisp/markdown-mode"))
;;(require 'markdown-mode)
;; rvm mode
(add-to-list 'load-path "~/.site-lisp/rvm")
(require 'rvm)
(rvm-use-default)
;; rspec mode
(require 'mode-compile)
(add-to-list 'load-path "~/.site-lisp/rspec-mode")
(setq rspec-use-rake-flag nil)
(setq rspec-use-rvm t)
;;(setq rspec-spec-command "rvm ree-1.8.6 exec bundle exec spec")
(setq rspec-spec-command "bundle exec spec")
;; (setq rspec-spec-command "which ruby")
(require 'rspec-mode)
;; magit
(add-to-list 'load-path (concat home-dir ".site-lisp/magit"))
(require 'magit)
(global-set-key "\C-c,g" 'magit-status)
;; fuzzy-format to keep tabs/spaces consistent
(require 'fuzzy-format)
(setq fuzzy-format-default-indent-tabs-mode nil)
(global-fuzzy-format-mode t)
;; coffee mode
(add-to-list 'load-path "~/.site-lisp/coffee-mode")
(require 'coffee-mode)
(defun coffee-custom ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2))
(add-hook 'coffee-mode-hook
          '(lambda() (coffee-custom)))
;; textile mode
(require 'textile-mode)
(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))
;; hideshow ruby support
(defun ruby-hs-minor-mode (&optional arg)
  (interactive)
  (require 'hideshow)
  (unless (assoc 'ruby-mode hs-special-modes-alist)
    (setq
     hs-special-modes-alist
     (cons (list 'ruby-mode
                 "\\(def\\|do\\)"
                 "end"
                 "#"
                 (lambda (&rest args) (ruby-end-of-block))
                 ;(lambda (&rest args) (ruby-beginning-of-defun))
                 )
           hs-special-modes-alist)))
  (hs-minor-mode arg))

;; (load "nxhtml/autostart.el")
;;  (setq
;;   nxhtml-global-minor-mode t
;;   mumamo-chunk-coloring 'submode-colored
;;   nxhtml-skip-welcome t
;;   indent-region-mode t
;;   rng-nxml-auto-validate-flag nil
;;   nxml-degraded t)
;;  (add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . eruby-nxhtml-mumamo-mode))

;; KEY BINDINGS

;; Translate C-h to DEL
(keyboard-translate ?\C-h ?\C-?)
;; Define M-h to help  ---  please don't add an extra ' after help!
(global-set-key "\M-h" 'help)
;; add ruby debugging at cursor
(global-set-key (kbd "C-c d") 
		(lambda() (interactive) 
		  (insert "require \"ruby-debug\"; debugger")))


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
                 (setq outline-regexp " *\\(def \\|class\\|module\\|#\\)")))
(setq-default outline-minor-mode-prefix  "\C-c") 

;; load color themes
(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)
;(load-file (concat home-dir ".site-lisp/zenburn.el"))
;(load-file (concat home-dir ".site-lisp/twilight.el"))
(load-file (concat home-dir ".site-lisp/color-theme-subdued.el"))
;(load-file (concat home-dir ".site-lisp/solarized/color-theme-sanityinc-solarized.el"))

(color-theme-subdued)

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
  "Execute or compile the current file."
  (interactive)
  (let (ext-map file-name file-ext prog-name cmd-str)
    (shell-command (buffer-file-name))))
(global-set-key (kbd "<f7>") 'run-current-file)


(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(egg-git-command "/opt/local/bin/git")
 '(fringe-mode 0 nil (fringe))
 '(grep-find-ignored-directories (quote ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "log" "cache" "tmp" "attachment_fu_local_development" "attachments")))
 '(paren-match-face (quote paren-face-match-light))
 '(paren-sexp-mode t)
 '(rspec-use-rake-flag nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#000" :foreground "#d3d7cf" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "apple" :family "Droid Sans Mono")))))

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

(defun restart-passenger ()
  "Restart passenger using the current rinari root" (interactive)
  (shell-command (concat "touch " (rinari-root) "tmp/restart.txt")))

(global-set-key (kbd "C-c ' p") 'restart-passenger)


;; some rinari helpers
(defun kill-buffers-in-subdir (subdir buffer)
  "Kills the given buffer if it is linked to a file in the current rinari project."
  (if (buffer-in-subdir-p subdir buffer)
     (kill-buffer buffer)))

(defun buffer-in-subdir-p (subdir buffer) 
  "Returns true if buffer belongs to the current rinari project"
  (and (buffer-file-name buffer)
       (string-match subdir (buffer-file-name buffer))))

(defun kill-all-rinari-buffers ()
  "Kills all buffers linked to the current rinari project"
  (interactive)
  (let ((path (rinari-root)))
    (if path
	(dolist (buffer (buffer-list))
	  (kill-buffers-in-subdir path buffer)))))


(defun copy-line (&optional arg)
  "Do a kill-line but copy rather than kill.  This function directly calls
kill-line, so see documentation of kill-line for how to use it including prefix
argument and relevant variables.  This function works by temporarily making the
buffer read-only, so I suggest setting kill-read-only-ok to t."
  (interactive "P")
  (toggle-read-only 1)
  (kill-line arg)
  (toggle-read-only 0))
 
(setq-default kill-read-only-ok t)
(global-set-key "\C-c\C-k" 'copy-line)

;; ORG MODE
;; open up my org file if it's around
(defun open-my-org ()
  "Opens my org file"
  (interactive)
  (if (file-exists-p "~/Documents/brad.org")
      (find-file "~/Documents/brad.org")))
(global-set-key "\C-c\ o" 'open-my-org)
(setq org-todo-keywords
      '((sequence "TODO" "TODAY" "STARTED" "DONE")))
(open-my-org)

;; Open up scratch file
(defun open-my-scratch ()
  "Opens my scratch file"
  (interactive)
  (if (file-exists-p "~/Dropbox/*scratch*")
      (find-file "~/Dropbox/*scratch*")))
(global-set-key "\C-c\ s" 'open-my-scratch)
(kill-buffer "*scratch*")
(open-my-scratch)

(defun iwb ()
  "Indents the entire buffer"
  (interactive)
  (indent-region (point-min) (point-max) nil))


; http://emacs-fu.blogspot.com/2010/01/duplicating-lines-and-commenting-them.html
(defun djcb-duplicate-line (&optional commentfirst)
  "comment line at point; if COMMENTFIRST is non-nil, comment the original" 
  (interactive)
  (beginning-of-line)
  (push-mark)
  (end-of-line)
  (let ((str (buffer-substring (region-beginning) (region-end))))
    (when commentfirst
    (comment-region (region-beginning) (region-end)))
    (insert-string
      (concat (if (= 0 (forward-line 1)) "" "\n") str "\n"))
    (forward-line -1)))
;; duplicate a line
(global-set-key (kbd "C-c y") 'djcb-duplicate-line)
;; duplicate a line and comment the first
(global-set-key (kbd "C-c c") (lambda()(interactive)(djcb-duplicate-line t)))



;; final setup of smex
;; needs to be at end of file
(smex-initialize)
