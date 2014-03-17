
;; to recompile all files after updating:
;; C-u 0 M-x byte-recompile-directory
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
(defun toggle-main-windows ()
  "Switches between main code windows"
  (interactive)
  (cond
   ((windmove-find-other-window 'left) (windmove-left))
   ((windmove-find-other-window 'right) (windmove-right))
   ((windmove-find-other-window 'up) (windmove-up))
   ((windmove-find-other-window 'down) (windmove-down))))
(global-set-key "\C-x\ o" 'toggle-main-windows)

; change yes-or-no to y-or-n
(fset 'yes-or-no-p 'y-or-n-p)
(set-default 'truncate-lines t)
; show trailing whitespace
(setq-default show-trailing-whitespace t)
; don't popup a new frame for files dropped on dock
(setq ns-pop-up-frames nil)

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")))
  (when (not package-archive-contents)
    (package-refresh-contents)))

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

(global-set-key (kbd "M-/") 'hippie-expand)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(egg-git-command "/opt/local/bin/git")
 '(fringe-mode 0 nil (fringe))
 '(grep-find-ignored-directories (quote ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "log" "cache" "tmp" "attachment_fu_local_development" "attachments" "bootstrap" "archive" "re2" "assets/teacher_toolkit" "assets/repos")))
 '(paren-match-face (quote paren-face-match-light))
 '(paren-sexp-mode t)
 '(rspec-use-rake-flag nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#000" :foreground "#d3d7cf" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "apple" :family "Droid Sans Mono"))))
 '(rainbow-delimiters-depth-1-face ((((background dark)) (:foreground "#ffffff"))))
 '(rainbow-delimiters-depth-2-face ((((background dark)) (:foreground "#6085ff")))))

;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; directory to put various el files into
(defvar home-dir (concat (expand-file-name "~") "/"))

;; two panes
(split-window-vertically)
(split-window-horizontally)

;; window size and location
(defun work-position ()
  "sets up the frame for work"
  (interactive)
  (set-frame-position (selected-frame) 300 0)
  (set-frame-height (selected-frame) 72)
  (set-frame-width (selected-frame) 170)
  (message "moved"))

(if (or (string-equal system-name "brad-work.local")
        (string-match "pascal.net.au" system-name))
    (work-position))

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
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'smex-update-and-run)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; rainbow delimiters
(setq-default frame-background-mode 'dark)

;; ruby block
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)

;; browse kill ring
(setq browse-kill-ring-quit-action 'save-and-restore)

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
(setq auto-mode-alist  (cons '(".gemspec$" . ruby-mode) auto-mode-alist))
(add-hook 'ruby-mode-hook 'rainbow-delimiters-mode)
;; loads html mode when erb file load
(setq auto-mode-alist  (cons '(".html.erb$" . html-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '(".rhtml$" . html-mode) auto-mode-alist))
;;; (defun ruby-eval-buffer () (interactive)
;;; "Evaluate the buffer with ruby."
;;; (shell-command-on-region (point-min) (point-max) "ruby"))

;; haml mode
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
;; sass mode
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))
;; js mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.as$" . javascript-mode))
(setq js-indent-level 2)
(autoload 'javascript-mode "javascript" nil t)
;; css mode
(autoload 'css-mode "css-mode-simple" nil t)
(add-to-list 'auto-mode-alist '(".css$" . css-mode))
;; yaml mode
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; cucumber mode
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))
;; Rinari (rails helpers)
(setq rinari-rgrep-file-endings "*.rb *.css *.rhtml *.sass *.haml *.rake *.js *.yml *.csv *.feature *.handlebars *.coffee *.erb *.emblem")
(global-rinari-mode)
;; csv mode
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)
;; rvm mode
(rvm-use-default)
(global-set-key "\C-c'v" 'rvm-activate-corresponding-ruby)
;; rspec mode
(setq rspec-use-rake-flag nil)
(setq rspec-use-rake-when-possible nil)
(setq rspec-use-rvm t)
(setq rspec-use-bundler-when-possible nil)
(setq rspec-use-opts-file-when-available t)
(defadvice rspec-compile (around rspec-compile-around)
  "Use BASH shell for running the specs because of ZSH issues."
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))
(ad-activate 'rspec-compile)
;; markdown mode
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
;; pretty lambda mode
(global-pretty-lambda-mode)
;; magit
(global-set-key "\C-c,g" 'magit-status)
;; mo-git-blame
(autoload 'mo-git-blame-file "mo-git-blame" nil t)
(autoload 'mo-git-blame-current "mo-git-blame" nil t)
;; coffee mode
(defun coffee-custom ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2))
(add-hook 'coffee-mode-hook
          '(lambda() (coffee-custom)))
;; textile mode
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
		  (insert "debugger")))

;; work helpers

;; from emacs-rails
(defun backward-ruby-word ()
  (if (looking-back "[-a-zA-Z_#:*]+" (line-beginning-position) t)
      (goto-char (match-beginning 0))))
(defun forward-ruby-word (n)
  (if (> 0 n)
      (when (search-backward-regexp "[^-a-zA-Z_#:*][-a-zA-Z_#:*]+" nil t (- n))
        (forward-char)
        (point))
    (when (search-forward-regexp "[-a-zA-Z_#:*]+" nil t n)
      (goto-char (match-end 0)))))

(defun blake-rgrep (&optional arg)
  "Based on rinari..."
  (interactive "P")
  (grep-compute-defaults)
  (if arg (call-interactively 'rgrep)
    (let ((query))
      (if mark-active
          (setq query (buffer-substring-no-properties (point) (mark)))
        (setq query (thing-at-point 'ruby-word)))
      (funcall 'rgrep
               (read-from-minibuffer "search for: " query)
               "*.rb *.haml *.rhtml *.erb *.coffee *.rake *.sass *.scss Gemfile Gemfile.lock *.emblem *.yml"
               "~/Blake"))))

(global-set-key (kbd "C-c , b") 'blake-rgrep)
(global-set-key (kbd "C-c ' b") 'blake-rgrep)

;; BACKUP FILES

;; From http://snarfed.org/space/gnu%20emacs%20backup%20files
;; Put autosave files (ie #foo#) in one place, *not*
;; scattered all over the file system!
(defvar autosave-dir
 (concat "~/tmp/emacs_autosaves/" (user-login-name) "/"))

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
(color-theme-initialize)
(setq color-theme-is-global t)
(load-file (concat home-dir ".emacs.d/color-theme-subdued.el"))

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

(defun github-current-file ()
  "Show the current file in github"
  (interactive)
  (shell-command
   (concat "cd "
	   (current-git-repo)
	   "&& gh "
	   (replace-regexp-in-string (current-git-repo) "" buffer-file-name))))


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


;; some project helpers
(defun kill-buffers-in-subdir (subdir buffer)
  "Kills the given buffer if it is linked to a file in the current rinari project."
  (if (buffer-in-subdir-p subdir buffer)
     (kill-buffer buffer)))

(defun buffer-in-subdir-p (subdir buffer)
  "Returns true if buffer belongs to the current rinari project"
  (and (buffer-file-name buffer)
       (string-match subdir (buffer-file-name buffer))))

(defun current-git-repo ()
  "Returns the path to the git repo for the current buffer"
  (magit-get-top-dir (file-name-directory buffer-file-name)))

(defun kill-all-project-buffers ()
  "Kills all buffers linked to the current git project"
  (interactive)
  (let ((path (magit-get-top-dir (file-name-directory buffer-file-name))))
  ;(let ((path (rinari-root)))
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
      '((sequence "STARTED" "TODAY" "TODO" "WAITING" "DONE")))
(global-set-key "\C-c\ a" 'org-agenda)
(setq org-agenda-custom-commands
           '(("j" occur-tree "TODAY")))
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

;; From https://github.com/garybernhardt/dotfiles/blob/master/.emacs
; GRB: open temporary buffers in a dedicated window split
(setq special-display-regexps
        '("^\\*Completions\\*$"
          "^\\*Help\\*$"
          "^\\*grep\\*$"
          "^\\*Apropos\\*$"
          "^\\*elisp macroexpansion\\*$"
          "^\\*local variables\\*$"
          "^\\*Compile-Log\\*$"
          "^\\*Quail Completions\\*$"
          "^\\*Occur\\*$"
          "^\\*frequencies\\*$"
          "^\\*compilation\\*$"
          "^\\*rspec-compilation\\*$"
          "^\\*Locate\\*$"
          "^\\*Colors\\*$"
          "^\\*tumme-display-image\\*$"
          "^\\*SLIME Description\\*$"
          "^\\*.* output\\*$"           ; tex compilation buffer
          "^\\*TeX Help\\*$"
          "^\\*Shell Command Output\\*$"
          "^\\*Async Shell Command\\*$"
          "^\\*Backtrace\\*$"))
(setq grb-temporary-window (nth 2 (window-list)))
(defun grb-special-display (buffer &optional data)
  (let ((window grb-temporary-window))
    (with-selected-window window
      (switch-to-buffer buffer)
      window)))
(setq special-display-function #'grb-special-display)
(setq-default display-buffer-reuse-frames t)

;; stop emacs from splitting http://blog.mpacula.com/2012/01/28/howto-prevent-emacs-from-splitting-windows/
(setq split-height-threshold 1200)
(setq split-width-threshold 2000)

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

;; indent ruby multiline fix
;; http://stackoverflow.com/questions/4412739/emacs-ruby-mode-indentation-behavior/7622971#7622971
(defadvice ruby-indent-line (after line-up-args activate)
  (let (indent prev-indent arg-indent)
    (save-excursion
      (back-to-indentation)
      (when (zerop (car (syntax-ppss)))
        (setq indent (current-column))
        (skip-chars-backward " \t\n")
        (when (eq ?, (char-before))
          (ruby-backward-sexp)
          (back-to-indentation)
          (setq prev-indent (current-column))
          (skip-syntax-forward "w_.")
          (skip-chars-forward " ")
          (setq arg-indent (current-column)))))
    (when prev-indent
      (let ((offset (- (current-column) indent)))
        (cond ((< indent prev-indent)
               (indent-line-to prev-indent))
              ((= indent prev-indent)
               (indent-line-to arg-indent)))
        (when (> offset 0) (forward-char offset))))))


(defun goto-column-number (number)
"Untabify, and go to a column number within the current line (1 is beginning
of the line)."
(interactive "nColumn number ( - 1 == C) ? ")
(beginning-of-line)
(untabify (point-min) (point-max))
(while (> number 1)
 (if (eolp)
     (insert ? )
   (forward-char))
 (setq number (1- number))))

;; final setup of smex
;; needs to be at end of file
(smex-initialize)
