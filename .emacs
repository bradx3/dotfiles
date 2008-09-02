(setq make-backup-files nil)
(setq inhibit-splash-screen t)
(setq default-major-mode 'text-mode)


; directory to put various el files into
(add-to-list 'load-path "/Users/brad/.site-lisp")
(add-to-list 'load-path "/home/brad/.site-lisp")

; loads ruby mode when a .rb file is opened.
(autoload 'ruby-mode "ruby-mode" "Major mode for editing ruby scripts." t)
(setq auto-mode-alist  (cons '(".rb$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '(".rhtml$" . html-mode) auto-mode-alist))