;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Jonathan Prairie"
      user-mail-address "jon.a.prairie@gmail.com")
(setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
       doom-variable-pitch-font (font-spec :family "sans" :size 13))

(setq doom-theme 'deeper-blue)

(setq org-directory "~/external/org/"
      org-agenda-files (directory-files-recursively org-directory "\\.org$")
      org-agenda-file-menu-enabled nil
      org-startup-folded 'content
      org-export-async-init-file "~/.config/doom/async-init.el"
      )

(setq truncate-lines t)
(setq display-line-numbers-type 'visual)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq evil-cross-lines t)
(setq sly-contribs '(sly-stepper sly-repl-ansi-color sly-fancy sly-asdf sly-macrostep sly-quicklisp))
(setq standard-indent 2
      js-indent-level 2)

(remove-hook 'doom-first-input-hook
    #'evil-snipe-mode)

(defun jp/send-space ()
  (interactive)
  (push '(t . 32) unread-command-events))
(defun jp/send-key-seq (seq)
  (interactive)
  (setq unread-command-events (listify-key-sequence seq)))
(defun jp/send-doom-leader ()
  (interactive)
  (jp/send-key-seq "\C-@"))

(after! key-chord
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay .05)
  (key-chord-define evil-insert-state-map  "jk" 'evil-normal-state))

(map!
 "<f3>" 'kill-current-buffer
 "<f5>" 'evil-prev-buffer
 "<f6>" 'evil-next-buffer
 "<f7>" 'evil-scroll-up
 "<f8>" 'evil-scroll-down
 "S-<f7>" 'evil-goto-first-line
 "S-<f8>" 'evil-goto-line
 "<f9>" 'window-swap-states
 "<f10>" '+workspace/switch-left
 "<f11>" '+workspace/switch-right

 "ESC h" 'evil-window-left
 "ESC l" 'evil-window-right
 "ESC j" 'evil-window-down
 "ESC k" 'evil-window-up

 "C-h" 'backward-kill-word ;; ctrl+backspace

 :n "g G" 'evil-goto-char
 :n "g o" doom-leader-workspace-map
 :n "s" 'evil-surround-region
 :n "S" 'evil-Surround-region
 )

;; i'd rather use s for surround
;; custom prefix binds can go on g for now
;;(map! :map evil-normal-state-map
;;      (:prefix ("s" . "custom prefix")
;;               :desc "xref forward" "n" 'xref-go-forward
;;               :desc "xref backward" "p" 'xref-go-back))

(map! :map doom-leader-open-map
      :desc "Org capture" "c" 'org-capture
      :prefix ("s" . "Org capture todo")
      :desc "With context" "k" (lambda () (interactive) (org-capture nil "t"))
      :desc "Without context" "j" (lambda () (interactive) (org-capture nil "T"))
      )

(map! :leader
      :desc "Switch other buffer window" ">" 'switch-to-buffer-other-window
      :desc "Toggle messages" "b j" '(lambda () (interactive)
                                       (if (string= (buffer-name) "*Messages*")
                                           (evil-prev-buffer)
                                         (switch-to-buffer "*Messages*")))
      :desc "Switch to scratch buffer" "b x" 'doom/switch-to-project-scratch-buffer
      :desc "Pop up scratch buffer" "b X" 'doom/open-project-scratch-buffer
      :desc "Maximize window" "w k" 'maximize-window
      :desc "Minimize window" "w j" 'minimize-window
      )

(map! :map org-mode-map
      :localleader
      :prefix ("s" . "+tree/subtree")
      :desc "archive item" "a" 'org-archive-subtree-default
      :desc "toggle archive tag" "A" 'toggle-archive-tag
      :desc "insert item below" "o" '+org/insert-item-below
      :desc "insert item below" "O" '+org/insert-item-above)

;; need to use evil-embrace instead
(defun jp-surround-org-src-block ()
  (message "%s" this-command)
  (let ((lang (evil-surround-read-from-minibuffer "" "")))
    (cons (format "\n#+BEGIN_SRC %s\n" lang) "\n#+END_SRC\n")))

;; maybe update to use embrace-with-org-block at some point?
;; does completing read on block type, language and arguments,
;; then returns (prefix . suffix).
(defun insert-src ()
  (interactive)
  (let* ((result-pair (embrace-with-org-block))
         (prefix (car result-pair))
         (suffix (cdr result-pair)))
    (when evil-insert-state-minor-mode
      (evil-open-below 1))
    (evil-beginning-of-line)
    (evil-normal-state t)
    (insert (format "%s\n%s" prefix suffix))
    (org-edit-special)))

;; (map! :map doom-leader-insert-map
;;       :prefix ("a" . "+Block")
;;       (:prefix ("s" . "Src block")
;;        :desc "js" "j" (insert_src "javascript")
;;        :desc "elisp" "m" (insert_src "emacs-lisp :lexical t")
;;        :desc "python" "p" (insert_src "python")
;;        ))
(map! :map doom-leader-insert-map
      :desc "Insert org block" "b" 'insert-src)

(map! :leader
      :prefix ("c" . "+code")
      :desc "abort special" "u"
      (lambda ()
        (interactive)
        (cond
         (org-src-mode (org-edit-src-abort))
         (org-capture-mode (org-capture-kill))
         (with-editor-mode (with-editor-cancel nil))
         (t (message "Special edit commands don't apply here"))
        ))
      :desc "edit/close special" ";"
      (lambda ()
        (interactive)
        (cond
         (org-src-mode (org-edit-src-exit))
         ((and org-capture-mode (not (org-in-src-block-p))) (org-capture-finalize))
         (evil-org-mode (org-edit-special))
         (with-editor-mode (with-editor-finish nil))
         (t (message "Special edit commands don't apply here"))
         ))
      :desc "execute special" "'" 'org-babel-execute-src-block
      )

(map! :map doom-leader-workspace-map
      "N" nil
      "m" '+workspace/new-named
      "k" '+workspace/kill
      "K" '+workspace/delete
      "d" nil
      "D" nil
      "a" '+workspace/switch-to-0 
      "s" '+workspace/switch-to-1 
      "d" '+workspace/switch-to-2 
      "f" '+workspace/switch-to-3 
      "q" '+workspace/switch-to-4 
      "w" '+workspace/switch-to-5 
      "e" '+workspace/switch-to-6 

      :map doom-leader-open-map
      :desc "toggle vterm popup" "v" '+vterm/toggle)


(key-chord-define evil-insert-state-map "dk" 'jp/send-doom-leader)

(after! evil-org
  (map! :map evil-org-mode-map
        :inv "M-j" nil
        :inv "M-k" nil
        :inv "M-l" nil
        :inv "M-h" nil
        :inv "ESC j" nil
        :inv "ESC k" nil
        :inv "ESC l" nil
        :inv "ESC h" nil
        ))

(map! :map org-mode-map
      :n "ESC j" nil
      :n "ESC k" nil
      :n "ESC l" nil
      :n "ESC h" nil
      :n "ESC j" 'evil-window-down
      :n "ESC k" 'evil-window-up
      :n "ESC l" 'evil-window-right
      :n "ESC h" 'evil-window-left
      )

(map! :map org-mode-map
      "M-j" nil
      "M-j" 'evil-window-down
      "M-k" nil
      "M-k" 'evil-window-up
      "M-l" nil
      "M-l" 'evil-window-right
      "M-h" nil
      "M-h" 'evil-window-left
      "ESC j" nil
      "ESC k" nil
      "ESC l" nil
      "ESC h" nil
      "ESC j" 'evil-window-down
      "ESC k" 'evil-window-up
      "ESC h" 'evil-window-left
      "ESC l" 'evil-window-right
      :localleader
      :desc "tangle" "z" 'org-babel-tangle)

(map!
 :n "U" 'evil-redo
 :nm "H" 'evil-beginning-of-line
 :nm "L" 'evil-end-of-line
 :nm "0" 'evil-beginning-of-line
 :nm "$" 'evil-end-of-line
 :nm ":" 'evil-repeat-find-char
 :n ";" 'evil-ex
 :nm "TAB" 'evil-jump-item
 :vo "i l" 'evil-inner-paren
 :vo "a l" 'evil-a-paren
 )

(map!
 :leader
 "l" doom-leader-workspace-map)
;;(map! :leader
;; :n :desc "new workspace" "l w a" '+workspace/new
;; :n :desc "new named workspace" "l w n" '+workspace/new-named
;; :n :desc "delete current workspace" "l w d" '+workspace/delete)

(map! :leader
      :desc "M-x" ";" 'execute-extended-command
      :desc "Eval Expression" ":" 'pp-eval-expression)

;;(setq doom-leader-alt-key "C-@")
(setq doom-leader-alt-key "M-<SPC>")
;(setq doom-localleader-alt-key "C-@ m")
(setq doom-localleader-alt-key "M-<SPC> m")
(push 'normal doom-leader-alt-key-states)
(push 'visual doom-leader-alt-key-states)
(push 'motion doom-leader-alt-key-states)

(defun jp/org-babel-tangle ()
  (let ((file (buffer-file-name)))
  (async-start
   (lambda ()
     (require 'org)
     (let ((org-confirm-babel-evaluate nil))
       (org-babel-tangle-file file))))))

(add-hook
 'org-mode-hook
 (lambda () (add-hook 'after-save-hook #'jp/org-babel-tangle)))

(setq which-key-idle-delay 0.01)

(defun rainbow-delimiters-mode () nil)

(use-package! org-roam
  :defer t
  :init
  (setq ;org-roam-directory (file-truename "~/org/zettel/")
   org-roam-directory (file-truename "~/external/org/zettel/")
   org-roam-db-gc-threshold most-positive-fixnum
   org-id-link-to-org-use-id t
   org-roam-completion-everywhere t
   org-roam-v2-ack t)
  ;; on Windows we need to set this to 'immediate, otherwise things run slowly for some reason
  ;; I think there's some sqlite timeout issues? See: https://github.com/org-roam/org-roam/issues/1289#issuecomment-744046148
  (setq org-roam-db-update-method 'immediate)
  :config
  (org-roam-db-autosync-mode +1)
  (setq org-roam-capture-templates
        '(("m" "main" plain
           "%?"
           :if-new (file+head "main/${slug}.org"
                              "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("r" "reference" plain "%?"
           :if-new
           (file+head "reference/${title}.org" "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)))
  (set-company-backend! 'org-mode '(company-capf))
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))
  (setq org-roam-node-display-template
        (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag))))

;;(use-package! deft
;;  :init
;;  (setq deft-directory "~/external/org"
;;        deft-extensions '("md" "org" "txt")
;;        deft-recursive t))

(setq lsp-restart 'ignore)

;;(use-package! lsp-pyright
;;  :defer t
;;  :custom (lsp-pyright-langserver-command "pyright")
;;  :hook (python-mode . (lambda ()
;;                         (require 'lsp-pyright)
;;                         (lsp-deferred))))

(after! cider
  (map! :map cider-mode-map
        :n "<f6>" nil
        :n "<f9>" nil
        :n "<f10>" nil
        :n "<f11>" nil))

(map! :map clojure-mode-map
      :localleader
      :n "c" 'cider-connect-sibling-clj)

(delete 'vterm-mode evil-escape-excluded-major-modes)
(map! :map vterm-mode-map
      "M-h" 'evil-window-left
      "M-l" 'evil-window-right
      "M-j" 'evil-window-down
      "M-k" 'evil-window-up
      "<f3>" 'kill-this-buffer
      "<f5>" 'evil-prev-buffer
      "<f6>" 'evil-next-buffer
      "<f7>" 'evil-scroll-up
      "<f8>" 'evil-scroll-down
      "S-<f7>" 'evil-goto-first-line
      "S-<f8>" 'evil-goto-line
      "<f9>" 'window-swap-states
      "<f10>" '+workspace/switch-left
      "<f11>" '+workspace/switch-right
      "C-c C-x" 'vterm--self-insert
      )

(setq anki-editor-api-host "jon-desktop-main"
      anki-editor-org-tags-as-anki-tags nil
      anki-editor-latex-style 'mathjax)

(add-hook
 'org-mode
 (lambda ()
   (anki-editor-mode t)
   (push '(?i . ("\(" . "\)")) evil-surround-pairs-alist)
   (push '(?j . ("\[" . "\]")) evil-surround-pairs-alist)))

(defun jp/insert-last-screenshot ()
  (interactive)
  (let* ((dir "~/external/org/assets/img/")
         (images (directory-files dir t "[^\.].*"))
         (images (sort images
                       :key (lambda (f)
                              (time-convert (file-attribute-modification-time (file-attributes f) 'integer)))
                       :reverse t))
         (image (car images)))
    (insert (format "[[file:%s]]" image))))

(map! :map org-mode-map
      :localleader
      :prefix ("m" . "org-roam")
      (:prefix ("a" . "anki")
       :desc "push all to anki" "p" (lambda () (interactive)
                                      (anki-editor-async-push-notes 'agenda))
       :desc "push this to anki" "o" 'anki-editor-push-note-at-point
       :desc "push new to anki" "n" 'anki-editor-push-new-notes
       :desc "push failed to anki" "f" 'anki-editor-retry-failed-notes
       :desc "insert anki note" "i" 'anki-editor-insert-note
       :desc "cloze dwim" "c" 'anki-editor-cloze-dwim
       :desc "push agenda to anki" "a" (lambda () (interactive)
                                         (anki-editor-push-notes 'agenda))
       :desc "FORCE push agenda to anki" "F" (lambda () (interactive)
                                               (anki-editor-force-push-notes 'agenda))
       :desc "insert last screenshot" "j" 'jp/insert-last-screenshot
       ))

(defun consult-ripgrep-roam ()
  "Interactively search org-roam notes"
  (interactive)
  (let ((consult-ripgrep-args "rg --null --ignore-case --type org --line-buffered --color=auto --max-columns=500 --no-heading --line-number"))
    (consult-ripgrep org-roam-directory)))

(map! :map doom-leader-search-map
      :desc "Search text in org-roam notes" "n" 'consult-ripgrep-roam
      )

(use-package! anaphora
  :defer t
  )
(use-package! jpt-apl-mode
  :defer t
  :config
  (map! :leader
        :prefix ("j" . "misc")
        :desc "apl expand" "l" 'jpt-apl-match-keycord))

(map! :map doom-leader-git-map
      :desc "Ignore" "i" 'magit-gitignore-in-topdir)

(setq sly-lisp-implementations nil)
(push '(ros-ccl ("ros" "-L" "ccl-bin" "run"))
      sly-lisp-implementations)
(push '(ros-sbcl ("ros" "-L" "sbcl-bin" "run"))
      sly-lisp-implementations)

(setq leetcode--lang "javascript")
(setq anki-prog-note-type-default "Hacker Hero 2")

(setq corfu-auto-delay 1)

(map! :map vertico-map
      "M-<ENT>" 'vertico-exit-input)

(after! org
  (setq org-fontify-quote-and-verse-blocks nil
        org-fontify-whole-heading-line nil
        org-hide-leading-stars nil
        org-startup-indented nil)
  (push '("T" "Personal todo w/o context" entry
          (file+headline +org-capture-todo-file "Inbox")
          "* [ ] %?\n%i\n" :prepend t)
        org-capture-templates))

(global-font-lock-mode 1)
