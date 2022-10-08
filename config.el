;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Jonathan Prairie"
      user-mail-address "jon.a.prairie@gmail.com")
(setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
       doom-variable-pitch-font (font-spec :family "sans" :size 13))

(setq doom-theme 'deeper-blue)

(setq org-directory "~/org/")

(setq display-line-numbers-type 'relative)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(defun jp/send-space ()
  (interactive)
  (push '(t . 32) unread-command-events))

(map!
 "<f1>" help-map
 "<f3>" 'kill-this-buffer
 "<f5>" 'evil-prev-buffer
 "<f6>" 'evil-next-buffer
 "<f7>" 'evil-scroll-up
 "<f8>" 'evil-scroll-down
 "S-<f7>" 'evil-goto-first-line
 "S-<f8>" 'evil-goto-line
 "<f9>" '+workspace/switch-left
 "<f10>" '+workspace/switch-right

 "ESC h" 'evil-window-left
 "ESC l" 'evil-window-right
 "ESC j" 'evil-window-down
 "ESC k" 'evil-window-up

 "C-h" 'backward-kill-word
 )

(map! :map evil-org-mode-map
      :inv "M-j" nil
      :inv "M-k" nil
      :inv "M-l" nil
      :inv "M-h" nil)

(map!
 :n "U" 'evil-redo
 :nm "H" 'evil-beginning-of-line
 :nm "L" 'evil-end-of-line
 :nm ":" 'evil-repeat-find-char
 :n ";" 'evil-ex
 :nm "TAB" 'evil-jump-item
 :vo "i l" 'evil-inner-paren
 :vo "a l" 'evil-a-paren
 )

(map! :leader
 :n :desc "new workspace" "d w a" '+workspace/new
 :n :desc "new named workspace" "d w n" '+workspace/new-named)

(map! :leader
      :desc "M-x" ";" 'execute-extended-command
      :desc "Eval Expression" ":" 'pp-eval-expression)

(map! :map org-mode-map
      :localleader
      :desc "tangle" "z" 'org-babel-tangle)

(setq doom-leader-alt-key "C-@")
(setq doom-localleader-alt-key "C-@ m")

(map! :n "C-@" 'jp/send-space)

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
  :init
  (setq org-roam-directory (file-truename "~/org/zettel/")
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
(org-roam-setup)

(setq lsp-restart 'ignore)

(after! cider
  (map! :map cider-mode-map
        :n "<f6>" nil))

(map! :map clojure-mode-map
      :localleader
      :n "c" 'cider-connect-sibling-clj)

(delete 'vterm-mode evil-escape-excluded-major-modes)
