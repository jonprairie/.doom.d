;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Jonathan Prairie"
      user-mail-address "jon.a.prairie@gmail.com")
(setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
       doom-variable-pitch-font (font-spec :family "sans" :size 13))

(setq doom-theme 'deeper-blue)

(setq org-directory "~/org/")

(setq display-line-numbers-type 'relative)

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

 "ESC h" 'evil-window-left
 "ESC l" 'evil-window-right
 "ESC j" 'evil-window-down
 "ESC k" 'evil-window-up

 "C-h" 'backward-kill-word
 )

(map!
 :n "U" 'evil-redo
 :n "H" 'evil-beginning-of-line
 :n "L" 'evil-end-of-line
 :n ";" 'evil-ex
 :n ":" 'evil-repeat-find-char
 )

(map! :leader
      :desc "M-x" ";" 'execute-extended-command
      :desc "Eval Expression" ":" 'pp-eval-expression)

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
  (defun jp/tag-new-node-as-draft ()
    (org-roam-tag-add '("draft")))
  (add-hook 'org-roam-capture-new-node-hook #'jp/tag-new-node-as-draft)
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
