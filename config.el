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
