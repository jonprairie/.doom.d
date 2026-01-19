;;; async-init.el -*- lexical-binding: t; -*-

(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

(require 'org) 
(require 'ox)
(require 'cl)  
(setq org-export-async-debug nil)

(require 'anki-editor)
