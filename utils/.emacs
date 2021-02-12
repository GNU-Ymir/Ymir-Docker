(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(doc-view-continuous t)
 '(inhibit-startup-screen t)
 '(send-mail-function (quote smtpmail-send-it))
 '(show-paren-mode t)
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 995))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line


(add-to-list 'load-path "~/.elips/d-mode/")
(load "~/.elips/yr-mode")

(setq auto-mode-alist
      (append '(("\\.yr$" . yr-mode))
	      auto-mode-alist))

(setq make-backup-files nil)
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)
(setq c-basic-offset 4)

(setq initial-frame-alist
      '((width . 120)
        (height . 50)))

(menu-bar-mode -1)
(tool-bar-mode -1)
(set-frame-font "-PfEd-DejaVu Sans Mono-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1")

(local-set-key "C-c g" 'org-plot/gnuplot)
