;;; early-init.el --- -*- lexical-binding:t -*-

;;; Code:
(setq gc-cons-threshold 100000000)
(setq package-enable-at-startup nil)
;; Don't ask if emacs should follow symlinks
(setq vc-follow-symlinks t)
(setenv "LIBRARY_PATH"
  (string-join
   '("/usr/lib64"
     "/usr/lib/gcc/x86_64-redhat-linux/12/") ":"))

(provide 'early-init)
;;; early-init ends here
