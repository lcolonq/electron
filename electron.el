;;; electron --- Emacs eLECTRON -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)

(load-file "~/src/fig/fig-network.el")
(setq fig//event-handlers
  (list
   (cons '(monitor twitch chat incoming) (lambda (_) (cl-incf counter)))))
(fig/connect)

(load-file "electron.so")
(electron//init-window 800 600 "hi clonkhead")
(electron//set-target-fps 60)

(defvar joel (electron//load-texture "joel.gif"))
(defvar joel-w 498)
(defvar joel-h 164)
(defvar joel-x 0)
(defvar joel-y 0)
(defvar counter 0)

(defun update ()
  (when (electron//is-key-down ?W) (cl-decf joel-y))
  (when (electron//is-key-down ?S) (cl-incf joel-y))
  (when (electron//is-key-down ?A) (cl-decf joel-x))
  (when (electron//is-key-down ?D) (cl-incf joel-x))
  (electron//begin-drawing)
  (electron//clear-background electron/color-gray)
  (electron//draw-text (format "%s" counter) 10 25 80 electron/color-blue)
  (electron//draw-texture joel joel-x joel-y (electron//make-color 255 0 255 127))
  (electron//end-drawing))

(defvar timer nil)
(defun run-timer ()
  "Run the update timer."
  (when timer
    (cancel-timer timer))
  (if (electron//window-should-close)
      (let ((confirm-kill-emacs nil)
            (confirm-kill-processes nil))
        (kill-emacs))
    (update)
    (setq
     timer
     (run-with-timer (/ 1.0 30) nil #'run-timer))))
(run-timer)

(provide 'electron)
;;; electron.el ends here
