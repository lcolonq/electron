;;; electron --- Emacs eLECTRON -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(pwd)

(load-file "electron.so")
(electron//init-window 800 600 "hi clonkhead")

(while (not (electron//window-should-close))
  (electron//begin-drawing)
  (electron//clear-background electron/color-gray)
  (electron//draw-text "hello computer" 10 25 80 electron/color-green)
  (electron//end-drawing))

(provide 'electron)
;;; electron.el ends here
