;;; electron --- Emacs eLECTRON -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'ht)

(defun clamp (low high x)
  (min (max x low) high))

(defun distance (x0 y0 x1 y1)
  (let ((dx (- x1 x0))
        (dy (- y1 y0)))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defun lerp (v0 v1 t0)
  (let ((t1 (clamp 0.0 1.0 t0)))
    (+ (* (- 1.0 t1) v0) (* t1 v1))))

(defun ease-sin (v0 v1 t0)
  (lerp v0 v1 (sin (* 0.5 float-pi (clamp 0.0 1.0 t0)))))

(defun to-polar (x y)
  (cons
   (atan y x)
   (distance x y 0 0)))

(defun to-cartesian (theta r)
  (cons
   (* r (cos theta))
   (* r (sin theta))))

(defun load-level (path)
  (with-temp-buffer
    (insert-file-contents path)
    (read (buffer-string))))

(load-file "electron.so")
(electron//init-window 800 600 "Uhhh, What? The Relationship Between The Polarities In My Ikaruga Clone Are Surprisingly \"Yuri\"?!")
(electron//init-audio-device)
(electron//set-target-fps 60)

(defvar screen (electron//load-rendertexture 128 128))
(defvar screenrect (electron//make-rectangle 0.0 0.0 128.0 -128.0))
(defvar finalrect (electron//make-rectangle 0.0 0.0 768.0 768.0))
(defvar tex-guywhite (electron//load-texture "assets/textures/guywhite.png"))
(defvar tex-guyblack (electron//load-texture "assets/textures/guyblack.png"))
(defvar tex-projectile (electron//load-texture "assets/textures/projectile.png"))
(defvar tex-player-projectile (electron//load-texture "assets/textures/playerprojectile.png"))
(defvar tex-foe-sinner (electron//load-texture "assets/textures/foe/sinner.png"))
(defvar tex-foe-orb (electron//load-texture "assets/textures/foe/orb.png"))
(defvar tex-anim-explosion (--map (electron//load-texture (format "assets/textures/explosion/animation%d.png" (+ it 1))) (-iota 17)))
(defvar sfx-switch (electron//load-sound "assets/sfx/switch.wav"))
(defvar sfx-explosion (electron//load-sound "assets/sfx/explosion.wav"))
(defvar sfx-shoot (electron//load-sound "assets/sfx/shoot.wav"))
(defvar sfx-yoda-death (electron//load-sound "assets/sfx/yodadeath.ogg"))
(defvar level-sample (load-level "assets/levels/sample.el"))

(defvar game-state 'game)
(defvar game-level level-sample)
(defvar game-tick 0)
(defvar game-score 0)

(defvar guy-x 0.0)
(defvar guy-y 0.0)
(defvar guy-mode t)
(defvar guy-mode-locked nil)
(defvar guy-exploding nil)
(defvar guy-shooting nil)
(defun contacting-guy (x y)
  (and
   (<= (abs (- guy-x x)) 1)
   (<= (abs (- guy-y y)) 2)))
(defun render-guy ()
  (electron//draw-texture
   (if guy-mode tex-guywhite tex-guyblack)
   (round (- guy-x 4.0))
   (round (- guy-y 4.0))
   electron/color-white))

(defvar player-projectile-next-id 0)
(defvar player-projectiles (ht-create))
(cl-defstruct player-projectile mode x y)
(defun spawn-player-projectile (mode)
  (ht-set!
   player-projectiles player-projectile-next-id
   (make-player-projectile :mode mode :x (- guy-x 3) :y (- guy-y 2)))
  (cl-incf player-projectile-next-id)
  (ht-set!
   player-projectiles player-projectile-next-id
   (make-player-projectile :mode mode :x (+ guy-x 3) :y (- guy-y 2)))
  (cl-incf player-projectile-next-id))
(defun update-player-projectile (id p)
  (setf (player-projectile-y p) (- (player-projectile-y p) 2.0))
  (if (and
       (>= (player-projectile-x p) 0) (<= (player-projectile-x p) 128)
       (>= (player-projectile-y p) 0) (<= (player-projectile-y p) 128))
      (--each (ht->alist foes)
        (when (and
               (not (eq (foe-mode (cdr it)) (player-projectile-mode p)))
               (contacting-foe (cdr it) (player-projectile-x p) (player-projectile-y p)))
          (electron//play-sound sfx-yoda-death)
          (ht-remove! foes (car it))))
    (ht-remove! player-projectiles id)))
(defun update-player-projectiles ()
  (--each (ht->alist player-projectiles)
    (update-player-projectile (car it) (cdr it))))
(defun render-player-projectile (p)
  (electron//draw-texture
   tex-player-projectile
   (round (player-projectile-x p))
   (round (player-projectile-y p))
   (if (player-projectile-mode p) electron/color-white electron/color-black)))
(defun render-player-projectiles ()
  (--each (ht-values player-projectiles)
    (render-player-projectile it)))

(defvar projectile-next-id 0)
(defvar projectiles (ht-create))
(defun spawn-projectile (ttl mode f)
  (ht-set!
   projectiles projectile-next-id
   (list game-tick ttl mode f)
   )
  (cl-incf projectile-next-id))
(defun render-projectiles ()
  (--each (ht->alist projectiles)
    (let* ((id (car it))
           (p (cdr it))
           (diff (- game-tick (car p))))
      (if (> diff (cadr p))
          (ht-remove! projectiles id)
       (let ((loc (funcall (cadddr p) diff)))
          (if (and
               (not guy-exploding)
               (contacting-guy (car loc) (cdr loc)))
              (progn
                (ht-remove! projectiles id)
                (if (eq guy-mode (caddr p))
                    (cl-incf game-score 1)
                  (electron//play-sound sfx-explosion)
                  (setq guy-exploding 0)
                  (setq game-state 'over)))
            (electron//draw-texture
             tex-projectile
             (round (- (car loc) 1.0))
             (round (- (cdr loc) 1.0))
             (if (caddr p) electron/color-white electron/color-black))))))))

(defvar foe-next-id 0)
(defvar foes (ht-create))
(cl-defstruct foe spawntime type mode r x y state move)
(cl-defstruct foe-sinner timer reset angle)
(cl-defstruct foe-orb timer reset)
(defun spawn-foe (type mode r &optional st move)
  (ht-set!
   foes foe-next-id
   (make-foe :spawntime game-tick :type type :mode mode :r r :x 0 :y 0 :state st :move move))
  (cl-incf foe-next-id))
(defun contacting-foe (f x y)
  (<= (distance x y (foe-x f) (foe-y f)) (foe-r f)))
(defun render-foe (f)
  (cl-case (foe-type f)
    (sinner
     (electron//draw-texture
      tex-foe-sinner
      (round (- (foe-x f) 3.0))
      (round (- (foe-y f) 3.0))
      (if (foe-mode f) electron/color-white electron/color-black)))
    (orb
     (electron//draw-texture
      tex-foe-orb
      (round (- (foe-x f) 4.0))
      (round (- (foe-y f) 4.0))
      (if (foe-mode f) electron/color-white electron/color-black)))
    (t nil)))
(defun render-foes ()
  (--each (ht-values foes)
    (render-foe it)))
(defun update-foe (f)
  (when (foe-move f)
    (let ((res (funcall (foe-move f) (- game-tick (foe-spawntime f)) f)))
      (setf (foe-x f) (car res))
      (setf (foe-y f) (cdr res))))
  (cl-case (foe-type f)
    (sinner
     (if (> (foe-sinner-timer (foe-state f)) 0)
         (cl-decf (foe-sinner-timer (foe-state f)))
       (setf (foe-sinner-timer (foe-state f)) (foe-sinner-reset (foe-state f)))
       (let ((startx (foe-x f))
             (starty (foe-y f)))
         (spawn-projectile
          300 (foe-mode f)
          (lambda (dt)
            (let* ((ox (/ dt 2.0))
                   (oy (* 10.0 (sin (/ dt 25.0))))
                   (p (to-polar ox oy))
                   (c (to-cartesian (+ (car p) (foe-sinner-angle (foe-state f))) (cdr p))))
              (cons
               (+ startx (car c))
               (+ starty (cdr c)))))))))
    (orb
     (if (> (foe-orb-timer (foe-state f)) 0)
         (cl-decf (foe-orb-timer (foe-state f)))
       (setf (foe-orb-timer (foe-state f)) (foe-orb-reset (foe-state f)))
       (let* ((startx (foe-x f))
              (starty (foe-y f))
              (dx (- guy-x startx))
              (dy (- guy-y starty))
              (r (sqrt (+ (* dx dx) (* dy dy))))
              (nx (/ dx r))
              (ny (/ dy r)))
         (spawn-projectile
          300 (foe-mode f)
          (lambda (dt)
            (cons
             (+ startx (* nx dt))
             (+ starty (* ny dt))))))))
    (t nil)))
(defun update-foes ()
  (--each (ht-values foes)
    (update-foe it)))

(defun oscillate-horizontal (xs y speed)
  (let* ((dx (- (cdr xs) (car xs)))
         (halfdx (/ dx 2.0)))
    (lambda (dt _)
      (cons
       (+ (car xs) halfdx (* halfdx (sin (* speed dt (/ float-pi 30.0)))))
       (ease-sin -32.0 y (/ dt 60.0))))))
(defun oscillate-vertical-left (x ys speed)
  (let* ((dy (- (cdr ys) (car ys)))
         (halfdy (/ dy 2.0)))
    (lambda (dt _)
      (cons
       (ease-sin -32.0 x (/ dt 60.0))
       (+ (car ys) halfdy (* halfdy (sin (* speed dt (/ float-pi 30.0)))))))))
(defun oscillate-vertical-right (x ys speed)
  (let* ((dy (- (cdr ys) (car ys)))
         (halfdy (/ dy 2.0)))
    (lambda (dt _)
      (cons
       (ease-sin (+ 128.0 32.0) x (/ dt 60.0))
       (+ (car ys) halfdy (* halfdy (sin (* speed dt (/ float-pi 30.0)))))))))
(defun swoop-in-left (x y)
  (lambda (dt _)
    (cons
     (ease-sin -32.0 x (/ dt 60.0))
     (ease-sin 16.0 y (/ dt 60.0)))))
(defun swoop-in-right (x y)
  (lambda (dt _)
    (cons
     (ease-sin (+ 128 32.0) x (/ dt 60.0))
     (ease-sin 16.0 y (/ dt 60.0)))))

(defun update-level ()
  (when game-level
    (let ((code (alist-get game-tick game-level nil nil #'=)))
      (when code
        (eval code)))))

(defun handle-input ()
  (let* ((d
          (--reduce
           (cons (+ (car acc) (car it)) (+ (cdr acc) (cdr it)))
           (list
            (if (electron//is-key-down ?W) (cons 0 -1) (cons 0 0))
            (if (electron//is-key-down ?S) (cons 0 1) (cons 0 0))
            (if (electron//is-key-down ?A) (cons -1 0) (cons 0 0))
            (if (electron//is-key-down ?D) (cons 1 0) (cons 0 0)))))
         (dscale (if (not (or (= 0 (car d)) (= 0 (cdr d)))) (/ (sqrt 2) 2) 1)))
    (setf guy-x (clamp 0.0 128.0 (+ guy-x (* dscale (car d)))))
    (setf guy-y (clamp 0.0 128.0 (+ guy-y (* dscale (cdr d))))))
  (if (electron//is-key-down ?E)
      (progn
        (unless guy-shooting (setq guy-shooting 0))
        (if (> guy-shooting 0)
            (cl-decf guy-shooting)
          (electron//play-sound sfx-shoot)
          (spawn-player-projectile guy-mode)
          (setf guy-shooting 10)))
    (setf guy-shooting nil))
  (if (electron//is-key-down ?Q)
      (when (not guy-mode-locked)
        (electron//play-sound sfx-switch)
        (setf guy-mode-locked t)
        (setf guy-mode (not guy-mode)))
    (setf guy-mode-locked nil)))

(while (not (electron//window-should-close))
  (cl-case game-state
    (game
     (cl-incf game-tick)
     (update-level)
     (handle-input)
     (update-player-projectiles)
     (update-foes))
    (t nil))
  (electron//begin-texture-mode screen)
  (electron//clear-background electron/color-gray)
  (unless (and guy-exploding (> guy-exploding 8))
    (render-guy))
  (render-foes)
  (render-projectiles)
  (render-player-projectiles)
  (cl-case game-state
    (over
     (let* ((msg "it's so over")
            (w (electron//measure-text msg 10)))
       (when (and guy-exploding (< guy-exploding 17))
         (electron//draw-texture
          (nth guy-exploding tex-anim-explosion)
          (- (round guy-x) 35)
          (- (round guy-y) 50)
          electron/color-white)
         (cl-incf guy-exploding))
       (electron//draw-text msg (- 64 (/ w 2)) 32 10 electron/color-red)))
    (t nil))
  (electron//draw-text (format "%s points" game-score) 1 (- 128 11) 1 electron/color-black)
  (electron//end-texture-mode)
  (electron//begin-drawing)
  (electron//clear-background electron/color-black)
  (electron//draw-rendertexture-src-dst screen screenrect finalrect electron/color-white)
  (electron//end-drawing))

(provide 'electron)
;;; electron.el ends here
