;;; uwtrbtpimicasy --- Uhhh, What? The Relationship Between The Polarities In My Ikaruga Clone Are Surprisingly "Yuri" -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;; Imports
(require 'cl-lib)
(require 'dash)
(require 'ht)
(require 'f)

;; Utility functions
(defun clamp (low high x)
  "Clamp X between LOW and HIGH inclusive."
  (min (max x low) high))

(defun distance (x0 y0 x1 y1)
  "Return the cartesian distance between (X0, Y0) and (X1, Y1)."
  (let ((dx (- x1 x0))
        (dy (- y1 y0)))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defun lerp (v0 v1 t0)
  "Linearly interpolate between V0 and V1 using T0."
  (let ((t1 (clamp 0.0 1.0 t0)))
    (+ (* (- 1.0 t1) v0) (* t1 v1))))

(defun ease-sin (v0 v1 t0)
  "Ease using sine from V0 to V1 based using T0."
  (lerp v0 v1 (sin (* 0.5 float-pi (clamp 0.0 1.0 t0)))))

(defun to-polar (x y)
  "Convert cartesian coordinates (X, Y) to polar coordinates."
  (cons
   (atan y x)
   (distance x y 0 0)))

(defun to-cartesian (theta r)
  "Convert polar coordinates (THETA, R) to cartesian coordinates."
  (cons
   (* r (cos theta))
   (* r (sin theta))))

(defun rand-mode ()
  "Randomly generate a mode."
  (= 0 (random 2)))

(defun rand-coord ()
  "Randomly generate a reasonable screen coordinate."
  (+ 14.0 (float (random 100))))

(defun rand-angle ()
  "Randomly generate an angle."
  (* float-pi (/ (float (random 360)) 180.0)))

;; Initialization
(electron//init-window 800 600 "Uhhh, What? The Relationship Between The Polarities In My Ikaruga Clone Are Surprisingly \"Yuri\"?!")
(electron//init-audio-device)
(electron//set-target-fps 60)
(defvar screen (electron//load-rendertexture 128 128))
(defvar screenrect (electron//make-rectangle 0.0 0.0 128.0 -128.0))
(defvar finalrect (electron//make-rectangle 0.0 0.0 768.0 768.0))

;; Load assets
(defun asset (s)
  "Return the asset path for S."
  (f-join (f-dirname load-file-name) "assets" s))
(defvar tex-guywhite (electron//load-texture (asset "textures/guywhite.png")))
(defvar tex-guyblack (electron//load-texture (asset "textures/guyblack.png")))
(defvar tex-projectile (electron//load-texture (asset "textures/projectile.png")))
(defvar tex-player-projectile (electron//load-texture (asset "textures/playerprojectile.png")))
(defvar tex-foe-sinner (electron//load-texture (asset "textures/foe/sinner.png")))
(defvar tex-foe-orb (electron//load-texture (asset "textures/foe/orb.png")))
(defvar tex-anim-explosion (--map (electron//load-texture (asset (format "textures/explosion/animation%d.png" (+ it 1)))) (-iota 17)))
(defvar sfx-switch (electron//load-sound (asset "sfx/switch.wav")))
(defvar sfx-explosion (electron//load-sound (asset "sfx/explosion.wav")))
(defvar sfx-shoot (electron//load-sound (asset "sfx/shoot.wav")))
(defvar sfx-yoda-death (electron//load-sound (asset "sfx/yodadeath.ogg")))
(defvar music-bgm (electron//load-music (asset "sfx/bgm.wav")))
(electron//play-music music-bgm)

;; Shared game state
(defvar game-state 'mainmenu)
(defvar game-tick 0)
(defvar game-score 0)
(defvar game-menu-index 0)
(defvar game-menu-locked nil)

;; "Guy" - the player avatar
(defvar guy-x 0.0)
(defvar guy-y 0.0)
(defvar guy-mode t)
(defvar guy-mode-locked nil)
(defvar guy-exploding nil)
(defvar guy-shooting nil)
(defun contacting-guy (x y)
  "Return non-nil if (X, Y) is contacting the player."
  (and
   (<= (abs (- guy-x x)) 1)
   (<= (abs (- guy-y y)) 2)))
(defun render-guy ()
  "Draw the player character."
  (electron//draw-texture
   (if guy-mode tex-guywhite tex-guyblack)
   (round (- guy-x 4.0))
   (round (- guy-y 4.0))
   electron/color-white))

;; Projectiles fired by the player
(defvar player-projectile-next-id 0)
(defvar player-projectiles (ht-create))
(cl-defstruct player-projectile mode x y)
(defun spawn-player-projectile (mode)
  "Create a new MODE player projectile at the player's position."
  (ht-set!
   player-projectiles player-projectile-next-id
   (make-player-projectile :mode mode :x (- guy-x 3) :y (- guy-y 2)))
  (cl-incf player-projectile-next-id)
  (ht-set!
   player-projectiles player-projectile-next-id
   (make-player-projectile :mode mode :x (+ guy-x 3) :y (- guy-y 2)))
  (cl-incf player-projectile-next-id))
(defun update-player-projectile (id p)
  "Update a player projectile P with ID."
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
  "Update all player projectiles."
  (--each (ht->alist player-projectiles)
    (update-player-projectile (car it) (cdr it))))
(defun render-player-projectile (p)
  "Render the player projectile P."
  (electron//draw-texture
   tex-player-projectile
   (round (player-projectile-x p))
   (round (player-projectile-y p))
   (if (player-projectile-mode p) electron/color-white electron/color-black)))
(defun render-player-projectiles ()
  "Render all player projectiles."
  (--each (ht-values player-projectiles)
    (render-player-projectile it)))

;; Enemy projectiles
(defvar projectile-next-id 0)
(defvar projectiles (ht-create))
(defun spawn-projectile (ttl mode f)
  "Spawn a projectile with time-to-live TTL, MODE, and position function F."
  (ht-set!
   projectiles projectile-next-id
   (list game-tick ttl mode f)
   )
  (cl-incf projectile-next-id))
(defun render-projectiles ()
  "Update and render all projectiles."
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

;; Enemies
(defvar foe-next-id 0)
(defvar foes (ht-create))
(cl-defstruct foe spawntime type mode r x y state move)
(cl-defstruct foe-sinner timer reset angle)
(cl-defstruct foe-orb timer reset)
(defun spawn-foe (type mode r &optional st move)
  "Spawn a foe of TYPE and MODE with radius R.
Optionally, set type-specific state to ST and move according to MOVE."
  (ht-set!
   foes foe-next-id
   (make-foe :spawntime game-tick :type type :mode mode :r r :x 0 :y 0 :state st :move move))
  (cl-incf foe-next-id))
(defun contacting-foe (f x y)
  "Return non-nil if foe F is contacting (X, Y)."
  (<= (distance x y (foe-x f) (foe-y f)) (foe-r f)))
(defun render-foe (f)
  "Render a foe F."
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
  "Render all foes."
  (--each (ht-values foes)
    (render-foe it)))
(defun update-foe (f)
  "Update a foe F."
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
  "Update all foes."
  (--each (ht-values foes)
    (update-foe it)))

;; Movement helpers
(defun oscillate-horizontal (xs y speed)
  "Movement helper: move between range XS at Y according to SPEED."
  (let* ((dx (- (cdr xs) (car xs)))
         (halfdx (/ dx 2.0)))
    (lambda (dt _)
      (cons
       (+ (car xs) halfdx (* halfdx (sin (* speed dt (/ float-pi 30.0)))))
       (ease-sin -32.0 y (/ dt 60.0))))))
(defun oscillate-vertical-left (x ys speed)
  "Movement helper: move between range YS at X according to SPEED.
Ease in to X from the left."
  (let* ((dy (- (cdr ys) (car ys)))
         (halfdy (/ dy 2.0)))
    (lambda (dt _)
      (cons
       (ease-sin -32.0 x (/ dt 60.0))
       (+ (car ys) halfdy (* halfdy (sin (* speed dt (/ float-pi 30.0)))))))))
(defun oscillate-vertical-right (x ys speed)
  "Movement helper: move between range YS at X according to SPEED.
Ease in to X from the right."
  (let* ((dy (- (cdr ys) (car ys)))
         (halfdy (/ dy 2.0)))
    (lambda (dt _)
      (cons
       (ease-sin (+ 128.0 32.0) x (/ dt 60.0))
       (+ (car ys) halfdy (* halfdy (sin (* speed dt (/ float-pi 30.0)))))))))
(defun swoop-in-left (x y)
  "Movement helper: ease in to (X, Y) from the left and then remain still."
  (lambda (dt _)
    (cons
     (ease-sin -32.0 x (/ dt 60.0))
     (ease-sin 16.0 y (/ dt 60.0)))))
(defun swoop-in-right (x y)
  "Movement helper: ease in to (X, Y) from the right and then remain still."
  (lambda (dt _)
    (cons
     (ease-sin (+ 128 32.0) x (/ dt 60.0))
     (ease-sin 16.0 y (/ dt 60.0)))))

;; Enemy generation
(defconst gen-spawn-costs
  '((300 . (spawn-foe 'orb (rand-mode) 4 (make-foe-orb :timer 120 :reset 120) (swoop-in-left (rand-coord) (rand-coord))))
    (300 . (spawn-foe 'orb (rand-mode) 4 (make-foe-orb :timer 120 :reset 120) (swoop-in-right (rand-coord) (rand-coord))))
    (400 . (spawn-foe 'sinner (rand-mode) 3 (make-foe-sinner :timer 30 :reset 30 :angle (rand-angle)) (swoop-in-left (rand-coord) (rand-coord))))
    (400 . (spawn-foe 'sinner (rand-mode) 3 (make-foe-sinner :timer 30 :reset 30 :angle (rand-angle)) (swoop-in-right (rand-coord) (rand-coord))))
    ))
(defvar gen-fuel 0)
(defun gen-attempt-spend ()
  "Attempt to spawn one enemy."
  (let ((valid (--filter (<= (car it) gen-fuel) gen-spawn-costs)))
    (if valid
        (progn
          (let ((ent (nth (random (length valid)) valid)))
            (cl-decf gen-fuel (car ent))
            (eval (cdr ent)))
          t)
      nil)))
(defun update-infinite-gen ()
  "Update fuel and spawn as many enemies as possible."
  (cl-incf gen-fuel (+ 1 (/ game-tick 3600)))
  (when (= 0 (% game-tick 600))
    (while (gen-attempt-spend))))

;; Game loop
(defun reset ()
  "Reset the global game state."
  (setf game-tick 0)
  (setf game-score 0)
  (setf guy-x 0.0)
  (setf guy-y 0.0)
  (setf guy-mode t)
  (setf guy-exploding nil)
  (setf guy-shooting nil)
  (setf player-projectile-next-id 0)
  (ht-clear! player-projectiles)
  (setf projectile-next-id 0)
  (ht-clear! projectiles)
  (setf foe-next-id 0)
  (ht-clear! foes))

(defun handle-input ()
  "Handle input while the game is active."
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
(defun render-gamescreen ()
  "Render the main game screen."
  (electron//clear-background electron/color-gray)
  (unless (and guy-exploding (> guy-exploding 8))
    (render-guy))
  (render-foes)
  (render-projectiles)
  (render-player-projectiles)
  (electron//draw-text (format "%s points" game-score) 1 (- 128 11) 1 electron/color-black))

(defun handle-menu-input ()
  "Handle input on the main menu."
  (cond
   ((electron//is-key-down ?A)
    (when (not game-menu-locked)
      (setf game-menu-locked t)
      (setf game-menu-index (% (+ (% (- game-menu-index 1) 2) 2) 2))))
   ((electron//is-key-down ?D)
    (when (not game-menu-locked)
      (setf game-menu-locked t)
      (setf game-menu-index (% (+ (% (+ game-menu-index 1) 2) 2) 2))))
   ((or (electron//is-key-down ?Q) (electron//is-key-down ?E))
    (when (not game-menu-locked)
      (setf game-menu-locked t)
      (cl-case game-menu-index
        (0
         (reset)
         (setf game-state 'game))
        (1 (kill-emacs)))))
   (t (setf game-menu-locked nil))))
(defun menu-draw-title-text (y sz msg)
  (let* ((w (electron//measure-text msg sz)))
    (electron//draw-text msg (- 64 (/ w 2)) y sz electron/color-black)))
(defun menu-draw-button (idx x y msg)
  (let* ((active (= game-menu-index idx))
         (fg (if active electron/color-white electron/color-black))
         (bg (if active electron/color-black electron/color-white)))
    (electron//draw-rectangle x y 32 12 bg)
    (electron//draw-text msg (+ x 3) y 10 fg)))
(defun render-menu ()
  "Render the main menu."
  (electron//clear-background electron/color-white)
  (menu-draw-title-text 20 10 "Uhhh, What? The")
  (menu-draw-title-text 30 10 "Relationship Between")
  (menu-draw-title-text 40 10 "The Polarities")
  (menu-draw-title-text 50 10 "In My Ikaruga Clone")
  (menu-draw-title-text 60 10 "Are Surprisingly")
  (menu-draw-title-text 70 10 "\"Yuri\"?!")
  (menu-draw-button 0 28 90 "begin")
  (menu-draw-button 1 68 90 "quit"))

(defun handle-gameover-input ()
  "Handle input on the game over screen."
  (cond
   ((electron//is-key-down ?A)
    (when (not game-menu-locked)
      (setf game-menu-locked t)
      (setf game-menu-index (% (+ (% (- game-menu-index 1) 2) 2) 2))))
   ((electron//is-key-down ?D)
    (when (not game-menu-locked)
      (setf game-menu-locked t)
      (setf game-menu-index (% (+ (% (+ game-menu-index 1) 2) 2) 2))))
   ((or (electron//is-key-down ?Q) (electron//is-key-down ?E))
    (when (not game-menu-locked)
      (setf game-menu-locked t)
      (cl-case game-menu-index
        (0
         (reset)
         (setf game-state 'game))
        (1 (setf game-state 'mainmenu)))))
   (t (setf game-menu-locked nil))))
(defun render-gameover ()
  "Render the game over screen."
  (render-gamescreen)
  (let* ((msg "it's so over")
         (w (electron//measure-text msg 10)))
    (when (and guy-exploding (< guy-exploding 17))
      (electron//draw-texture
       (nth guy-exploding tex-anim-explosion)
       (- (round guy-x) 35)
       (- (round guy-y) 50)
       electron/color-white)
      (cl-incf guy-exploding))
    (electron//draw-text msg (- 64 (/ w 2)) 32 10 electron/color-red)
    (menu-draw-button 0 28 90 "retry")
    (menu-draw-button 1 68 90 "quit")))

;; The main loop
(while (not (electron//window-should-close))
  (electron//update-music music-bgm)
  (cl-case game-state
    (mainmenu
     (handle-menu-input))
    (game
     (cl-incf game-tick)
     ;; (update-level)
     (update-infinite-gen)
     (handle-input)
     (update-player-projectiles)
     (update-foes))
    (over
     (handle-gameover-input))
    (t nil))
  (electron//begin-texture-mode screen)
  (cl-case game-state
    (mainmenu (render-menu))
    (game (render-gamescreen))
    (over (render-gameover))
    (t nil))
  (electron//end-texture-mode)
  (electron//begin-drawing)
  (electron//clear-background electron/color-black)
  (electron//draw-rendertexture-src-dst screen screenrect finalrect electron/color-white)
  (electron//end-drawing))

(provide 'uwtrbtpimicasy)
;;; uwtrbtpimicasy.el ends here
