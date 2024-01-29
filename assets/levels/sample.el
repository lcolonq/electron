((120
  . (spawn-foe
     'sinner nil 3
     (make-foe-sinner :timer 15 :reset 15 :angle (* 0.5 float-pi))
     (oscillate-horizontal '(0.0 . 128.0) 10.0 0.1)))
 (300
  . (progn
      (spawn-foe 'orb t 3 (make-foe-orb :timer 120 :reset 120) (swoop-in-left 28.0 100.0))
      (spawn-foe 'orb t 3 (make-foe-orb :timer 120 :reset 120) (swoop-in-left 28.0 110.0))
      (spawn-foe 'orb t 3 (make-foe-orb :timer 120 :reset 120) (swoop-in-right 100.0 100.0))
      (spawn-foe 'orb t 3 (make-foe-orb :timer 120 :reset 120) (swoop-in-right 100.0 110.0)))))
