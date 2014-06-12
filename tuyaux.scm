
(define my-canvas%         ; une sous-classe de canvas% qui redefinit la methode on-event
  (class canvas%
    (define/override (on-event evt) ; il se passe un évenement dans le canvas
      (let* ((x1 (send evt get-x)) ; coordonnées de la souris
             (y1 (send evt get-y))
             (x (quotient x1 70)) ; numéro de la case survolée dans [0,9]
             (y (quotient y1 70))
             (x2 (* x 70)) ; coordonnées du coin supérieur gauche de la case survolée
             (y2 (* y 70)))
        (if (and (< x 10) (< y 10))
            (if (not (get-piece x y)) ; si la case est vide
                (if (send evt button-down? 'left)
                    (begin
                      (set-piece! x y piece-suivante)
                      (send dc draw-bitmap (cadr piece-suivante) x2 y2)
                      (play-sound "click.wav" #t)
                      (random-suivant)
                      (if (= tour 100) (fin-de-partie))))))))
    (super-new)))


(define (random-suivant)
  (let ((piece (case (vector-ref liste tour)
                 ((0) `(coin-inf-droit ,coin-inf-droit ,coin-inf-droit-plein))
                 ((1) `(coin-inf-gauche ,coin-inf-gauche ,coin-inf-gauche-plein))
                 ((2) `(coin-sup-droit ,coin-sup-droit ,coin-sup-droit-plein))
                 ((3) `(coin-sup-gauche ,coin-sup-gauche ,coin-sup-gauche-plein))
                 ((4) `(droit-horizontal ,droit-horizontal ,droit-horizontal-plein))
                 ((5) `(droit-vertical ,droit-vertical ,droit-vertical-plein))
                 ((6) `(croisement ,croisement))
                 ((7) `(accueil ,accueil)))))
    (set! tour (add1 tour))
    (set! piece-suivante piece)
    (send suivant set-label (cadr piece-suivante))))

(define (echanger a b)
  (let ((tmp (vector-ref liste a)))
    (vector-set! liste a (vector-ref liste b))
    (vector-set! liste b tmp)))

(define (debut-de-partie)
  (set! type (send choix-type get-selection))
  (send nouvelle-partie enable #f)
  (send choix-type enable #f)
  (send message-score set-label "")
  (send message-type set-label (case type ('0 "Une minute") ('1 "Deux minutes") ('2 "Trois minutes") ('3 "Illimité")))
  (afficher)
  (set! tuyaux (build-vector 10 (λ (n) (make-vector 10 #f))))
  (set! liste (vector 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                      1 1 1 1 1 1 1 1 1 1 1 1 1 1
                      2 2 2 2 2 2 2 2 2 2 2 2 2 2
                      3 3 3 3 3 3 3 3 3 3 3 3 3 3
                      4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
                      5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
                      6 6 6 6 6 6 6 6 6 6 6 6 6 7))
  (do ((i 0 (add1 i)))
    ((= i 99))
    (echanger i (random 99)))
  (set! tour 0)
  (set! score 0)
  (set! temps (* (add1 type) 60))
  (set! x-depart (+ 2 (random 6)))
  (set! y-depart (+ 2 (random 6)))
  (set! piece-depart (case (random 4)
                       ((0) `(depart-droite ,depart-droite ,depart-droite-plein))
                       ((1) `(depart-gauche ,depart-gauche ,depart-gauche-plein))
                       ((2) `(depart-haut ,depart-haut ,depart-haut-plein))
                       ((3) `(depart-bas ,depart-bas ,depart-bas-plein))))
  (set-piece! x-depart y-depart piece-depart)
  (send dc clear)
  (send dc draw-bitmap fond 0 0)
  (send dc draw-bitmap (cadr piece-depart) (* 70 x-depart) (* 70 y-depart))
  (random-suivant)
  (send minuteur set-range temps)
  (send minuteur set-value 0)
  (if (< type 3) (send timer start 1000))
  (send canvas enable #t)
  (send terminer enable #t))

(define (fin-de-partie)
  (send timer stop)
  (send canvas enable #f)
  (send terminer enable #f)
  (send minuteur set-value 0)
  (send suivant set-label accueil)
  (send minuteur set-label "")
  (send message-score set-label "")
  (comptage-score)
  (send message-score set-label (format "~a" score))
  (sauver)
  (if (> score (vector-ref high-score (* type 2)))
      (begin
        (sleep 3)
        (play-sound "applause.wav" #f)
        (send vpanel add-child nom)
        (send vpanel add-child enregistrer))
      (begin
        (send choix-type enable #t)
        (send nouvelle-partie enable #t))))

(define (f n) ; retourne l'entier n en string à deux caractères
  (if (>= n 10)
      (number->string n)
      (format " ~a" n)))

(define (get-piece x y)
  (if (and (>= x 0) (<= x 9) (>= y 0) (<= y 9))
      (vector-ref (vector-ref tuyaux y) x)
      #f))

(define (set-piece! x y piece)
  (vector-set! (vector-ref tuyaux y) x piece))

(define (seconde)
  (let ((s (send minuteur get-value)))
    (if (>= s temps)
        (begin
          (play-sound "gong.wav" #t)
          (fin-de-partie))
        (begin
          (send minuteur set-value (add1 s))
          (send minuteur set-label (number->string (- temps s)))
          (if (>= s (- temps 10)) (send message-score set-label (format "~a !" (- temps s))))))))

(define (comptage-score)
  (define (aux x y dir) ; la case (x,y) est BONNE et on veut aller en direction dir
    (set! score (add1 score))
    (sleep 0.05)
    (let ((piece (get-piece x y)))
      (send dc draw-bitmap
            (case (car piece)
              ('croisement (case dir ; remplissage des croisements
                             ((gauche droite) (set-piece! x y '(croisement-horizontal)) croisement-plein-horizontal)
                             ((haut bas) (set-piece! x y '(croisement-vertical)) croisement-plein-vertical)))
              ((croisement-horizontal croisement-vertical) croisement-plein)
              (else (caddr piece)))
            (* 70 x) (* 70 y)))
    (case dir
      ('haut (let ((suiv (get-piece x (sub1 y))))
               (if suiv (case (car suiv)
                          ('coin-inf-gauche (aux x (sub1 y) 'gauche))
                          ('coin-inf-droit (aux x (sub1 y) 'droite))
                          ((droit-vertical croisement croisement-horizontal croisement-vertical) (aux x (sub1 y) 'haut))))))
      ('bas (let ((suiv (get-piece x (add1 y))))
              (if suiv (case (car suiv)
                         ('coin-sup-gauche (aux x (add1 y) 'gauche))
                         ('coin-sup-droit (aux x (add1 y) 'droite))
                         ((droit-vertical croisement croisement-horizontal croisement-vertical) (aux x (add1 y) 'bas))))))
      ('gauche (let ((suiv (get-piece (sub1 x) y)))
                 (if suiv (case (car suiv)
                            ('coin-sup-droit (aux (sub1 x) y 'haut))
                            ('coin-inf-droit (aux (sub1 x) y 'bas))
                            ((droit-horizontal croisement croisement-horizontal croisement-vertical) (aux (sub1 x) y 'gauche))))))
      ('droite (let ((suiv (get-piece (add1 x) y)))
                 (if suiv (case (car suiv)
                            ('coin-sup-gauche (aux (add1 x) y 'haut))
                            ('coin-inf-gauche (aux (add1 x) y 'bas))
                            ((droit-horizontal croisement croisement-horizontal croisement-vertical) (aux (add1 x) y 'droite))))))))
  (aux x-depart y-depart (case (car piece-depart)
                           ('depart-droite 'droite)
                           ('depart-gauche 'gauche)
                           ('depart-haut 'haut)
                           ('depart-bas 'bas)))
  (play-sound (if (< score 9) "laugh.wav" "trumpet.wav") #t))

(define (sauver)
  ;(if (file-exists? save)
  ;(delete-file save))
  (call-with-output-file save (λ (p-out) (write high-score p-out)) 'replace))

(define (charger)
  (if (file-exists? save)
      (begin
        (call-with-input-file save (λ (p-in) (set! high-score (read p-in)))))
      (begin
        (set! high-score (vector 0 "" 0 "" 0 "" 0 ""))
        (sauver)))
  (afficher))

(define (afficher)
  (send message-high-score-1 set-label (format "1m  ~a  ~a"
                                               (f (vector-ref high-score 0)) (vector-ref high-score 1)))
  (send message-high-score-2 set-label (format "2m  ~a  ~a"
                                               (f (vector-ref high-score 2)) (vector-ref high-score 3)))
  (send message-high-score-3 set-label (format "3m  ~a  ~a"
                                               (f (vector-ref high-score 4)) (vector-ref high-score 5)))
  (send message-high-score-free set-label (format " ∞  ~a  ~a"
                                                  (f (vector-ref high-score 6)) (vector-ref high-score 7))))


;  -=-=-=- PARTIE GRAPHIQUE -=-=-=-
(define depart-droite (make-object bitmap% "depart-droite.png"))
(define depart-gauche (make-object bitmap% "depart-gauche.png"))
(define depart-haut (make-object bitmap% "depart-haut.png"))
(define depart-bas (make-object bitmap% "depart-bas.png"))
(define coin-inf-droit (make-object bitmap% "coin-inf-droit.png"))
(define coin-inf-gauche (make-object bitmap% "coin-inf-gauche.png"))
(define coin-sup-droit (make-object bitmap% "coin-sup-droit.png"))
(define coin-sup-gauche (make-object bitmap% "coin-sup-gauche.png"))
(define droit-horizontal (make-object bitmap% "droit-horizontal.png"))
(define droit-vertical (make-object bitmap% "droit-vertical.png"))
(define croisement (make-object bitmap% "croisement.png"))
(define depart-droite-plein (make-object bitmap% "depart-droite-plein.png"))
(define depart-gauche-plein (make-object bitmap% "depart-gauche-plein.png"))
(define depart-haut-plein (make-object bitmap% "depart-haut-plein.png"))
(define depart-bas-plein (make-object bitmap% "depart-bas-plein.png"))
(define coin-inf-droit-plein (make-object bitmap% "coin-inf-droit-plein.png"))
(define coin-inf-gauche-plein (make-object bitmap% "coin-inf-gauche-plein.png"))
(define coin-sup-droit-plein (make-object bitmap% "coin-sup-droit-plein.png"))
(define coin-sup-gauche-plein (make-object bitmap% "coin-sup-gauche-plein.png"))
(define droit-horizontal-plein (make-object bitmap% "droit-horizontal-plein.png"))
(define droit-vertical-plein (make-object bitmap% "droit-vertical-plein.png"))
(define croisement-plein-horizontal (make-object bitmap% "croisement-plein-horizontal.png"))
(define croisement-plein-vertical (make-object bitmap% "croisement-plein-vertical.png"))
(define croisement-plein (make-object bitmap% "croisement-plein.png"))
(define accueil (make-object bitmap% "accueil.png"))
(define fond (make-object bitmap% "fond.png"))

(define frame (new frame% (label "Tuyaux")
                   (style '(metal no-resize-border))))

(define hpanel (new horizontal-panel% (parent frame)))

(define vpanel (new vertical-panel% (parent hpanel)
                    (border 10) (spacing 10)))

(define minuteur (new gauge% (parent vpanel)
                      (label "      ")
                      (range 1)))

(define timer (new timer% (notify-callback seconde)))

(define suivant (new message% (parent vpanel)
                     (label accueil)))

(define choix-type (new choice% (parent vpanel)
                        (label "Type")
                        (choices '("1 minute" "2 minutes" "3 minutes" "Illimité"))))

(define nouvelle-partie (new button% (parent vpanel)
                             (label "Nouvelle partie")
                             (callback (λ (b e) (debut-de-partie)))))

(define terminer (new button% (parent vpanel)
                      (label "Terminer maintenant")
                      (enabled #f)
                      (callback (λ (b e) (fin-de-partie)))))

(define message-type (new message% (parent vpanel)
                          (label "                      ")))

(define message-score (new message% (parent vpanel)
                           (label "       ")
                           (font (make-object font% 50 'default))))

(define font (make-object font% 13 'modern))

(define message-high-score (new message% (parent vpanel)
                                (label "High Scores :")))

(define message-high-score-1 (new message% (parent vpanel)
                                  (font font)
                                  (label "1m  0                  ")))

(define message-high-score-2 (new message% (parent vpanel)
                                  (font font)
                                  (label "2m  0                  ")))

(define message-high-score-3 (new message% (parent vpanel)
                                  (font font)
                                  (label "3m  0                  ")))

(define message-high-score-free (new message% (parent vpanel)
                                     (font font)
                                     (label " ∞  0                  ")))

(define nom (new text-field% (parent vpanel)
                 (label "Nom :")
                 (style '(single deleted))))

(define enregistrer (new button% (parent vpanel)
                         (label "Enregistrer")
                         (style '(deleted))
                         (callback (λ (b e)
                                     (let ((pseudo (send nom get-value)))
                                       (vector-set! high-score (* type 2) score)
                                       (vector-set! high-score (add1 (* type 2)) (if (> (string-length pseudo) 10)
                                                                                     (substring pseudo 0 10)
                                                                                     pseudo))
                                       (sauver)
                                       (afficher)
                                       (send vpanel delete-child nom)
                                       (send vpanel delete-child enregistrer)
                                       (send choix-type enable #t)
                                       (send nouvelle-partie enable #t))))))

(define canvas (new my-canvas% (parent hpanel)
                    (enabled #f)
                    (min-width 700) (min-height 700)))
(define dc (send canvas get-dc))


;   >---<  VARIABLES GLOBALES  >---<
(define high-score '?)
(define temps '?)
(define tuyaux '?)
(define tour '?)
(define liste '?)
(define piece-depart '?)
(define x-depart '?)
(define y-depart '?)
(define piece-suivante '?)
(define score '?)
(define type '?)
(define save "scores")

(charger)
(send frame show #t)
(send dc draw-bitmap fond 0 0)

