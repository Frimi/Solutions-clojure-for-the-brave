(ns clojure-for-the-brave.Chapter-10)

;---------------------------------------------------------------------------------------------------------------
;Watchers
;---------------------------------------------------------------------------------------------------------------
(def fred (atom {:cuddle-hunger-level 0
                 :percent-deteriorated 0}))

(defn shuffle-speed
  [zombie]
  (* (:cuddle-hunger-level zombie)
     (- 100 (:percent-deteriorated zombie))))

(defn shuffle-alert
  [key watched old-state new-state]
  (let [sph (shuffle-speed new-state)]
    (if (> sph 5000)
      (do
        (println "Run, you fool!")
        (println "The zombie's SPH is now " sph)
        (println "This message brought to your courtesy of " key))
      (do
        (println "All's well with " key)
        (println "Cuddle hunger: " (:cuddle-hunger-level new-state))
        (println "Percent deteriorated: " (:percent-deteriorated new-state))
        (println "SPH: " sph)))))

(comment (do
           (reset! fred {:cuddle-hunger-level  22
                         :percent-deteriorated 2})
           (add-watch fred :fred-shuffle-alert shuffle-alert)
           (swap! fred update-in [:percent-deteriorated] + 1)
           (swap! fred update-in [:cuddle-hunger-level] + 30)))

;---------------------------------------------------------------------------------------------------------------
;Validators
;---------------------------------------------------------------------------------------------------------------

(defn percent-deteriorated-validator
  [{:keys [percent-deteriorated]}]
  (and (>= percent-deteriorated 0)
       (<= percent-deteriorated 100)))

(def bobby
  (atom
    {:cuddle-hunger-level 0 :percent-deteriorated 0}
    :validator percent-deteriorated-validator))

(comment (swap! bobby update-in [:percent-deteriorated] + 200))

;---------------------------------------------------------------------------------------------------------------
;Refs
;---------------------------------------------------------------------------------------------------------------

(def sock-varieties
  #{"darned" "argyle" "wool" "horsehair" "mulleted"
    "passive-aggressive" "striped" "polka-dotted"
    "athletic" "business" "power" "invisible" "gollumed"})

(defn sock-count
  [sock-variety count]
  {:variety sock-variety
   :count count})

(defn generate-sock-gnome
  "Create an initial sock gnome state with no socks"
  [name]
  {:name name
   :socks #{}})

(comment
  (do
    (def sock-gnome (ref (generate-sock-gnome "Barumpharumph")))
    (def dryer (ref {:name "LG 1337"
                     :socks (set (map #(sock-count % 2) sock-varieties))}))))

(defn steal-sock
  [gnome dryer]
  (dosync
    (when-let [pair (some #(if (= (:count %) 2) %) (:socks @dryer))]
      (let [updated-count (sock-count (:variety pair) 1)]
        (alter gnome update-in [:socks] conj updated-count)
        (alter dryer update-in [:socks] disj pair)
        (alter dryer update-in [:socks] conj updated-count)))))

(comment (steal-sock sock-gnome dryer))

(defn similar-socks
  [target-sock sock-set]
  (filter #(= (:variety %) (:variety target-sock)) sock-set))

(comment (similar-socks (first (:socks @sock-gnome)) (:socks @dryer)))

;---------------------------------------------------------------------------------------------------------------
;Vars
;---------------------------------------------------------------------------------------------------------------
(def ^:dynamic *troll-thought* nil)

(defn troll-riddle
  [your-answer]
  (let [number "man meat"]
    (when (thread-bound? #'*troll-thought*)
      (set! *troll-thought* number))
    (if (= number your-answer)
      "TROLL: You can cross the bridge!"
      "TROLL: Time to eat you, succulent human!")))

(binding [*troll-thought* nil]
  (println (troll-riddle 2))
  (println "SUCCULENT HUMAN: Oooooh! The answer was" *troll-thought*))

;---------------------------------------------------------------------------------------------------------------
;Exercise 1
;---------------------------------------------------------------------------------------------------------------
(def test-atom (atom 0))

(defn inc-n-atom!
  [atom n]
  (dotimes [_ n]
    (swap! atom inc))
  @atom)


;---------------------------------------------------------------------------------------------------------------
;Exercise 2
;---------------------------------------------------------------------------------------------------------------

;https://www.braveclojure.com/random-quote -> Site Offline


;---------------------------------------------------------------------------------------------------------------
;Exercise 3
;---------------------------------------------------------------------------------------------------------------
(def character-1 (ref {:username "Frimi" :class "Soldier" :hit-points 15 :healing-potions 0}))
(def character-2 (ref {:username "Wizard" :class "Mage" :hit-points 30 :healing-potions 1}))

(def max-life 40)

(defn available-healing-potions?
  "Check if the character has any healing potions"
  [{:keys [healing-potions]}]
  (pos? healing-potions))

(defn exchange-potion
  "Exchange de potion from character-2 to character-1"
  [character-1 character-2]
  (when (available-healing-potions? @character-2)
    (dosync
      (alter character-2 update :healing-potions dec)
      (alter character-1 update :healing-potions inc))))

(defn use-potion
  "Use potion to heal all hitpoints"
  [character]
  (when (available-healing-potions? @character)
    (dosync
      (alter character update :healing-potions dec)
      (alter character assoc :hit-points 40))))

(exchange-potion character-1 character-2)
(use-potion character-1)
