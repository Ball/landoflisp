(use 'clojure.java.io)

; Data structures
; ^:dynamic means this will be updated  Clojure is warning because of the 'earmuffs' without it
; atoms are used to allow the update of global state
(def ^:dynamic *nodes* (atom { :living-room "You are in the living-room.\nA Wizard is snoring loudly on the couch."
                               :garden "You are in a beautiful garden.  There is a well in front of you."
                               :attic "You are in the attic.  There is a giant welding torch in the corner."}))
(def ^:dynamic *edges* (atom {:living-room [[:garden :west "door"] [:attic :upstairs "ladder"]]
                              :garden [[:living-room :east "door"]]
                              :att ^:dynamicic [[:living-room :downstairs "ladder"]]}))
(def ^:dynamic *objects* (atom #{:whiskey :bucket :frog :chain}))
(def ^:dynamic *object-locations* (atom {:whiskey :living-room
                                         :bucket :living-room
                                         :chain :garden
                                         :frog :garden}))
(def ^:dynamic *location* (atom :living-room))
; this doesn't change
(def *allowed-commands* #{'look 'walk 'pickup 'inventory})


; Helper function
(defn objects-at [loc objs obj-loc]
  (filter #(= loc (% obj-loc)) objs))
;---------------------------------------------------------------------------------------
; Description methods
(defn describe-location
  "pulls the location description from the location collection"
  [location nodes]
  (location nodes))
(defn describe-path
  "tells you what a path should be called"
  [edge]
  (str "\tThere is a " (edge 2) " going " (name (edge 1)) " from here."))
(defn describe-paths
  "collects the path descriptions of a thing"
  [location edges]
  (apply vector (map describe-path (location edges))))
(defn describe-objects
  "describes the objects at a location"
  [loc objs obj-loc]
  (apply vector (map #(str "\tYou see a " (name %) " on the floor.")
                     (objects-at loc objs obj-loc))))

;---------------------------------------------------------------------------------------
; Commands
;   Responses must be collections of strings
(defn look []
  (lazy-cat [(describe-location @*location* @*nodes*)]
          (describe-paths @*location* @*edges*)
          (describe-objects @*location* @*objects* @*object-locations*)))

(defn walk [direction]
  (let [next (first (filter #(= direction (% 1)) (@*location* @*edges*)))]
       (if next
           (do (reset! *location* (first next))
               (look))
           ["You can't go that way"])))
(defn pickup [object]
  (cond (some #(= % object) (objects-at @*location* @*objects* @*object-locations*))
              (do (reset! *object-locations* (assoc @*object-locations* object :body))
                  [(str "You are now carrying the " (name object))])
        :else ["You cannot get that"]))
(defn inventory []
  [(apply str "items- " (interpose ", " (map name (objects-at :body @*objects* @*object-locations*))))])

;----------------------------------------------------------------------------------------
; Flow control
; opens the console and splits it
(defn game-read []
  (let [r (reader System/in)]
    (seq (.split (.readLine r) " "))))
; prints each string on a new line
(defn game-print [messages]
  (doseq [x messages] (println x)))
(defn game-eval [cmd]
  (let [c (resolve (symbol (first cmd)))
        args (map #(keyword %) (rest cmd))]
      (if (contains? *allowed-commands* c)
          (apply c args))
          ["I don't know how to do that"]))
; different because clojure doesn't allow for perfect recursion
(defn game-repl []
  (loop [cmd (game-read)]
    (if (not= "quit" (first cmd))
      (do (game-print (game-eval cmd))
          (recur (game-read))))))
