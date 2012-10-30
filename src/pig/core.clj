(ns pig.core
  (:use [clojure.tools.trace]
        [seesaw core dev mig]
        [seesaw.widgets.log-window]))

;; The optimal game of pig as seen in Udacity 212.
;;
;; The rules of pig.
;; - 2 players each taking turns.
;; - First player with a score >= 40 wins.
;; - Player has two possible actions.  Roll the dice or hold.
;; - Rolling the dice accumulates a pending score unless the
;;   player rolls a 1 which is a pig out.
;; - A pig out causes the player to lose their pending score and
;;   only accumulate 1 point to their score.  Play passes to the
;;   next player.
;; - Holding takes the pending score and adds it to the players
;;   current score.  Play passes to the other player.

;; Simple lookup table to figure out the next player.
(def other {0 1
            1 0})

(defn roll 
  "Apply the roll action to a state (and a die roll d) to yield a new state:
    If d is 1, get 1 point (losing any accumulated 'pending' points),
    and it is the other player's turn. If d > 1, add d to 'pending' points."
  [state d]
  (let [[p me you pending] state]
    (if (= d 1)
      [(other p) you (inc me) 0] ; pig out; other player's turn
      [p me you (+ pending d)] ; accumulate die roll in pending
      )))

(defn hold
  "Apply the hold action to a state to yield a new state:
    Reap the 'pending' points and it becomes the other player's turn."
  [state]
  (let [[p me you pending] state]
    [(other p) you (+ me pending) 0]))

(defn Q-pig
  "The expected value of choosing action in state."
  [state action p-win]
  (cond
   (= action :hold) (- (double 1) (p-win (hold state)))
   (= action :roll) (/ (+ (double 1)
                          (- (p-win (roll state 1)))
                          (reduce +
                                  (map (fn [d] (p-win (roll state d)))
                                       [2 3 4 5 6])))
                       6)
   :else (throw (RuntimeException. "Invalid action"))))

(defn best-action
  "Return the optimal action for a state, given U. U is the utility function."
  [state actions Q U]
  (letfn [(EU [action]
            (Q state action U))]
    (apply max-key EU (actions state))))

(defn pig-actions
  "The legal actions from a state."
  [state]
  (let [[_ _ _ pending] state]
    (if (not= pending 0)
      [:roll :hold]
      [:roll])))

(def goal 40)

(def ^{:doc "The utility of a state; here just the probability that an optimal player
             whose turn it is to move can win from the current state."}
  P-win
  (memoize
   (fn[state]     
     (let [[p me you pending] state]
       (if (>= (+ me pending) goal)
         (double 1)
         (if (>= you goal)
           (double 0)
           (apply max (map #(Q-pig state % P-win)
                           (pig-actions state)))))))))

(defn max-wins
  "Calculate the action for the maximum chance of winning"
  [state]
  (best-action state pig-actions Q-pig P-win))

(defn game-over?
  "Is the game complete?"
  [state]
  (let [[_ me you _] state]
    (or (>= me goal) (>= you goal))))

(defn points-for-player [[p me you pending] player]
  (if (= p player) me you))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^{:doc "Current state of the game"}
  state 
  (atom [0 0 0 0]))

(defn action-button [id label]
  (button :id id :text label))

(def content (mig-panel :constraints ["", "[right,grow,fill]", "[][][][][][][grow,fill]"]
                        :items [
                                [ "Welcome to the game of pig!"                 "span"]
                                [ :separator                                    "span, growx"]
                                [ "Do you wish to roll or hold?"                "wrap"]
                                [ (action-button :roll "roll")                  "wrap"]
                                [ (action-button :hold "hold")                  "wrap"]
                                [ :separator                                    "span, growx"]
                                [ (scrollable (log-window :id :log-window))     "span, growx, growy, w 600, h 200"]
                                ]))

(def main-frame (frame :title "Pig" :content content))

(defn message [& xs] (log (select main-frame [:#log-window])
                          (str (apply str xs) "\n")))

(defn continue [state]
  (let [[p _ _ _] state
        log-frame (select main-frame [:#log-window])]
    (cond
     (game-over? state) (do
                          (message "Game over. Scores reset.")
                          [0 0 0 0])
     (= p 0)           state
     :else             (condp = (max-wins state)
                         :roll (let [d         (inc (rand-int 6))
                                     new-state (roll state d)]
                                 (message "Computer rolls " d)
                                 (message "Computer has " (points-for-player new-state 1) " point(s) and " (nth new-state 3) " pending point(s)")
                                 (recur  new-state))
                         :hold (let [new-state (hold state)]
                                 (message "Computer holds")
                                 (message "Computer has " (points-for-player new-state 1) " point(s)")
                                 (recur new-state))))))

(defn player-roll [state]
  (let [log-frame (select main-frame [:#log-window])
        dice-roll (inc (rand-int 6))
        new-state (roll state dice-roll)]
    (message "You rolled " dice-roll)
    (message "You have " (points-for-player new-state 0) " point(s) and " (nth new-state 3) " pending point(s)")
    (continue new-state)))

(defn player-hold [state]
  (let [new-state (hold state)]
    (message "Holding.  You have " (points-for-player new-state 0) " point(s)")
    (continue new-state)))

(defn init-frame []
  (native!)
  (listen (select main-frame [:#roll]) :mouse-clicked (fn [e] (swap! state player-roll)))
  (listen (select main-frame [:#hold]) :mouse-clicked (fn [e] (swap! state player-hold)))
  (-> main-frame pack! show!)
  (message "First to " goal " wins!")
  )

(defn -main
  "Play pig"
  [& args]
  (init-frame)
  )


