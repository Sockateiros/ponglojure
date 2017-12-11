(ns pong.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [amalloy.ring-buffer :as r]
            [clojure.math.combinatorics :as c]))

(def history (atom (r/ring-buffer (* 30 30))))
(def debug (atom {:active false
                  :step 0
                  :dragging false
                  :dt 33
                  :frames 20}))

(def id (atom -1))
(defn gen-id! []
  (swap! id inc))

(defn setup []
  (q/frame-rate 60)
  (q/no-cursor)
  (let [init-state {:field-w (q/width)
                    :field-h (q/height)
                    :p1-up 0
                    :p2-up 0
                    :p1-down 0
                    :p2-down 0
                    :os [{:id (gen-id!)
                          :x (* 0.5 (q/width))
                          :y (* 0.5 (q/height))
                          :vx 0
                          :vy 0
                          :w (* (q/width) 0.02)
                          :h (* (q/width) 0.02)
                          :m 1}
                         {:id (gen-id!)
                          :x (* 0.52 (q/width))
                          :y (* 0.25 (q/height))
                          :vx 0
                          :vy 0.1
                          :w (* (q/width) 0.02)
                          :h (* (q/width) 0.08)
                          :m 1}]
                    :ts (System/currentTimeMillis)}]
    (swap! history #(conj % init-state))
    init-state))

(defn mag [x y]
  (Math/sqrt (+ (* x x) (* y y))))

(defn intersect? [x1 y1 w1 h1 x2 y2 w2 h2]
 (and (<= x1 (+ x2 w2)) 
      (<= x2 (+ x1 w1))
      (<= y1 (+ y2 h2)) 
      (<= y2 (+ y1 h1))))

(defn intersects? [[o1 o2]]
 (intersect? (o1 :x) (o1 :y) (o1 :w) (o1 :h)
             (o2 :x) (o2 :y) (o2 :w) (o2 :h)))

(defn advance [dt o]
  (assoc o
         :x (+ (o :x) (* dt (o :vx)))
         :prev-x (o :x)
         :y (+ (o :y) (* dt (o :vy)))
         :prev-y (o :y)))

(defn collision-axis [[o1 o2]]
  (let [x1m (+ (o1 :x) (* 0.5 (o1 :w)))
        x2m (+ (o2 :x) (* 0.5 (o2 :w)))
        y1m (+ (o1 :y) (* 0.5 (o1 :h)))
        y2m (+ (o2 :y) (* 0.5 (o2 :h)))
        dxm (Math/abs (- x1m x2m))
        dym (Math/abs (- y1m y2m))]
    (if (< dxm dym) :x :y)))

(defn intersects-prev-x? [o1 o2]
  (or
   (and (>= (o2 :prev-x) (o1 :prev-x))
        (<= (o2 :prev-x) (+ (o1 :prev-x) (o1 :w))))
   (and (>= (o1 :prev-x) (o2 :prev-x))
        (<= (o1 :prev-x) (+ (o2 :prev-x) (o2 :w))))))

(defn remove-intersections [[o1 o2]]
  [(assoc o1
          :x (o1 :prev-x)
          :y (o1 :prev-y))
   (assoc o2
          :x (o2 :prev-x)
          :y (o2 :prev-y))])

(defn new-vs [[o1 o2]]
  (if (intersects-prev-x? o1 o2)
    [(assoc o1 :vy (o2 :vy)) (assoc o2 :vy (o1 :vy))]
    [(assoc o1 :vx (o2 :vx)) (assoc o2 :vx (o1 :vx))]))

(defn elastic-collision [os-pair]
  (remove-intersections (new-vs os-pair)))

(defn calc-vs [os-pairs]
  (into [] (mapcat #(if (intersects? %) (elastic-collision %) %) os-pairs)))

(defn resolve-collisions [os]
  (calc-vs (c/combinations os 2)))

(defn step [nts state] 
  (assoc state
         :os (resolve-collisions
              (mapv #(advance (- nts (state :ts)) %) (state :os)))
         :ts nts))

(defn update-state [state]
  (if (@debug :active)
    (do
      (q/cursor :hand)
      (let [new-state (nth @history
                           (+ (@debug :step) (count @history)) state)]
        (cond (and (@debug :dragging) (= ((@debug :m-ievt) :button) :left))
              (assoc-in new-state [:os (@debug :dragging)]
                        (assoc (get-in new-state [:os (@debug :dragging)])
                               :x ((@debug :m-evt) :x)
                               :y ((@debug :m-evt) :y)))
              (and (@debug :dragging) (= ((@debug :m-ievt) :button) :right))
              (assoc-in new-state [:os (@debug :dragging)]
                        (assoc (get-in new-state [:os (@debug :dragging)])
                               :vx (* 0.001 (- ((@debug :m-evt) :x) ((@debug :m-ievt) :x)))
                               :vy (* 0.001 (- ((@debug :m-evt) :y) ((@debug :m-ievt) :y)))))
              :else new-state)))
    (let [new-ts (System/currentTimeMillis)
          new-state (step new-ts state)]
      (swap! history #(conj % new-state))
      new-state)))

(defn draw-obj [o]
  (q/rect (o :x) (o :y) (o :w) (o :h)))

(defn draw-future [dt frames state]
  (let [start-ts (state :ts)
        end-ts (+ start-ts (* dt frames))
        future-states (reduce (fn [states ts]
                                (conj states (step ts (last states))))
                              [state] (range start-ts end-ts dt))
        cfill (q/current-fill)
        cstroke (q/current-stroke)]
    (q/no-fill)
    (q/stroke 125)
    (doseq [s future-states]
      (doseq [o (s :os)]
        (draw-obj o)))
    (q/fill cfill)
    (q/stroke cstroke)))

(defn draw-state [state]
  (q/background 0 0 0)
  (q/fill 255)
  (doseq [o (state :os)]
    (draw-obj o))
  (q/text (prn-str state) 0 30)
  (q/text (prn-str @debug) 0 60)
  (q/text (prn-str (q/current-frame-rate)) 0 90)
  (if (@debug :active)
    (draw-future (@debug :dt) (@debug :frames) state)))

(defn rpop! [n ring]
  (swap! ring #(into (r/ring-buffer 900) (drop-last n %))))

(defn key-pressed [state event]
  (case (:key-code event)
    87 (assoc state :p1-up -1)
    83 (assoc state :p1-down 1)
    38 (assoc state :p2-up -1)
    40 (assoc state :p2-down 1)
    32 (do
         (swap! debug #(assoc % :active (not (% :active))))
         (if (and (not (@debug :active)) (neg? (@debug :step)))
           (rpop! (* -1 (@debug :step)) history))
         (swap! debug #(assoc % :step 0))
         (assoc state :ts (System/currentTimeMillis)))
    90 (do (swap! debug #(assoc % :step (dec (% :step)))) state)
    88 (do (swap! debug #(assoc % :step (inc (% :step)))) state)
    state))

(defn key-released [state event]
  (case (:key-code event)
    87 (assoc state :p1-up 0)
    83 (assoc state :p1-down 0)
    38 (assoc state :p2-up 0)
    40 (assoc state :p2-down 0)
    state))

(defn mouse-inside? [x y w h mx my]
  (and (>= mx x) (<= mx (+ x w))
       (>= my y) (<= my (+ y h))))


(defn positions
  [pred coll]
  (keep-indexed (fn [idx x] (when (pred x) idx)) coll))

(defn clicked [os mx my]
  (first (positions #(mouse-inside? (% :x) (% :y) (% :w) (% :h) mx my)
                    os)))

(defn mouse-pressed [state event]
  (do
    (swap! debug
           #(assoc %
                   :dragging (clicked (state :os) (event :x) (event :y))
                   :m-ievt event :m-evt event))
    state))

(defn mouse-dragged [state event]
  (swap! debug #(assoc % :m-evt event))
  state)

(defn mouse-released [state event]
  (swap! debug #(assoc % :dragging false :step 0))
  state)

(q/defsketch pong
  :title "Pong"
  :size [(q/screen-width) (q/screen-height)]
  :setup setup
  :update update-state
  :draw draw-state
  :middleware [m/fun-mode m/pause-on-error]
  :key-pressed key-pressed
  :key-released key-released
  :mouse-pressed mouse-pressed
  :mouse-released mouse-released
  :mouse-dragged mouse-dragged)
