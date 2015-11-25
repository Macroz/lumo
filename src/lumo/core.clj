(ns lumo.core
  (:use [valo.loxone])
  (:use [midi]))

(def data (atom {:r 0.5 :g 0.8 :b 1 :hue 0}))

(defn set-all [valo r g b]
  (doseq [i (range 4 13)]
    (.set-light loxone (str "AI" i) r g b)))

(defn set-all-hsl [h s l]
  (doseq [i (range 4 13)]
    (.set-light-hsl loxone (str "AI" i) h s l)))

(defn handle-msg [msg timestamp]
  (println msg)
  (let [note (:note msg)
        cmd (:cmd msg)
        vel (:vel msg)
        ;;ln (+ (mod (- note 48) 9) 4)
        ;;x @light;;(/ (- note 48) (- 72 48))
        y (/ vel 128)
        ;;h x;;(* 360 x)
        ;;s 100
        ;;l (* 100.0 y)
        b (/ (/ vel 127.0) 2)
        notes [48 50 52 53 55 57 59 60 62]
        ln (+ (.indexOf notes note) 4)]
    (println "note" note "ln" ln)
    (when (= cmd 144)
      (when (= note 49)
        (.set-scene loxone 0))
      (when (= note 51)
        (set-all (@data :r) (@data :g) (@data :b)))
      (when (= note 66)
        (swap! data assoc :r y))
      (when (= note 68)
        (swap! data assoc :g y))
      (when (= note 70)
        (swap! data assoc :b y))
      (when (= note 54)
        (.set-light loxone (str "AI" 1) b 0 0))
      (when (= note 56)
        (.set-light loxone (str "AI" 2) b 0 0))
      (when (= note 58)
        (.set-light loxone (str "AI" 3) b 0 0))
      (when (= note 61)
        (set-all-hsl (rand 360) 100 (+ 20 (* b 60))))
      (when (= note 63)
        (doseq [i (range 4 13)]
          (.set-light-hsl loxone (str "AI" i) (rand 360) 100 (+ 20 (* b 60)))))
      (when (< 3 ln 13)
        (let [{:keys [r g b]} @data]
          (println r g b)
          (.set-light loxone (str "AI" ln) r g b)))
      )
    (when (= cmd 128)
      (when (= note 54)
        (.set-light loxone (str "AI" 1) 0 0 0))
      (when (= note 56)
        (.set-light loxone (str "AI" 2) 0 0 0))
      (when (= note 58)
        (.set-light loxone (str "AI" 3) 0 0 0))
      (when (< 3 ln 13)
        (.set-light-hsl loxone (str "AI" ln) (@data :hue) 100 50)
        (swap! data update-in [:hue] (fn [i] (mod (+ i 15) 360)))
        ))
    ))

(def handler (atom handle-msg))

(defn listen []
  (let [keyboard (midi-in "LPK25")]
    (midi-handle-events keyboard #(@handler %1 %2))))

(reset! handler handle-msg)

(def kitt-series
  (let [series (cycle (concat (range 4 12) (range 12 4 -1)))
        series (map list series (rest series) (rest (rest series)) (drop 3 series))]
    series))

(defn set-kitt-series [i]
  (let [[i1 i2 i3 i4] (nth kitt-series i)]
    (.set-light loxone (str "AI" i4) 0.5 0 0)
    (.set-light loxone (str "AI" i3) 0.4 0 0)
    (.set-light loxone (str "AI" i2) 0.3 0 0)
    (.set-light loxone (str "AI" i1) 0.0 0 0)
    ))

(defn knightrider []
  (set-all 0 0 0)
  (Thread/sleep 2000)
  (doseq [i (range 1000)]
    (set-kitt-series i)
    (Thread/sleep 50)))

(defn wave [x]
  (doseq [i (range 4 13)]
    (.set-light-hsl loxone (str "AI" i) (mod x 360.0) 20.0 50.0)
    (Thread/sleep 20)
    ))

(defn waves []
  (doseq [i (range 0 10000 10)] (wave i)))

(defn lerp [x1 x2 f]
  (+ (* (- 1.0 f) x1) (* f x2)))

(defn rand-color []
  [(+ 0.5 (rand 0.5))
   (+ 0.5 (rand 0.5))
   (+ 0.5 (rand 0.5))])

(defn rand-intense-color []
  [(rand 360.0) 100.0 50.0])

(defn liuku [c1 c2]
  (doseq [i (range 4 13)]
    (let [[r1 g1 b1] c1
          [r2 g2 b2] c2
          f (/ (- i 4) 8.0)
          r (lerp r1 r2 f)
          g (lerp g1 g2 f)
          b (lerp b1 b2 f)]
      ;;(println f [r g b])
      (.set-light-hsl loxone (str "AI" i) r g b)
      (Thread/sleep 400))))

(defn rand-liuku []
  (liuku (rand-intense-color) (rand-intense-color)))
