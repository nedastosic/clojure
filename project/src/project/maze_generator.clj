(ns project.maze-generator)

(defn generate-matrix [n]
  (def atom-matrix (atom (vec (repeat n (vec (repeat n 0)))))))

(defn generate-vector [n]
  (def atom-vector (atom (vec (repeat n 0)))))

(defn initialize []
  (let [n (int (Math/sqrt (count @atom-vector)))]
    (loop [i 0]
      (when (< i n)

        (loop [j 0]
          (when (< j n)

            (when (not= -1 (- j 1))
              (swap! atom-matrix assoc (+ (* i n) j) (assoc (get @atom-matrix (+ (* i n) j)) (- (+ (* i n) j) 1) 1))
              (swap! atom-matrix assoc (- (+ (* i n) j) 1) (assoc (get @atom-matrix (- (+ (* i n) j) 1)) (+ (* i n) j) 1))
              )


            (when (not= n (+ j 1))
              (swap! atom-matrix assoc (+ (* i n) j) (assoc (get @atom-matrix (+ (* i n) j)) (+ (+ (* i n) j) 1) 1))
              (swap! atom-matrix assoc (+ (+ (* i n) j) 1) (assoc (get @atom-matrix (+ (+ (* i n) j) 1)) (+ (* i n) j) 1))
              )


            (when (not= -1 (- i 1))
              (swap! atom-matrix assoc (+ (* i n) j) (assoc (get @atom-matrix (+ (* i n) j)) (- (+ (* i n) j) n) 1))
              (swap! atom-matrix assoc (- (+ (* i n) j) n) (assoc (get @atom-matrix (- (+ (* i n) j) n)) (+ (* i n) j) 1))
              )


            (when (not= n (+ i 1))
              (swap! atom-matrix assoc (+ (* i n) j) (assoc (get @atom-matrix (+ (* i n) j)) (+ (+ (* i n) j) n) 1))
              (swap! atom-matrix assoc (+ (+ (* i n) j) n) (assoc (get @atom-matrix (+ (+ (* i n) j) n)) (+ (* i n) j) 1))
              )


            ;(println i j (+ (* i n) j))


            (recur (+ j 1))))
        (recur (+ i 1))))
    )
  )

(defn visit [v]
  (swap! atom-vector assoc v 1))

(defn remove-wall [v1 v2]
  (swap! atom-matrix assoc v1 (assoc (get @atom-matrix v1) v2 0)))

(defn dfs [v]
  (visit v)
  (def shuffled [])
  (let [n (count @atom-vector)]
    (loop [i 0]
      (when (< i n)
        (when (= 1 (get (get @atom-matrix v) i))
          (def shuffled (conj shuffled i)))
        (recur (+ i 1))
        ))
    (def shuffled (shuffle shuffled))
    (def shuffled-global (assoc shuffled-global v shuffled))
    (loop [i 0]
      (when (< i (count (get shuffled-global v)))
        ;(println i)
        (when (= (get @atom-vector (get (get shuffled-global v) i)) 0)
          (println (str v "->" (get (get shuffled-global v) i)))
          (remove-wall v (get (get shuffled-global v) i))
          (remove-wall (get (get shuffled-global v) i) v)
          (dfs (get (get shuffled-global v) i))
          )
        (recur (+ i 1))
        ))
    ))


(defn print-maze []
  (let [n (int (Math/sqrt (count @atom-vector)))]
    (print ".in .")
    (loop [i 0]
      (when (< i (- n 1))
        (print "___.")
        (recur (+ i 1))))
    (loop [i 0]
      (when (< i n)
        (println)
        (loop [j 0]
          (when (< j n)

            (if (= -1 (- j 1)) (print "|"))

            (when (not= -1 (- j 1))
              (if (= 1 (get (get @atom-matrix (+ (* i n) j)) (- (+ (* i n) j) 1)))
                (print "|")
                (print ".")
                )
              )


            (if (not= n (+ i 1))
              (if (= 1 (get (get @atom-matrix (+ (* i n) j)) (+ (+ (* i n) j) n) 1))
                (print "___")
                (print "   "))
              (if (= n (+ j 1)) (print "out") (print "___")))


            (if (= n (+ j 1)) (print "|"))

            (recur (+ j 1))))
        (recur (+ i 1))))
    ))

(defn can-pass [v neighbour]
  (let [n (int (Math/sqrt (count @atom-vector)))]
    (cond
      (and (= neighbour "left") (not= 0 (mod v n)) (= 0 (get (get @atom-matrix v) (- v 1)))) true
      (and (= neighbour "right") (not= (- n 1) (mod v n)) (= 0 (get (get @atom-matrix v) (+ v 1)))) true
      (and (= neighbour "top") (>= v n) (= 0 (get (get @atom-matrix v) (- v n)))) true
      (and (= neighbour "bottom") (< v (* n (- n 1))) (= 0 (get (get @atom-matrix v) (+ v n)))) true
      :else false)))

(def shuffled-global {})
(generate-matrix 144)
(generate-vector 144)
(initialize)

(dfs 0)
(print-maze)