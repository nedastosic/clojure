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
  (let [n (count @atom-vector)]
    (loop [i 0]
      (when (< i n)
        ;(println i)
        (visit v)
        (when (and (= (get @atom-vector i) 0) (= (get (get @atom-matrix v) i) 1))
          (println (str v "->" i))
          (remove-wall v i)
          (remove-wall i v)
          (dfs i)
          )
        (recur (+ i 1))
        ))
    ))


(generate-matrix 9)
(generate-vector 9)

(println (initialize))
(dfs 0)

(println @atom-vector)
(println @atom-matrix)

;(println @atom-matrix)