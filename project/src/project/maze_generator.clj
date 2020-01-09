(ns project.maze-generator)


(defmacro do-while
  [test & body]
  `(loop []
     ~@body
     (when ~test
       (recur))))

(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

(defn vec-remove
  "remove elem in coll"
  [pos coll]
  (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))

(defn generate-matrix [n]
  (def atom-matrix (atom (vec (repeat n (vec (repeat n 0)))))))

(defn generate-vector [n]
  (def atom-vector (atom (vec (repeat n 0)))))

(defn initialize [size]
  (do
    (def path [])
    (def start-nodes [])
    (def end-nodes [])
    (def to-be-removed [])
    (def shuffled-global {})
    (generate-matrix size)
    (generate-vector size)
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

              (recur (+ j 1))))
          (recur (+ i 1)))))
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
        (when (= (get @atom-vector (get (get shuffled-global v) i)) 0)
          ;(println (str v "->" (get (get shuffled-global v) i)))
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
    (println)
    ))

(defn prune []
  (do
    (do-while (not= 0 (count to-be-removed))
              (do
                (def to-be-removed [])
                (loop [i 0]
                  (when (< i (count end-nodes))
                    (do
                      (when (and (= nil (in? start-nodes (get end-nodes i))) (not= (- (count @atom-vector) 1) (get end-nodes i)))
                        (do
                          (def to-be-removed (conj to-be-removed i)))
                        )
                      (recur (+ i 1)))

                    ))

                (loop [i 0]
                  (when (< i (count to-be-removed))
                    (do
                      (def end-nodes (vec-remove (- (get to-be-removed i) i) end-nodes))
                      (def start-nodes (vec-remove (- (get to-be-removed i) i) start-nodes))
                      (recur (+ i 1)))
                    ))
                ))
    (println)
    (print (str (get start-nodes 0) " -> " (get end-nodes 0)))
    (loop [i 1]
      (when (< i (count start-nodes))
        (do
          (print (str " -> " (get end-nodes i)))
          (recur (+ i 1)))
        ))
    (println)
    ))

(defn print-solved-maze []
  (do
    ;(prune)
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
                  (if (or (in? start-nodes (+ (* i n) j)) (in? end-nodes (+ (* i n) j)))
                    (print "_*_")
                    (print "___"))
                  (if (or (in? start-nodes (+ (* i n) j)) (in? end-nodes (+ (* i n) j)))
                    (print " * ")
                    (print "   "))
                  )
                (if (= n (+ j 1))
                  (print "out")
                  (if (or (in? start-nodes (+ (* i n) j)) (in? end-nodes (+ (* i n) j)))
                    (print "_*_")
                    (print "___"))))


              (if (= n (+ j 1)) (print "|"))

              (recur (+ j 1))))
          (recur (+ i 1))))
      (println))
    ))

(defn can-pass [v neighbour]
  (let [n (int (Math/sqrt (count @atom-vector)))]
    (cond
      (and (= neighbour "left") (not= 0 (mod v n)) (= 0 (get (get @atom-matrix v) (- v 1)))) true
      (and (= neighbour "right") (not= (- n 1) (mod v n)) (= 0 (get (get @atom-matrix v) (+ v 1)))) true
      (and (= neighbour "top") (>= v n) (= 0 (get (get @atom-matrix v) (- v n)))) true
      (and (= neighbour "bottom") (< v (* n (- n 1))) (= 0 (get (get @atom-matrix v) (+ v n)))) true
      :else false)))

(defn set-path [v]
  (when (= nil (in? path v))
    (def path (conj path v))))

(defn set-start-node [v]
  (def start-nodes (conj start-nodes v)))


(defn set-end-node [v]
  (def end-nodes (conj end-nodes v)))


(defn solve-maze [v]
  (if (not= v (- (count @atom-vector) 1))
    (do
      (set-path v)
      (def shuffled [])
      (let [n (int (Math/sqrt (count @atom-vector)))]
        (do
          (when (and (can-pass v "left") (= nil (in? path (- v 1))))
            (def shuffled (conj shuffled (- v 1))))
          (when (and (can-pass v "right") (= nil (in? path (+ v 1))))
            (def shuffled (conj shuffled (+ v 1))))
          (when (and (can-pass v "bottom") (= nil (in? path (+ v n))))
            (def shuffled (conj shuffled (+ v n))))
          (when (and (can-pass v "top") (= nil (in? path (- v n))))
            (def shuffled (conj shuffled (- v n))))
          )

        (def shuffled (shuffle shuffled))
        (def shuffled-global (assoc shuffled-global v shuffled))
        (loop [i 0]
          (when (< i (count (get shuffled-global v)))
            (do
              (set-start-node v)
              (set-end-node (get (get shuffled-global v) i))
              (println (str v "->" (get (get shuffled-global v) i)))
              (solve-maze (get (get shuffled-global v) i))

              (recur (+ i 1)))
            ))
        ))
    (set-path v)))



(initialize 16)
(dfs 0)
(print-maze)
(def shuffled-global {})
(solve-maze 0)
(print-solved-maze)
