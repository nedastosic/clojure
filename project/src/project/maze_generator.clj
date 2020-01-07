(ns project.maze-generator)

;(def dir "south")
;(def visited [])
(def compass "south")
(def path [])
(def start-nodes [])
(def end-nodes [])
(def node-pairs {})
(def node-pairs-reverted {})
(def to-be-removed [])
;(def found-next-direction (atom false))

(defn select-values [map ks]
  (reduce #(conj %1 (map %2)) [] ks))

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


(defn print-maze-solver []
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
    (println)
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

(defn set-node-pairs [k v]
  (def node-pairs (assoc node-pairs k v)))

(defn set-node-pairs-reverted [k v]
  (when (= false (contains? node-pairs-reverted k))
    (def node-pairs-reverted (assoc node-pairs-reverted k v))))

(defn walk [v]
  (if (not= v (- (count @atom-vector) 1))
    (do
      (set-path v)
      (def shuffled [])
      (let [n (int (Math/sqrt (count @atom-vector)))]
        (do
          (when (and (can-pass v "left") (= 0 (get (get @atom-matrix v) (- v 1))) (= nil (in? path (- v 1))))
            (def shuffled (conj shuffled (- v 1))))
          (when (and (can-pass v "right") (= 0 (get (get @atom-matrix v) (+ v 1))) (= nil (in? path (+ v 1))))
            (def shuffled (conj shuffled (+ v 1))))
          (when (and (can-pass v "bottom") (= 0 (get (get @atom-matrix v) (+ v n))) (= nil (in? path (+ v n))))
            (def shuffled (conj shuffled (+ v n))))
          (when (and (can-pass v "top") (= 0 (get (get @atom-matrix v) (- v n))) (= nil (in? path (- v n))))
            (def shuffled (conj shuffled (- v n))))
          )


        ;(def shuffled (shuffle shuffled))
        (def shuffled-global (assoc shuffled-global v shuffled))
        (loop [i 0]
          (when (< i (count (get shuffled-global v)))
            (do
              (set-start-node v)
              (set-end-node (get (get shuffled-global v) i))
              ;(set-node-pairs v (get (get shuffled-global v) i))
              ;(set-node-pairs-reverted (get (get shuffled-global v) i) v)
              (println (str v "->" (get (get shuffled-global v) i)))
              (walk (get (get shuffled-global v) i))

              (recur (+ i 1)))
            ))
        ))
    (set-path v)))

(defn vec-remove
  "remove elem in coll"
  [pos coll]
  (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))

(defn prune []
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
              ;(def to-be-removed (into [] (concat to-be-removed (select-values node-pairs-reverted to-be-removed))))

              (loop [i 0]
                (when (< i (count to-be-removed))
                  (do
                    (def end-nodes (vec-remove (- (get to-be-removed i) i) end-nodes))
                    (def start-nodes (vec-remove (- (get to-be-removed i) i) start-nodes))
                    (recur (+ i 1)))
                  ))

              ;(loop [i 0]
              ;  (when (< i (count start-nodes))
              ;    (if (not= nil (in? to-be-removed (get start-nodes i)))
              ;      (do
              ;        (def start-nodes (vec-remove i start-nodes))
              ;        (recur i))
              ;      (recur (+ i 1))
              ;      )
              ;    ))

              )))




(def shuffled-global {})
(generate-matrix 100)
(generate-vector 100)
(initialize)

(dfs 57)
(print-maze)


(def shuffled-global {})
(walk 0)
;(println node-pairs)
;(println (count node-pairs))
;(print path)
(println start-nodes)
(println end-nodes)
;(println node-pairs)

(prune)


(println start-nodes)
(println end-nodes)


(print-maze-solver)


;
;(defn toggle [v]
;  (do
;    (def path (conj path v))
;    (when (= nil (in? visited v))
;      (def visited (conj visited v)))))
;
;
;;(remove visited v)
;
;(defn can-go-forward [v]
;  (cond
;    (and (= dir "south") (= true (can-pass v "bottom"))) true
;    (and (= dir "east") (= true (can-pass v "right"))) true
;    (and (= dir "north") (= true (can-pass v "top"))) true
;    (and (= dir "west") (= true (can-pass v "left"))) true
;    :else false))
;
;(defn get-next-neighbour [v]
;  (let [n (int (Math/sqrt (count @atom-vector)))]
;    (cond
;      (= dir "south") (+ v n)
;      (= dir "east") (+ v 1)
;      (= dir "north") (- v n)
;      (= dir "west") (- v 1)
;      )))
;
;(defn get-next-dir []
;  (cond
;    (= dir "south") (def dir "west")
;    (= dir "east") (def dir "south")
;    (= dir "north") (def dir "east")
;    (= dir "west") (def dir "north")))
;
;
;(defn is-visited [v]
;  (let [n (int (Math/sqrt (count @atom-vector)))]
;    (cond
;      (and (= dir "south") (not= nil (in? visited (+ v n)))) true
;      (and (= dir "east") (not= nil (in? visited (+ v 1)))) true
;      (and (= dir "north") (not= nil (in? visited (- v n)))) true
;      (and (= dir "west") (not= nil (in? visited (- v 1)))) true
;      :else false)
;    ))
;
;
;(defn reset []
;  (def found-next-direction (atom false)))
;
;(defn get-next-unvisited-dir [v]
;  (do
;    (reset)
;    (while (not= true @found-next-direction)
;      (do
;        (get-next-dir)
;        (when (and (can-go-forward v) (= false (is-visited v)))
;          (def found-next-direction (atom true)))))
;    ))
;
;(defn has-right-wall [v]
;  (cond
;    (and (= dir "south") (= false (can-pass v "left"))) true
;    (and (= dir "east") (= false (can-pass v "bottom"))) true
;    (and (= dir "north") (= false (can-pass v "right"))) true
;    (and (= dir "west") (= false (can-pass v "top"))) true
;    :else false))
;
;(defn was-there [v]
;  (let [n (int (Math/sqrt (count @atom-vector)))]
;    (cond
;      (and (= dir "south") (= true (can-pass v "left")) (= (nth path (- (count path) 2)) (- v 1))) true
;      (and (= dir "east") (= true (can-pass v "bottom")) (= (nth path (- (count path) 2)) (+ v n))) true
;      (and (= dir "north") (= true (can-pass v "right")) (= (nth path (- (count path) 2)) (+ v 1))) true
;      (and (= dir "west") (= true (can-pass v "top")) (= (nth path (- (count path) 2)) (- v n))) true
;      :else false)))
;
;
;(defn get-next-visited-dir [v]
;  (do
;    (reset)
;    (while (not= true @found-next-direction)
;      (do
;        (get-next-dir)
;        (when (and (can-go-forward v) (or (has-right-wall v) (was-there v)))
;          (def found-next-direction (atom true)))))
;    ))
;
;
;(defn change-dir [v]
;  (let [n (int (Math/sqrt (count @atom-vector)))]
;    (if (and
;          (or (and (can-pass v "top") (not= nil (in? visited (- v n)))) (= false (can-pass v "top")))
;          (or (and (can-pass v "bottom") (not= nil (in? visited (+ v n)))) (= false (can-pass v "bottom")))
;          (or (and (can-pass v "left") (not= nil (in? visited (- v 1)))) (= false (can-pass v "left")))
;          (or (and (can-pass v "right") (not= nil (in? visited (+ v 1)))) (= false (can-pass v "right"))))
;      (get-next-visited-dir v)
;      (get-next-unvisited-dir v)
;      )))
;
;(defn walk [v]
;  (do
;    (toggle v)
;    (if (not= v (- (count @atom-vector) 1))
;      (if (and (= true (can-go-forward v)) (or (= true (has-right-wall v)) ()))
;        (let [next (get-next-neighbour v)]
;          (do
;            (println (str v "->" next " - " dir " " visited))
;            (walk next))
;          )
;        (do
;          (change-dir v)
;          (walk v)
;          ))
;      (do
;        (toggle v)
;        (println visited)))
;    ))