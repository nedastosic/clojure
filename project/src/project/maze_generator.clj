(ns project.maze-generator)

(defn dfs [n]
  (loop [i 0]
    (when (< i n)
      (println i)
      (recur (+ i 1))
      )))


(println (dfs 3))

