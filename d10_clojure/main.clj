(ns main
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

;; Define a record Command that holds an op (:addx or :noop) and number
(defrecord Command [op number])

(defn read-file [file-path]
  (try
    ;; Open the file and return its lines as a vector
    (with-open [rdr (io/reader file-path)]
      (into [] (line-seq rdr)))
    (catch Exception e
      (println (str "Error reading file: " (.getMessage e))))))

;; Function to parse a line of text into a Command record
(defn parse [line]
  (let [words  (str/split line #" ")
        op     (if (= "addx" (first words)) :addx :noop)
        number (if (second words)
                 (Integer/parseInt (second words))
                 0)]
    (Command. op number)))

(defn run-program [program]
  (let [value  (atom 1)
        cycles (atom [1])]
    (doseq [cmd program]
      (if (= :noop (:op cmd))
        ;; If the Command is :noop, add the current value of X to the cycles
        (swap! cycles #(conj % @value))
        ;; If the Command is :addx, add the current value of X twice and update X
        (do (swap! cycles #(conj % @value))
            (swap! cycles #(conj % @value))
            (swap! value #(+ % (:number cmd))))))
    @cycles))

(defn part1 [program]
        ; run the program and get the resulting signal values
  (let [signal-values    (run-program program)
        ; multiply each signal value with its corresponding index
        signal-strengths (map-indexed * signal-values)
        ; pick signal strengths from signal-strengths using the range 20, 260, 40
        result           (map #(nth signal-strengths %) (range 20, 260, 40))]
    (println "Part 1:" (reduce + result))))

(defn is-adjacent [value x]
  (or (= value x)
      (= value (- x 1))
      (= value (+ x 1))))

(defn part2 [program]
  (let [signal-values (drop 1 (run-program program))
        crt-position  (atom 0)]  ; create an atom to keep track of the current position on the screen
    (println "Part 2:")
    (doseq [value signal-values] ; iterate over the values in the signal
      (do
        (if (is-adjacent @crt-position value) (print "#") (print "."))
        (if (= 0 (mod (+ @crt-position 1) 40)) ; check if the current position is at the end of a row
          (do (reset! crt-position  0) (println ""))
          (swap! crt-position inc))))))

(let [file-path (nth *command-line-args* 0)]
  (if file-path
    (let [lines (read-file file-path)
          program (map parse lines)]
      (part1 program)
      (part2 program))
    (println "Error: Please provide a file path.")))
