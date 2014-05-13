(ns game2048.core
  (:import (java.awt Color Dimension)
           (javax.swing JPanel JFrame Timer JOptionPane)
           (java.awt.event ActionListener KeyListener))
  (:use game2048.import-static))

(import-static java.awt.event.KeyEvent VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN)
(import-static java.awt.Color WHITE YELLOW BLACK)

(def matrix-size 4)

(defn new-num []
  (rand-nth [2, 4]))

(defn get-empty-cords [matrix]
  (for [x (range (count matrix))
        y (range (count matrix))
        :when (= 0 (get-in matrix [x y]))]
    [x y]))

(defn insert-new [matrix]
  (assoc-in matrix (rand-nth (get-empty-cords matrix)) (new-num)))

(defn create-empty-matrix []
  (vec (repeat matrix-size (vec (repeat matrix-size 0)))))

(defn create-new-matrix []
  (-> (create-empty-matrix)
      insert-new
      insert-new ))

(defn merge-raw-row [row]
  (loop [rest-row (rest row)
         head (first row)
         new-row []]
    (if (or (= (first rest-row) head) (empty? rest-row))
      (into (conj new-row (if (empty? rest-row) head (* 2 head))) (rest rest-row))
      (recur (rest rest-row)
             (first rest-row)
             (conj new-row head)))))

;(defn merge-row
; ([row] (merge-row row []))
; ([[x y & xs] res]
;    (cond
;     (nil? x) res
;     (nil? y) (conj res x)
;     (= x y)  (recur xs (conj res (* 2 x)))
;     :t       (recur (cons y xs) (conj res x)))))

(defn compress-row [row size dir]
  (let [compressed (vec (filter #(not= 0 %) row))
        pad-len (- size (count compressed))]
    (case dir
      :left (into compressed (repeat pad-len 0))
      :right (into (vec (repeat pad-len 0))  compressed))))

(defn merge-row [row dir]
  (vec (compress-row (merge-raw-row row) (count row) dir)))

(defn merge-left [row]
  (merge-row row :left))

(defn merge-right [row]
  (merge-row row :right))

(defn move-left [matrix]
  (vec (map merge-left matrix)))

(def reverse-vec (comp vec rseq))

(defn move-right [matrix]
  (vec (map reverse-vec (map merge-left (vec (map reverse-vec matrix))))))

(defn transpose [matrix]
  (vec (apply map vector matrix)))

(defn move-up [matrix]
  (vec (transpose (move-left (transpose matrix)))))

(defn move-down [matrix]
  (vec (transpose (move-right (transpose matrix)))))

(defn move [matrix direction]
  (case direction
    :left (move-left matrix)
    :right (move-right matrix)
    :up (move-up matrix)
    :down (move-down matrix)))

(defn can-move [matrix direction]
  (not= matrix (move matrix direction)))

(can-move [[1 1 0 1] [2 2 1 0] [1 2 1 2] [1 0 0 0 ]] :right)

(def m [[1 1 0 1] [2 2 1 0] [1 2 1 2] [1 0 0 0 ]])
(move-right m)
(move-left m)
(move-up m)
(move-down m)

(def m1 [[0 1 1 1] [2 2 1 0] [1 2 1 2] [1 0 0 0 ]])
(move-left m1)
(move-right m1)
(move-up m1)
(move-down m1)



(defn move-and-insert [matrix direction]
  (insert-new (move matrix direction)))

(move-and-insert m :left)

; GUI PART
(def lattice-size 50)

(def margin 5)
(def padding 10)
(def width  (* matrix-size (+ lattice-size margin)))
(def height  (* matrix-size (+ lattice-size margin)))

(def text-color WHITE)
(def dirs { VK_LEFT  :left
            VK_RIGHT :right
            VK_UP    :up
            VK_DOWN  :down})


(defn cord-to-lattice [cord]
  (let [[x y] cord]
    (if (or (nil? x) (nil? y))
      []
    [(* x (+ margin lattice-size)) (* y (+ margin lattice-size)) lattice-size lattice-size])))


(cord-to-lattice [1 2])
(defn fill-lattice1 [g {cord :cord}]
  (let [[x y width height] (cord-to-lattice cord)]
    (list x y)))


(defn paint1 [g matrix]
  (let [matrix (index-matrix matrix)])
  (doseq [lattice matrix]
    (fill-lattice1 g lattice)))

(doseq [x [ 1 2 3]]
  (println x))

(index-matrix (create-new-matrix))
(doseq [x (index-matrix (create-new-matrix))] (println (:cord x)))


(doseq [lattice (index-matrix (create-new-matrix))]
    (fill-lattice1 0 lattice))

(fill-lattice1 0 (second (index-matrix (create-new-matrix))))
(paint1 100 (create-new-matrix))

(defn fill-lattice [g {cord :cord v :v color :color}]
  (let [[x y width height] (cord-to-lattice cord)]
    (.setColor g color)
    (.fillRect g x y width height)
    (.setColor g BLACK)
    (.drawString g (str v) (+ x padding) (+ padding y))))

(defn index-matrix [matrix]
    (for [x (range (count matrix))
        y (range (count matrix))]
    {:cord [x y] :v (get-in matrix [x y]) :color YELLOW}))

(defn paint [g matrix]
  (let [matrix (index-matrix matrix)])
  (doseq [lattice (vec matrix)]
    (fill-lattice g lattice)))

(defn game-panel [frame matrix]
  (proxy [JPanel ActionListener KeyListener] []
    (paintComponent [g]
                    (proxy-super paintComponent g)
                    (paint g @matrix))
    (actionPerformed [e]
                    (.repaint this))
    (keyPressed [e]
                (move @matrix (dirs (.getKeyCode e))))
    (getPreferredSize []
                      (Dimension. width height))
    (keyReleased [e])
    (keyTyped [e])))


(defn test-panel [frame matrix]
    (proxy [JPanel ActionListener KeyListener] []
    (paintComponent [g]
                    (proxy-super paintComponent g)
                    (paint g @matrix)
                    (fill-lattice g {:cord [ 0 1] :v 10 :color YELLOW}))
    (actionPerformed [e]
                    (.repaint this))
    (keyPressed [e])
    (getPreferredSize []
                      (Dimension. width height))
    (keyReleased [e])
    (keyTyped [e])))

(defn game []
  (let [matrix (ref (create-new-matrix))
        frame (JFrame. "2048")
        panel (game-panel frame matrix)]
    (doto panel
      (.setFocusable true)
      (.addKeyListener panel))
    (doto frame
      (.add panel)
      (.pack)
      (.setVisible true))
    [matrix]))
(game)


(defn -main [& args]
  (game))



