(ns ui-gallery.chars
  (:require
   [play-cljc.instances :as i]
    #?(:clj  [play-cljc.macros-java :refer [gl]]
      :cljs [play-cljc.macros-js :refer-macros [gl]])
   [play-cljc.math :as m]
   [play-cljc.transforms :as t]))

#?(:clj (set! *warn-on-reflection* true))

(defn crop-char [{:keys [baked-font] :as font-entity} ch]
  (let [{:keys [baked-chars baseline first-char]} baked-font
        char-code (- #?(:clj (int ch) :cljs (.charCodeAt ch 0)) first-char)
        baked-char (nth baked-chars char-code)
        {:keys [x y w h xoff yoff]} baked-char]
    (-> font-entity
        (t/crop x y w h)
        (assoc-in [:uniforms 'u_scale_matrix]
                  (m/scaling-matrix w h))
        (assoc-in [:uniforms 'u_translate_matrix]
                  (m/translation-matrix xoff (+ baseline yoff)))
        (assoc :baked-char baked-char))))

(defn assoc-char
  ([text-entity index char-entity]
   (assoc-char text-entity 0 index char-entity))
  ([{:keys [baked-font characters] :or {characters []} :as text-entity} line-num index {:keys [baked-char] :as char-entity}]
   (let [characters (loop [chars characters]
                      (if (<= (count chars) line-num)
                        (recur (conj chars []))
                        chars))
         line (get characters line-num)
         prev-chars (subvec line 0 index)
         prev-xadv (reduce + 0 (map #(-> % :baked-char :xadv) prev-chars))
         x-total (+ (:xadv baked-char) prev-xadv)
         y-total (* line-num (:font-height baked-font))
         prev-lines (subvec characters 0 line-num)
         prev-count (reduce + 0 (map count prev-lines))
         replaced-char (get line index)
         line (assoc line index (assoc char-entity :x-total x-total))
         next-char (get line (inc index))]
     (-> text-entity
         (assoc :characters (assoc characters line-num line))
         (i/assoc (+ index prev-count)
                  (-> char-entity
                      (update-in [:uniforms 'u_translate_matrix]
                                 #(m/multiply-matrices 3 (m/translation-matrix prev-xadv y-total) %))))
         ;; adjust the next char if its horizontal position changed
         (cond-> (and next-char (not= (:x-total replaced-char) x-total))
           (assoc-char line-num (inc index) next-char))))))

(defn get-baked-char [font ch]
  (let [{:keys [baked-chars first-char]} (:baked-font font)
        char-code (- #?(:clj (int ch) :cljs (.charCodeAt ch 0)) first-char)]
    (nth baked-chars char-code)))

(defn assoc-lines0
  [dynamic-entity font-entity lines]
  (reduce
   (partial apply assoc-char)
   dynamic-entity
   (for [line-num (range (count lines))
         char-num (range (count (nth lines line-num)))
         :let     [ch (get-in lines [line-num char-num])]]
     [line-num char-num (crop-char font-entity ch)])))

(defn assoc-lines1
  [dynamic-entity font-entity lines]
  (let [baseline    (-> font-entity :baked-font :baseline)
        font-height (-> font-entity :baked-font :font-height)
        i->baked-ch (into []
                          (comp (map-indexed (fn [line-num line] (map (fn [ch] [line-num (get-baked-char font-entity ch)]) (vec line))))
                                (mapcat identity)
                                (map-indexed (fn [i [line-num baked]] [i line-num baked])))
                          lines)
        i->xadv     (into []
                          (comp (map #(reductions + 0 (map (fn [[_i _line-num baked-ch]] (:xadv baked-ch)) (drop-last %))))
                                (mapcat identity))
                          (vals (group-by second i->baked-ch)))]
    (reduce
     (fn [entity [char-i line-num baked-char]]
       (let [{:keys [x y w h xoff yoff]} baked-char
             xadv        (get i->xadv char-i)
             y-total     (* line-num font-height)
             char-entity (-> font-entity
                             (t/crop x y w h)
                             (assoc-in [:uniforms 'u_scale_matrix]
                                       (m/scaling-matrix w h))
                             (assoc-in [:uniforms 'u_translate_matrix]
                                       (m/translation-matrix (+ xoff xadv) (+ baseline yoff y-total))))]
         (i/assoc entity char-i char-entity)))
     dynamic-entity
     i->baked-ch)))

(defn assoc-lines2
  [dynamic-entity font-entity lines]
  (let [baseline    (-> font-entity :baked-font :baseline)
        font-height (-> font-entity :baked-font :font-height)
        i->line+ch  (into []
                          (comp (map-indexed vector)
                                (map (fn [[line-num line]] (->> (vec line) (map (fn [ch] [line-num ch])))))
                                (mapcat identity))
                          lines)
        total-ch    (count i->line+ch)]
    (loop [char-i 0 total-xadv 0.0 prev-line-num -1 entity dynamic-entity]
      (if (< char-i total-ch)
        (let [[line-num ch] (nth i->line+ch char-i)
              baked-ch      (get-baked-char font-entity ch)
              {:keys [x y w h xoff yoff ^double xadv]} baked-ch
              y-total     (* line-num font-height)
              total-xadv  (if (= line-num prev-line-num) total-xadv 0)
              char-entity (-> font-entity
                              (t/crop x y w h)
                              (assoc-in [:uniforms 'u_scale_matrix]
                                        (m/scaling-matrix w h))
                              (assoc-in [:uniforms 'u_translate_matrix]
                                        (m/translation-matrix (+ xoff total-xadv) (+ baseline yoff y-total))))
              entity      (i/assoc entity char-i char-entity)]
          (recur (inc char-i) (+ total-xadv xadv) line-num entity))
        entity))))

(defn get-crop [font-entity baked-ch]
  (let [{:keys [width height]} font-entity
        font_texture_matrix (get-in font-entity [:uniforms 'u_texture_matrix])
        {:keys [x y w h]} baked-ch]
    (->> font_texture_matrix
         (m/multiply-matrices
          (m/translation-matrix (/ x width) (/ y height)))
         (m/multiply-matrices
          (m/scaling-matrix (/ w width) (/ h height))))))

(def memo-crop
  #?(:clj (memoize get-crop)
     :cljs (memoize get-crop)))

(defn assoc-lines3
  [dynamic-entity font-entity lines]
  (let [{:keys [baked-font]} font-entity
        baseline            (-> baked-font :baseline)
        font-height         (-> baked-font :font-height)
        i->line+ch          (into []
                                  (comp (map-indexed vector)
                                        (map (fn [[line-num line]] (->> (vec line) (map (fn [ch] [line-num ch])))))
                                        (mapcat identity))
                                  lines)
        total-ch    (count i->line+ch)]
    (loop [char-i 0 total-xadv 0.0 prev-line-num #?(:clj (Long. -1) :cljs -1)
           a_texture   (transient [])
           a_scaling   (transient [])
           a_translate (transient [])
           a_color     (transient [])]
      (if (< char-i total-ch)
        (let [[line-num ch] (get i->line+ch char-i)
              baked-ch      (get-baked-char font-entity ch)

              {:keys [w h xoff yoff ^double xadv]} baked-ch
              y-total     (* line-num font-height)
              total-xadv  (if (= line-num prev-line-num) total-xadv 0)

              u_crop      (get-crop font-entity baked-ch)
              u_scaling   (m/scaling-matrix w h)
              u_translate (m/translation-matrix (+ xoff total-xadv) (+ baseline yoff y-total))
              u_color     [0.2 0.4 0.3 1]]
          ;; (println (type line-num) (type xadv) (type total-xadv))
          (recur (inc char-i) (+ total-xadv xadv) line-num
                 (reduce conj! a_texture u_crop)
                 (reduce conj! a_scaling u_scaling)
                 (reduce conj! a_translate u_translate)
                 (reduce conj! a_color u_color)))
        (let [a_texture   (persistent! a_texture)
              a_scaling   (persistent! a_scaling)
              a_translate (persistent! a_translate)
              a_color     (persistent! a_color)
              res (-> dynamic-entity
                      (update :attributes
                              (fn [attrs]
                                (assoc attrs
                                       'a_texture_matrix {:data a_texture :divisor 1}
                                       'a_scale_matrix {:data a_scaling :divisor 1}
                                       'a_translate_matrix {:data a_translate :divisor 1}
                                       'a_color {:data a_color :divisor 1}))))]
          ;; #?(:clj (do (throw (Exception. "hmm2"))))
          res)))))

(defn assoc-lines4
  [dynamic-entity font-entity lines]
  (let [{:keys [baked-font]} font-entity
        baseline            (-> baked-font :baseline)
        font-height         (-> baked-font :font-height)
        i->line+ch          (into []
                                  (comp (map-indexed vector)
                                        (map (fn [[line-num line]] (->> (vec line) (map (fn [ch] [line-num ch])))))
                                        (mapcat identity))
                                  lines)
        total-ch    (count i->line+ch)
        a_texture   (float-array (* total-ch 9))
        a_scaling   (float-array (* total-ch 9))
        a_translate (float-array (* total-ch 9))
        a_color     (float-array (* total-ch 4))]
    (loop [char-i 0 total-xadv 0.0 prev-line-num #?(:clj (Long. -1) :cljs -1)]
      (if (< char-i total-ch)
        (let [[line-num ch] (get i->line+ch char-i)
              baked-ch      (get-baked-char font-entity ch)

              {:keys [w h xoff yoff ^double xadv]} baked-ch
              y-total     (* line-num font-height)
              total-xadv  (if (= line-num prev-line-num) total-xadv 0)

              u_crop      (memo-crop font-entity baked-ch)
              u_scaling   (m/scaling-matrix w h)
              u_translate (m/translation-matrix (+ xoff total-xadv) (+ baseline yoff y-total))
              u_color     [0.2 0.4 0.3 1]]
          ;; (println (type line-num) (type xadv) (type total-xadv))
          (dotimes [i 9]
            (aset-float a_texture (+ (* char-i 9) i) (nth u_crop i))
            (aset-float a_scaling (+ (* char-i 9) i) (nth u_scaling i))
            (aset-float a_translate (+ (* char-i 9) i) (nth u_translate i)))
          (dotimes [i 4]
            (aset-float a_color (+ (* char-i 4) i) (nth u_color i)))
          (recur (inc char-i) (+ total-xadv xadv) line-num))
        (let [res (-> dynamic-entity
                      (update :attributes
                              (fn [attrs]
                                (assoc attrs
                                       'a_texture_matrix {:data a_texture :type (gl game FLOAT) :divisor 1}
                                       'a_scale_matrix {:data a_scaling :type (gl game FLOAT) :divisor 1}
                                       'a_translate_matrix {:data a_translate :type (gl game FLOAT) :divisor 1}
                                       'a_color {:data a_color :type (gl game FLOAT) :divisor 1}))))]
          ;; #?(:clj (do (throw (Exception. "hmm2"))))
          res)))))

(defn dissoc-char
  ([text-entity index]
   (dissoc-char text-entity 0 index))
  ([{:keys [characters] :as text-entity} line-num index]
   (let [line (nth characters line-num)
         prev-lines (subvec characters 0 line-num)
         prev-count (reduce + 0 (map count prev-lines))
         v1 (subvec line 0 index)
         v2 (subvec line (inc index))
         line (into (into [] v1) v2)
         next-char (get line index)]
     (-> text-entity
         (assoc-in [:characters line-num] line)
         (i/dissoc (+ index prev-count))
         (cond-> next-char
           (assoc-char line-num index next-char))))))

