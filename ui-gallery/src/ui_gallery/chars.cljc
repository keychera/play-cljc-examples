(ns ui-gallery.chars
  (:require [play-cljc.transforms :as t]
            [play-cljc.math :as m]
            [play-cljc.instances :as i]))

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

(defn assoc-lines
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
    (loop [char-i 0 total-xadv 0 prev-line-num -1 entity dynamic-entity]
      (if (< char-i total-ch)
        (let [[line-num ch] (nth i->line+ch char-i)
              baked-ch      (get-baked-char font-entity ch)
              {:keys [x y w h xoff yoff xadv]} baked-ch
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

(defn assoc-lines3
  [dynamic-entity font-entity lines]
  (let [baseline    (-> font-entity :baked-font :baseline)
        font-height (-> font-entity :baked-font :font-height)
        text        (reduce #(str %1 "\n" %2) lines) 
        total-ch    (- (count text) (dec (count lines)))]
    (loop [char-i 0 total-xadv 0 curr-line-num 0 prev-line-num -1 entity dynamic-entity]
      (if (< char-i total-ch)
        (let [ch (get text (+ curr-line-num char-i))] 
          (if (not= "\n" ch)
            (let [baked-ch      (get-baked-char font-entity ch)
                  {:keys [x y w h xoff yoff xadv]} baked-ch
                  y-total     (* curr-line-num font-height)
                  total-xadv  (if (= curr-line-num prev-line-num) total-xadv 0)
                  char-entity (-> font-entity
                                  (t/crop x y w h)
                                  (assoc-in [:uniforms 'u_scale_matrix]
                                            (m/scaling-matrix w h))
                                  (assoc-in [:uniforms 'u_translate_matrix]
                                            (m/translation-matrix (+ xoff total-xadv) (+ baseline yoff y-total))))
                  entity      (i/assoc entity char-i char-entity)]
              (recur (inc char-i) (+ total-xadv xadv) curr-line-num curr-line-num entity))
            (recur char-i total-xadv (inc curr-line-num) prev-line-num entity)))
        entity))))

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

