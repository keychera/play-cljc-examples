(ns ui-gallery.core
  (:require [clojure.string :as str]
            [ui-gallery.utils :as utils]
            [ui-gallery.chars :as chars]
            [play-cljc.gl.core :as c]
            [play-cljc.transforms :as t]
            [play-cljc.gl.text :as text]
            [play-cljc.instances :as i]
            #?(:clj  [play-cljc.macros-java :refer [gl math]]
               :cljs [play-cljc.macros-js :refer-macros [gl math]])
            #?(:clj [ui-gallery.text :refer [load-font-clj]]))
  #?(:cljs (:require-macros [ui-gallery.text :refer [load-font-cljs]])))

(defonce *state (atom {:mouse-x 0
                       :mouse-y 0
                       :pressed-keys #{}
                       :counter 0}))

(defn init [game]
  ;; allow transparency in images
  (gl game enable (gl game BLEND))
  (gl game blendFunc (gl game SRC_ALPHA) (gl game ONE_MINUS_SRC_ALPHA))
  ;; load font
  (#?(:clj load-font-clj :cljs load-font-cljs) :roboto
     (fn [{:keys [data]} baked-font]
       (let [font-entity (text/->font-entity game data baked-font)
             compiled-font-entity (c/compile game font-entity)
             ;; an entity whose text can't change
             static-entity (c/compile game (text/->text-entity game compiled-font-entity "Hello, world!"))
             ;; an entity whose text can be set dynamically
             dynamic-entity (c/compile game (i/->instanced-entity font-entity))]
         (swap! *state assoc
                :font-entity font-entity
                :static-entity static-entity
                :dynamic-entity dynamic-entity)))))

(def screen-entity
  {:viewport {:x 0 :y 0 :width 0 :height 0}
   :clear {:color [(/ 173 255) (/ 216 255) (/ 230 255) 1] :depth 1}})

(defonce fps-counter*
  (volatile! {:last-time #?(:clj (System/currentTimeMillis) :cljs (js/performance.now)) :frames 0 :fps 0}))

(defn update-fps! []
  (let [now #?(:clj (System/currentTimeMillis) :cljs (js/performance.now))
        {:keys [last-time frames]} @fps-counter*
        delta (- now last-time)]
    (if (> delta 1000) ;; 1 second has passed
      (vswap! fps-counter* assoc :last-time now :frames 0 :fps frames)
      (vswap! fps-counter* update :frames inc))))

(defn tick [game]
  (let [state (swap! *state update :counter inc)
        {:keys [font-entity
                static-entity
                dynamic-entity
                counter]} state
        [game-width game-height] (utils/get-size game)]
    (when (and (pos? game-width) (pos? game-height))
      ;; render the blue background
      (c/render game (update screen-entity :viewport
                             assoc :width game-width :height game-height))
      (when (and static-entity dynamic-entity)
        ;; render the static text
        (c/render game (-> static-entity
                           (t/project game-width game-height)
                           (t/scale (:width static-entity) (:height static-entity))
                           (t/translate 0 0)))
        ;; render the colored text
        (update-fps!)
        (c/render game (-> (reduce-kv
                             chars/assoc-char
                             dynamic-entity
                             (mapv (fn [ch color]
                                     (-> font-entity
                                         (chars/crop-char ch)
                                         (t/color color)))
                               "Colors"
                               (cycle
                                 [[1 0 0 1]
                                  [0 1 0 1]
                                  [0 0 1 1]])))
                           (t/project game-width game-height)
                           (t/translate 0 100)))
        ;; render the frame count
        (let [num-of-text 200
              fps (:fps @fps-counter*)
              text ["Frame count:" 
                    (str/join " " (take num-of-text (repeat (str counter))))
                    "Frame time"
                    (str (* (:delta-time game) 1000) " ms")
                    "FPS" 
                    (str/join " " (take num-of-text (repeat (str fps)))) ]
              text (conj text "char count" (str (reduce + (map count text))))]
          (c/render game (-> #_(reduce
                              (partial apply chars/assoc-char)
                              dynamic-entity
                              (for [line-num (range (count text))
                                    char-num (range (count (nth text line-num)))
                                    :let [ch (get-in text [line-num char-num])]]
                                [line-num char-num (chars/crop-char font-entity ch)]))
                             (chars/assoc-lines dynamic-entity font-entity text)
                             (t/project game-width game-height)
                             (t/translate 0 200)))))))
  ;; return the game map
  game)

