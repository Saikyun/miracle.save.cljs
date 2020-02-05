(ns miracle.save
  (:require [clojure.pprint :refer [pp pprint]]
            [clojure.string :as str]
            [cljs.analyzer.api :as cljs-api]
            [clojure.core.protocols :as p])
  (:require-macros [miracle.save :refer [save watch]]))

(defonce 
  ^{:doc "Used to keep track of individual saves."}
  id (atom 0))

(defn gen-id
  "Simple incremental id."
  []
  (swap! id inc))

(def ^:dynamic *max-saves* "The maximum number of saves per id." 100)

(defn inspect-map
  "Helper function to pprint big maps without breaking the repl."
  [map-to-print & {:keys [desired-level safe-count]
                   :or {desired-level 4 safe-count 10}}]
  (binding [*print-level* desired-level *print-length* safe-count]
    (clojure.pprint/pprint map-to-print)))

(defn gensym? 
  [s]
  (re-find #"__\d+" s))

(defonce saves (atom {}))
(defonce context (atom nil))

(defn set-context!
  "Sets the miracle.save/context, which is used in `eval-in-context`."
  [{:keys [key id] :as ki}]
  (save :wate)
  (let [data (-> (filter #(= id (first %))
                         (get @saves key))
                 first 
                 second)]
    (reset! context (assoc ki :data data))))

(defn clear-saves! [] (reset! saves {}))

(defn eval-in-context
  "Evals the form with the local bindings that are in `context`."
  ([form]
   (if-let [ks (keys (::args (:data @miracle.save/context)))]
     `(let [{:syms [~@ks]} (::args (:data @miracle.save/context))] 
        ~form)
     `~form)))

(defn watch-ns-form
  "Generates code that generates code that watches all functions in the ns `ns-sym`."
  [ns-sym]
  `(->> (keys (ns-interns '~ns-sym))
        (map #(symbol ~(str ns-sym) %))
        (map #(list 'miracle.save/watch %))
        (cons 'do)))

(defn eval-in-context-by-id
  "Evals the form with the local bindings that have been saved using `save` with `id` as parameter."
  ([form id]
   (eval-in-context-by-id
    form 
    id 
    (-> @miracle.save/saves (get id) last first)))
  ([form id pos]
   (let [locals (second
                 (first
                  (filter #(= (first %) pos)
                          (-> @miracle.save/saves (get id)))))
         ks (keys locals)]
     `(let [~'all (second (first (filter #(= (first %) ~pos) (-> @miracle.save/saves (get ~id)))))
            ~@(apply concat (for [k ks] [k `(get ~'all '~k)]))]
        ~form))))

(defn get-last
  [id]
  (let [res (-> @saves (get id) last)]
    [(first res) (with-out-str (inspect-map (second res)))]))

(defn get-last-nof
  [id nof]
  (for [res (reverse (take nof (-> @saves (get id))))]
    [(first res) (pr-str (subs (with-out-str (inspect-map (second res))) 0 250))]))

#_(defn ld
    "Loads the local bindings that have been saved using `save` with `id` as parameter."
    ([id] (ld id nil))
    ([id pos]
     (let [locals (second (if pos
                            (first (filter #(= (key %) pos) (get @saves id)))
                            (last (get @saves id))))]
       (when locals
         (println "Defining:")
         (inspect-map locals))
       (doseq [[sym val] locals]
         (try
           (eval `(def ~(symbol sym) '~val))
           (catch js/Error e (prn sym val) (throw e)))))))

(defn print-saves
  [id]
  (let [locals (take 10 (get @saves id))]
    (doseq [i (reverse (range (count locals)))
            :let [[k v] (first (drop i locals))]]
      (println "Entry id." k)
      (inspect-map v)
      (prn))))



(defonce f-saves (atom {}))  

(defn which-args
  "Takes a list of arg-names, e.g. ([x] [x y] [x y {:keys [a b]}])
  and a number.
  Returns the arg-names that matches n-args."
  [arg-names-list n-args]
  (first
   (filter 
    some?
    (for [arg-names arg-names-list]
      (if (and (some #{'&} arg-names)
               (>= n-args (count (take-while #(not= % '&) arg-names))))
        arg-names
        (when (= n-args (count arg-names))
          arg-names))))))  

(defonce watched-vars (atom {}))

(defn add
  ([x]
   (add x 0))
  ([x y]
   (+ x y))
  ([x y & args]
   (+ x y (apply add args))))  

(defn new*
  [id bindings]
  (swap! miracle.save/saves
         update
         id
         #(into [] (take-last *max-saves* (conj % [(gen-id) bindings]))))
  :ok)

(defn wrapper-fn
  [var f]
  (let [arglists (get @watched-vars var)]
    (if-not arglists
      (throw (js/Error. (str "No meta-data stored for var " var)))
      (fn [& args]
        (let [arglist (first (filter #(= (count %) (count args))
                                     (map #(into [] (filter (partial not= '&) %)) arglists)))
              arglist (if-let [al (and (not arglist)
                                       (first (filter #(some (partial = '&) %) arglists)))]
                        (if (>= (count args) (count al)) al {})
                        arglist)
              [named-args rest-args] (split-with #(not= % '&) arglist)
              _ (println "arglist" arglist "named args" named-args "rest args" rest-args)
              args-vals (into {}
                              (map-indexed 
                               (fn [i a] [a (nth args i)])
                               named-args))
              args-vals (if (seq rest-args)
                          (assoc args-vals (second rest-args) (drop (count named-args) args))
                          args-vals)]
          (try
            (let [res (apply f args)]
              (new* var {::ret res, ::args args-vals, ::arglist arglist})
              res)
            (catch js/Error e
              (new* var {::error e, ::args args-vals, ::arglist arglist})
              (throw e))))))))

(extend-type js/Error
  IPrintWithWriter
  (-pr-writer [err writer opts]
    (.-prototype js/Error)
    (-write writer "#Error[")
    (-write writer (pr-str {:message (.-message err)
                            :name (.-name err)
                            :file-name (.-fileName err)
                            :line-number (.-lineNumber err)
                            :stack (.-stack err)
                            :as-str (.toString err)}))
    (-write writer "]")))

(comment
  ;; Example usage of basic `save`
  (defn add [x y] (save :a) (+ x y))
  (add 5 10)
  (ld :a)
  x ;; 5
  y ;; 10
  
  (add 20 30)
  (add 7 13)
  
  (print-saves :a))



(comment
  ;; Example usage of `save-var*`
  (do
    (defn yo ([x] x) ([x {:keys [a b]}] (+ x a b)) ([x y & rest] (apply + x y rest)))
    (def wat (partial yo 5))  
    (save-var* 'yo)  
    (save-var* 'wat)  
    (reset! f-saves {})
    (yo 5)
    (wat 5 10)
    (yo 5 {:a 20 :b 30})
    (yo 5 30 40)
    (yo 5 30 40 50)
    @f-saves)
  
  (unsave-var* 'yo))

