(ns miracle.save
  (:require [cljs.analyzer :as cljs]))

(defmacro var-data
  [sym]
  (cljs/resolve-var &env sym))

(defn store-var-data-f
  "Helper function that stores the `:arglists` of the metadata of a var."
  [sym arglists]
  `(swap! miracle.save/watched-vars assoc '~sym ~arglists))

(defmacro store-var-data
  [sym]
  "Fetches `:arglists` from the metadata of a var."
  `(do
     ~(store-var-data-f sym (:arglists (:meta (cljs/resolve-var &env sym))))
     :ok))

(defmacro watch
  [sym]
  "Same as `store-var-data`, but also wraps a function with `wrapper-fn`, which instruments it."
  `(do
     ~(store-var-data-f sym (:arglists (:meta (cljs/resolve-var &env sym))))
     (set! ~sym (miracle.save/wrapper-fn '~sym ~sym))))

(defmacro get-env
  "Returns the locally defined variables and their values."
  []
  (into {} (for [k (keys (:locals &env))]
             [`'~k k])))  

(defmacro save
  "Used to save all local bindings, takes an identifier as a parameter.
  The identifier is used with `ld` in order to load the local bindings where `save` was called."
  [key]
  `(do (swap! miracle.save/saves
              update
              ~key
              #(into []
                     (take-last 
                      *max-saves*
                      (conj %
                            [(miracle.save/gen-id)
                             (into {} (list ~@(let [ks (keys (:locals &env))]
                                                (for [k ks]
                                                  `['~k ~k]))))]))))
       :ok))

(defmacro save-do
  "Same as `save`, but also acts as `do` on the `body`.
  Useful in e.g. `->` forms."
  [key & body]
  `(let [~'_ (swap! miracle.save/saves
                    update
                    ~key
                    #(into []
                           (take-last 
                            *max-saves*
                            (conj %
                                  [(miracle.save/gen-id)
                                   (into {} (list ~@(let [ks (keys (:locals &env))]
                                                      (for [k ks]
                                                        `['~k ~k]))))]))))]
     ~@body))
