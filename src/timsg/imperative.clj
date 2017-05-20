(ns timsg.imperative)

(defmacro set-with! [obj [sym & props] & body]
  `(let [obj# ~obj
         ~sym (.. obj# ~@props)]
     (set! (.. obj# ~@props) (do ~@body))))

(defmacro sets! [obj & field-vals]
  (let [objsym (gensym "obj_")]
    `(let [~objsym ~obj]
       ~@(for [[field val] (partition 2 field-vals)]
           `(set! (. ~objsym ~field) ~val))
       ~objsym)))


