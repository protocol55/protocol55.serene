(ns protocol55.serene.compiler.transducers
  (:require [paren.serene.compiler :as compiler]
            [paren.serene :as serene]
            [paren.serene.schema :as schema]
            [clojure.spec.alpha :as s]))

(defn keys-spec? [form]
  (and (sequential? form)
       (= 'clojure.spec.alpha/keys (first form))))

;; Takes a function (or map) that will receive a schema spec key for a
;; keys spec form and returns required keys.
;; Returns a transducer that moves required keys to :req-un.
(defmethod compiler/transducer :required [_ required-fn]
  (map (fn [{::compiler/keys [schema-key form] :as spec}]
         (if (and (keys-spec? form)
                  (required-fn schema-key))
           (let [reqs (into #{} (map name) (required-fn schema-key))
                 required? #(reqs (name %))
                 [_ & args] form
                 {:keys [opt-un req-un]} (apply hash-map args)
                 opt-un' (remove required? opt-un)
                 req-un' (into (set req-un) (filter required? opt-un))
                 form' `(s/keys :opt-un ~(vec opt-un') :req-un ~(vec req-un'))]
             (assoc spec ::compiler/form form'))
           spec))))
