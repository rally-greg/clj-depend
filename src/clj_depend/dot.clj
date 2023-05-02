(ns clj-depend.dot
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as str]))

; function naming comes from grammar at https://graphviz.org/doc/info/lang.html

(def ^:dynamic indent-level 0)

(defn- indent
  ([]
   (indent indent-level "  "))
  ([n spacer]
   (str/join (repeat n spacer))))

(defn- quote-str "Surround `s` with double quotes." [s]
  (str "\"" s "\""))

(def ^:private string-regex (re-pattern #"[_a-zA-Z\\200-\\377][_0-9a-zA-Z\\200-\\377]*"))

(def ^:private numeral-regex (re-pattern #"[-]?((\.[0-9]+)|([0-9]+(\.[0-9]*)?))"))

(def ^:private quoted-regex (re-pattern #"\"(.*)\""))

(defn- ID? [s]
  (boolean (or (re-matches string-regex s)
               (re-matches numeral-regex s)
               (re-matches quoted-regex s))))

(defn- v->ID "turn `v` into a DOT ID" [v]
  (let [s (if (keyword? v) (name v) (str v))]
    (if (ID? s) s (quote-str s))))

(defn- print-stmt [stmt]
  (println (str (indent) stmt ";")))

(defn- print-a [[k v]]
  (print-stmt (str (name k) "=" (v->ID v))))

(defn- print-a-list [attr-map]
  (doall (map #(print-a %) attr-map)))

(defn- attr-list [node-name attr-map]
  (let [assignments  (map (fn [[k v]] (str (name k) "=" (v->ID v))) attr-map)
        align-indent (indent (+ 2 (count node-name)) " ")
        separator    (str ",\n" (indent) align-indent)]
    (str "[" (str/join separator assignments) "]")))

(defn- print-node
  ([{:keys [name attr-map]}]
   (print-node name attr-map))
  ([name attr-map]
   (print-stmt (str name " " (attr-list name attr-map)))))

; an attr_stmt looks just like a node_stmt,
;  but with 3 allowed values for name: :graph | :node | :edge
(defn- print-attr [which attr-map]
  (print-node (name which) attr-map))

(defn- print-subgraph [{:keys [name label node-names]}]
  (println "  subgraph" name "{")
  (with-redefs [indent-level 2]
    (print-a-list {:label   label
                   :bgcolor :lightgray})
    (doseq [name node-names]
      (print-stmt name)))
  (println "  }"))

(defn- print-edge [[from to]]
  (println (str "  " from " -> " to ";")))

(defn- print-graph
  [nodes subgraphs edges]
  (println "digraph G {")
  (with-redefs [indent-level 1]
    (print-attr :node {:fontname  :monospace
                       :style     :filled
                       :fillcolor :white})
    (println)
    (doall (map print-node nodes))
    (println)
    (doall (map print-subgraph subgraphs))
    (println)
    (doall (map print-edge edges)))
  (println "}"))

(defn dot [config namespaces deps-by-namespace layer-by-namespace]
  ;; (pprint {:layers (:layers config)})
  (let [ignore    (into #{} (:ignore config))
        ;; _ (pprint {:ignore ignore})
        namespaces (filter #(not (contains? ignore (str %))) namespaces)
        ;; _ (pprint {:filtered-namespaces namespaces})
        layers    (keys (:layers config))
        ns_set    (into #{} namespaces)
        ;; _ (pprint {:ns_set ns_set})
        ->deps    (fn [ns] (filter #(contains? ns_set %) (deps-by-namespace ns)))
        ns-deps   (reduce #(assoc %1 %2 (->deps %2)) {} namespaces)
        ;; _ (pprint {:ns-deps ns-deps})
        ns-layers (reduce #(assoc %1 %2 (layer-by-namespace config %2)) {} namespaces)
        layer-nss (reduce (fn [m [ns layer]]
                            (if layer (update m layer #(conj % ns)) m))
                          (into {} (map #(vector % []) layers)) ns-layers)
        ->node    (fn [ns n] [ns {:name (str "ns" n)
                                  :attr-map {:label ns}}])
        ns-nodes  (into {} (map ->node namespaces (range)))
        node-name (fn [ns] (:name (get ns-nodes ns)))
        nodes     (map #(get ns-nodes %) namespaces)
        subgraphs (map (fn [layer n]
                         {:name  (str "cluster_" n)
                          :label layer
                          :node-names (sort (map #(node-name %) (get layer-nss layer)))})
                       layers (range))
        edges     (mapcat (fn [[ns deps]]
                            (map #(vector (node-name ns) (node-name %)) deps))
                          (map #(vector % (get ns-deps %)) namespaces))]
    (print-graph nodes subgraphs edges)))
