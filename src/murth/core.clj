(ns murth.core
  (:require  [clojure.math.numeric-tower :as math])
  (:require  [instaparse.core :as insta])
  (:use [clojure.core.match :only [match]])
  (:require  [clojure.edn :as edn])
  (:require  [clojure.string :as str])
  (:gen-class))



; https://github.com/clojure/core.match
; https://github.com/richhickey/harmonikit
; http://rosettacode.org/wiki/Category:J
; http://www.ennex.com/~fabbers/StL.asp

; TODO:
; nth -> get
; remove comments
; let?
; Don't allow combinations of { and end, etc.
; I think expo should be moved forward



(def murth-parser
  (insta/parser (slurp "./murth_grammar.txt")))



(declare murth-eval-s)


(defn process-identifier [acc xp]
  (let [locals (:_locals acc)
        ident (keyword (nth xp 1))]
    (get locals ident (get acc ident ident))))


(defn process-let [acc xp]
  (assoc acc (process-identifier acc (nth xp 1)) (murth-eval-s acc (nth xp 2))))


(defn bin-op [acc xp op]
  (op (murth-eval-s acc (list (nth xp 1)))
      (murth-eval-s acc (list (nth xp 2)))))


(defn process-if [acc xp]
  (let [ifxp (murth-eval-s acc (nth xp 1))
        thenxp (get xp 2)
        elsexp (get xp 3)]
    (if ifxp
      (murth-eval-s acc (rest thenxp))
      (if elsexp
        (if (= :elsif-expr (first elsexp))
          (murth-eval-s acc elsexp)
          (murth-eval-s acc (rest elsexp)))
        nil))))


(defn process-fn-assn [acc xp]
  (let [fname (keyword (get (get xp 1) 1))
        fdef (murth-eval-s acc (get xp 2))]
    (assoc acc fname fdef)))


(defn process-fn-def [acc xp]
  (let [params (murth-eval-s acc (get xp 1))
        body (get xp 2)]
    {:type :function
     :params params
     :body body}))


(defn rt-print [arg]
  (print (str arg)))


(defn rt-join [params]
  (let [sep (if (= 2 (count params))
              (first params)
              nil)
        ls (last params)]
    (str/join sep ls)))


(defn process-cust-fn-call [acc xp]
  (let [fname (keyword (get (get xp 1) 1))
  ;(let [fname (murth-eval-s acc (get xp 1))
        params (murth-eval-s acc (get xp 2))
        fdef (get acc fname)
        fparams (get fdef :params)
        fbody (get fdef :body)
        pparams (rest (get xp 2))
        facc (assoc acc :_locals (into {} (map (fn [f p] (vector f (murth-eval-s acc p))) fparams pparams)))]
    (murth-eval-s facc (rest fbody))))


(defn process-fn-call [acc xp]
  (let [fname (keyword (get (get xp 1) 1))
        params (murth-eval-s acc (get xp 2))]
    (match [fname]
           [:join] (rt-join params)
           :else (process-cust-fn-call acc xp))))


(defn bind-vars [acc f p]
  (into acc (map vector f p)))


(defn process-range [acc xp]
  (let [r1 (murth-eval-s acc (nth xp 1))
        r2 (murth-eval-s acc (nth xp 2))
        stepxp (get xp 3)
        step (if stepxp
               (murth-eval-s acc (nth stepxp 1))
               1)]
    (range r1 r2 step)))


(defn p-c-1 [acc [v1 & vs] [r1 & rs] xp & {:keys [ac] :or {ac nil}}]
  (for [x r1
        :let [k v1
              lacc (assoc acc k x)]]
    (murth-eval-s lacc xp)))


(defn p-c-2 [acc [v1 & vs] [r1 & rs] xp]
  (for [x r1
        y (nth rs 0)
        :let [lacc (assoc acc v1 x (nth vs 0) y)]]
    (murth-eval-s lacc xp)))


(defn p-c-3 [acc [v1 & vs] [r1 & rs] xp & {:keys [ac] :or {ac nil}}]
  (for [x r1
        y (nth rs 0)
        z (nth rs 1)
        :let [lacc (assoc acc v1 x
                              (nth vs 0) y
                              (nth vs 1) z)]]
    (murth-eval-s lacc xp)))


(defn proc-comp-1 [acc vs rs xp]
  (flatten (p-c-1 acc vs rs xp)))


(defn proc-comp-2 [acc vs rs xp]
  (flatten (p-c-2 acc vs rs xp)))


(defn proc-comp-3 [acc vs rs xp]
  (flatten (p-c-3 acc vs rs xp)))


(defn process-comprehension [acc xp]
  (let [exp (nth xp 1)
        gens (rest (rest xp))
        vs (map #(murth-eval-s acc (fnext %)) gens)
        rs (map #(murth-eval-s acc (last %)) gens)
        dim (count vs)]
    (cond
     (= dim 1) (proc-comp-1 acc vs rs exp)
     (= dim 2) (proc-comp-2 acc vs rs exp)
     (= dim 3) (proc-comp-3 acc vs rs exp)
     :else "error")))


(defn process-dict-keyval [acc xp]
  [(murth-eval-s acc (nth xp 1)) (murth-eval-s acc (nth xp 2))])


(defn process-dictionary [acc xp]
  (reduce #(assoc %1 (first %2) (last %2)) {}
          (vec (map (partial murth-eval-s acc) (rest xp)))))


(defn process-dict-elem [acc xp]
  (let [dict       (murth-eval-s acc (nth xp 1))
        elem-id    (nth xp 2)
        elem-t     (if (= :fn-call-expr (first elem-id))
                     (let [fname    (murth-eval-s acc (nth elem-id 1))
                           pparams  (rest (nth elem-id 2))
                           fdef     (get dict fname)
                           fbody    (get fdef :body)
                           fparams  (get fdef :params)
                           facc (assoc acc :_locals (into {} (map (fn [f p] (vector f (murth-eval-s acc p))) fparams pparams)))]
                       (murth-eval-s facc (rest fbody)))
                     (get dict (murth-eval-s acc elem-id)))]
    elem-t))


(defn process-and [acc xp]
  (and
   (murth-eval-s acc (get xp 1))
   (murth-eval-s acc (get xp 2))))


(defn process-or [acc xp]
  (or
   (murth-eval-s acc (get xp 1))
   (murth-eval-s acc (get xp 2))))


(defn murth-red [acc xp]
    (match [(first xp)]
           [:number] (edn/read-string (nth xp 1))
           [:identifier] (process-identifier acc xp)
           [:add]   (bin-op acc xp +)
           [:sub]   (bin-op acc xp -)
           [:mul]   (bin-op acc xp *)
           [:div]   (bin-op acc xp /)
           [:expo]  (bin-op acc xp math/expt)
           [:mod]   (bin-op acc xp mod)
           [:let-expr] (process-let acc xp)
           [:eq-expr]  (bin-op acc xp =)
           [:lt-expr]  (bin-op acc xp <)
           [:gt-expr]  (bin-op acc xp >)
           [:lte-expr]  (bin-op acc xp <=)
           [:gte-expr]  (bin-op acc xp >=)
           [:param-list]  (vec (map (partial murth-eval-s acc) (rest xp)))
           [:array]       (vec (map (partial murth-eval-s acc) (rest xp)))
           [:dictionary]  (process-dictionary acc xp)
           [:dict-keyval] (process-dict-keyval acc xp)
           [:dict-elem]   (process-dict-elem acc xp)
           [:string]      (str (nth (nth xp 1) 1))
           [:range]       (process-range acc xp)
           [:comprehension] (process-comprehension acc xp)
           [:str-concat] (str (murth-eval-s acc (get xp 1)) (murth-eval-s acc (get xp 2)))
           [:if-expr]    (process-if acc xp)
           [:elsif-expr] (process-if acc xp)
           [:and-expr] (process-and acc xp)
           [:or-expr]  (process-or acc xp)
           [:fn-assn-expr] (process-fn-assn acc xp)
           [:fn-def-expr] (process-fn-def acc xp)
           [:fn-call-expr] (process-fn-call acc xp)
           [:comment] acc
           [:boolean] (if (= (nth xp 1) "true") true false)
           [nil] nil
           :else (throw (Exception. (str "Parse error in murth-red: " xp)))))


(defn murth-eval-s [vm0 pexpr]
  (if (vector? (first pexpr))
    (reduce
      murth-red
      vm0
      pexpr)
    (murth-red vm0 pexpr)))


(defn murth-eval [expr & {:keys [parser vm] :or {parser murth-parser, vm {}}}]
  (let [pexpr (parser expr)
        ret (murth-eval-s vm pexpr)]
    ret))


(defn get-inp []
    (print "> ")
    (flush)
    (read-line))


(defn murth-repl []
  (loop [n 0
         inp (do (get-inp))]
      (do
        (flush)
        (println (str (murth-eval inp)))
        (flush)
        (recur (+ n 1) (get-inp)))))


(defn disp-help []
  (println "Options:
         -h, --help          Display this help.
         -i, --repl          Start a repl.
         -f, --file <file>   Run source file <file>."))


(defn process-args [args]
  (cond (or (= (first args) "-h")
            (= (first args) "--help"))
        (disp-help)
        (or (= (first args) "-i")
            (= (first args) "--repl"))
        (murth-repl)
        (or (= (first args) "-f")
            (= (first args) "--file"))
        (println (murth-eval (slurp (nth args 1))))
        :else
        (disp-help)))


(defn -main
  "Start a repl or process a file"
  [& args]
  (println (str "Args: " args))
  (process-args args))


