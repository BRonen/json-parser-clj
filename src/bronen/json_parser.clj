(ns bronen.json-parser
  (:gen-class))

(def characters-checkings-map
  {\{ (fn [] {:token "lbraces"})
   \} (fn [] {:token "rbraces"})
   \, (fn [] {:token "comma"})
   \: (fn [] {:token "colon"})
   \" (fn [] {:token "quotes"})})

(defn lexer-literals
  "Takes strings contents as a token"
  [jsonstring]
  {:token "literal" :value (take-while #(re-matches #"[A-Za-z]" (str %)) (vec jsonstring))})

(defn lexer-numerals
  "Takes entire literals like strings contents or numbers from characters"
  [jsonstring]
  {:token "numeral" :value (take-while #(re-matches #"\d" (str %)) (vec jsonstring))})

(defn lexer
  "Breaks a json string into a vector of tokens"
  [jsonstring]
  (if (> (count jsonstring) 0)
    (if (= (first jsonstring) \space)
      (lexer (rest jsonstring))
      (let [mappedchar (get characters-checkings-map (first jsonstring))]
         (if mappedchar
           (conj (lexer (rest jsonstring)) (mappedchar))
           (if (re-matches #"\d" (str (first jsonstring)))
             (let [numeral (lexer-numerals jsonstring)]
               (conj (lexer (drop (count (:value numeral)) jsonstring)) numeral))
             (if (re-matches #"[A-Za-z]" (str (first jsonstring)))
               (let [literal (lexer-literals jsonstring)]
                 (conj (lexer (drop (count (:value literal)) jsonstring)) literal))
               {:token "err" :value jsonstring})))))
    nil))

(defn parse
  "Parses json tokens into a valid clojure structure."
  [jsontokens]
  (println jsontokens))

(defn -main
  [& args]
  (-> (lexer (:input (first args))) parse))
