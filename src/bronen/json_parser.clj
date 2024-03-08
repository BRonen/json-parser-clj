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

(declare parse)

(defn parse-number [value] (Integer/parseInt (apply str value)))

(defn parse-string [value] (apply str value))

(defn parse-object
  [tokens]
  (let [assignment (take-while #(not (or (= (:token %) "comma") (= (:token %) "rbraces"))) tokens)]
    (if (> (count assignment) 2)
      (conj {(apply str (:value (first assignment)))
               (parse (drop 2 assignment))}
              (parse-object (drop (+ (count assignment) 1) tokens)))
      {})))

(defn parse
  "Parses json tokens into a valid clojure structure."
  [jsontokens]
  (let [[{token :token value :value} & tokens] jsontokens]
    (if (= token "lbraces")
      (parse-object tokens)
      (if (= token "numeral")
        (parse-number value)
        (if (= token "quotes")
          (parse-string (:value (first tokens)))
          "not implemented")))))

(defn -main
  [& args]
  (println (-> (lexer (:input (first args))) parse)))
