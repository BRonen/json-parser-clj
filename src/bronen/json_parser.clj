(ns bronen.json-parser
  (:gen-class)
  (:require [clojure.string :refer [starts-with?]]))

(def characters-checkings-map
  {\{ (fn [] {:token "lbraces"})
   \} (fn [] {:token "rbraces"})
   \, (fn [] {:token "comma"})
   \: (fn [] {:token "colon"})})

(defn lexer-literals
  "Takes strings contents as a token"
  [jsonstring]
  (let [value (take-while #(not (= % \")) (vec jsonstring))]
    {:token "literal" :value value}))

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
            (if (= \" (first jsonstring))
              (let [literal (lexer-literals (rest jsonstring))
                    charslength (+ (count (:value literal)) 2)
                    r (drop charslength jsonstring)]
                (conj (lexer r) literal))
              (if (starts-with? (apply str jsonstring) "true")
                '({:token "boolean" :value true})
                (if (starts-with? (apply str jsonstring) "false")
                  '({:token "boolean" :value false})
                  '({:token "err"}))))))))
    nil))

(declare parse)

(defn parse-number [value] (Integer/parseInt (apply str value)))

(defn parse-literal [value] (apply str value))

(defn parse-object
  [tokens]
  (let [assignment (take-while #(not (or (= (:token %) "comma") (= (:token %) "rbraces"))) tokens)
        assignmentlength (count assignment)]
    (if (> assignmentlength 2)
      (let [key (apply str (:value (first assignment)))
            value (parse (drop 2 assignment))]
        (conj {key value}
              (let [r (drop assignmentlength tokens)]
                (if (= (first r)
                       {:token "comma"})
                  (parse-object (rest r))
                  (parse-object r)))))
      {})))

(defn parse
  "Parses json tokens into a valid clojure structure."
  [tokens]
  (let [{token :token value :value} (first tokens)] 
    (if (= token "lbraces")
      (parse-object (rest tokens))
      (if (= token "numeral")
        (parse-number value)
        (if (= token "literal")
          (parse-literal value)
          value)))))

(defn -main
  [& args]
  (-> args first :input lexer parse))
