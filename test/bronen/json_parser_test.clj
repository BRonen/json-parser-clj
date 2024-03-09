(ns bronen.json-parser-test
  (:require [clojure.test :refer [deftest testing is]]
            [bronen.json-parser :refer [parse lexer]]))

(deftest lexer-test
  (testing "lexing object"
    (let [in "{}"
          out [{:token "lbraces"} {:token "rbraces"}]]
      (is (= (lexer in) out))))
  (testing "lexing a number value"
    (let [in "123132"
          out [{:token "numeral", :value '(\1 \2 \3 \1 \3 \2)}]]
      (is (= (lexer in) out))))
  (testing "lexing booleans"
    (let [in "true"
          out [{:token "boolean" :value true}]]
      (is (= (lexer in) out)))
    (let [in "false"
          out [{:token "boolean" :value false}]]
      (is (= (lexer in) out))))
  (testing "lexing a string"
    (let [in "\"hello world\""
          out [{:token "literal" :value '(\h \e \l \l \o \space \w \o \r \l \d)}]]
      (is (= (lexer in) out)))))

(deftest parser-test
  (testing "parsing a string"
    (let [in "\"hello world\""
          out "hello world"]
      (is (= (-> in lexer parse) out))))
  (testing "parsing a numeral"
    (let [in "12321"
          out 12321]
      (is (= (-> in lexer parse) out))))
  (testing "parsing booleans"
    (let [in "true"
          out true]
      (is (= (-> in lexer parse) out)))
    (let [in "false"
          out false]
      (is (= (-> in lexer parse) out))))
  (testing "parsing a object"
    (let [in "{ \"wasd\": 123 }"
          out {"wasd" 123}]
      (is (= (-> in lexer parse) out))))
  (testing "parsing a nested object"
    (let [in "{ \"foo\": \"bar\", \"bools\": { \"t\": true } }"
          out {"foo" "bar", "bools" {"t" true}}]
      (is (= (-> in lexer parse) out)))))
