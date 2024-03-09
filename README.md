# bronen/json-parser

A simple json parser that receives a string and parses to a clojure map

## Usage

Run the project, overriding the input to be parsed:

    $ clj -Xrun-x :input '"{ \"wasd\": 123 }"'
    {wasd 123}

Run the project's tests (not implemented yet):

    $ clj -T:build test

## Examples


    $ clj -Xrun-x :input '"{ \"userid\": 1 }"'
    {userid 1}

    $ clj -Xrun-x :input '"{ \"foo\": \"bar\", \"nested\": { \"foo\": \"bar\" } }"'
    {foo bar, nested {foo bar}}

