(ns murth.core-test
  (:use clojure.test
        murth.core)
  (:require [clojure.math.numeric-tower :as math])
  (:require [instaparse.core :as insta]))


(deftest test-whitespace
  (testing "Whitespace"
    (is (nil? (murth-eval "")))
    (is (nil? (murth-eval " ")))
    (is (nil? (murth-eval "           ")))
    (is (nil? (murth-eval "
                         ")))))


(deftest test-integers
  (testing "Recognition of integers"
    (is (=       0 (murth-eval         "0")))
    (is (=       1 (murth-eval         "1")))
    (is (=      -1 (murth-eval        "-1")))
    (is (=      92 (murth-eval        "92")))
    (is (=    -478 (murth-eval      "-478")))
    (is (=    6080 (murth-eval      "6080")))
    (is (= -350173 (murth-eval   "-350173")))
    (is (= 8177245 (murth-eval   "8177245")))))


(deftest test-floats
  (testing "Recognition of floats"
    (is (=       0.0     (murth-eval         "0.0")))
    (is (=       1.0     (murth-eval         "1.0")))
    (is (=       0.2709  (murth-eval         "0.2709")))
    (is (=      -1.0     (murth-eval        "-1.0")))
    (is (=      12.345   (murth-eval        "12.345")))
    (is (=    -575.0907  (murth-eval      "-575.0907")))
    (is (=    2001.002   (murth-eval      "2001.002")))
    (is (= -482126.1639  (murth-eval   "-482126.1639")))
    (is (= 5927338.71244 (murth-eval   "5927338.71244")))))


(deftest test-strings
  (testing "Recognition of strings"
    (is (= "foo"           (murth-eval  "\"foo\"")))
    (is (= ""              (murth-eval  "\"\"")))
    (is (= " "             (murth-eval  "\" \"")))
    (is (= "17"            (murth-eval  "\"17\"")))
    (is (= "8a8"           (murth-eval  "\"8a8\"")))
    (is (= "dance or die"  (murth-eval  "\"dance or die\"")))
    ))


(deftest test-arrays
  (testing "Recognition of arrays"
    (is (= []         (murth-eval  "[]")))
    (is (= [1 2 3]    (murth-eval  "[1,2,3]")))
    (is (= [6]        (murth-eval  "[1+2+3]")))
    (is (= [4, 9, 16] (murth-eval  "[2^2, 3^2, 4^2]")))
    (is (= ["dance"]  (murth-eval  "[\"dance\"]")))
    (is (= ["dance" "or" "die"]  (murth-eval  "[   \"dance\",\"or\",     \"die\" ]")))
    ))


(deftest test-ranges
  (testing "Recognition of ranges"
    (is (= (range 1 10)    (murth-eval "[1..10]")))
    (is (= (range 1)       (murth-eval "[0..1]")))
    (is (= [1 2 3 4]       (murth-eval "[1..5]")))
    (is (= [5 4 3]         (murth-eval "[5..2,-1]")))
    (is (= [1 3 5 7 9]     (murth-eval "[1..10,2]")))
    (is (= (range 8 3 -1)  (murth-eval "[8..3, -1]")))
    ))


(defn- hack-float= [m n]
  (< (- m n) 0.001))


(deftest test-simple-operations
  (testing "Simple binary operations"
    (is (=               0   (murth-eval "0+0")))
    (is (=               1   (murth-eval "0+1")))
    (is (=               1   (murth-eval "1+0")))
    (is (=              10   (murth-eval "2*5")))
    (is (hack-float=    -2.3 (murth-eval "3.4 - 5.7")))
    (is (=              96   (murth-eval "90 + 6")))
    (is (=             723   (murth-eval "3*241")))
    (is (=             169   (murth-eval "-13*-13")))
    (is (=             202   (murth-eval "808 / 4")))
    (is (=             152/3 (murth-eval "456/9")))
    (is (=               9   (murth-eval "3^2")))
    (is (=            3125   (murth-eval "5^5")))
    (is (=               1   (murth-eval "9 % 4")))
    (is (=               0   (murth-eval "100 % 5")))
    ))


(deftest test-compound-operations
  (testing "Compound binary operations"
    (is (= 0              (murth-eval "(0 + 0) + 0")))
    (is (= 5              (murth-eval "2 + (1 + 2)")))
    (is (= 5              (murth-eval "2 + 1 + 2")))
    (is (= 38.5           (murth-eval "10.5+(14*2)")))
    (is (= 38.5           (murth-eval "10.5 + 14*2")))
    (is (= 49.0           (murth-eval "(10.5 + 14)*2")))
    (is (= 343            (murth-eval "(7^2)*7")))
    (is (= 2929           (murth-eval "((12+7)*(8 + 169)) + (31*-14)")))
    (is (= 10637          (murth-eval "((17*3)+(583*18))+92")))
    (is (= 16815125378552 (murth-eval "((870*2)+(45^8))-(727*19)")))
    (is (= 4264.8         (murth-eval "((10.2 + 20.1) * (3 + (7*19))) + (-12^2)")))))


(deftest test-var-names
  (testing "Variable names"
    (is (= :x               (murth-eval "x")))
    (is (= :a1              (murth-eval "a1")))
    (is (= :xyz_123         (murth-eval "xyz_123")))
    (is (= :total_count_100 (murth-eval "total_count_100")))
    (is (= :a_b_c_d         (murth-eval "a_b_c_d")))))


(deftest test-statements
  (testing "Statements"
    (let [r (murth-eval "
                         let a=1+2
                         let b=3+4

                         let c = 12 * 13
                         let d = 43 + 19.4; let e = c
                        ")]
      (is (= 3    (get r :a)))
      (is (= 7    (get r :b)))
      (is (= 156  (get r :c)))
      (is (= 62.4 (get r :d)))
      (is (= 156  (get r :e))))))


(deftest test-let-statements
  (testing "Let variable statements"
    (let [r (murth-eval "
                         let x = 4 - 1

                         let y = x*4

                         let z = (y - x) + 2

                         let w = y^x

                         let x2 = (x+1)*(y+2)
                        ")]
      (is (= 3    (get r :x)))
      (is (= 12   (get r :y)))
      (is (= 11   (get r :z)))
      (is (= 1728 (get r :w)))
      (is (= 56   (get r :x2))))))


(deftest test-conditions
  (testing "Conditions"
    (is (= true  (murth-eval          "0 == 0")))
    (is (= true  (murth-eval          "1 == 1")))
    (is (= false (murth-eval          "3 == 4")))
    (is (= true  (murth-eval          "3 < 4")))
    (is (= false (murth-eval         "12 < 4")))
    (is (= false (murth-eval       "0.05 > 0.904")))
    (is (= true  (murth-eval          "5 > 3")))
    (is (= true  (murth-eval         "10 <= 10")))
    (is (= false (murth-eval        "439 <= -198")))
    (is (= true  (murth-eval "2834770.43 >= 320")))
    (is (= true  (murth-eval    "-12.345 >= -12.345")))
    (is (= true  (murth-eval          "(1 == 1) == (3 == 3)")))
    (is (= true  (murth-eval "4 > 2 and 10 > 2")))
    (is (= true  (murth-eval "1 >= 22 or 5 > 3")))
    (is (= false (murth-eval "12 < 2 and 12 > 4")))
    (is (= false (murth-eval "(84 > 22) and (12 < 2)")))
    (is (= true  (murth-eval "(84 > 22) and (12 < 2 or 12 > 4)")))
    (is (= true  (murth-eval "(84 > 22 and 12 < 2) or 12 > 4")))
    ))


(deftest test-ifs
  (testing "If statements"
    (is (= 2   (murth-eval "if 1 == 1 then 2 else 3 end")))
    (is (= 3   (murth-eval "let x = 2; if x > 5 then 2 else 3 end")))
    (is (= 2   (murth-eval "if 3 > 5 {
                              1
                            } else {
                              2
                            }")))
    ))


(deftest combo-statements
  (testing "Various combinations"
    (let [r (murth-eval "
                         let x = -12.345 >= -12.345
                         let y = 4
                         let z = 54 == 54
                         let q = 12
                         let p = q > y
                         let n = y >= q
                         let a = if 4 > 9 then 7 else 5 end
                         let b = if 7 >= a then q + 1 else y + 1 end
                         # Just a comment.
                         let c = if y > 0.2 then
                           15
                         else
                           16
                         end
                         ")]
      (is (= true   (get r :x)))
      (is (= 4      (get r :y)))
      (is (= true   (get r :z)))
      (is (= true   (get r :p)))
      (is (= false  (get r :n)))
      (is (= 5      (get r :a)))
      (is (= 13     (get r :b)))
      (is (= 15     (get r :c)))
      )))


(deftest test-functions-no-params
  (testing "Functions with no parameters"
    (is (=  6      (murth-eval "add3 = () -> { 1 + 2 + 3 }; add3()")))
    (is (=  6      (murth-eval "add3 = () -> { 1 + 2 + 3 }; add3( )")))
    (is (=  6      (murth-eval "add3 = () -> { 1 + 2 + 3 }; add3(  )")))
    (is (=  6      (murth-eval "add3 = () -> { 1 + 2 + 3 }; add3(   )")))
    (is (= 50      (murth-eval "foo = () -> {
                                              let x = 10*10
                                              let y = 20/10
                                              x/y
                                            }
                                foo()")))
    (is (= true    (murth-eval "baz = () -> {
                                              if 3 > 5 then
                                                false
                                              else
                                                true
                                              end
                                            }; baz()")))
    ))


(deftest test-functions-with-params
  (testing "Functions with with parameters"
    (is (= 21      (murth-eval "fn1 = (a) -> { a + 1 }; fn1(20)")))
    (is (= 21      (murth-eval "fn1 = (a) -> { a + 1 }; fn1( 20 )")))
    (is (= 30      (murth-eval "add2 = (x,y) -> { x + y }; add2(10,20)")))
    (is (= 30      (murth-eval "add2 = (x,y) -> { x + y }; add2(10, 20)")))
    (is (= 30      (murth-eval "add2 = (x,y) -> { x + y }; add2(10,
                                                                20)")))
    (is (= true    (murth-eval "gt10 = (n) -> { n > 10 }; gt10(11)")))
    (is (= false   (murth-eval "lte98 = (n) -> { if n <= 98 then true else false end }
                              lte98(1234)")))
    (is (= 30      (murth-eval "add2 = (x,y) -> {
                                  x + y }
                                let x = 10
                                add2(x,20)")))
     (is (= 30      (murth-eval "add2 = (x,y) -> {
                                   x + y
                                 }
                                 let x = 10
                                 add2(
                                      x,
                                      20
                                     )")))
    ))


(deftest test-builtin-functions
  (testing "Testing built-in functions"
    (is (= "abc"            (murth-eval "join([\"a\", \"b\", \"c\"])")))
    (is (= "a.b.c"          (murth-eval "join(\".\", [\"a\", \"b\", \"c\"])")))
    (is (= "dance,or,die!"  (murth-eval "join(\",\", [\"dance\", \"or\", \"die!\"])")))
    ))


(deftest test-list-comprehensions
  (testing "Testing list comprehensions"
    (is (= '(2 4 6)                   (murth-eval "[x * 2 | x <- [1..4]]")))
    (is (= '(1 4 9 16 25)             (murth-eval "[x^2 | x <- [1..6]]")))
    (is (= '(5 6 7 6 7 8 7 8 9)       (murth-eval "[x + y | x <- [1..4], y <- [4..7]]")))
    (is (= '(9 10 10 11 10 11 11 12)  (murth-eval "[x + y + z| x <- [1..3], y <- [3..5], z <- [5..7]]")))
    (is (= '(0 0 0 0 5 6 7 8 9)       (murth-eval "[ if x > 4 then x else 0 end | x <- [1..10]]")))
  ))


(deftest test-dictionaries
  (testing "Testing dictionaries"
    (is (=  3                  (murth-eval "let d = { x: 1, y: 2, z: 3 }; d.z")))
    (is (=  3                  (murth-eval "let d = { x: 1,
                                                      y: 2,
                                                      z: 3
                                                    }
                                                    d.z
                                                    ")))
    (is (= 10                  (murth-eval "let d = { x: 1, y: 2, z: 3 }; 2 * d.z + 2*d.y")))
    (is (= 123                 (murth-eval "let hsh = { x: 1, fn1: () -> { 123 }, z: 3 }; hsh.fn1();")))
    (is (= 20                  (murth-eval "let hsh = { x: 1, y: (a,b) -> { a*b }, z: 3 }; hsh.y(4, 5)")))
    (is (= "threve"            (murth-eval "let d = { one:   \"o_n_e\",
                                                      two:   2,
                                                      three: \"threve\" }
                                            d.three")))
    (is (= true
           (murth-eval "let h = {
                          one: 1,
                          two: 2,
                          three: 3
                        }
                        h.key?(\"two\")
                        ")))
    (is (= false
           (murth-eval "let h = {
                          one: 1,
                          two: 2,
                          three: 3
                        }
                        h.key?(\"nope\")
                        ")))
  ))


(deftest test-string-ops
  (testing "Testing Various String Operations"
    (is (= "foobar"     (murth-eval "\"foo\" ++ \"bar\"")))
    (is (= "foobarbaz"  (murth-eval "\"foo\" ++ \"bar\" ++ \"baz\"")))
  ))


(deftest test-fizzbuzz
  (testing "Testing FizzBuzz"
    (is (=
         [1 2 "Fizz" 4 "Buzz" "Fizz" 7 8 "Fizz" "Buzz" 11 "Fizz" 13 14 "FizzBuzz" 16 17 "Fizz" 19 "Buzz" "Fizz" 22 23 "Fizz" "Buzz" 26 "Fizz" 28 29 "FizzBuzz" 31 32 "Fizz" 34 "Buzz" "Fizz" 37 38 "Fizz" "Buzz" 41 "Fizz" 43 44 "FizzBuzz" 46 47 "Fizz" 49 "Buzz" "Fizz" 52 53 "Fizz" "Buzz" 56 "Fizz" 58 59 "FizzBuzz" 61 62 "Fizz" 64 "Buzz" "Fizz" 67 68 "Fizz" "Buzz" 71 "Fizz" 73 74 "FizzBuzz" 76 77 "Fizz" 79 "Buzz" "Fizz" 82 83 "Fizz" "Buzz" 86 "Fizz" 88 89 "FizzBuzz" 91 92 "Fizz" 94 "Buzz" "Fizz" 97 98 "Fizz" "Buzz"]
         (murth-eval "[if x % 3 == 0 and x % 5 == 0 {
                         \"FizzBuzz\"
                       } elsif x % 3 == 0 {
                         \"Fizz\"
                       } elsif x % 5 == 0 {
                         \"Buzz\"
                       } else {
                         x
                       }
                       | x <- [1..101]]")))))


(deftest test-fibonacci
  (testing "Testing Fibonacci"
    (is (= 8
           (murth-eval "fib = (x) -> {
                          if (x == 0) or (x == 1) {
                            1
                          } else {
                            fib(x - 1) + fib(x - 2)
                          }
                        }
                        fib(5)")))
    (is (= 34
           (murth-eval "fib = (x) -> {
                          if (x == 0) or (x == 1) {
                            1
                          } else {
                            fib(x - 1) + fib(x - 2)
                          }
                        }
                        fib(8)")))
    ))


(deftest test-repeat-string
  (testing "Testing Repeating a String"
    (is (= ["foo" "foo" "foo" "foo" "foo"]
           (murth-eval "rep = (s, n) -> {
                          [s | x <- [1..n+1]]
                        }
                        rep(\"foo\", 5)")))
    (is (= "foofoofoo"
           (murth-eval "rep = (s, n) -> {
                          join([s | x <- [1..n+1]])
                        }
                        rep(\"foo\", 3)")))
    ))


(deftest test-factorial
  (testing "Testing Factorial"
    (is (= 720
           (murth-eval "fact = (n) -> {
                          let x = if n <= 1 {
                                    n
                                  } else {
                                    # Recurse
                                    n*fact(n-1)
                                  }
                          x
                        }
                        fact(6)
                        ")))

  ))


(deftest test-more-ranges
  (testing "Testing Other Ranges"
    (is (= [1 3 5]       (murth-eval "[1..5+1,2]")))
    (is (= [2 4 6 8]     (murth-eval "let n = 2
                                      [n..n+7,2]")))
    (is (= [1 3 5 7 9]   (murth-eval "odds_under = (n) -> {
                                        [1..n,2]
                                      }
                                      odds_under(10)")))
  ))


(deftest test-dictionary-parameters
  (testing "Testing Dictionary Parameters"
    (is (= 11
           (murth-eval "f1 = (h) -> {
                          1 + h.one
                        }
                        f1({zero: 0, one: 10})
                        ")))
  ))


