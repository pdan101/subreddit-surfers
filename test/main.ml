open OUnit2
open Analyzer
open Intake
open WordProcessor
open Sentiment
open RedditApi

let state_test : test = "name" >:: fun _ -> assert_equal "" ""

let make_state_test : test = state_test

(** let rec list_printer_helper list accumulator = match list with | []
    -> accumulator ^ "]" | h :: t -> list_printer_helper t (accumulator
    ^ " " ^ h ^ ";")

    let rec list_printer list = match list with | [] -> "[]" | _ :: _ ->
    list_printer_helper list "" *)

(* let id (x : string) = x *)
(*BISECT-IGNORE-BEGIN*)

(** FROM A2: [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** FROM A2: [pp_list pp_elt lst] pretty-prints list [lst], using
    [pp_elt] to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(*BISECT-IGNORE-END*)
(* let cmp_word_list words1 words2 = if List.compare_lengths words1
   words2 = 0 then let sorted_words1 = List.sort compare words1 in let
   sorted_words2 = List.sort compare words2 in List.length words1 =
   List.length words2 && sorted_words1 = sorted_words2 else false *)
let parse_test
    (name : string)
    (input_text : string)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output (parse input_text)
    ~printer:(pp_list pp_string)

let word_processor_tests =
  [
    parse_test "Empty string" "" [];
    parse_test "Parsing text with no punctuation"
      "And just like that a copy pasta was born"
      [
        "And"; "just"; "like"; "that"; "a"; "copy"; "pasta"; "was";
        "born";
      ];
    parse_test "Parsing text on multiple lines"
      "They should really be more clear on the fact that the deploy \
       button means to production not to locally on your machine.\n\n\
      \    Send help"
      [
        "They"; "should"; "really"; "be"; "more"; "clear"; "on"; "the";
        "fact"; "that"; "the"; "deploy"; "button"; "means"; "to";
        "production"; "not"; "to"; "locally"; "on"; "your"; "machine";
        "Send"; "help";
      ];
    parse_test "Parsing text with punctuation"
      "So like I missed my test and I’m about to get tested rn. How \
       long till I get canvas back?"
      [
        "So"; "like"; "I"; "missed"; "my"; "test"; "and"; "Im"; "about";
        "to"; "get"; "tested"; "rn"; "How"; "long"; "till"; "I"; "get";
        "canvas"; "back";
      ];
  ]

let sentiment_test (name : string) expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output
    (connotation_str "this is a very funny sentence I love it")
    ~printer:string_of_float

let sentiment_tests = [ sentiment_test "trying to run this" 1.0 ]

let reddit_api_test (name : string) expected_output : test =
  name >:: fun _ -> assert_equal expected_output run_api

let reddit_api_tests = [ reddit_api_test "this is a test" () ]

let intake_tests = []

let suite =
  "test suite for Final"
  >::: List.flatten
         [
           intake_tests; word_processor_tests; sentiment_tests;
           reddit_api_tests;
         ]

let _ = run_test_tt_main suite
