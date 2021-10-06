open OUnit2
open Analyzer
open Intake
open Word_processor

let state_test : test = "name" >:: fun _ -> assert_equal "" ""

let make_state_test : test = state_test

let id (x : string) = x

let rec list_printer_helper list accumulator =
  match list with
  | [] -> accumulator ^ "]"
  | h :: t -> list_printer_helper t (accumulator ^ " " ^ h ^ ";")

let rec list_printer list =
  match list with
  | [] -> "[]"
  | _ :: _ -> list_printer_helper list ""

let cmp_word_list words1 words2 =
  if List.compare_lengths words1 words2 = 0 then
    let sorted_words1 = List.sort compare words1 in
    let sorted_words2 = List.sort compare words2 in
    List.length words1 = List.length words2
    && sorted_words1 = sorted_words2
  else false

let parse_test
    (name : string)
    (input_text : string)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_word_list expected_output (parse input_text)
    ~printer:list_printer

let word_processor_tests =
  [
    parse_test "Empty string" "" [];
    parse_test "Parsing text with no punctuation"
      "And just like that a copy pasta was born"
      [
        "And";
        "just";
        "like";
        "that";
        "a";
        "copy";
        "pasta";
        "was";
        "born";
      ];
    parse_test "Parsiong text on multiple lines"
      "They should really be more clear on the fact that the deploy \
       button means to production not to locally on your machine.\n\n\
      \    Send help"
      [
        "They";
        "should";
        "really";
        "be";
        "more";
        "clear";
        "on";
        "the";
        "fact";
        "that";
        "the";
        "deploy";
        "button";
        "means";
        "to";
        "production";
        "not";
        "to";
        "locally";
        "on";
        "your";
        "machine";
      ];
    parse_test "Parsing text with punctuation"
      "So like I missed my test and I’m about to get tested rn. How \
       long till I get canvas back?"
      [
        "So";
        "like";
        "I";
        "missed";
        "my";
        "test";
        "and";
        "I'm";
        "about";
        "to";
        "get";
        "tested";
        "rn";
        "How";
        "long";
        "till";
        "I";
        "get";
        "canvas";
        "back";
      ];
  ]

let intake_tests = []

let suite =
  "test suite for Final"
  >::: List.flatten [ intake_tests; word_processor_tests ]

let _ = run_test_tt_main suite
