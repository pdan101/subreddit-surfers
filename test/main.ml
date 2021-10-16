open OUnit2
open Analyzer
open Intake
open WordProcessor
open Sentiment
open Str

let state_test : test = "name" >:: fun _ -> assert_equal "" ""

let make_state_test : test = state_test

(** FROM A2: let rec list_printer_helper list accumulator = match list
    with | [] -> accumulator ^ "]" | h :: t -> list_printer_helper t
    (accumulator ^ " " ^ h ^ ";")

    let rec list_printer list = match list with | [] -> "[]" | _ :: _ ->
    list_printer_helper list "" *)

(* let id (x : string) = x *)

(** FROM A2: [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
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

(* let cmp_word_list words1 words2 = if List.compare_lengths words1
   words2 = 0 then let sorted_words1 = List.sort compare words1 in let
   sorted_words2 = List.sort compare words2 in List.length words1 =
   List.length words2 && sorted_words1 = sorted_words2 else false *)
let parse_test
    (name : string)
    (parser : string -> string list)
    (input_text : string)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output (parser input_text)
    ~printer:(pp_list pp_string)

let word_processor_tests =
  [
    parse_test "Empty string" parse "" [];
    parse_test "Parsing text with no punctuation" parse
      "And just like that a copy pasta was born"
      [
        "And"; "just"; "like"; "that"; "a"; "copy"; "pasta"; "was";
        "born";
      ];
    parse_test
      "Parsing text with conjunctions but no sentence punctuation" parse
      "I'm a sophomore and I didn't really apply to many clubs and I \
       got rejected from all the ones I applied to this semester"
      [
        "I'm"; "a"; "sophomore"; "and"; "I"; "didn't"; "really";
        "apply"; "to"; "many"; "clubs"; "and"; "I"; "got"; "rejected";
        "from"; "all"; "the"; "ones"; "I"; "applied"; "to"; "this";
        "semester";
      ];
    parse_test "Parsing text on multiple lines" parse
      "They should really be more clear on the fact that the deploy \
       button means to production not to locally on your machine.\n\n\
      \    Send help"
      [
        "They"; "should"; "really"; "be"; "more"; "clear"; "on"; "the";
        "fact"; "that"; "the"; "deploy"; "button"; "means"; "to";
        "production"; "not"; "to"; "locally"; "on"; "your"; "machine";
        "Send"; "help";
      ];
    parse_test
      "Parsing text with punctuation, doesn't remove punctuation in \
       the middle of the word"
      parse
      "So like I missed my test and I'm about to get tested rn. How \
       long till I get canvas back?"
      [
        "So"; "like"; "I"; "missed"; "my"; "test"; "and"; "I'm";
        "about"; "to"; "get"; "tested"; "rn"; "How"; "long"; "till";
        "I"; "get"; "canvas"; "back";
      ];
    parse_test "Don't conjunction and punctuation" parse
      "And he has spent a long time constantly targeting me in these \
       implicit ways by either pretending I don't contribute, quickly \
       moving on without an acknowledgement, or emphasizing how I \
       should have followed the point of his fave."
      [
        "And"; "he"; "has"; "spent"; "a"; "long"; "time"; "constantly";
        "targeting"; "me"; "in"; "these"; "implicit"; "ways"; "by";
        "either"; "pretending"; "I"; "don't"; "contribute"; "quickly";
        "moving"; "on"; "without"; "an"; "acknowledgement"; "or";
        "emphasizing"; "how"; "I"; "should"; "have"; "followed"; "the";
        "point"; "of"; "his"; "fave";
      ];
    parse_test "Many conjunctions and types of punctuation" parse
      "the professor hasn't released prelim grades, doesn't know how \
       to teach the material, and didn't give us a syllabus! They're \
       really slow to realize homework grades, it's ridiculous! "
      [
        "the"; "professor"; "hasn't"; "released"; "prelim"; "grades";
        "doesn't"; "know"; "how"; "to"; "teach"; "the"; "material";
        "and"; "didn't"; "give"; "us"; "a"; "syllabus"; "They're";
        "really"; "slow"; "to"; "realize"; "homework"; "grades"; "it's";
        "ridiculous";
      ];
    (*Parse does not work with right apostrophe parse_test "Round right
      apostrophe" (parse) "It’s" [ "It’s" ];*)
    parse_test "Parses text into sentences separated by periods"
      parse_sentence "the professor is great. He gave everyone an A."
      [ "the professor is great."; "He gave everyone an A." ];
    parse_test
      "Parses text into sentences separated by exclamation marks"
      parse_sentence "Hello there! It's so great to see you!"
      [ "Hello there!"; "It's so great to see you!" ];
    parse_test "Parses text into sentences separated by question marks"
      parse_sentence "Hello there? Is anybody home?"
      [ "Hello there?"; "Is anybody home?" ];
    parse_test
      "Parses text into sentences with punctuation in middle of \
       sentence"
      parse_sentence "Hello there, my name is Usnavi. Who are you?"
      [ "Hello there, my name is Usnavi."; "Who are you?" ];
    parse_test
      "Doesn't parse text with text containing no sentence delimiting \
       punctuation"
      parse_sentence "Hello there, my name is Usnavi Who are you"
      [ "Hello there, my name is Usnavi Who are you" ];
  ]

let sentiment_test (name : string) expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output
    (connotation_str "this is a very funny sentence I love it")
    ~printer:string_of_float

let sentiment_tests = [ sentiment_test "trying to run this" 0.839 ]

let intake_tests = []

let suite =
  "test suite for Final"
  >::: List.flatten
         [ intake_tests; word_processor_tests; sentiment_tests ]

let _ = run_test_tt_main suite
