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

let create_units_test
    (name : string)
    (word : string)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (create_units word)
    ~printer:String.escaped

let calc_vc_test
    (name : string)
    (char_string : string)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (calc_vc char_string)
    ~printer:string_of_int

let remove_plurals_test
    (name : string)
    (word : string)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (remove_plurals word)
    ~printer:String.escaped

let remove_past_participles_test
    (name : string)
    (word : string)
    (num_vc : int)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (remove_past_participles word num_vc)
    ~printer:String.escaped

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
        "So"; "like"; "I"; "missed"; "my"; "test"; "and"; "Im"; "about";
        "to"; "get"; "tested"; "rn"; "How"; "long"; "till"; "I"; "get";
        "canvas"; "back";
      ]; create_units_test "Consonant group" "hello" "CVCV";
    create_units_test "Vowel group" "helloooooo" "CVCV";
    create_units_test "Basic test" "helo" "CVCV";
    create_units_test "Numerous groups" "hhhhhheeeeeellllloooooo" "CVCV";
    create_units_test "Numerous groups with different characters"
      "hkealolo" "CVCVCV"; calc_vc_test "no VC" "CV" 0;
    calc_vc_test "Empty string" "" 0; calc_vc_test "Odd length" "CVC" 1;
    calc_vc_test "Even pairs" "VCVCVC" 3;
    remove_plurals_test "Ending with SSES" "possesses" "possess";
    remove_plurals_test "Ending with IES" "libraries" "librari";
    remove_plurals_test "Ending with SS" "loneliness" "loneline";
    remove_plurals_test "Ending with S" "cars" "car";
    remove_plurals_test "Not plural" "car" "car";
    remove_past_participles_test "No VC with EED" "steed" 0 "steed";
    remove_past_participles_test "Ending with EED" "agreed" 1 "agree";
    remove_past_participles_test "Ending with ING" "wondering" 1
      "wonder";
    remove_past_participles_test "Ending with ING and no vowel in stem"
      "wndring" 1 "wndring";
    remove_past_participles_test "Ending with ED" "helped" 1 "help";
    remove_past_participles_test "Ending with ED and no vowel in stem"
      "hlped" 1 "hlped";
  ]

let sentiment_of_score score =
  if score <= -0.05 then "Negative"
  else if score >= 0.05 then "Positive"
  else "Neutral"

let sentiment_test
    (name : string)
    (text : string)
    (expected_output : string) : test =
  let _ = print_float (polarity_score text) in
  name >:: fun _ ->
  assert_equal expected_output
    (sentiment_of_score (polarity_score text))
    ~printer:String.escaped


let sentiment_tests =
  [
    sentiment_test "Positive sentence"
      "This is a very happy sentence that thrills me." "Positive";
    sentiment_test "Neutral sentence"
      "Cornell University is located in New York." "Neutral";
    sentiment_test "Negative sentence" "I hate all of the snow."
      "Negative";
  ]

let intake_tests = []

let suite =
  "test suite for Final"
  >::: List.flatten
         [ intake_tests; word_processor_tests; sentiment_tests ]

let _ = run_test_tt_main suite
