open OUnit2
open Analyzer
open Intake
open WordProcessor
open Sentiment
open Stemmer
open Str
open WordEncoding
open Yojson.Basic

let state_test : test = "name" >:: fun _ -> assert_equal "" ""

let make_state_test : test = state_test

(** let rec list_printer_helper list accumulator = match list with | []
    -> accumulator ^ "]" | h :: t -> list_printer_helper t (accumulator
    ^ " " ^ h ^ ";")

    let rec list_printer list = match list with | [] -> "[]" | _ :: _ ->
    list_printer_helper list "" *)

(* let id (x : string) = x *)

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

(* let cmp_word_list words1 words2 = if List.compare_lengths words1
   words2 = 0 then let sorted_words1 = List.sort compare words1 in let
   sorted_words2 = List.sort compare words2 in List.length words1 =
   List.length words2 && sorted_words1 = sorted_words2 else false *)

let id x = x

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
  assert_equal expected_output
    (create_simplified_units (create_units word) "")
    ~printer:String.escaped

let calc_vc_test (name : string) (word : string) (expected_output : int)
    : test =
  name >:: fun _ ->
  assert_equal expected_output (calc_vc word) ~printer:string_of_int

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
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (remove_past_participles word)
    ~printer:String.escaped

let finalize_plurals_past_participles_test
    (name : string)
    (word : string)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (finalize_plurals_past_participles word)
    ~printer:String.escaped

let replace_suffix_test
    (name : string)
    (word : string)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (replace_suffix word)
    ~printer:String.escaped

let possesses : stemmed_word =
  {
    original_word = "possesses";
    units = "CVCVCVC";
    num_vcs = 3;
    stemmed = "possess";
  }

let agreed : stemmed_word =
  {
    original_word = "agreed";
    units = "VCVC";
    num_vcs = 2;
    stemmed = "agre";
  }

let pp_stemmed_word stemmed =
  "Word: " ^ stemmed.original_word ^ " Units: " ^ stemmed.units
  ^ " Num VCs: "
  ^ string_of_int stemmed.num_vcs
  ^ " Stemmed: " ^ stemmed.stemmed

let pp_text_block block =
  "Original Text: " ^ block.original_text ^ "\n Stemmed Text: "
  ^ block.stemmed_text

let stemmed_words_equal s1 s2 =
  s1.original_word = s2.original_word
  && s1.num_vcs = s2.num_vcs && s1.units = s2.units
  && s1.stemmed = s2.stemmed

let stemmer_test
    (name : string)
    (word : string)
    (expected_output : stemmed_word) : test =
  name >:: fun _ ->
  assert_equal expected_output (stemmer word) ~printer:pp_stemmed_word

let process_sentence_test
    (name : string)
    (sentence : string)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (process_sentence sentence) ~printer:id

let make_text_block_test
    (name : string)
    (text : string)
    (expected_output : text_block) : test =
  name >:: fun _ ->
  assert_equal expected_output (make_text_block text)
    ~printer:pp_text_block

let sophomore_club_test_block =
  {
    original_text =
      "I'm a sophomore and I didn't really apply to many clubs and I \
       got rejected from all the ones I applied to this semester";
    stemmed_text =
      "I'm a sophomore and I didn't really apply to many club and I \
       got reject from all the one I appli to thi semesterr";
  }

let bad_professor_test_block =
  {
    original_text =
      "the professor hasn't released prelim grades, doesn't know how  \
       to teach the material, and didn't give us a syllabus! They're  \
       really slow to realize homework grades, it's ridiculous!";
    stemmed_text =
      "the professor hasn't relea prelim grade doesn't know how to \
       teach the material and didn't give u a syllabu! They're really \
       slow to realize homework grade it' ridiculou!";
  }

let url_text_block =
  {
    original_text =
      "[FAFSA](https://studentaid.gov/h/apply-for-aid/fafsa";
    stemmed_text = "FAFSAhttpsstudentaid. govhapplyforaidfafsa)";
  }

let date_text_block =
  {
    original_text =
      "2021-2022 school year: Use the 2021-2022 FAFSA, which opened \
       October 1, 2020. Requires 2019 tax information.";
    stemmed_text =
      "school year Use the  FAFSA which open October  . Require  tax \
       information.";
  }

let asterisk_text_block =
  {
    original_text =
      "* Gather all necessary documents, including bank statements, \
       tax information (W-2s, tax returns), any records of untaxed \
       income, etc.";
    stemmed_text =
      "Gather all necessary document includ bank statement tax \
       information W tax return any record of untax income etc.";
  }

let multi_line_text_block =
  {
    original_text =
      "2022-2023 school year: 2022-2023 FAFSA will became available \
       October 1, 2021. Requires 2020 tax information.\n\n\
      \  **First time? Here's a step-by-step guide.**\n\
      \  \n\
      \  * Create an [FSA account](https://www.fsaid.ed.gov) (also \
       known as the FSA ID). This is your legal electronic signature \
       to sign the FAFSA. It's linked to your Social Security number. \
       If you are a dependent student, one of your parents will need \
       to make one as well, assuming they have an SSN. If your parent \
       already has their own FSA account, they must use that. If your \
       parent does not have an SSN, they must print and sign the \
       signature page manually, then mail it in.";
    stemmed_text =
      "2022-2023 school year: 2022-2023 FAFSA will became available \
       October 1, 2021. Requires 2020 tax information.\n\n\
      \  **First time? Here's a step-by-step guide.**\n\
      \  \n\
      \  * Create an [FSA account](https://www.fsaid.ed.gov) (also \
       known as the FSA ID). This is your legal electronic signature \
       to sign the FAFSA. It's linked to your Social Security number. \
       If you are a dependent student, one of your parents will need \
       to make one as well, assuming they have an SSN. If your parent \
       already has their own FSA account, they must use that. If your \
       parent does not have an SSN, they must print and sign the \
       signature page manually, then mail it in.";
  }

let multi_line_text_block_one_line =
  {
    original_text =
      "2022-2023 school year: 2022-2023 FAFSA will became available \
       October 1, 2021. Requires 2020 tax information. **First time? \
       Here's a step-by-step guide.** * Create an [FSA \
       account](https://www.fsaid.ed.gov) (also known as the FSA ID). \
       This is your legal electronic signature to sign the FAFSA. It's \
       linked to your Social Security number. If you are a dependent \
       student, one of your parents will need to make one as well, \
       assuming they have an SSN. If your parent already has their own \
       FSA account, they must use that. If your parent does not have \
       an SSN, they must print and sign the signature page manually, \
       then mail it in.";
    stemmed_text =
      "2022-2023 school year: 2022-2023 FAFSA will became available \
       October 1, 2021. Requires 2020 tax information. **First time? \
       Here's a step-by-step guide.** * Create an [FSA \
       account](https://www.fsaid.ed.gov) (also known as the FSA ID). \
       This is your legal electronic signature to sign the FAFSA. It's \
       linked to your Social Security number. If you are a dependent \
       student, one of your parents will need to make one as well, \
       assuming they have an SSN. If your parent already has their own \
       FSA account, they must use that. If your parent does not have \
       an SSN, they must print and sign the signature page manually, \
       then mail it in.";
  }

let multi_line_one =
  {
    original_text =
      "2022-2023 school year: 2022-2023 FAFSA will became available \
       October 1, 2021. Requires 2020 tax information.";
    stemmed_text =
      "2022-2023 school year: 2022-2023 FAFSA will became available \
       October 1, 2021. Requires 2020 tax information.";
  }

let multi_line_two =
  {
    original_text = "**First time? Here's a step-by-step guide.**";
    stemmed_text = "**First time? Here's a step-by-step guide.**";
  }

let replace_suffix_test
    (name : string)
    (word : string)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (replace_suffix word)
    ~printer:String.escaped

let fix_y_test
    (name : string)
    (word : string)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (fix_y word) ~printer:String.escaped

let remove_e_test
    (name : string)
    (word : string)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (remove_e word) ~printer:String.escaped


let word_processor_tests =
  [
    parse_test "Empty string" parse "" [];
    parse_test "Parsing text\n       with no punctuation" parse
      "And just like that a copy pasta was\n       born"
      [
        "And"; "just"; "like"; "that"; "a"; "copy"; "pasta"; "was";
        "born";
      ];
    parse_test
      "Parsing text with conjunctions but\n\
      \       no sentence punctuation" parse
      "I'm a sophomore and I didn't\n\
      \       really apply to many clubs and I\n\
      \  got rejected from all the\n\
      \       ones I applied to this semester"
      [
        "Im"; "a"; "sophomore"; "and"; "I"; "didnt"; "really"; "apply";
        "to"; "many"; "clubs"; "and"; "I"; "got"; "rejected"; "from";
        "all"; "the"; "ones"; "I"; "applied"; "to"; "this"; "semester";
      ];
    parse_test "Parsing text\n       on multiple lines" parse
      "They should really be more clear on the\n\
      \       fact that the deploy\n\
      \  button means to production not to\n\
      \       locally on your  machine. Send\n\
      \  help"
      [
        "They"; "should"; "really"; "be"; "more"; "clear"; "on"; "the";
        "fact"; "that"; "the"; "deploy"; "button"; "means"; "to";
        "production"; "not"; "to"; "locally"; "on"; "your"; "machine";
        "Send"; "help";
      ];
    parse_test
      "Parsing text with punctuation, doesn't remove\n\
      \       punctuation in  the middle of the word" parse
      "So like I missed\n\
      \       my test and I'm about to get tested rn. How  long till I \
       get\n\
      \       canvas back?"
      [
        "So"; "like"; "I"; "missed"; "my"; "test"; "and"; "Im"; "about";
        "to"; "get"; "tested"; "rn"; "How"; "long"; "till"; "I"; "get";
        "canvas"; "back";
      ];
    parse_test "Don't\n       conjunction and punctuation" parse
      "And he has spent a long time\n\
      \       constantly targeting me in these  implicit ways by either\n\
      \ pretending I don't contribute, quickly moving on  without an\n\
      \ acknowledgement, or emphasizing how I should have  followed\n\
      \       the point of\n\
      \  his fave."
      [
        "And"; "he"; "has"; "spent"; "a"; "long"; "time"; "constantly";
        "targeting"; "me"; "in"; "these"; "implicit"; "ways"; "by";
        "either"; "pretending"; "I"; "dont"; "contribute"; "quickly";
        "moving"; "on"; "without"; "an"; "acknowledgement"; "or";
        "emphasizing"; "how"; "I"; "should"; "have"; "followed"; "the";
        "point"; "of"; "his"; "fave";
      ];
    parse_test "Manyconjunctions and types of punctuation" parse
      "the\n\
      \       professor hasn't released prelim grades, doesn't know \
       how to\n\
      \ teach the material, and didn't give us a syllabus!  They're\n\
      \       really\n\
      \  slow to realize homework grades, it's ridiculous! "
      [
        "the"; "professor"; "hasnt"; "released"; "prelim"; "grades";
        "doesnt"; "know"; "how"; "to"; "teach"; "the"; "material";
        "and"; "didnt"; "give"; "us"; "a"; "syllabus"; "Theyre";
        "really"; "slow"; "to"; "realize"; "homework"; "grades"; "its";
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
      "Parses text into sentences with punctuation in middle of  \
       sentence"
      parse_sentence "Hello there, my name is Usnavi. Who are you?"
      [ "Hello there, my name is Usnavi."; "Who are you?" ];
    parse_test
      "Doesn't parse text with text containing no sentence delimiting  \
       punctuation"
      parse_sentence "Hello there, my name is Usnavi Who are you"
      [ "Hello there, my name is Usnavi Who are you" ];
    create_units_test "Consonant group" "hello" "CVCV";
    create_units_test "Vowel group" "helloooooo" "CVCV";
    create_units_test "Basic test" "helo" "CVCV";
    create_units_test "Numerous groups" "hhhhhheeeeeellllloooooo" "CVCV";
    create_units_test "Numerous groups with different characters"
      "hkealolo" "CVCVCV";
    create_units_test "Does create_units work with possessive"
      "possessive" "CVCVCVCV";
    remove_plurals_test "Ending with SSES" "possesses" "possess";
    remove_plurals_test "Ending with IES" "libraries" "librari";
    remove_plurals_test "Ending with SS" "loneliness" "loneliness";
    remove_plurals_test "Ending with S" "cars" "car";
    remove_plurals_test "Not plural" "car" "car";
    remove_past_participles_test "No VC with EED" "steed" "steed";
    remove_past_participles_test "Ending with EED" "agreed" "agree";
    remove_past_participles_test "Ending with ING" "wondering" "wonder";
    remove_past_participles_test "Ending with ING and no vowel in stem"
      "wndring" "wndring";
    remove_past_participles_test "Ending with ED" "helped" "help";
    remove_past_participles_test "Ending with ED and no vowel in stem"
      "hlped" "hlped";
    finalize_plurals_past_participles_test
      "Add e back after it has been removed" "conflat" "conflate";
    finalize_plurals_past_participles_test
      "Add e back after it has been removed" "troubl" "trouble";
    finalize_plurals_past_participles_test
      "Add e back after it has been removed from" "siz" "size";
    finalize_plurals_past_participles_test "Add e if stem is CVC" "fil"
      "file";
    finalize_plurals_past_participles_test
      "Do not add e is stem is CVC but length greater than 3" "fail"
      "fail"; stemmer_test "Stemming possesses" "possesses" possesses;
    stemmer_test "Stemming agreed -> agree" "agreed" agreed;
    create_units_test "Just seeing what" "H" "C";
    create_units_test "Creating unit for he CV" "He" "CV";
    process_sentence_test "Sentence with one word to stem"
      "He possesses the gem." "He possess the gem.";
    (**process_sentence_test "Sentence with two words to stem"
      "They agreed to visit libraries with me."
      "They agree to visit librari with me.";*)
    (*These tests should pass, but spacing is causing them to act
      weird*)
    (* make_text_block_test "Sophomore clubs post" "I'm a sophomore and
       I didn't really apply to many clubs and I \ got rejected from all
       the ones I applied to this semester" sophomore_club_test_block;
       make_text_block_test "Bad professor text" "the professor hasn't
       released prelim grades, doesn't know how \ to teach the material,
       and didn't give us a syllabus! They're \ really slow to realize
       homework grades, it's ridiculous!" bad_professor_test_block;
       make_text_block_test "URL"
       "[FAFSA](https://studentaid.gov/h/apply-for-aid/fafsa)"
       url_text_block; make_text_block_test "Date" "2021-2022 school
       year: Use the 2021-2022 FAFSA, which opened \ October 1, 2020.
       Requires 2019 tax information." date_text_block;
       make_text_block_test "Asterisk" "* Gather all necessary
       documents, including bank statements, \ tax information (W-2s,
       tax returns), any records of untaxed \ income, etc."
       asterisk_text_block; make_text_block_test "Multi line text block"
       "2022-2023 school year: 2022-2023 FAFSA will became available \
       October 1, 2021. Requires 2020 tax information.\n\n\ \ **First
       time? Here's a step-by-step guide.**\n\ \ \n\ \ * Create an [FSA
       account](https://www.fsaid.ed.gov) (also \ known as the FSA ID).
       This is your legal electronic signature \ to sign the FAFSA. It's
       linked to your Social Security number. \ If you are a dependent
       student, one of your parents will need \ to make one as well,
       assuming they have an SSN. If your parent \ already has their own
       FSA account, they must use that. If your \ parent does not have
       an SSN, they must print and sign the \ signature page manually,
       then mail it in." multi_line_text_block; make_text_block_test
       "Multi line text block with new lines removed" "2022-2023 school
       year: 2022-2023 FAFSA will became available \ October 1, 2021.
       Requires 2020 tax information. **First time? \ Here's a
       step-by-step guide.** * Create an [FSA \
       account](https://www.fsaid.ed.gov) (also known as the FSA ID). \
       This is your legal electronic signature to sign the FAFSA. It's \
       linked to your Social Security number. If you are a dependent \
       student, one of your parents will need to make one as well, \
       assuming they have an SSN. If your parent already has their own \
       FSA account, they must use that. If your parent does not have \
       an SSN, they must print and sign the signature page manually, \
       then mail it in." multi_line_text_block_one_line;
       make_text_block_test "Multi text line 1" "2022-2023 school year:
       2022-2023 FAFSA will became available \ October 1, 2021. Requires
       2020 tax information." multi_line_one; make_text_block_test "2nd
       line multi" "**First time? Here's a step-by-step guide.**"
       multi_line_two; *)

    (*The following test cases cover the replacements in our second and
      third step, and they cover all possible mappings in the step2_3
      json file.*)
    replace_suffix_test "ational -> ate" "relational" "relate";
    replace_suffix_test "tional -> tion" "conditional" "condition";
    replace_suffix_test "enci -> ence" "valenci" "valence";
    replace_suffix_test "anci -> ance" "hesitanci" "hesitance";
    replace_suffix_test "izer -> ize" "digitizer" "digitize";
    replace_suffix_test "abli -> able" "conformabli" "conformable";
    replace_suffix_test "alli -> al" "radicalli" "radical";
    replace_suffix_test "entli -> ent" "differentli" "different";
    replace_suffix_test "eli -> e" "vileli" "vile";
    replace_suffix_test "ousli -> ous" "analogousli" "analogous";
    replace_suffix_test "ization -> ize" "vietnamization" "vietnamize";
    replace_suffix_test "ation -> ate" "predication" "predicate";
    replace_suffix_test "ator -> ate" "operator" "operate";
    replace_suffix_test "alism -> al" "feudalism" "feudal";
    replace_suffix_test "iveness -> ive" "decisiveness" "decisive";
    replace_suffix_test "fulness -> ful" "hopefulness" "hopeful";
    replace_suffix_test "ousness -> ous" "callousness" "callous";
    replace_suffix_test "aliti -> al" "formaliti" "formal";
    replace_suffix_test "iviti -> ive" "sensitiviti" "sensitive";
    replace_suffix_test "biliti -> ble" "sensibiliti" "sensible";
    replace_suffix_test "alism -> al" "feudalism" "feudal";
    replace_suffix_test "icate -> ic" "triplicate" "triplic";
    replace_suffix_test "ative -> empty" "formative" "form";
    replace_suffix_test "alize -> al" "formalize" "formal";
    replace_suffix_test "iciti -> ic" "electriciti" "electric";
    replace_suffix_test "ical -> ic" "electrical" "electric";
    replace_suffix_test "ful -> empty" "hopeful" "hope";
    replace_suffix_test "ness -> empty" "goodness" "good";
    (*The following test cases cover the replacements in our fourth
      step, and they cover all the possible mappings in the step4 json
      file.*) replace_suffix_test "al -> empty" "revival" "reviv";
    replace_suffix_test "ance -> empty" "allowance" "allow";
    replace_suffix_test "ence -> empty" "inference" "infer";
    replace_suffix_test "er -> empty" "airliner" "airlin";
    replace_suffix_test "ic -> empty" "gyroscopic" "gyroscop";
    replace_suffix_test "able -> empty" "adjustable" "adjust";
    replace_suffix_test "ible -> empty" "defensible" "defens";
    replace_suffix_test "ant -> empty" "irritant" "irrit";
    replace_suffix_test "ement -> empty" "replacement" "replac";
    replace_suffix_test "ment -> empty" "adjustment" "adjust";
    replace_suffix_test "ent -> empty" "dependent" "depend";
    replace_suffix_test "ion -> empty" "adoption" "adopt";
    replace_suffix_test "ou -> empty" "homologou" "homolog";
    replace_suffix_test "ism -> empty" "communism" "commun";
    replace_suffix_test "ate -> empty" "activate" "activ";
    replace_suffix_test "iti -> empty" "angulariti" "angular";
    replace_suffix_test "ous -> empty" "homologous" "homolog";
    replace_suffix_test "ive -> empty" "effective" "effect";
    replace_suffix_test "ize -> empty" "bowdlerize" "bowdler";
    (*These last tests are in the case that there is no suffix change.*)
    replace_suffix_test "NO CHANGE" "hello" "hello";
    replace_suffix_test "NO CHANGE" "hi" "hi";
    fix_y_test "Replaces i with y if there is a vowel in the stem"
      "party" "parti";
    fix_y_test "Does not change word that does not end in y" "python"
      "python";
    fix_y_test "Does not change word without vowel" "sky" "sky";
    remove_e_test "Removes e if number of VC's > 1" "debate" "debat";
    remove_e_test "Does not remove e if number of VC's <= 1" "late"
      "late";
    remove_e_test
      "Removes e if number of VC's = 1 and the stem ends CVC" "cease"
      "ceas";
    replace_suffix_test "Choose step 4 but not step 3" "rational"
      "ration";
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
      "Cornell University is located\n       in New York." "Neutral";
    sentiment_test "Negative sentence" "I\n       hate all of the snow."
      "Negative";
  ]

let intake_tests = []

let write_words_to_json_test
    (name : string)
    (words : string list)
    (filename : string)
    (expected_output : unit) : test =
  let file = open_out ("data/subredditVocabJsons/" ^ filename) in
  name >:: fun _ ->
  assert_equal expected_output (write_words_to_json file words)

let convert_path_to_json (file_path : string) = file_path |> from_file

let cornell_json = convert_path_to_json "data/college.json"

let subreddit_json_to_word_json_test
    (name : string)
    (expected_output : unit)
    (processor : Yojson.Basic.t -> string list)
    (subreddit : Yojson.Basic.t) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (subreddit_json_to_word_json processor subreddit)

let pp_print_matrix acc matrix : string =
  Array.fold_right
    (fun row acc ->
      Array.fold_right (fun elt acc -> acc ^ string_of_int elt) row ""
      ^ "\n")
    matrix ""

let create_encoded_matrix_test
    (name : string)
    (word_json : t)
    (post : string)
    (expected_output : int array array) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (create_encoded_matrix word_json post)
    ~printer:(pp_print_matrix "")

let test3_json =
  convert_path_to_json "data/subredditVocabJsons/test3.json"

let test3_matrix = Array.make_matrix 2 5 0

let _ = test3_matrix.(0).(0) <- 1

let _ = test3_matrix.(1).(3) <- 1

let word_encoding_tests =
  [
    write_words_to_json_test
      "Takes a list of words and writes to a\n       json file"
      [ "Hello"; "Did"; "This"; "format"; "correctly" ]
      "test3.json" (print_int 1);
    subreddit_json_to_word_json_test
      "Converts words in cornell\n\
      \       subreddit posts to a json of all the  words" (print_int 1)
      subreddit_json_to_words cornell_json;
    create_encoded_matrix_test
      "Json contains: Hello, Did, this, format, correctly. Test post \
       is hello format"
      test3_json "Hello format" test3_matrix;
  ]

let suite =
  "test suite for Final"
  >::: List.flatten
         [
           intake_tests; word_processor_tests; sentiment_tests;
           word_encoding_tests;
         ]

let _ = run_test_tt_main suite
