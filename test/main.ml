open OUnit2
open Analyzer
open Intake
open WordProcessor
open Stemmer
open Str
open WordEncoding
open Yojson.Basic
open CustomRegression
open Owl
open ThemeEncoder

type text_block = {
  original_text : string;
  stemmed_text : string;
}

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

let stemmer_paragraph_test
    (name : string)
    (sentence : string)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal
    (String.trim expected_output)
    (String.trim (stem_paragraph sentence))
    ~printer:id

let process_sentence_test
    (name : string)
    (sentence : string)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (process_sentence sentence) ~printer:id

let sophomore_club_text_block =
  {
    original_text =
      "I'm a sophomore and I didn't really apply to many clubs and I \
       got rejected from all the ones I applied to this semester";
    stemmed_text =
      "Im sophomor didnt realli appli mani club got reject on appli \
       semestr";
  }

let bad_professor_text_block =
  {
    original_text =
      "the professor hasn't released prelim grades, doesn't know how  \
       to teach the material, and didn't give us a syllabus! They're  \
       really slow to realize homework grades, it's ridiculous!";
    stemmed_text =
      "professor hasnt releas prelim grade doesnt know teach materi \
       didnt give u syllabu! Theyr realli slow realiz homework grade \
       ridicul!";
  }

let url_text_block =
  {
    original_text =
      "[FAFSA](https://studentaid.gov/h/apply-for-aid/fafsa";
    stemmed_text = "FAFSAhttpsstudentaid. govhapplyforaidfafsaa";
  }

let date_text_block =
  {
    original_text =
      "2021-2022 school year: Use the 2021-2022 FAFSA, which opened \
       October 1, 2020. Requires 2019 tax information.";
    stemmed_text =
      "school year Us  FAFSA open Octob  . Requir  tax informat.";
  }

let asterisk_text_block =
  {
    original_text =
      "* Gather all necessary documents, including bank statements, \
       tax information (W-2s, tax returns), any records of untaxed \
       income, etc.";
    stemmed_text =
      "Gather necessari docum includ bank statem tax informat W tax \
       return record untax incom etc.";
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

let stopword_test
    (name : string)
    (word : string)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (WordProcessor.is_stopword word)
    ~printer:string_of_bool

let word_processor_tests =
  [
    parse_test "Empty string" parse "" [];
    parse_test "Parsing text\n       with no punctuation" parse
      "And just like that a copy pasta was\n       born"
      [ "like"; "copy"; "pasta"; "born" ];
    parse_test
      "Parsing text with conjunctions but\n\
      \       no sentence punctuation" parse
      "I'm a sophomore and I didn't\n\
      \       really apply to many clubs and I\n\
      \  got rejected from all the\n\
      \       ones I applied to this semester"
      [
        "Im"; "sophomore"; "didnt"; "really"; "apply"; "many"; "clubs";
        "got"; "rejected"; "ones"; "applied"; "semester";
      ];
    parse_test "Parsing text\n       on multiple lines" parse
      "They should really be more clear on the\n\
      \       fact that the deploy\n\
      \  button means to production not to\n\
      \       locally on your  machine. Send\n\
      \  help"
      [
        "really"; "clear"; "fact"; "deploy"; "button"; "means";
        "production"; "locally"; "machine"; "Send"; "help";
      ];
    parse_test
      "Parsing text with punctuation, doesn't remove\n\
      \       punctuation in  the middle of the word" parse
      "So like I missed\n\
      \       my test and I'm about to get tested rn. How  long till I \
       get\n\
      \       canvas back?"
      [
        "like"; "missed"; "test"; "Im"; "get"; "tested"; "rn"; "long";
        "till"; "get"; "canvas"; "back";
      ];
    parse_test "Don't\n       conjunction and punctuation" parse
      "And he has spent a long time\n\
      \       constantly targeting me in these  implicit ways by either\n\
      \ pretending I don't contribute, quickly moving on  without an\n\
      \ acknowledgement, or emphasizing how I should have  followed\n\
      \       the point of\n\
      \  his fave."
      [
        "spent"; "long"; "time"; "constantly"; "targeting"; "implicit";
        "ways"; "either"; "pretending"; "dont"; "contribute"; "quickly";
        "moving"; "without"; "acknowledgement"; "emphasizing";
        "followed"; "point"; "fave";
      ];
    parse_test "Manyconjunctions and types of punctuation" parse
      "the\n\
      \       professor hasn't released prelim grades, doesn't know \
       how to\n\
      \ teach the material, and didn't give us a syllabus!  They're\n\
      \       really\n\
      \  slow to realize homework grades, it's ridiculous! "
      [
        "professor"; "hasnt"; "released"; "prelim"; "grades"; "doesnt";
        "know"; "teach"; "material"; "didnt"; "give"; "us"; "syllabus";
        "Theyre"; "really"; "slow"; "realize"; "homework"; "grades";
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
    stopword_test "Check if 'a' stopword" "a" true;
    stopword_test "Check if 'to' stopword" "to" true;
    stopword_test "Check if 'hello' stopword" "hello" false;
    stemmer_test "Stemming agreed -> agree" "agreed" agreed;
    stemmer_paragraph_test "Stemming sophomore club text"
      sophomore_club_text_block.original_text
      sophomore_club_text_block.stemmed_text;
    stemmer_paragraph_test "Stemming bad prof text"
      bad_professor_text_block.original_text
      bad_professor_text_block.stemmed_text;
    stemmer_paragraph_test "Stemming url text"
      url_text_block.original_text url_text_block.stemmed_text;
    stemmer_paragraph_test "Stemming date text"
      date_text_block.original_text date_text_block.stemmed_text;
    stemmer_paragraph_test "Stemming asterisk text"
      asterisk_text_block.original_text asterisk_text_block.stemmed_text;
    create_units_test "Creating unit for he CV" "He" "CV";
    process_sentence_test "Sentence with one word to stem"
      "He possesses the gem." "possess gem.";
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
    replace_suffix_test "Choose step 4 but not step 3" "rational"
      "ration";
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
  ]

let convert_path_to_json (file_path : string) = file_path |> from_file

let cornell_json = convert_path_to_json "data/cornell.json"

let cornell_sub_post =
  Intake.from_json cornell_json |> Intake.recent_post

let cornell_json2 =
  convert_path_to_json "data/subredditVocabJsons/cornell.json"

let college_json = convert_path_to_json "data/college.json"

let anime_json = convert_path_to_json "data/anime.json"

let anime_sub_post = Intake.from_json anime_json |> Intake.recent_post

let author_test name input expected_output =
  name >:: fun _ ->
  assert_equal expected_output (Intake.author input)
    ~printer:String.escaped

let created_utc_test name input expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (Intake.created_utc input)
    ~printer:string_of_float

let id_test name input expected_output =
  name >:: fun _ ->
  assert_equal expected_output (Intake.id input) ~printer:String.escaped

let num_comments_test name input expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (Intake.num_comments input)
    ~printer:string_of_int

let num_crossposts_test name input expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (Intake.num_crossposts input)
    ~printer:string_of_int

let selftext_test name input expected_output =
  name >:: fun _ ->
  assert_equal expected_output (Intake.selftext input)
    ~printer:String.escaped

let spoiler_test name input expected_output =
  name >:: fun _ ->
  assert_equal expected_output (Intake.spoiler input)
    ~printer:string_of_bool

let upvotes_test name input expected_output =
  name >:: fun _ ->
  assert_equal expected_output (Intake.upvotes input)
    ~printer:string_of_int

let subreddit_name_test name input expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (Intake.subreddit_name input)
    ~printer:String.escaped

let title_test name input expected_output =
  name >:: fun _ ->
  assert_equal expected_output (Intake.title input)
    ~printer:String.escaped

let intake_tests =
  [
    author_test "Author of recent is ..." cornell_sub_post "pw11111";
    author_test "Author of recent is ..." anime_sub_post "mpp00";
    created_utc_test "Created utc of recent is ..." cornell_sub_post
      1618803237.;
    created_utc_test "Created utc of recent is ..." anime_sub_post
      1633726977.;
    id_test "ID of recent is ..." cornell_sub_post "mts7re";
    id_test "ID of recent is ..." anime_sub_post "q46s7i";
    num_comments_test "Num comments of recent is ..." cornell_sub_post
      806;
    num_comments_test "Num comments of recent is ..." anime_sub_post 93;
    num_crossposts_test "Num crossposts of recent is ..."
      cornell_sub_post 0;
    num_crossposts_test "Num crossposts of recent is ..." anime_sub_post
      0;
    selftext_test "Selftext of recent is ..." cornell_sub_post
      "Please place all admissions related posts here, in the form of \
       comments, and current Cornell students will reply. Try to be \
       detailed; if we don't have enough information, we can't help. \
       Also, if you are a prospective student, and have questions \
       about life at Cornell, feel free to post them here! \n\n\
       Any \"Chance Me\" or admissions related posts placed elsewhere \
       will be removed. If you are a current student, and think that \
       you could offer advice to someone considering Cornell, feel \
       free to respond to some of the posts! Please only respond if \
       you are qualified to do so. We will be checking through these \
       regularly for spam.";
    (*The text for this post is too long, so we aren't including it.
      selftext_test "Selftext of recent is ..." anime_sub_post "";*)
    spoiler_test "Spoiler of recent is ..." cornell_sub_post false;
    spoiler_test "Spoiler of recent is ..." anime_sub_post false;
    upvotes_test "Upvotes of recent is ..." cornell_sub_post 99;
    upvotes_test "Upvotes of recent is ..." anime_sub_post 293;
    subreddit_name_test "Subreddit name of recent is ..."
      cornell_sub_post "Cornell";
    subreddit_name_test "Subreddit name of recent is ..." anime_sub_post
      "anime";
    title_test "Title of recent is ..." cornell_sub_post
      "Chance Me! and Prospective Student Q&amp;A";
    title_test "Title of recent is ..." anime_sub_post
      "The 2021 r/anime Awards Announcement and Jury Application";
  ]

let write_words_to_json_test
    (name : string)
    (words : string list)
    (filename : string)
    (expected_output : unit) : test =
  let file = open_out ("data/subredditVocabJsons/" ^ filename) in
  name >:: fun _ ->
  assert_equal expected_output (write_words_to_json file words)

let subreddit_json_to_word_json_test
    (name : string)
    (expected_output : unit)
    (processor : Yojson.Basic.t -> string list)
    (subreddit : Yojson.Basic.t)
    (filename : string) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (subreddit_json_to_word_json processor subreddit filename)

let pp_print_matrix acc matrix : string =
  Array.fold_right
    (fun row acc ->
      Array.fold_right (fun elt acc -> acc ^ string_of_int elt) row ""
      ^ "\n")
    matrix ""

(* let encode_post_test (name : string) (vocab : t) (processor : string
   -> string list) (post : string) (expected_output : int array) : test
   = name >:: fun _ -> assert_equal (Array.to_list expected_output)
   (Array.to_list (encode_post vocab processor Intake.upvotes)) *)

let test3_json =
  convert_path_to_json "data/subredditVocabJsons/test3.json"

let test3_matrix = Array.make_matrix 2 5 0

let _ = test3_matrix.(0).(0) <- 1

let _ = test3_matrix.(1).(3) <- 1

let cornell_test_1_matrix = Array.make 492 0

let _ = cornell_test_1_matrix.(39) <- 1

let _ = cornell_test_1_matrix.(40) <- 1

let word_encoding_tests =
  [
    write_words_to_json_test
      "Takes a list of words and writes to a\n       json file"
      [ "Hello"; "Did"; "This"; "format"; "correctly" ]
      "test3.json" (print_int 1);
    subreddit_json_to_word_json_test
      "Converts words in college\n\
      \       subreddit posts to a json of all the  words" (print_int 1)
      subreddit_json_to_words college_json "college";
    subreddit_json_to_word_json_test
      "Converts words in cornell\n\
      \       subreddit posts to a json of all the  words" (print_int 1)
      subreddit_json_to_stemmed_words cornell_json "cornell";
    subreddit_json_to_word_json_test
      "Converts words in anime\n\
      \       subreddit posts to a json of all the  words" (print_int 1)
      subreddit_json_to_stemmed_words anime_json "anime";
    subreddit_json_to_word_json_test
      "Converts words in anime\n\
      \       subreddit posts to a json of all the  words" (print_int 1)
      subreddit_json_to_stemmed_words
      (convert_path_to_json "data/anime_new.json")
      "anime_new";
    subreddit_json_to_word_json_test
      "Converts words in anime\n\
      \       subreddit posts to a json of all the  words" (print_int 1)
      subreddit_json_to_stemmed_words
      (convert_path_to_json "data/college_new.json")
      "college_new";
    subreddit_json_to_word_json_test
      "Converts words in anime\n\
      \       subreddit posts to a json of all the  words" (print_int 1)
      subreddit_json_to_stemmed_words
      (convert_path_to_json "data/cornell_new.json")
      "cornell_new";
    subreddit_json_to_word_json_test
      "Converts words in anime\n\
      \       subreddit posts to a json of all the  words" (print_int 1)
      subreddit_json_to_stemmed_words
      (convert_path_to_json "data/csmajors_new.json")
      "csmajors_new";
    subreddit_json_to_word_json_test
      "Converts words in anime\n\
      \       subreddit posts to a json of all the  words" (print_int 1)
      subreddit_json_to_stemmed_words
      (convert_path_to_json "data/ocaml_new.json")
      "ocaml_new";
    subreddit_json_to_word_json_test
      "Converts words in anime\n\
      \       subreddit posts to a json of all the  words" (print_int 1)
      subreddit_json_to_stemmed_words
      (convert_path_to_json "data/running_new.json")
      "running_new";
  ]

let rec pp_print_association_list assoc_list =
  match assoc_list with
  | [] -> ""
  | (key, value) :: t ->
      key ^ " -> " ^ string_of_int value ^ "\n"
      ^ pp_print_association_list t

let create_find_frequencies_test
    (name : string)
    (word_json : t)
    (matrix : int array array)
    (expected_output : (string * int) list) : test =
  name >:: fun _ ->
  assert_equal
    (List.sort compare expected_output)
    (List.sort compare (find_frequencies word_json matrix))
    ~printer:pp_print_association_list

let test4_matrix = Array.make_matrix 2 5 0

let _ = test4_matrix.(0).(0) <- 1

let _ = test4_matrix.(1).(0) <- 1

let _ = test4_matrix.(1).(3) <- 1

let statistics_tests =
  [
    create_find_frequencies_test "different words" test3_json
      test3_matrix
      [
        ("format", 1); ("hello", 1); ("correctly", 0); ("this", 0);
        ("did", 0);
      ];
    create_find_frequencies_test "repeated word" test3_json test4_matrix
      [
        ("hello", 2); ("format", 1); ("correctly", 0); ("this", 0);
        ("did", 0);
      ];
  ]

let cornell_encoded =
  encode_subreddit
    ("data/subredditVocabJsons/cornell.json" |> Yojson.Basic.from_file)
    WordProcessor.stem_text
    (Yojson.Basic.from_file "data/cornell.json")
    upvotes

let cornell_matrix = create_matrix cornell_encoded

let get_vocab_length matrix =
  match matrix with
  | h :: t -> Array.length h
  | [] -> 0

let get_training_data_shapes train_test_data =
  [
    Mat.shape train_test_data.features_training;
    Mat.shape train_test_data.features_test;
    Mat.shape train_test_data.output_training;
    Mat.shape train_test_data.output_testing;
  ]

let pp_int_pair (row, col) =
  "(" ^ string_of_int row ^ ", " ^ string_of_int col ^ ")"

let create_get_training_data_test
    (name : string)
    (matrix : Owl.Mat.mat)
    (percent_training : float)
    (expected_output : (int * int) list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (get_training_data_shapes
       (get_training_data matrix percent_training))
    ~printer:(pp_list pp_int_pair)

let regression_tests =
  [
    create_get_training_data_test "check number of columns"
      cornell_matrix 0.75
      [ (21, 483); (7, 483); (21, 1); (7, 1) ];
  ]

let food_theme_json =
  "data" ^ Filename.dir_sep ^ "test_themes" ^ Filename.dir_sep ^ "food"
  ^ ".json"
  |> Yojson.Basic.from_file

let school_theme_json =
  "data" ^ Filename.dir_sep ^ "test_themes" ^ Filename.dir_sep
  ^ "school" ^ ".json"
  |> Yojson.Basic.from_file

let theme_test_post =
  {
    author = "maria";
    created_utc = 5.;
    subreddit = "yoyo";
    id = "identifier";
    num_comments = 30;
    num_crossposts = 25;
    selftext =
      "I prefer noodles, especially hand-ripped spicy noodles. At \
       school, the noodles aren't very good and tend to be salty so I \
       opt to get rice instead";
    spoiler = false;
    title = "Noodles over rice ?";
    upvotes = 37;
  }

let theme_table_of_test_post =
  let h = Hashtbl.create 8 in
  Hashtbl.add h "food" 8;
  Hashtbl.add h "music" 0;
  Hashtbl.add h "school" 1;
  Hashtbl.add h "tv" 0;
  h

let num_theme_of_word_posts_test
    (name : string)
    (post : Intake.post)
    (theme_json : t)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (ThemeEncoder.num_theme_words_of_post post theme_json)
    ~printer:string_of_int

let theme_table_of_post_test
    (name : string)
    (post : Intake.post)
    (themes : string array)
    (expected_output : (string, int) Hashtbl.t) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (theme_table_of_post post themes "test_themes")

let theme_array =
  let themes = Array.make 4 "" in
  themes.(0) <- "food";
  themes.(1) <- "music";
  themes.(2) <- "school";
  themes.(3) <- "tv";
  themes

let percentage_breakdown_of_test_post =
  [
    ("food", 28.); ("miscellaneous", 69.); ("music", 0.);
    ("school", 3.); ("tv", 0.);
  ]

let theme_breakdown_of_post_test
    (name : string)
    (post : Intake.post)
    (theme_table : (string, int) Hashtbl.t)
    (expected_output : (string * float) list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (theme_breakdown_of_post post theme_table)

let theme_encoder_tests =
  [
    num_theme_of_word_posts_test
      "testing post against food theme, should be 8" theme_test_post
      food_theme_json 8;
    num_theme_of_word_posts_test
      "testing post against school theme,\n       should be 1"
      theme_test_post school_theme_json 1;
    theme_table_of_post_test "generating theme table for test post"
      theme_test_post theme_array theme_table_of_test_post;
    theme_breakdown_of_post_test "testing theme breakdown of test post"
      theme_test_post theme_table_of_test_post
      percentage_breakdown_of_test_post;
  ]

let suite =
  "test suite for Final"
  >::: List.flatten
         [
           intake_tests; word_processor_tests; word_encoding_tests;
           statistics_tests; regression_tests; theme_encoder_tests;
         ]

let _ = run_test_tt_main suite