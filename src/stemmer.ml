let consonants = "bcdfghjklmnpqrstwvxzy"

let vowels = "aeiou"

let tail word = String.sub word 1 (String.length word - 1)

let is_vowel character =
  let lowercase_char = Char.lowercase_ascii character in
  String.contains vowels lowercase_char

let get_last word num = String.sub word (String.length word - num) num

let remove_last word num = String.sub word 0 (String.length word - num)

let rec create_units (word : string) =
  let length = String.length word in
  if length > 0 then
    if is_vowel (String.get word 0) then "V" ^ create_units (tail word)
    else "C" ^ create_units (tail word)
  else ""

let rec create_simplified_units raw_units acc =
  if String.length raw_units > 0 then
    let first_character = Char.escaped (String.get raw_units 0) in
    if String.length acc > 0 && get_last acc 1 = first_character then
      create_simplified_units (tail raw_units) acc
    else create_simplified_units (tail raw_units) (acc ^ first_character)
  else acc

let rec remove_before_v char_string =
  if String.length char_string > 0 then
    if String.get char_string 0 = 'V' then char_string
    else remove_before_v (tail char_string)
  else ""

let rec calc_vc word =
  if String.length word > 0 then
    let char_string =
      create_simplified_units
        (create_units (String.lowercase_ascii word))
        ""
    in
    let get_vc = remove_before_v char_string in
    if get_vc = "CV" then 0 else String.length get_vc / 2
  else 0

let remove_plurals word =
  let len = String.length word in
  if len >= 4 && get_last word 4 = "sses" then remove_last word 2
  else if len >= 3 && get_last word 3 = "ies" then remove_last word 2
  else if len >= 2 && get_last word 2 = "ss" then word
  else if len >= 2 && get_last word 1 = "s" then remove_last word 1
  else word

let rec contains_vowel word =
  if String.length word = 0 then false
  else if String.contains vowels (String.get word 0) then true
  else contains_vowel (tail word)

let remove_past_participles word =
  let len = String.length word in
  if
    len >= 3
    && calc_vc (remove_last word 1) > 0
    && get_last word 3 = "eed"
  then remove_last word 1
  else if len >= 3 && get_last word 3 = "eed" then word
  else if
    len >= 2
    && get_last word 2 = "ed"
    && contains_vowel (remove_last word 2)
  then remove_last word 2
  else if
    len >= 3
    && get_last word 3 = "ing"
    && contains_vowel (remove_last word 3)
  then remove_last word 3
  else word

let apply_finalize word =
  let len = String.length word in
  if len >= 3 && get_last word 3 = "eed" then false
  else if
    len >= 2
    && get_last word 2 = "ed"
    && contains_vowel (remove_last word 2)
  then true
  else if
    len >= 3
    && get_last word 3 = "ing"
    && contains_vowel (remove_last word 3)
  then true
  else false

let end_cvc word =
  let units = create_units word in
  if String.length units >= 3 then
    let last_three = get_last units 3 in
    let last_letter = get_last word 1 in
    if
      last_three = "CVC" && last_letter <> "w" && last_letter <> "x"
      && last_letter <> "y"
    then true
    else false
  else false

let double_consonant word =
  let last_two = get_last word 2 in
  let second_to_last = remove_last last_two 1 in
  let last = get_last word 1 in
  second_to_last = last

let finalize_plurals_past_participles word =
  let len = String.length word in
  if len > 0 && end_cvc word && calc_vc word = 1 then word ^ "e"
  else if len >= 2 then
    let last_two = get_last word 2 in
    if last_two = "at" || last_two = "bl" || last_two = "iz" then
      word ^ "e"
    else if double_consonant word then remove_last word 1
    else word
  else word

let fix_y word =
  if contains_vowel word && get_last word 1 = "y" then
    remove_last word 1 ^ "i"
  else word

let remove_e word =
  let length = String.length word in
  if
    length > 0
    && calc_vc (remove_last word 1) > 1
    && get_last word 1 = "e"
  then remove_last word 1
  else if
    length > 0
    && calc_vc (remove_last word 1) = 1
    && end_cvc (remove_last word 1) = false
    && get_last word 1 = "e"
  then remove_last word 1
  else word

let check_double_consonant word =
  let length = String.length word in
  if
    length > 0
    && calc_vc (remove_last word 1) > 1
    && double_consonant word
    && get_last word 1 <> "l"
    && get_last word 1 <> "s"
    && get_last word 1 <> "z"
  then remove_last word 1
  else word
