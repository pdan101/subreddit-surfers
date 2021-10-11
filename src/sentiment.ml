(** taken from
    https://github.com/Duncan-McD/OCamlStocks/blob/5d2c3d62decf7ee3ae4265655f014b0f34961ada/src/parser.ml*)
let connotation_str str =
  Py.initialize ();
  let vader = Py.import "vaderSentiment.vaderSentiment" in
  let sentAnalyzer =
    Py.Module.get_function vader "SentimentIntensityAnalyzer" [||]
  in
  let polarityScore =
    Py.Module.get_function sentAnalyzer "polarity_scores"
  in
  let resultDict = polarityScore [| Py.String.of_string str |] in
  let compound =
    Py.Dict.get_item resultDict (Py.String.of_string "compound")
  in
  let result =
    match compound with
    | None -> failwith "impossible"
    | Some x -> Py.Float.to_float x
  in
  Py.finalize ();
  result