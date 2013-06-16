let rec spew depth = function
  | YamlNode.SCALAR (uri, value) ->
      Printf.printf "!<%s> %s\n" uri value
  | YamlNode.SEQUENCE (tag, seq) ->
      Printf.printf "spewing SEQUENCE (%s)\n" tag ;
      List.iter
        (fun value ->
           Printf.printf "- " ;
           spew (depth + 1) value)
        seq
  | YamlNode.MAPPING (tag, map) ->
      Printf.printf "spewing MAPPING (%s)\n" tag ;
      List.iter
        (fun (key, value) ->
           Printf.printf "? " ;
           spew (depth + 1) key ;
           Printf.printf ": " ;
           spew (depth + 1) value)
        map

let _ =
  let p = YamlParser.make () in
  let v = YamlParser.parse_string p "---\n- not" in
    spew 0 v


