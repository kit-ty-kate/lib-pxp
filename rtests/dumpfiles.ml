

let dump_file name =
  let ch = open_in_bin name in
  let len = in_channel_length ch in
  let sin = String.create len in
  really_input ch sin 0 len;
  close_in ch;

  Printf.printf "\\noindent\\begin{minipage}{5.5cm}\n";
  (* Printf.printf "\\rule{5.5cm}{1pt}\n"; *)
  Printf.printf "\\footnotesize\\bf File %s:\\\\\n" name;
  Printf.printf "\\tt{}";
  
  for i = 0 to len - 1 do
    match sin.[i] with
	('\000'..'\008'|'\011'|'\012'|'\014'..'\031'|'\127'..'\255') as c ->
	  Printf.printf "{\\sl (%02x)}\\linebreak[2]" (Char.code c)
      | '\009' ->
	  Printf.printf "{\\sl HT}\\linebreak[3]"
      | '\010' ->
	  Printf.printf "{\\sl LF}\\\\\n"
      | '\013' ->
	  Printf.printf "{\\sl CR}";
	  if not(i < len - 1 && sin.[i+1] = '\010') then
	    Printf.printf "\\\\\n";
      | ' ' ->
	  Printf.printf "\\symbol{32}\\linebreak[3]"

      | ('"'|'#'|'$'|'%'|'&'|'-'|'<'|'>'|'['|'\\'|']'|'^'|'_'|'`'|
	 '{'|'|'|'}'|'~') as c ->
	  Printf.printf "\\symbol{%d}\\linebreak[2]" (Char.code c)
      | c ->
	  print_char c;
	  print_string "\\linebreak[0]"
  done;

  Printf.printf "\\mbox{}\\\\\n";
  Printf.printf "\\rule{5.5cm}{1pt}\n";
  Printf.printf "\\end{minipage}\n"
;;


print_endline "\\documentclass[a4paper]{article}";
print_endline "\\usepackage{multicol}";
print_endline "\\begin{document}";
print_endline "\\begin{multicols}{2}";
for i = 1 to Array.length(Sys.argv)-1 do
  dump_file Sys.argv.(i)
done;
print_endline "\\end{multicols}";
print_endline "\\end{document}"
;;



