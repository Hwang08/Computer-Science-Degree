(*
   Patrick Hwang
   CSC345 - ML Homework
   ML-homework.sml
   11/15/18 11:33 AM
*)

(*-------------------------------------------------------------------------------------------------*)
(* Problem 3.3.2. *)
(* Function flips alternate elements of any list *)

fun flip nil = nil
| flip (x::y::xs) = y :: x :: flip xs
| flip (x::xs) = x :: flip xs;


(*-------------------------------------------------------------------------------------------------*)
(* Problem 3.3.3. *)
(* Delete the ith element of a list, if it exists *)

fun deleteIth (L, i) =
 let
      fun delete (nil, i, position) = nil
	| delete (x::xs, i, position) = if i = position then xs else x :: delete (xs, i, position + 1)
  in 
      if i >= 0 then delete (L, i, 1) else L
  end; 


(*-------------------------------------------------------------------------------------------------*)
(* Problem 3.3.10. *)
(* Function converts a word into piglatin *)

fun piglatinize "" = ""
| piglatinize W =
    let
        (* Splits a word into individual characters *)
        val w = explode W;

        (* Checks if a character is a vowel excluding the semi-vowel 'y' *)
        fun isVowel #"a" = true
        | isVowel #"e" = true
        | isVowel #"i" = true
        | isVowel #"o" = true
        | isVowel #"u" = true
        | isVowel _ = false

        (* Checks if a list of characters contains a vowel *)
        fun hasVowel nil = false
        | hasVowel (x::xs) = if isVowel x then true else hasVowel xs

        (* Converts a word beginning with a vowel to piglatin *)
        fun doVowel L = implode L ^ "yay"

        (* Converts a word beginning with a consonant to piglatin *)
        fun doConsonant nil = ""
        | doConsonant (x::xs) = if isVowel (hd xs) then implode (xs @ [x]) ^ "ay" else doConsonant (xs @ [x])
    in
        if isVowel (hd w) then doVowel w else if hasVowel w then doConsonant w else implode w
    end;

