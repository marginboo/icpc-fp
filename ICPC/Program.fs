module ICPC
open System

let commaSprinkler input =
    let senlist = input.ToString().Split(' ') //get array of words from input excluding spaces
    let inputlist = Seq.toList senlist // turn array into list
    let firstelement = false  
    
    let allowedChars (string:string) = // make sure input contains the correct chars using ASCII values
        let charlist = (Seq.toList (string))
        let rec checkChar (charlist:char list) = 
            match charlist with
            |h::t -> match h|>int >=97 && h|>int <= 122 || h = ' ' || h = ',' || h = '.' with
                     | true -> checkChar t
                     | false -> None
            |[] -> Some true
        checkChar charlist
    
    let rec twoChars (input:string list) = // make sure that each word is at least two chars
        match input with 
        |h::t -> match h.Length >=2 with
                 |true -> match allowedChars h with 
                          | Some true -> twoChars t
                          | _ -> None
                 |false -> None
        |[] -> Some true

    let lastStop (input:string list) = // make sure the end of a string is a full stop
        let lastWord = List.last input
        match lastWord.EndsWith(".") with 
        | true -> Some true
        | false -> None 
    
    let firstWord (input:string list) = // make sure the string starts with a word 
        match input.Head = "," || input.Head = "." || input.Head = "" with
        | true -> None
        | false -> Some false
    
    let rec inBetween (input:string list) = // make sure only spaces are between words
        match input with
        | h::t -> match h = "" || h = "." || h = "," || h.Contains("..") || h.Contains(",") && not (h.EndsWith(",")) with
                  | true -> None 
                  | false -> inBetween t 
        | [] -> Some true

    let inputValidation = 
        match twoChars inputlist with
        | Some true -> match lastStop inputlist with
                       | Some true -> match firstWord inputlist with 
                                      | Some false -> inBetween inputlist
                                      | _ -> None
                       | _ -> None 
        | _ -> None

    let checkcomma sentence = 
            let tocheck = Seq.toList sentence
            match (List.contains(',') tocheck) with 
            | true -> true
            | false -> false

    let stripcomma (word:string) =
        word.Trim(',')

    let insertcomma (word:string) =
        String.concat "" [word;","]

    let rec finder wordlist =
        match wordlist with 
        | [] -> input
        | h::t -> match (checkcomma h) with 
                    | true -> let commaword = h.ToString()
                              commaword
                    | false -> finder t
  
    let rec addcomma (nocomma:string) (comma:string) wordlist newlist = // add a comma to all instances of the target word
            match wordlist with 
            | [] -> List.rev newlist
            | h::t -> let replacee = h.ToString()
                      match (replacee.Contains(".") || replacee.Contains(",")) with // if that instance already has a comma or full stop skip it
                      | true -> let newlist = h::newlist
                                addcomma nocomma comma t newlist
                      | false -> match h = replacee with 
                                 | true -> let newlist = replacee.Replace(nocomma,comma)::newlist
                                           addcomma nocomma comma t newlist
                                 | false -> let newlist = h::newlist
                                            addcomma nocomma comma t newlist

    let rec precomma wordlist (templist:string list) foundwords = // find a word that has a comma before it
            match wordlist with
            | h::t -> match h.ToString().Contains(",") with
                      | true -> let precword = templist.Head.Trim('.')
                                match List.contains(precword) foundwords with
                                |true -> let templist = h::templist
                                         precomma t templist foundwords  
                                |false -> precword          
                      | false -> let templist = h::templist
                                 precomma t templist foundwords        
            | [] -> "List done" // no preceeding coma 

    let rec ``Find preceeding word to sprinkle`` pword wordlist (templist:string list) element  = // find a word to add a comma to, using the word found to have a comma preceeding it
            match wordlist with 
            | h::t ->  match element with
                       | false -> let templist = h::templist
                                  let firstelement = true
                                  ``Find preceeding word to sprinkle`` pword t templist firstelement
                       | true -> let listead = templist.Head.Remove(templist.Head.IndexOf('.'))
                                 match listead = pword with
                                 |true -> h
                                 |false -> match h.Trim(',','.') = pword.Trim(',','.') with
                                           |true -> match t.Head.Contains(".") || t.Head.Contains(",") with
                                                    | true -> ``Find preceeding word to sprinkle`` pword t templist element
                                                    | false -> t.Head
                                           |false -> match t.IsEmpty with
                                                     |true -> "List Empty" 
                                                     |false -> ``Find preceeding word to sprinkle`` pword t templist element
            | _ -> failwith "examine match case, this should not be reachable"

    let rec runit nocommaword commaword inputlist emptylist foundwords =
        let newlist = addcomma nocommaword commaword inputlist []
        let pword = precomma (List.rev newlist) [] foundwords
        match pword with 
        |"List done" -> Some (String.concat " " (newlist))
        |_-> let foundwords = pword::foundwords // add resulting word to a list of found words so it isnt checked again
             let preceedingword = ``Find preceeding word to sprinkle`` pword (List.rev newlist) [] firstelement
             match preceedingword with
             | "List Empty" -> Some (String.concat " " (newlist))
             | _ -> let preceedingwordcomma = preceedingword|>insertcomma
                    runit preceedingword preceedingwordcomma newlist [] foundwords
    
    let foundwords = []
    let commaword = finder inputlist // find a word with a comma 
    let nocommaword = stripcomma commaword

    match inputValidation with 
    |Some true ->  runit nocommaword commaword inputlist [] foundwords
    |_ -> None
   
let rivers input =
    let wordarray = input.ToString().Split(' ') // split the string into words
    let wordlist = Seq.toList wordarray // convert array to list 

    let onlyLetters (list:string) = // check if the input contains only the desired characters
        let charlist = Seq.toList list
        let rec checkCase (clist:char list) = 
            match clist with 
            |h::t -> match h|>int >=97 && h|>int <= 122 || h = ' ' || h|>int >=65 && h|>int <= 90 with // validation is done by checking ASCII values 
                        |true -> checkCase t
                        |false -> None
            |[]-> Some true
        checkCase charlist
    
    let singleSpace (list:string list) = // check if input has a double space 
        match (List.contains("") list) with 
        | true -> None 
        | _ -> Some true

    let rec longestWord length (list:string list) = // find the longest word in the sentence - used as the minimum line length
        match list with // could be done with a fold
        |h::t -> match h.Length > length with
                 | true -> match h.Length >80 with
                           |false -> longestWord h.Length t
                           |_-> None
                 | _-> longestWord length t
        |[] -> Some length

    let twoWords (list:string list)= // make sure there are at least two words in a sentence
       match List.length list >=2 with
       |true -> Some true
       |false -> None

    let validation = // put all the validation methids into one
        match onlyLetters input with
        |Some true -> match singleSpace wordlist with
                      | Some true -> match longestWord 0 wordlist with
                                     | None  -> None
                                     | _ -> match twoWords wordlist with
                                            | Some true -> Some true
                                            | _ -> None
                      |_-> None
        |_-> None
    
    let findLargest accumulator x = //find the largest value in a list
        match x > accumulator with 
        |true -> x 
        |_ -> accumulator
    
    let maxim malum valum = // finds the largest value in a tuple with an arity of 2 using the second value
        match malum with 
                  |(b,a) -> match valum with
                            | (d,c) -> match c>a with
                                       |true -> (d,c)
                                       |false -> (b,a)
    
    let rec realsplit (list:string list) (newlist:string list) biglist length ogLength = // splits a sentence into a list of strings with the specified line width
        match list with
        | [] -> let correctWidth = String.concat " " (List.rev newlist)
                let biglist = correctWidth::biglist
                List.rev biglist // biglist contains is a list of strings with equal char length
        | h::t -> match h.Length <= length  with // check if word can fit line width
                  | true -> let newlist = h::newlist 
                            realsplit t newlist biglist ((length - h.Length)-1) ogLength // -1 to account for space character
                  | false -> let correctWidth = String.concat " " (List.rev newlist)
                             let biglist = correctWidth::biglist // add new string to list of strings
                             realsplit list [] biglist ogLength ogLength // evaluate remaining words

    let rec comparespace (list:string list) rivercount (startIndex:int) fullList (rivercountlist:int list) backwards= // finds the largest river given the length of a list
        match list with 
        | [] -> rivercountlist |> List.fold findLargest 0 //return largest value in the list
        | h::t -> match h.IndexOf(' ',startIndex) with 
                  | -1 -> comparespace t 0 0 t rivercountlist backwards // no space found in string from given index, remove head from fulllist
                  | _ -> let targetIndex = h.IndexOf(' ',startIndex)
                         let targetIndexplus =  targetIndex+1
                         let targetIndexminus = targetIndex-1
                         match t.IsEmpty with // if the tail is empty return the value immeadiatly
                         |true -> match rivercount with
                                  | 0 ->  1::rivercountlist |> List.fold findLargest 0 // represents one line
                                  | _ ->  rivercountlist |> List.fold findLargest 0
                         |false -> 
                                   let checki = t.Head 
                                   match checki.Length> targetIndex+1 with // check if bottom line is within range of the target index 
                                   | false ->let rivercountlist = (rivercount+1)::rivercountlist
                                             comparespace t 0 0 t rivercountlist backwards
                                   | true -> match checki.[targetIndex] = ' ' || checki.[targetIndexplus] = ' ' || checki.[targetIndexminus] = ' ' with // check if the sentence underneath has a space in the same position or next to the position
                                             |true -> match checki.[targetIndex] = ' ' with
                                                        | true ->   comparespace t (rivercount+1) targetIndex fullList rivercountlist backwards // space in the same place
                                                        | false ->  match checki.[targetIndexplus] = ' ' with
                                                                    |true -> comparespace t (rivercount+1) targetIndexplus fullList rivercountlist backwards // space to the right
                                                                    | false ->let backwards = true //  backwards is a bool to correct the logic of the algorithm without it there would be an infinite loop if a river that treackles backwards was found
                                                                              comparespace t (rivercount+1) targetIndexminus fullList rivercountlist backwards // space to the left
                                             |false ->let rivercountlist = (rivercount+1)::rivercountlist
                                                      match backwards with
                                                      | true -> comparespace fullList 0 (targetIndex+(rivercount+1)) fullList rivercountlist false // go back to the original target index +1 
                                                      | false -> comparespace fullList 0 (targetIndex+1) fullList rivercountlist false
                                                      
    let minLen = match longestWord 0 wordlist with
                     | None -> -1 //validation function will catch this case, -1 is just to bind during build
                     | Some a -> a //unwrap option
    let maxLen = input.Length // maximum line length = length of string

    let rec locateLongestRiver iter max riverlineList = // this function takes in the minimum length for the string and the max and then finds the river count for each line length within the range
        let justified = (realsplit wordlist [] [] iter iter)
        let rivercountlinex = comparespace justified 0 0 justified [] false
        let riverlineList = (iter,rivercountlinex)::riverlineList // add tuple to list (Line length,river length)
        match iter >= max with // check that line length is still in range i.e not longer than the string itself
        |true -> Some (List.rev riverlineList |> List.fold maxim (0,0)) // List is reversed so that it is from lowest line length to highest
        |false -> locateLongestRiver (iter+1) max riverlineList // check rivers in next line length

    match validation with 
    | Some true -> locateLongestRiver minLen maxLen []
    | _ -> None

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
