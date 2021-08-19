// See the file alphabet-cipher.md for detailed information.

open System
open System.Text.RegularExpressions

type Message = string
type Keyword = string

let aAsciiValue = 97

let letterToIndex (letter:char) =
    letter
    |> Convert.ToInt32
    |> (fun i -> i - aAsciiValue)

let indexToLetter (index:int) =
    index
    |> (fun i -> i + aAsciiValue)
    |> Convert.ToChar

let matrix =
    Array2D.init 26 26 (fun i j -> (i + j) % 26 |> indexToLetter)

// Repeat the keywork as many times as necessary to get tothe  desired length
let createKeyOfLength (key:Keyword) (desiredlength:int) : Keyword =
    let replicateTimes =
        (float desiredlength) / (float key.Length) 
        |> Math.Ceiling
        |> int
    key
    |> String.replicate replicateTimes
    |> (fun s -> s.Substring(0, desiredlength))

let encode (key:Keyword) (message:Message) : Message =
    let messageArray = message.ToLowerInvariant().ToCharArray()
    let keyArray =
        createKeyOfLength (key.ToLowerInvariant()) message.Length
        |> (fun s -> s.ToCharArray())
    // zip the arrays to get pairs of chars
    Array.zip messageArray keyArray
        |> Array.map (fun (m, k) -> matrix.[(letterToIndex m), (letterToIndex k)] )  // look up the value in the matrix 💊
        |> (fun a -> String(a))

let decode (key:Keyword) (message:Message) : Message =
    let messageArray = message.ToLowerInvariant().ToCharArray()
    let keyArray =
        createKeyOfLength (key.ToLowerInvariant()) message.Length
        |> (fun s -> s.ToCharArray())
    // zip the arrays to get pairs of chars
    Array.zip messageArray keyArray
        |> Array.map (fun (m, k) -> matrix.[(letterToIndex k), *]  // get a whole row
                                    |> Array.findIndex(fun r -> r = m)  // find the matching item
                                    |> indexToLetter)
        |> (fun a -> String(a))

let decipher (cipher:Message) (message:Message) : Keyword =
    let messageArray = message.ToLowerInvariant().ToCharArray()
    let cipherArray = cipher.ToLowerInvariant().ToCharArray()
    let messageKeyword =
        // zip the arrays to get pairs of chars
        Array.zip messageArray cipherArray
            |> Array.map (fun (m, c) -> matrix.[(letterToIndex m), *]  // get a whole row
                                        |> Array.findIndex(fun r -> r = c)
                                        |> indexToLetter)
            |> (fun a -> String(a))
    // Now we need to un-repeat the keyword 🤔
    // https://regex101.com/r/YK2BZu/1 
    Regex.Match(messageKeyword, @"([a-z]+?)\1").Groups.[1].Value


#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

let tests () =

    // verify letter to index and vice versa
    test <@ letterToIndex 'a' = 0 @>
    test <@ letterToIndex 'z' = 25 @>
    test <@ indexToLetter 0 = 'a' @>
    test <@ indexToLetter 25 = 'z' @>

    // verify creating the key for a specific message
    test <@ createKeyOfLength "abc" 1 = "a" @>
    test <@ createKeyOfLength "abc" 3 = "abc" @>
    test <@ createKeyOfLength "abc" 6 = "abcabc" @>
    test <@ createKeyOfLength "abc" 7 = "abcabca" @>

    // verify encoding
    test <@ encode "vigilance" "meetmeontuesdayeveningatseven" = "hmkbxebpxpmyllyrxiiqtoltfgzzv" @>
    test <@ encode "scones" "meetmebythetree" = "egsgqwtahuiljgs" @>

    // verify decoding
    test <@ decode "vigilance" "hmkbxebpxpmyllyrxiiqtoltfgzzv" = "meetmeontuesdayeveningatseven" @>
    test <@ decode "scones" "egsgqwtahuiljgs" = "meetmebythetree" @>

    // verify decyphering
    test <@ decipher "opkyfipmfmwcvqoklyhxywgeecpvhelzg" "thequickbrownfoxjumpsoveralazydog" = "vigilance" @>
    test <@ decipher "hcqxqqtqljmlzhwiivgbsapaiwcenmyu" "packmyboxwithfivedozenliquorjugs" = "scones" @>

// run the tests
tests ()
