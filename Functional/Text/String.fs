[<RequireQualifiedAccess>]
module Functional.String

open System
open System.Collections.Generic
open System.Text

open Functional

/// Represents the empty string.
let empty = String.Empty

/// Indicates whether the specified string is either null or an empty string.
let inline isNullOrEmpty value =
    String.IsNullOrEmpty value

/// Indicates whether the specified string is either null or consists entirely of whitespace characters.
let inline isNullOrWhitespace value =
    String.IsNullOrWhiteSpace value

/// Returns a value indicating whether a specified substring occurs within a specified string.
let contains substring (string: string) =
    if isNull string || isNull substring then
        false
    else
        string.Contains substring

/// Compares two specified strings and returns an integer that indicates their relative positions in sort order.
let inline compare (comparision: StringComparison) a b =
    String.Compare(a, b, comparision)

/// Determines whether the start of the specified string instances matches the specified substring.
let startsWith substring (string: string) =
    if isNull string || isNull substring then
        false
    else
        string.StartsWith substring

/// Determines whether the start of the specified string instances matches the specified substring when compared using the specified comparision option.
let startsWithComparision comparision substring (string: string) =
    if isNull string || isNull substring then
        false
    else
        string.StartsWith(substring, comparision)

/// Determines whether the end of the specified string instances matches the specified substring.
let endsWith substring (string: string) =
    if isNull string || isNull substring then
        false
    else
        string.EndsWith substring

/// Determines whether the end of the specified string instances matches the specified substring when compared using the specified comparision option.
let endsWithComparision comparision substring (string: string) =
    if isNull string || isNull substring then
        false
    else
        string.EndsWith(substring, comparision)

/// Determines whether the specified strings have the same value when compared using the specified comparision options.
let equals comparision a b =
    String.Equals(a, b, comparision)

/// Reports the zero-based index of the first occurrence of the specified substring within the specified string.
let indexOf substring (string: string) =
    if isNull string || isNull substring then
        -1
    else
        string.IndexOf substring

/// Reports the zero-based index of the first occurrence of the specified substring within the specified string when compared using the specified comparision options.
let indexOfComparision (comparision: StringComparison) (substring: string) (string: string) =
    if isNull string || isNull substring then
        -1
    else
        string.IndexOf(substring, comparision)

/// Reports the zero-based index of the last occurrence of the specified substring within the specified string.
let lastIndexOf (substring: string) (string: string) =
    if isNull string || isNull substring then
        -1
    else
        string.LastIndexOf substring

/// Reports the zero-based index of the last occurrence of the specified substring within the specified string when compared using the specified comparision options.
let lastIndexOfComparision (comparision: StringComparison) (substring: string) (string: string) =
    if isNull string || isNull substring then
        -1
    else
        string.LastIndexOf (substring, comparision)

/// Returns a new string in which all occurrences of a specified Unicode character are replaced with another specified Unicode character.
/// This function will return null if the input is null.
let replaceChar (old: char) ``new`` (string: string) =
    if isNull string then
        null
    else
        string.Replace (old, ``new``)

/// Returns a new string in which all occurrences of a specified string are replaced with another specified string.
/// This function will return null if the input is null.
let inline replace (old: string) ``new`` (string: string) =
    if isNull string then
        null
    elif isNull old then
        string
    else
        string.Replace(old, if isNull ``new`` then "" else ``new``)
        
let replaceAll (replacements: #seq<string * string>) (string: string) =
    if isNull string then
        null
    elif isNull replacements then
        string
    else
        let replacements = Seq.toArray replacements
        let buffer = StringBuilder string.Length
        
        let mutable index = 0
        
        while index < string.Length do
            
            
            index <- index + 1
        
        buffer.ToString()

/// Splits a string into substrings based on the characters in the sequence.
let splitChar (characters: #seq<char>) (string: string) =
    if isNull string then
        [||]
    else
        let characters = Seq.toArray characters

        if Array.isEmpty characters then
            [| string |]
        else
            string.Split characters

/// Splits a string into substrings based on the characters in the sequence and the specified split options.
let splitCharOptions (options: StringSplitOptions) (characters: #seq<char>) (string: string) =
    if isNull string then
        [||]
    else
        let characters = Seq.toArray characters

        if Array.isEmpty characters then
            [| string |]
        else
            string.Split (characters, options)

/// Splits a string into substrings based on the strings in the sequence.
let split (separators: #seq<string>) (string: string) =
    if isNull string then
        [||]
    else
        let separators =
            separators
            |> Seq.filter (isNullOrEmpty >> not)
            |> Seq.toArray

        if Array.isEmpty separators then
            [| string |]
        else
            string.Split (separators, StringSplitOptions.None)

/// Splits a string into substrings based on the characters in the sequence and the specified split options.
let splitOptions (options: StringSplitOptions) (separators: #seq<string>) (string: string) =
    if isNull string then
        [||]
    else
        let separators = Seq.toArray separators

        if Array.isEmpty separators then
            [| string |]
        else
            string.Split (separators, options)

/// Splits a string into substrings by taking everything before the given index as the first string, and everything after the given index as the second.
let splitAt (index: int) (string: string) =
    if index <= 0 then
        "", string
    elif index >= string.Length then
        string, ""
    else
        (string.Substring(0, index), string.Substring index)
        
/// Inserts the specified value into the specified string at the given index.
let insertAt (index: int) (value: string) (string: string) =
    if isNull string && index = 0 then
        value
    else
        let builder = StringBuilder()
        let index = clamp index 0 string.Length
        
        builder
            .Append(string.Substring(0, index))
            .Append(value)
            .Append(string.Substring(index))
            |> ignore
            
        builder.ToString()

/// Returns the specified substring of the specified string.
let substring (length: int option) (startIndex: int) (string: string) =
    if isNull string then
        ""
    else
        match length with
        | Some length ->
            string.Substring(startIndex, length)
        | None ->
            string.Substring startIndex

/// Returns the first n characters from the string.
let take (count: int) (string: string) =
    if isNull string && count = 0 then
        null
    elif isNull string || count < 0 || count > string.Length then
        raise (ArgumentOutOfRangeException $"{nameof count} must be greater than zero and less than the size of the string. To return at most count elements, use String.truncate instead.")
    else
        string.Substring(0, count)

/// Returns all but the first n characters from the string.
let skip (count: int) (string: string) =
    if isNull string && count = 0 then
        null
    elif isNull string || count > string.Length then
        raise (ArgumentOutOfRangeException $"{nameof count} must be less than the size of the string. To skip at most count elements, use String.drop instead.")
    elif count <= 0 then
        string
    else
        string.Substring count
      
/// Returns at most the first n characters from the string.  
let truncate (count: int) (string: string) =
    if isNull string then
        null
    elif count <= 0 then
        ""
    elif count < string.Length then
        string.Substring(0, count)
    else
        string
      
/// Returns the result of skipping no more than, but potentially less than, n characters from the string.  
let drop (count: int) (string: string) =
    if isNull string then
        null
    elif count <= 0 then
        string
    elif count < string.Length then
        string.Substring count
    else
        ""

/// Removes all leading and trailing whitespace characters from the specified string.
let inline trim (string: string) =
    if isNull string then
        null
    else
        string.Trim()

/// Removes all leading and trailing occurrences of the specified set of characters.
let trimChars (chars: #seq<char>) (string: string) =
    if isNull string then
        string
    else
        string.Trim (Seq.toArray chars)

/// Removes all leading occurrences of the specified set of characters.
let trimStartChars (chars: #seq<char>) (string: string) =
    if isNull string then
        string
    else
        string.TrimStart (Seq.toArray chars)

/// Removes all trailing occurrences of the specified set of characters.
let trimEndChars (chars: #seq<char>) (string: string) =
    if isNull string then
        string
    else
        string.TrimEnd (Seq.toArray chars)

/// Returns a copy of the specified string converted to lowercase.
let toLower (string: string) =
    if isNull string then
        null
    else
        string.ToLower()

/// Returns a copy of the specified string converted to uppercase.
let toUpper (string: string) =
    if isNull string then
        null
    else
        string.ToUpper()

/// Copies the characters from the string into a unicode character array.
let toCharArray (string: string) =
    if isNull string then
        [||]
    else
        string.ToCharArray()

/// Copies the characters from the specified section of the string into a unicode character array.
let toSliceArray (start: int) (length: int) (string: string) =
    if isNull string || start < 0 || length > string.Length || length < start then
        [||]
    else
        string.ToCharArray (start, length)

/// Returns the first character of the string.
let head (string: string) =
    if isNull string then
        nullArg (nameof string)
    else if string = "" then
        invalidArg (nameof string) "The input string was empty."
    else
        string.[0]

/// Returns the first character of the string, or None if the string is null or empty.
let tryHead (string: string) =
    match string with
    | null | "" -> None
    | _ -> Some string.[0]

/// Indexes into the string. The first character has index 0.
let item index (string: string) =
    if isNull string then
        '\x00'
    elif index < 0 || index > string.Length then
        raise (IndexOutOfRangeException $"{nameof index} must be greater than zero and less than the length of the string.")
    else
        string.[index]

/// <summary>Tries to find the nth character in the string. Returns <c>None</c> if index is negative or the string does not contain enough characters.</summary>
let tryItem index (string: string) =
    if isNull string || index < 0 || index >= string.Length then
        None
    else
        Some string.[index]