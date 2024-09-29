[<RequireQualifiedAccess>]
module Functional.String

open System
open System.Globalization
open System.Text

open Functional

/// Represents the empty string.
let empty = String.Empty

/// <summary>
/// Indicates whether the specified string is either null or an empty string.
/// </summary>
/// <param name="value">The string to test.</param>
/// <returns>True if the specified string is either null or an empty string.</returns>
let inline isNullOrEmpty value =
    String.IsNullOrEmpty value

/// <summary>
/// Indicates whether the specified string is either null or consists entirely of whitespace characters.
/// </summary>
/// <param name="value">The string to est.</param>
/// <returns>True if specified string is either null or consists entirely of whitespace characters.</returns>
let inline isNullOrWhitespace value =
    String.IsNullOrWhiteSpace value

/// <summary>
/// Returns the specified string value unless it is null, in which case an empty string is returned instead.
/// </summary>
/// <param name="value">The value to check.</param>
/// <returns>The specified string value unless it is null, in which case an empty string is returned instead.</returns>
let inline emptyOnNull value =
    if isNull value then "" else value

/// <summary>
/// Returns a value indicating whether a specified substring occurs within a specified string.
/// </summary>
/// <param name="substring">The substring to search for.</param>
/// <param name="string">The string to search in.</param>
/// <returns>True if a specified substring occurs within a specified string.</returns>
let contains substring (string: string) =
    if isNull string || isNull substring then
        false
    else
        string.Contains substring

/// <summary>
/// Compares two specified strings and returns an integer that indicates their relative positions in sort order.
/// </summary>
/// <param name="comparision">The string comparison kind to use.</param>
/// <param name="a">The first string to compare.</param>
/// <param name="b">The second string to compare.</param>
/// <returns>Less than zero then <paramref name="a"/> is before <paramref name="b"/>, zero means <paramref name="a"/> <paramref name="b"/> are equal, and greater than zero means <paramref name="a"/> is after <paramref name="b"/>.</returns>
/// <exception cref="System.ArgumentException">The provided comparison method is not a valid <see cref="System.StringComparison"/> value.</exception>
/// <exception cref="System.NotSupportedException">The provided comparison method is not supported.</exception>
let inline compare (comparision: StringComparison) a b =
    String.Compare(a, b, comparision)

/// <summary>
/// Determines whether the start of the specified string instances matches the specified substring.
/// </summary>
/// <param name="substring">The string to search for.</param>
/// <param name="string">The string to search within.</param>
/// <returns>True if the start of the specified string instances matches the specified substring.</returns>
let startsWith substring (string: string) =
    if isNullOrEmpty substring then
        true
    elif isNull string then
        false
    else
        string.StartsWith substring

/// <summary>
/// Determines whether the specified substring occurs within the text at the specified index.
/// </summary>
/// <param name="index">The index to check.</param>
/// <param name="substring">The string to search for.</param>
/// <param name="string">The string to search within.</param>
/// <returns>True if the specified substring occurs within the text at the specified index.</returns>
let containsAtIndex index (substring: string) (string: string) =
    if isNullOrEmpty substring then
        true
    elif isNull string || index < 0 || index + substring.Length > string.Length then
        false
    else
        string.IndexOf(substring, index, substring.Length) = index

/// <summary>
/// Determines whether the start of the specified string instances matches the specified substring when compared using the specified comparision option.
/// </summary>
/// <param name="comparision">The string comparison kind to use.</param>
/// <param name="substring">The string to look for..</param>
/// <param name="string">The string to look within..</param>
/// <returns>True if the start of the specified string instances matches the specified substring when compared using the specified comparision option.</returns>
/// <exception cref="System.ArgumentException">The provided comparison method is not a valid <see cref="System.StringComparison"/> value.</exception>
/// <exception cref="System.NotSupportedException">The provided comparison method is not supported.</exception>
let startsWithComparision comparision substring (string: string) =
    if isNullOrEmpty substring then
        true
    elif isNull string then
        false
    else
        string.StartsWith(substring, comparision)

/// <summary>
/// Determines whether the end of the specified string instances matches the specified substring.
/// </summary>
/// <param name="substring">The string to search for.</param>
/// <param name="string">The string to search within.</param>
/// <returns>True if the end of the specified string instances matches the specified substring.</returns>
let endsWith substring (string: string) =
    if isNull substring then
        true
    elif isNull string then
        false
    else
        string.EndsWith substring

/// <summary>
/// Determines whether the end of the specified string instances matches the specified substring when compared using the specified comparision option.
/// </summary>
/// <param name="comparision">The string comparison kind to use.</param>
/// <param name="substring">The string to look for..</param>
/// <param name="string">The string to look within..</param>
/// <returns>True the end of the specified string instances matches the specified substring when compared using the specified comparision option.</returns>
/// <exception cref="System.ArgumentException">The provided comparison method is not a valid <see cref="System.StringComparison"/> value.</exception>
/// <exception cref="System.NotSupportedException">The provided comparison method is not supported.</exception>
let endsWithComparision comparision substring (string: string) =
    if isNull substring then
        true
    elif isNull string then
        false
    else
        string.EndsWith(substring, comparision)

/// <summary>
/// Determines whether the specified strings are equal when compared using the specified comparision options.
/// </summary>
/// <param name="comparision">The string comparison kind to use.</param>
/// <param name="a">The first string to compare.</param>
/// <param name="b">The second string to compare.</param>
/// <returns>True if the specified strings are equal when compared using the specified comparision options.</returns>
/// <exception cref="System.ArgumentException">The provided comparison method is not a valid <see cref="System.StringComparison"/> value.</exception>
/// <exception cref="System.NotSupportedException">The provided comparison method is not supported.</exception>
let equals comparision a b =
    String.Equals(a, b, comparision)

/// <summary>
/// Reports the zero-based index of the first occurrence of the specified substring within the specified string.
/// </summary>
/// <param name="substring">The string to search for.</param>
/// <param name="string">The string to search within.</param>
/// <returns>The zero-based index of the first occurrence of the specified substring within the specified string.</returns>
let indexOf substring (string: string) =
    if isNullOrEmpty substring then
        0
    elif isNull string then
        -1
    else
        string.IndexOf substring

/// <summary>
/// Reports the zero-based index of the first occurrence of the specified substring within the specified string when compared using the specified comparision options.
/// </summary>
/// <param name="comparision">The string comparision method to use.</param>
/// <param name="substring">The string to search for.</param>
/// <param name="string">The string to search within.</param>
/// <returns>The zero-based index of the first occurrence of the specified substring within the specified string when compared using the specified comparision options.</returns>
/// <exception cref="System.ArgumentException">The provided comparison method is not a valid <see cref="System.StringComparison"/> value.</exception>
/// <exception cref="System.NotSupportedException">The provided comparison method is not supported.</exception>
let indexOfComparision (comparision: StringComparison) (substring: string) (string: string) =
    if isNullOrEmpty substring then
        0
    elif isNull string then
        -1
    else
        string.IndexOf(substring, comparision)

/// <summary>
/// Returns the first index at which one of the specified characters appears or negative one if none of the characters appear within the string.
/// </summary>
/// <param name="chars">The characters to search for.</param>
/// <param name="string">The string to search within.</param>
/// <returns>The first index at which one of the specified characters appears or negative one if none of the characters appear within the string.</returns>
let indexOfAnyChar chars (string: string) =
    if isNull string then -1
    else
        let mutable index = 0
        let mutable found = false
        
        while not found && index < string.Length do
            if chars |> List.exists ((=) string[index]) then
                found <- true
            else
                index <- index + 1
        
        if found then index else -1

/// <summary>
/// Returns the first index at which one of the specified substrings appears or negative if none of the substrings appear within the string.
/// </summary>
/// <param name="substrings">The strings to search for.</param>
/// <param name="string">The strings to search within.</param>
/// <returns>The first index at which one of the specified substrings appears or negative if none of the substrings appear within the string.</returns>
let indexOfAny (substrings: string list) (string: string) =
    if isNull string then -1
    else
        let mutable index = 0
        let mutable found = false
        
        while not found && index < string.Length do
            if
                substrings
                |> List.exists (fun substring ->
                    not (isNull substring) && index + substring.Length <= string.Length && string.Substring(index, substring.Length) = substring
                )
            then
                found <- true
            else
                index <- index + 1
        
        if found then index else -1

/// <summary>
/// Reports the zero-based index of the last occurrence of the specified substring within the specified string.
/// </summary>
/// <param name="substring">The string to search for.</param>
/// <param name="string">The string to search within.</param>
/// <returns>The zero-based index of the last occurrence of the specified substring within the specified string.</returns>
let lastIndexOf (substring: string) (string: string) =
    if isNullOrEmpty substring then
        0
    elif isNull string then
        -1
    else
        string.LastIndexOf substring

/// <summary>
/// Reports the zero-based index of the last occurrence of the specified substring within the specified string when compared using the specified comparision options.
/// </summary>
/// <param name="comparision">The string comparision method to use.</param>
/// <param name="substring">The string to search for.</param>
/// <param name="string">The string to search within.</param>
/// <returns>The zero-based index of the last occurrence of the specified substring within the specified string when compared using the specified comparision options.</returns>
/// <exception cref="System.ArgumentException">The provided comparison method is not a valid <see cref="System.StringComparison"/> value.</exception>
/// <exception cref="System.NotSupportedException">The provided comparison method is not supported.</exception>
let lastIndexOfComparision (comparision: StringComparison) (substring: string) (string: string) =
    if isNullOrEmpty substring then
        0
    elif isNull string then
        -1
    else
        string.LastIndexOf (substring, comparision)

/// <summary>
/// Returns a new string in which all occurrences of a specified Unicode character are replaced with another specified Unicode character.
/// </summary>
/// <param name="old">The character to replace.</param>
/// <param name="new">The character that replaces the old character.</param>
/// <param name="string">The string to perform the replacement upon.</param>
/// <returns>A new string in which all occurrences of a specified Unicode character are replaced with another specified Unicode character.</returns>
let replaceChar (old: char) ``new`` (string: string) =
    if isNull string then
        null
    else
        string.Replace (old, ``new``)

/// <summary>
/// Returns a new string in which all occurrences of a specified string are replaced with another specified string.
/// </summary>
/// <param name="old">The string to replace.</param>
/// <param name="new">The string that will replace <paramref name="old"/>.</param>
/// <param name="string">The string to perform replacements upon.</param>
/// <returns>A new string in which all occurrences of a specified string are replaced with another specified string.</returns>
let inline replace (old: string) ``new`` (string: string) =
    if isNull string then
        null
    elif isNull old then
        string
    else
        string.Replace(old, if isNull ``new`` then "" else ``new``)
        
/// <summary>
/// Performs all the specified replacements.
/// Replacements are not done recursively; instead they are processed strictly linearly from first to last.
/// </summary>
/// <param name="replacements">The replacements to perform.</param>
/// <param name="string">The string to perform the replacements upon.</param>
/// <returns>The input string with all the specified replacements applied.</returns>
/// <exception cref="System.ArgumentOutOfRangeException">Performing a replacement would make the new text longer than <see cref="System.Text.StringBuilder.MaxCapacity"/>.</exception>
let replaceAll (replacements: #seq<string * string>) (string: string) =
    if isNull string then
        null
    elif isNull (box replacements) then
        string
    else
        let replacements = Seq.toArray replacements
        let buffer = StringBuilder string

        for key, value in replacements do
            if not (isNullOrEmpty key) then
                buffer.Replace(key, if isNull value then "" else value) |> ignore
        
        buffer.ToString()

/// <summary>
/// Splits a string into substrings based on the characters in the sequence.
/// </summary>
/// <param name="character">The character to split on.</param>
/// <param name="string">The string to split.</param>
/// <returns>An array of string segments.</returns>
let splitChar (character: char) (string: string) =
    if isNull string then
        [||]
    else
        string.Split character

/// <summary>
/// Splits a string into substrings based on the characters in the sequence based on the split options.
/// </summary>
/// <param name="options">The options to use when splitting the string.</param>
/// <param name="character">The character to split on.</param>
/// <param name="string">The string to split.</param>
/// <returns>An array of string segments.</returns>
/// <exception cref="System.ArgumentException">The provided split options are not a valid <see cref="System.StringSplitOptions"/> value.</exception>
let splitCharOptions (options: StringSplitOptions) (character: char) (string: string) =
    if isNull string then
        [||]
    else
        string.Split (character, options)

/// <summary>
/// Splits a string into substrings based on the characters in the sequence.
/// </summary>
/// <param name="characters">The characters to split on.</param>
/// <param name="string">The string to split.</param>
/// <returns>An array of substrings split on the specified characters.</returns>
let splitChars (characters: #seq<char>) (string: string) =
    if isNull string || isNull (box characters) then
        [||]
    else
        let characters = Seq.toArray characters

        if Array.isEmpty characters then
            [| string |]
        else
            string.Split characters

/// <summary>
/// Splits a string into substrings based on the characters in the sequence and the specified split options.
/// </summary>
/// <param name="options">The options to use when splitting the string.</param>
/// <param name="characters">The characters to split on.</param>
/// <param name="string">The string to split.</param>
/// <returns>An array of string segments.</returns>
/// <exception cref="System.ArgumentException">The provided split options are not a valid <see cref="System.StringSplitOptions"/> value.</exception>
let splitCharsOptions (options: StringSplitOptions) (characters: #seq<char>) (string: string) =
    if isNull string || isNull (box characters) then
        [||]
    else
        let characters = Seq.toArray characters

        if Array.isEmpty characters then
            [| string |]
        else
            string.Split (characters, options)

/// <summary>
/// Splits a string into substrings based on the strings in the sequence.
/// </summary>
/// <param name="separators">The strings to split on.</param>
/// <param name="string">The string to split.</param>
let splitStrings (separators: #seq<string>) (string: string) =
    if isNull string || isNull (box separators) then
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

/// <summary>
/// Splits a string into substrings based on the characters in the sequence and the specified split options.
/// </summary>
/// <param name="options">The options to use when splitting the string.</param>
/// <param name="separators">The strings to split on.</param>
/// <param name="string">The string to split.</param>
/// <returns>An array of string segments.</returns>
/// <exception cref="System.ArgumentException">The provided split options are not a valid <see cref="System.StringSplitOptions"/> value.</exception>
let splitStringsOptions (options: StringSplitOptions) (separators: #seq<string>) (string: string) =
    if isNull string then
        [||]
    else
        let separators = Seq.toArray separators

        if Array.isEmpty separators then
            [| string |]
        else
            string.Split (separators, options)

/// <summary>
/// Splits on a string by the given separator.
/// </summary>
/// <param name="separator">The string to split on.</param>
/// <param name="string">The string to split.</param>
/// <returns>An array of string segments.</returns>
let split (separator: string) (string: string) =
    if isNull string then
        [||]
    else
        string.Split ([| separator |], StringSplitOptions.None)

/// <summary>
/// Splits a string into substrings based on the specified separator. and the specified split options.
/// </summary>
/// <param name="options">The options to use when splitting the string.</param>
/// <param name="separator">The string to split on.</param>
/// <param name="string">The string to split.</param>
/// <returns>An array of string segments.</returns>
/// <exception cref="System.ArgumentException">The provided split options are not a valid <see cref="System.StringSplitOptions"/> value.</exception>
let splitOptions (options: StringSplitOptions) (separator: string) (string: string) =
    if isNull string then
        [||]
    else
        string.Split ([| separator |], options)

/// <summary>
/// Splits a string into substrings by taking everything before the given index as the first string and everything after the given index as the second.
/// </summary>
/// <param name="index">The index to split at.</param>
/// <param name="string">The string to split.</param>
/// <returns>A tuple of everything before the given index as the first element and everything after the given index as the second element.</returns>
let splitAt (index: int) (string: string) =
    if index <= 0 then
        "", string
    elif index >= string.Length then
        string, ""
    else
        (string.Substring(0, index), string.Substring index)
        
/// <summary>
/// Inserts <paramref name="value"/> into the specified string at the given index.
/// </summary>
/// <param name="index">The index to insert at.</param>
/// <param name="value">The value to insert.</param>
/// <param name="string">The string to insert into.</param>
/// <returns>The given string with the given value inserted at the specified index.</returns>
let insertAt (index: int) (value: string) (string: string) =
    if isNull string && index = 0 then
        if isNull value then "" else value
    else
        let builder = StringBuilder()
        let index = clamp index 0 string.Length
        
        builder
            .Append(string.Substring(0, index))
            .Append(if isNull value then "" else value)
            .Append(string.Substring(index))
            |> ignore
            
        builder.ToString()

/// <summary>
/// Returns the specified substring of the specified string.
/// </summary>
/// <param name="length">The optional length of the substring to create.</param>
/// <param name="startIndex">The index at which to start the substring.</param>
/// <param name="string">The string to take a slice from.</param>
/// <returns>The specified substring.</returns>
let substring (length: int option) (startIndex: int) (string: string) =
    if isNull string then
        ""
    else
        match length with
        | Some length ->
            string.Substring(min string.Length startIndex, min length (string.Length - startIndex))
        | None ->
            string.Substring (min string.Length startIndex)

/// <summary>
/// Returns the first <paramref name="count"/> characters from the string.
/// </summary>
/// <param name="count">The number of characters to return.</param>
/// <param name="string">The string to fetch the characters from.</param>
/// <returns>A substring of length count.</returns>
/// <exception cref="System.ArgumentOutOfRangeException"><paramref name="count"/> was less than zero or greater than the length of the string.</exception>
let take (count: int) (string: string) =
    if isNull string && count = 0 then
        null
    elif isNull string || count < 0 || count > string.Length then
        argumentOutOfRange (nameof count) $"{nameof count} must be greater than or equal to zero and less than the size of the string. To return at most {nameof count} elements, use String.truncate instead."
    else
        string.Substring(0, count)

/// <summary>
/// Returns all but the first <paramref name="count"/> characters from the string.
/// </summary>
/// <param name="count">The number of characters to skip.</param>
/// <param name="string">The string to skip characters from.</param>
/// <returns>All but the first <paramref name="count"/> characters of the input string.</returns>
let skip (count: int) (string: string) =
    if isNull string && count = 0 then
        null
    elif isNull string || count > string.Length then
        argumentOutOfRange (nameof count) $"{nameof count} must be less than the size of the string. To skip at most {nameof count} elements, use String.drop instead."
    elif count <= 0 then
        string
    else
        string.Substring count

/// <summary>
/// Returns at most the first <paramref name="count"/> characters from the string.
/// </summary>
/// <param name="count">The maximum number of characters to return.</param>
/// <param name="string">The string to fetch the characters from.</param>
/// <returns>A substring of at most <paramref name="count"/> characters.</returns>
let truncate (count: int) (string: string) =
    if isNull string then
        null
    elif count <= 0 then
        ""
    elif count < string.Length then
        string.Substring(0, count)
    else
        string
      
/// <summary>
/// Returns the result of skipping no more than, but potentially less than, <paramref name="count"/> characters from the string.
/// </summary>
/// <param name="count">The maximum number of characters to skip.</param>
/// <param name="string">The string to skip characters from.</param>
/// <returns>The result of skipping no more than, but potentially less than, <paramref name="count"/> characters from the string.</returns>
let drop (count: int) (string: string) =
    if isNull string then
        null
    elif count <= 0 then
        string
    elif count < string.Length then
        string.Substring count
    else
        ""

/// <summary>
/// Removes all leading and trailing whitespace characters from the specified string.
/// </summary>
/// <param name="string">The string to trim.</param>
/// <returns>The string will all leading and trailing whitespace removed.</returns>
let inline trim (string: string) =
    if isNull string then
        null
    else
        string.Trim()

/// <summary>
/// Removes all leading and trailing occurrences of the specified set of characters.
/// </summary>
/// <param name="chars">The characters to remove.</param>
/// <param name="string">The string to trim.</param>
/// <returns>The string with all occurrences of the specified set of characters at the start or end removed.</returns>
let trimChars (chars: #seq<char>) (string: string) =
    if isNull string then
        string
    else
        string.Trim (Seq.toArray chars)

/// <summary>
/// Removes all leading occurrences of the specified set of characters.
/// </summary>
/// <param name="chars">The characters to remove.</param>
/// <param name="string">The string to trim.</param>
/// <returns>The string with all occurrences of the specified set of characters at the start removed.</returns>
let trimStartChars (chars: #seq<char>) (string: string) =
    if isNull string then
        string
    else
        string.TrimStart (Seq.toArray chars)

/// <summary>
/// Removes all trailing occurrences of the specified set of characters.
/// </summary>
/// <param name="chars">The characters to remove.</param>
/// <param name="string">The string to trim.</param>
/// <returns>The string with all occurrences of the specified set of characters at the end removed.</returns>
let trimEndChars (chars: #seq<char>) (string: string) =
    if isNull string then
        string
    else
        string.TrimEnd (Seq.toArray chars)

/// <summary>
/// Returns a copy of the specified string converted to lowercase using <c>CultureInfo.InvariantCulture</c>.
/// </summary>
/// <param name="string">The string to convert.</param>
/// <returns>A copy of the specified string converted to lowercase using <c>CultureInfo.InvariantCulture</c>.</returns>
let toLower (string: string) =
    if isNull string then
        null
    else
        string.ToLower CultureInfo.InvariantCulture
        
/// <summary>
/// Returns a copy of the specified string converted to lowercase using the specified culture.
/// </summary>
/// <param name="culture">The culture specific casing rules to use.</param>
/// <param name="string">The string to convert.</param>
/// <returns>A copy of the specified string converted to lowercase using the specified culture.</returns>
let toLowerCulture (culture: CultureInfo) (string: string) =
    if isNull string then
        null
    else
        string.ToLower culture

/// <summary>
/// Returns a copy of the specified string converted to uppercase using <c>CultureInfo.InvariantCulture</c>.
/// </summary>
/// <param name="string">The string to convert.</param>
/// <returns>A copy of the specified string converted to uppercase using <c>CultureInfo.InvariantCulture</c>.</returns>
let toUpper (string: string) =
    if isNull string then
        null
    else
        string.ToUpper CultureInfo.InvariantCulture
        
/// <summary>
/// Returns a copy of the specified string converted to uppercase using the specified culture.
/// </summary>
/// <param name="culture">The culture specific casing rules to use.</param>
/// <param name="string">The string to convert.</param>
/// <returns>A copy of the specified string converted to uppercase using the specified culture.</returns>
let toUpperCulture (culture: CultureInfo) (string: string) =
    if isNull string then
        null
    else
        string.ToUpper culture

/// <summary>
/// Converts a string to title case, that is, capitalizes each word using the specified culture.
/// </summary>
/// <param name="culture">The culture specific casing rules to use.</param>
/// <param name="string">The string to convert to title case.</param>
/// <returns>The string in title case.</returns>
let toTitleCulture (culture: CultureInfo) (string: string) =
    if isNull string then
        null
    else
        string
        |> splitChar ' '
        |> Array.map (fun word ->
            if word.Length <= 1 then
                word.ToUpper culture
            else
                Char.ToUpper(word[0], culture).ToString() + word[1..]
        )
        |> String.concat " "

/// <summary>
/// Converts a string to title case, that is, capitalizes each word using <c>System.CultureInfo.InvariantCulture</c>.
/// </summary>
/// <param name="string">The string to convert to title case.</param>
/// <returns>The string in title case.</returns>
let toTitle (string: string) =
    toTitleCulture CultureInfo.InvariantCulture string

/// <summary>
/// Copies the characters from the string into a Unicode character array.
/// </summary>
/// <param name="string">The string to copy the characters from.</param>
/// <returns>The string as an array of Unicode characters.</returns>
let toCharArray (string: string) =
    if isNull string then
        [||]
    else
        string.ToCharArray()

/// <summary>
/// Copies the characters from the specified section of the string into a Unicode character array.
/// </summary>
/// <param name="start">The starting index of the slice.</param>
/// <param name="length">The length of the slice.</param>
/// <param name="string">The string to take the slice from.</param>
/// <returns>The specified slice of the string as a character array.</returns>
let toSliceArray (start: int) (length: int) (string: string) =
    if isNull string then
        [||]
    else
        string.ToCharArray (max start 0, min length (string.Length - start))

/// <summary>
/// Returns the first character of the string.
/// </summary>
/// <param name="string">The string to fetch the first character from.</param>
/// <returns>The first character of the string.</returns>
/// <exception cref="System.NullArgumentException"><paramref name="string"/> was null.</exception>
/// <exception cref="System.InvalidArgumentException"><paramref name="string"/> was empty.</exception>
let inline head (string: string) =
    if isNull string then
        nullArg (nameof string)
    else if string = "" then
        invalidArg (nameof string) "The input string was empty."
    else
        string[0]

/// <summary>
/// Returns the first character of the string, or ValueNone if the string is null or empty.
/// </summary>
/// <param name="string">The string to fetch the character from.</param>
/// <returns>The first character of the string, or ValueNone if the string is null or empty.</returns>
let inline tryHead (string: string) =
    match string with
    | null | "" -> ValueNone
    | _ -> ValueSome string[0]

/// <summary>
/// Indexes into the string. The first character has index 0.
/// </summary>
/// <param name="index">The index of the character to fetch.</param>
/// <param name="string">The string to index into.</param>
/// <returns>The character at the specified index.</returns>
/// <exception cref="System.NullArgumentException"><paramref name="string"/> was null.</exception>
/// <exception cref="System.IndexOutOfRangeException"><paramref name="index"/> was less than zero or greater than the length of the string.</exception>
let inline item index (string: string) =
    if isNull string then
        nullArg (nameof string)
    elif index < 0 || index >= string.Length then
        indexOutOfRange $"{nameof index} must be greater than or equal to zero and less than the length of the string."
    else
        string[index]

/// <summary>Tries to find the nth character in the string. Returns <c>ValueNone</c> if index is negative or the string does not contain enough characters.</summary>
/// <param name="index">The index of the character to fetch.</param>
/// <param name="string">The string to fetch the character from.</param>
/// <returns>The nth character from the string or <c>ValueNone</c> if the string was null or the index was invalid.</returns>
let inline tryItem index (string: string) =
    if isNull string || index < 0 || index >= string.Length then
        ValueNone
    else
        ValueSome string[index]
      
/// <summary>
/// Reverses the specified string.
/// </summary>
/// <param name="text">The string to reverse.</param>
/// <returns>The reverse of the input string.</returns>
let rev (text: string) =
    if isNull text then
        null
    else
        toCharArray text
        |> Array.rev
        |> String

/// <summary>
/// Determines if the string is a palindrome or not.
/// </summary>
/// <param name="text">The string to check.</param>
/// <returns>True if the string is a plaindrome.</returns>
let isPalindrome (text: string) = text = rev text