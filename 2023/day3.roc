# part 1 works, part 2 does not.

app "day2"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        array: "https://github.com/mulias/roc-array2d/releases/download/v0.1.0/ssMT0bDIv-qE7d_yNUyCByGQHvpNkQJZsGUS6xEFsIY.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        array.Array2D.{ Array2D, Index },
        "input/3.txt" as file : Str,
        "input/3ex.txt" as example : Str,
    ]
    provides [main] to pf

main : Task {} I32
main =
    Stdout.line "Part 1: \(part1 file)\nPart 2: _"

part1 = \input ->
    input
    |> Str.split "\n"
    |> List.map Str.toUtf8
    |> Array2D.fromLists FitShortest
    |> getAdjArray
    |> Array2D.toLists
    |> List.map getNumbersInList
    |> dbge
    |> List.join
    |> List.sum
    |> Num.toStr


getNumbersInList : List [AdjDigit Nat, Digit Nat, None] -> List Nat
getNumbersInList = \list -> 
    dbg list

    list 
    |> List.append None # this forces the last number to be collected into the first component of the tuple
    |> List.walk ([], 0, Bool.false) \(nums, curr, isAdj), elem -> 
        when elem is 
            None -> 
                if curr != 0 && isAdj
                then (List.append nums curr, 0, Bool.false)
                else (nums, 0, Bool.false)
            Digit val -> 
                (nums, curr * 10 + val, isAdj)
            AdjDigit val -> 
                (nums, curr * 10 + val, Bool.true)    
    |> .0



getAdjArray : Array2D U8 -> Array2D [AdjDigit Nat, Digit Nat, None]
getAdjArray = \array ->
    Array2D.mapWithIndex array \elem, index ->
        if List.contains zeroThroughNine elem then 
            hasNeighbor = List.any (getAdjacentIndices index) \i ->
                when Array2D.get array i is
                    Err _ -> Bool.false
                    Ok val -> !(List.contains zeroThroughNine val) && val != '.'
            if
                hasNeighbor
            then
                AdjDigit (Num.toNat elem - 48)
            else
                Digit (Num.toNat elem - 48)
        else 
            None

zeroThroughNine : List U8
zeroThroughNine =
    List.range {start: At 48, end: At 57}


getAdjacentIndices : Index -> List Index
getAdjacentIndices = \{x: natX, y: natY} -> 
    # Converting to signed to allow possible negative values and then converting back to unsigned
    x = Num.toI32 natX
    y = Num.toI32 natY
    signedCoords = [
        { x: x - 1, y: y - 1 },
        { x: x, y: y - 1 },
        { x: x + 1, y: y - 1 },
        { x: x - 1, y: y },
        { x: x + 1, y: y },
        { x: x - 1, y: y + 1 },
        { x: x, y: y + 1 },
        { x: x + 1, y: y + 1 },
    ]

    signedCoords 
    |> List.dropIf \index ->
        index.x < 0 || index.y < 0
    |> List.map \index -> 
        {x: Num.toNat index.x, y: Num.toNat index.y}


part2 = \input ->
    input
    |> Str.split "\n"
    |> List.map Str.toUtf8
    |> Array2D.fromLists FitShortest
    |> getGearRatioAdjArray
    # |> Array2D.toLists
    # |> List.map getNumbersInList
    # |> dbge
    # |> List.join
    # |> List.sum
    # |> Num.toStr


getGearRatioAdjArray = \array ->
    # dbg "in method"
    Array2D.walk array [] {direction: Forwards} \state, elem, index -> 
        # dbg "in when"
        when elem is
            '*' -> 
                # dbg "in branch"
                adjacentNumbers = getAdjacentIndices index 
                    |> List.keepIf \i ->
                        when Array2D.get array i is
                            Ok x if List.contains zeroThroughNine x -> Bool.true
                            _ -> Bool.false
                    |> List.map \i -> 
                        getLeftDigits [] array i
                        |> List.append (Array2D.get array i |> unwrap)
                        |> List.concat (getRightDigits [] array i)

                dbg adjacentNumbers

                state
                
            _ -> state


getLeftDigits = \state, array, {x,y} -> 
    if x == 0
    then state
    else 
        when Array2D.get array {x: x-1,y} is
            Ok digit if List.contains zeroThroughNine digit -> getLeftDigits (List.prepend state digit) array {x: x-1,y}
            _ -> state

getRightDigits = \state, array, {x,y} -> 
    when Array2D.get array {x: x+1,y} is
        Ok digit if List.contains zeroThroughNine digit -> getRightDigits (List.append state digit) array {x: x+1,y}
        _ -> state


unwrap = \r ->
    when r is
        Err _ -> crash "unwrap encountered an Err"
        Ok val -> val

dbge = \x -> 
    dbg x 
    x