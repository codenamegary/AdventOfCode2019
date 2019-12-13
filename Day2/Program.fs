let input = "1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,19,10,23,2,10,23,27,1,27,6,31,1,13,31,35,1,13,35,39,1,39,10,43,2,43,13,47,1,47,9,51,2,51,13,55,1,5,55,59,2,59,9,63,1,13,63,67,2,13,67,71,1,71,5,75,2,75,13,79,1,79,6,83,1,83,5,87,2,87,6,91,1,5,91,95,1,95,13,99,2,99,6,103,1,5,103,107,1,107,9,111,2,6,111,115,1,5,115,119,1,119,2,123,1,6,123,0,99,2,14,0,0".Split(',')
let program = input |> Array.map( int )

// Everything is immutable including arrays, this takes
// some arguments and returns a new array with the item
// at index 'position' updated to the value of newValue
let updateElement ( position, newValue, program: int[] ) =
    program
    |> Array.indexed
    |> Array.map( fun ( i, v ) -> if i = position then newValue else v )

// Functions to perform opcodes 1 and 2
let add ( a, b ) = a + b
let mult ( a, b ) = a * b

// Processes an instruction for the given position
// and returns a new program
let processInstruction ( program: int[], position, calcFunction ) =
    let fromA = program.[program.[position + 1]]
    let fromB = program.[program.[position + 2]]
    let store = program.[position + 3]
    let newValue = calcFunction( fromA, fromB )
    updateElement( store, newValue, program )

// Recursive function that iterates through all of the
// instructions in the program input until halt or error
let rec execute ( program: int[], position ) =
    //printfn "Received instructions %i, %i, %i, %i" program.[position] program.[position + 1] program.[position + 2] program.[position + 3]
    match program.[position] with
    | 99 -> "halted", program, position
    | 1 -> 
        let newProgram = processInstruction( program, position, add )
        execute( newProgram, position + 4 )
    | 2 ->
        let newProgram = processInstruction( program, position, mult )
        execute( newProgram, position + 4 )
    | _ -> "error", program, position

let state, finishedProgram, finishedPosition = execute( program, 0 )

printfn "Part1 finished with state %s at position %i. Value at position 0 is %i" state finishedPosition finishedProgram.[0]

let desiredOutput = 19690720

// Super slow, synchronous, brute force solution for part 2
// Todo: Parallelize and make it snappy
let rec test ( noun, verb ) =
    let newProgram = updateElement( 1, noun, updateElement( 2, verb, program ) )
    let state, finishedProgram, finishedPosition =
        try
            execute( newProgram, 0 )
        with
            | ex -> "exception", newProgram, -1
    // printfn "Testing values %i, %i got %i" noun verb finishedProgram.[0]
    if finishedProgram.[0] = desiredOutput
        then noun, verb, state, finishedProgram, finishedPosition
        else if verb = 1000 && noun = 1000 then noun, verb, "error", finishedProgram, finishedPosition
        else if verb = 1000 && noun < 1000 then test( noun + 1, 0 )
        else test( noun, verb + 1 )

let noun, verb, part2state, part2finprog, part2finpos = test( 0, 0 )

printfn "Part2 finished with state %s at position %i. Value at position 0 is %i." part2state part2finpos part2finprog.[0]
printfn "Part2 answer is 100 * %i + %i = %i" noun verb (100 * noun + verb)

[<EntryPoint>]
let main argv =
    printfn "Done."
    0 // return an integer exit code
