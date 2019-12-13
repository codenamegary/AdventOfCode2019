// Learn more about F# at http://fsharp.org
let input = [|
    139301
    84565
    124180
    133902
    138726
    62665
    142967
    95598
    118044
    73234
    76476
    51634
    71582
    63619
    148430
    134733
    101537
    101140
    144543
    102233
    62048
    128633
    130113
    92531
    73820
    54964
    103485
    96364
    104119
    121954
    79215
    99235
    120179
    69237
    145584
    79193
    50684
    146481
    67783
    112741
    85024
    62298
    54083
    137704
    116561
    76862
    81410
    96341
    89992
    132926
    97955
    74751
    147553
    121496
    113303
    119671
    120871
    114278
    125628
    144275
    78826
    87092
    65883
    87517
    93974
    55358
    100922
    113304
    115728
    144556
    91728
    86367
    55283
    101841
    55454
    140703
    70706
    98173
    106920
    126984
    148960
    77909
    128304
    140036
    81044
    141419
    126770
    52787
    115783
    128647
    125986
    124506
    113935
    142203
    106404
    78433
    146573
    68575
    63563
    115616
|]

// Formula for calculating required fuel for a given mass
let fuelCalc n = ( n / 3 ) - 2

// runningTotal param is just for compatibility when swapping for the recursive implementation
let massFuel ( mass, runningTotal ) = fuelCalc mass

// Function to recursively determine mass fuel
let rec moduleFuel ( mass, runningTotal ) =
    let requiredFuel = massFuel ( mass, runningTotal )
    if requiredFuel < 1 then
        runningTotal
    else
        moduleFuel ( requiredFuel, runningTotal + requiredFuel )

// Function to sump up results of specified calcFunc
let calcTotalFuel ( i, calcFunc ) = Seq.ofArray i |> Seq.map( fun f -> calcFunc ( f, 0 ) ) |> Seq.sum

// Calculations for part 1 and 2
let totalMassFuel = calcTotalFuel ( input, massFuel )
let totalModuleFuel = calcTotalFuel ( input, moduleFuel )

[<EntryPoint>]
let main argv =
    printfn "Part 1 answer is %i\nPart 2 answer is %i" totalMassFuel totalModuleFuel
    0 // return an integer exit code
