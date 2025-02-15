module Test

open NUnit.Framework
open GameOfLife

[<SetUp>]
let Setup () =
    ()

[<Test>]
let ``Test cell revival`` () =
    let initialBoard = Set.empty
    let newBoard = nextGeneration (Set.ofList [(1, 0); (1, 1); (1, 2)])
    Assert.That(newBoard.Contains (1, 1))

[<Test>]
let ``Test cell removal`` () =
    let initialBoard = Set.ofList [(1, 1); (1, 2); (1, 3)]
    let newBoard = nextGeneration initialBoard
    Assert.That(newBoard.Contains (1, 1), Is.False)

[<Test>]
let ``Test grid update`` () =
    let initialBoard = Set.ofList [(1, 0); (1, 1); (1, 2)]
    let expectedBoard = Set.ofList [(0, 1); (1, 1); (2, 1)]
    let newBoard = nextGeneration initialBoard
    Assert.That(newBoard, Is.EqualTo(expectedBoard))
