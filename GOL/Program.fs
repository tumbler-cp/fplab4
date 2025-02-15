module GameOfLife

open Gtk
open System.IO

type Cell = int * int

let neighbors (x, y) =
    [ for dx in -1 .. 1 do
          for dy in -1 .. 1 do
              if dx <> 0 || dy <> 0 then
                  yield x + dx, y + dy ]

let nextGeneration (board: Set<Cell>) : Set<Cell> =
    let aliveNeighbors cell =
        neighbors cell |> List.filter (fun n -> board.Contains n) |> List.length

    let candidates =
        board
        |> Set.fold (fun acc cell -> Set.union acc (neighbors cell |> Set.ofList)) board

    candidates
    |> Set.filter (fun cell ->
        if board.Contains cell then
            let n = aliveNeighbors cell
            n = 2 || n = 3
        else
            aliveNeighbors cell = 3)

let cellSize = 16

type Msg =
    | Tick
    | GetState of AsyncReplyChannel<Set<Cell>>
    | ToggleRunning

let parseInitialBoard (filePath: string) : Set<Cell> =
    File.ReadAllLines(filePath)
    |> Array.choose (fun line ->
        match line.Split(',') with
        | [| x; y |] -> Some(int x, int y)
        | _ -> None)
    |> Set.ofArray

[<EntryPoint>]
let main argv =
    Application.Init()
    let window = new Window "Game of Life"
    window.SetDefaultSize(800, 600)
    window.SetPosition WindowPosition.Center

    window.DeleteEvent.Add(fun _ ->
        Application.Quit()
        ())

    let drawingArea: DrawingArea = new DrawingArea()
    window.Add drawingArea

    let initialBoard = parseInitialBoard "initBoard.txt"

    let boardAgent =
        MailboxProcessor.Start(fun inbox ->
            let rec loop (board: Set<Cell>, running: bool) =
                async {
                    let! msg = inbox.Receive()

                    match msg with
                    | Tick when running ->
                        let newBoard = nextGeneration board

                        let _ =
                            GLib.Idle.Add(fun () ->
                                drawingArea.QueueDraw()
                                false)

                        return! loop (newBoard, running)
                    | Tick -> return! loop (board, running)
                    | GetState reply ->
                        reply.Reply board
                        return! loop (board, running)
                    | ToggleRunning -> return! loop (board, not running)
                }

            loop (initialBoard, true))

    drawingArea.Drawn.Add(fun args ->
        let cr = args.Cr
        cr.SetSourceRGB(0.0, 0.0, 0.0)
        cr.Paint()
        cr.SetSourceRGB(1.0, 1.0, 1.0)

        let board = boardAgent.PostAndReply(fun reply -> GetState reply)
        let offsetX, offsetY = 50, 50

        board
        |> Set.iter (fun (x, y) ->
            let x' = float (x * cellSize + offsetX)
            let y' = float (y * cellSize + offsetY)
            let size = float cellSize
            cr.Rectangle(x', y', size, size)
            cr.Fill())

        ())

    let _ =
        GLib.Timeout.Add(
            100u,
            fun () ->
                boardAgent.Post Tick
                true
        )

    window.KeyPressEvent.Add(fun args ->
        if args.Event.Key = Gdk.Key.space then
            boardAgent.Post ToggleRunning

        ())

    window.ShowAll()
    Application.Run()
    0
