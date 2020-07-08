namespace Sudoku.Extensions

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Array2D =
    let toSeq ar =
        seq {
            for x = 0 to (Array2D.length1 ar) - 1 do
                for y = 0 to (Array2D.length2 ar) - 1 do
                    yield ar.[x, y]
        }

    let flat (ar: 'a [,]): 'a [] =
        ar |> toSeq |> Array.ofSeq

    let toArrayOfArrays (ar: 'a [,]) =
        [| for x = 0 to (Array2D.length1 ar) - 1 do
            [| for y = 0 to (Array2D.length2 ar) - 1 do
                yield ar.[x, y] |] |]

    let fromArrayOfArrays (ar: 'a [] []): 'a [,] =
        let h, w = ar.Length, ar.[0].Length

        let newAr = Array2D.zeroCreate h w

        Array.iteri (fun y row -> Array.iteri (fun x el -> Array2D.set newAr y x el) row) ar
        newAr

    let zip (ar1: 'a [,]) (ar2: 'b [,]): ('a * 'b) [,] =
        let w1, h1 = ar1.GetLength(0), ar1.GetLength(1)
        let w2, h2 = ar2.GetLength(0), ar2.GetLength(1)

        if (w1, h1) <> (w2, h2) then failwith "Arrays have different size"

        [| for y = 0 to h1 - 1 do
            [| for x = 0 to w1 - 1 do
                ar1.[y, x], ar2.[y, x] |] |]
        |> fromArrayOfArrays

    let swapRows (first: int) (second: int) (ar: 'a [,]): 'a [,] =
        let length = ar.GetLength(1)

        for x = 0 to length - 1 do
            let tmp = ar.[first, x]
            ar.[first, x] <- ar.[second, x]
            ar.[second, x] <- tmp
        ar

    let swapCols (first: int) (second: int) (ar: 'a [,]): 'a [,] =
        let length = ar.GetLength(0)

        for y = 0 to length - 1 do
            let tmp = ar.[y, first]
            ar.[y, first] <- ar.[y, second]
            ar.[y, second] <- tmp
        ar

    let transpose (ar: 'a [,]): 'a [,] =
        ar
        |> toArrayOfArrays
        |> Array.transpose
        |> fromArrayOfArrays
