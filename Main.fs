module Program
open System
open System.IO
open System.Reflection
open System.Text.RegularExpressions

let mutable useTestData = false
let private numberRegex = Regex(@"\d+")

type Input = {
    Year: int
    Day: int
    Part: int
}

type ExecutionResult =
    {
        Input: Input
        Answer: obj
        Elapsed: TimeSpan
    }
    member me.Print() =
        printfn "Year %A - Day %s - Part %A - Elapsed: %sms - Answer: %A" me.Input.Year ((string me.Input.Day).PadLeft(2)) me.Input.Part ((sprintf "%.4f" me.Elapsed.TotalMilliseconds).PadLeft(9)) me.Answer

type Program = 
    {
        SortKey: (int * int * int * bool)
        Input: Input
        Invoke: string[] -> obj
    }
    member me.FilePath =
        let path (fileName: string) = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, $"{me.Input.Year}", $"Day{(string me.Input.Day).PadLeft(2, '0')}", fileName)
        match useTestData with
        | true ->
            if File.Exists (path "Test.txt") then
                path "Test.txt"
            else
                path $"TestPart{me.Input.Part}.txt"
        | false ->
            path "Input.txt"

let private allPrograms () =
    Assembly.GetExecutingAssembly().GetTypes()
    |> Array.collect (fun typ ->
        typ.GetMethods()
        |> Array.choose (fun each ->
            if typ.Name.StartsWith "Year" && (each.Name.StartsWith "part" || (useTestData && each.Name.StartsWith "testPart")) then
                let typeNameMatches = numberRegex.Matches(typ.Name)
                let dayPartMatches = numberRegex.Matches(each.Name)
                let year = int typeNameMatches[0].Value
                let day = int typeNameMatches[1].Value
                let part = int dayPartMatches[0].Value
                let input = { Year = year; Day = day; Part = part }
                Some {
                    SortKey = (year, day, part, each.Name.StartsWith("part"))
                    Input = input
                    Invoke = fun file -> each.Invoke (typ, [| file |])
                }
            else
                None
        )
    )
    |> Array.sortBy _.SortKey
    |> Array.distinctBy _.Input

let execute (nbIterations: int) (program: Program) =
    let file = File.ReadAllLines program.FilePath

    let timeStamp = DateTime.Now
    let answer = program.Invoke file

    if nbIterations > 1 then
        seq { 2..nbIterations } |> Seq.iter (fun _ -> program.Invoke file |> ignore)
    
    let elapsed = DateTime.Now - timeStamp

    let result = {
        Input = program.Input
        Answer = answer
        Elapsed = if nbIterations > 1 then elapsed.Divide(nbIterations) else elapsed
    }

    result.Print()
    

let executeSpecificPart (nbIterations: int) (year: string, day: string, part: string) =
    match allPrograms () |> Array.tryFind (_.Input >> (=) { Year = int year; Day = int day; Part = int part }) with
    | None ->
        failwith $"Function on year {year}, day {day}, part {part} not found.."
    | Some found ->
        execute nbIterations found

let executeYear (nbIterations: int) (runForYear: string) =
    allPrograms ()
    |> Array.filter (_.Input >> fun input -> string input.Year = runForYear)
    |> Array.iter (execute nbIterations)

let executeDay (nbIterations: int) (year: string, day: string) =
    allPrograms ()
    |> Array.filter (_.Input >> fun input -> string input.Year = year && string input.Day = day)
    |> Array.iter (execute nbIterations)

let getLatestProgram () =
    allPrograms ()
    |> Array.maxBy _.Input

let executeAll (nbIterations: int) =
    allPrograms ()
    |> Array.iter (execute nbIterations)

[<EntryPoint>]
let main args =
    printfn "----------------------------------------------------------------------------"
    printfn "'dotnet run' for the latest solution [default]"
    printfn "'dotnet run all' to run all of the solutions"
    printfn "'dotnet run <year>' for running the solution for all days of the given year"
    printfn "'dotnet run <year> <day>' for running the solution for the specific day of the given year"
    printfn "'dotnet run <year> <day> <part>' to run the solution for the specific year, day and part"
    printfn "'dotnet run test <arguments>' to run the solution with any args, but with the given test input"
    printfn "'dotnet run benchmark <arguments> to run the latest solution multiple times and return an average result'"
    printfn "----------------------------------------------------------------------------"

    useTestData <- args |> Array.exists ((=) "test")
    let useBenchmark = args |> Array.exists ((=) "benchmark")
    let nbIterations = if useBenchmark then 10 else 1

    let startOfSentence = 
        seq {
            "Running"

            if useBenchmark then
                "benchmark"

            if useTestData then
                "test"

            "solution"
        }
        |> String.joinWith " "

    match args |> Array.filter (fun x -> x <> "test" && x <> "benchmark") with
    | [| "all" |] | [| "All" |] ->
        printfn "%s for all" startOfSentence
        printfn ""
        executeAll nbIterations
    | [| year |] ->
        printfn "%s for year '%s'" startOfSentence year
        printfn ""
        executeYear nbIterations year
    | [| year; day |] ->
        printfn "%s for year '%s', day '%s'" startOfSentence year day
        printfn ""
        executeDay nbIterations (year, day)
    | [| year; day; part |] ->
        printfn "%s for year '%s', day '%s', part '%s'" startOfSentence year day part
        printfn ""
        executeSpecificPart nbIterations (year, day, part)
    | other ->
        let program = getLatestProgram ()
        printfn "%s for year '%i', day '%i', part '%i'" startOfSentence program.Input.Year program.Input.Day program.Input.Part
        printfn ""
        execute nbIterations program

    0