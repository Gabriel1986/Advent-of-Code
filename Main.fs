module Program
open System
open System.IO
open System.Reflection
open System.Text.RegularExpressions

let mutable useTestData = false
let private numberRegex = Regex(@"\d+")

type ExecutionResult =
    {
        Answer: obj
        Elapsed: TimeSpan
        Year: int
        Day: int
        Part: int
    }
    member me.Print() =
        printfn "Year %A - Day %s - Part %A - Elapsed: %sms - Answer: %A" me.Year ((string me.Day).PadLeft(2)) me.Part ((sprintf "%.4f" me.Elapsed.TotalMilliseconds).PadLeft(9)) me.Answer

//Map of (year, day, part) to function
let private allPrograms =
    let input (year) (day) (part) =
        let path (fileName: string) = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, $"{year}", $"Day{(string day).PadLeft(2, '0')}", fileName)
        match useTestData with
        | true ->
            if File.Exists (path "Test.txt") then
                File.ReadAllLines (path "Test.txt")
            else
                File.ReadAllLines (path $"TestPart{part}.txt")
        | false ->
            File.ReadAllLines (path "Input.txt")

    Assembly.GetExecutingAssembly().GetTypes()
    |> Array.collect (fun typ ->
        typ.GetMethods()
        |> Array.choose (fun each ->
            if typ.Name.StartsWith("Year") && each.Name.StartsWith "part" then
                let typeNameMatches = numberRegex.Matches(typ.Name)
                let dayPartMatches = numberRegex.Matches(each.Name)
                let year = int typeNameMatches[0].Value
                let day = int typeNameMatches[1].Value
                let part = int dayPartMatches[0].Value
                Some ((year, day, part), (fun () -> each.Invoke (typ, [| input year day part |])))
            else
                None
        )
    )

let execute (nbIterations: int) ((year: int, day: int, part: int), fn: unit -> obj) =
    let timeStamp = DateTime.Now
    let answer = fn ()

    if nbIterations > 1 then
        seq { 2..nbIterations } |> Seq.iter (fun _ -> fn () |> ignore)
    
    let elapsed = DateTime.Now - timeStamp

    let result = {
        Year = year
        Day = day
        Part = part
        Answer = answer
        Elapsed = if nbIterations > 1 then elapsed.Divide(nbIterations) else elapsed
    }

    result.Print()
    

let executeSpecificPart (nbIterations: int) (year: string, day: string, part: string) =
    match allPrograms |> Array.tryFind (fst >> (=) (int year, int day, int part)) with
    | None ->
        failwith $"Function on year {year}, day {day}, part {part} not found.."
    | Some found ->
        execute nbIterations found

let executeYear (nbIterations: int) (runForYear: string) =
    allPrograms
    |> Array.filter (fun ((year, _, _), _) -> string year = runForYear)
    |> Array.sortBy fst
    |> Array.iter (execute nbIterations)

let executeDay (nbIterations: int) (year: string, day: string) =
    allPrograms
    |> Array.filter (fun ((y, d, _), _) -> string y = year && string d = day)
    |> Array.sortBy fst
    |> Array.iter (execute nbIterations)

let getLatestProgram () =
    allPrograms
    |> Array.maxBy fst
    |> fun ((year, day, month), _) -> (string year, string day, string month)

let executeAll (nbIterations: int) =
    allPrograms
    |> Array.sortBy fst
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
        let (year, day, part) = getLatestProgram ()
        printfn "%s for year '%s', day '%s', part '%s'" startOfSentence year day part
        printfn ""
        executeSpecificPart nbIterations (year, day, part)

    0