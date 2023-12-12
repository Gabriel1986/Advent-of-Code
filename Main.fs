module Program
open System
open System.Reflection
open System.Text.RegularExpressions

let mutable useTestData = false

let private numberRegex = Regex(@"\d+")

//Map of (year, day, part) to function
let private allPrograms =
    let input (year) (day) (part) =
        let path (fileName) = System.IO.Path.Combine(System.AppDomain.CurrentDomain.BaseDirectory, $"{year}", $"Day{day}", fileName)
        match useTestData with
        | true ->
            if System.IO.File.Exists (path "Test.txt") then
                System.IO.File.ReadAllLines (path "Test.txt")
            else
                System.IO.File.ReadAllLines (path $"TestPart{part}.txt")
        | false ->
            System.IO.File.ReadAllLines (path "Input.txt")

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


let execute ((year, day, part), fn: unit -> obj) =
    let timeStamp = DateTime.Now
    let answer = fn ()
    let elapsed = DateTime.Now - timeStamp
    printfn "Year %A - Day %s - Part %A - Elapsed: %sms - Answer: %A" year ((string day).PadLeft(2)) part ((sprintf "%.4f" elapsed.TotalMilliseconds).PadLeft(9)) answer

let executeSpecificPart (year: string, day: string, part: string) =
    match allPrograms |> Array.tryFind (fst >> (=) (int year, int day, int part)) with
    | None ->
        failwith $"Function on year {year}, day {day}, part {part} not found.."
    | Some found ->
        execute found

let executeYear (runForYear) =
    allPrograms
    |> Array.filter (fun ((year, _, _), _) -> string year = runForYear)
    |> Array.sortBy fst
    |> Array.iter execute

let executeDay (year: string, day: string) =
    allPrograms
    |> Array.filter (fun ((y, d, _), _) -> string y = year && string d = day)
    |> Array.sortBy fst
    |> Array.iter execute

let executeLatest () =
    allPrograms
    |> Array.maxBy fst
    |> execute

let executeAll () =
    allPrograms
    |> Array.sortBy fst
    |> Array.iter execute

[<EntryPoint>]
let main args =
    printfn "----------------------------------------------------------------------------"
    printfn "'dotnet run' for the latest solution [default]"
    printfn "'dotnet run all' to run all of the solutions"
    printfn "'dotnet run <year>' for running the solution for all days of the given year"
    printfn "'dotnet run <year> <day>' for running the solution for the specific day of the given year"
    printfn "'dotnet run <year> <day> <part>' to run the solution for the specific year, day and part"
    printfn "'dotnet run test <arguments>' to run the solution with any args, but with the given test input"
    printfn "----------------------------------------------------------------------------"

    useTestData <- args |> Array.exists ((=) "test")

    match args |> Array.filter ((<>) "test") with
    | [| "all" |] | [| "All" |] ->
        printfn "Running solution for all"
        printfn ""
        executeAll ()
    | [| year |] ->
        printfn "Running solutions for year '%s'" year
        printfn ""
        executeYear year
    | [| year; day |] ->
        printfn "Running solution for year '%s', day '%s'" year day
        printfn ""
        executeDay (year, day)
    | [| year; day; part |] ->
        printfn "Running solution for year '%s', day '%s', part '%s'" year day part
        printfn ""
        executeSpecificPart (year, day, part)
    | other ->
        printfn "Running latest solution..."
        printfn ""
        executeLatest ()

    0