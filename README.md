[![AoC Benchmarks](https://github.com/Gabriel1986/Advent-of-Code/actions/workflows/benchmarks.yml/badge.svg)](https://github.com/Gabriel1986/Advent-of-Code/actions/workflows/benchmarks.yml)

# Advent of Code solutions

Wrote some solutions for Advent of Code (https://adventofcode.com) in F#.

## Run the code

Install .net 9.0, then in a console:

```
dotnet run
```

This will run the latest solution for the latest year

---

Additional parameters:

`all` - Runs all of the solutions for all years

`<year>` - Runs all of the solutions for the specific year

`<year> <day>` - Runs the solutions for a specific day

`<year> <day> <part>` - Runs the solution for a specific part of a specific day

`test <other args>` - Runs the test cases for the specified other arguments (e.g. dotnet run test 2023 9 1)

`benchmark <other args>` - Runs the solutions returned by other arguments 10x and averages the result (e.g. dotnet run benchmark all, dotnet run test benchmark all)
