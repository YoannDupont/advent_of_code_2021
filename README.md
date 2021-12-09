# Ada Advent of Code 2021

Ada solutions to Advent of Code 2021. I may not be able to publish solutions
daily, but they will come sooner or later.

I'll try to use available Ada 202X constructs.

## Ada202X features

Here are referenced days that use Ada202X features, based on the following list: https://blog.adacore.com/ada-202x-support-in-gnat

- assignment target name `@`: all of them.
- `Indexes in array aggregate`: [day07](src/day07.adb)
- `'Reduce`:  [day01](src/day01.adb), [day07](src/day07.adb)

# Solutions "philosophy"

More like guidelines I'll try to follow, but will break if it makes sense:

- code should be as much as possible self-contained (might abstract away input reading in some cases)
- use as much terminology from the puzzle wording as possible
- custom types, lots of them
- as little hard coding as possible
- `renames` over `use`
    - `use type` is good for making operators visibles
- names `part_1` and `part_2` should preferably be present in the code, as variables or methods

# Build

You can build the project using the command:

`gnat make -Paoc2021.gpr`

# Run

Executables are in the `exec` folder. They always take 1 argument: the path to
the input puzzle.

If you want to run day 1, use the command:

`path/to/advent_of_code_2021/exec/day01 /path/to/input.txt`
