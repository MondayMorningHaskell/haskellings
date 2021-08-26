# Contributing to Haskellings

Haskellings is an open source project, so you are welcome to contribute and we thank you very much for doing so!

## Quick Reference

I want to...

1. Add an exercise - [Read this](#adding-an-exercise), and then [Open a Pull Request](#opening-a-pull-request).
2. Update an existing exercise - [Open a Pull Request](#opening-a-pull-request).
3. Report a bug - [Open an Issue](#opening-an-issue).
4. Fix a bug - [Open a Pull Request](#opening-a-pull-request).
5. Implement a new feature - [Open an Issue](#opening-an-issue) for discussion, then [Open a Pull Request](#opening-a-pull-request).

## Opening an Issue

You can open an issue [here](https://github.com/MondayMorningHaskell/haskellings/issues/new). If reporting a bug, please include several details:

1. The command you ran
2. The output of the command
3. Your OS version

## Opening a Pull Request

To open a pull request, you should fork the repository and make your commits. There are a few things to check for before you open the PR against the main repository.

### Run Tests

There are currently two test suites. The [`unit-tests`](https://github.com/MondayMorningHaskell/haskellings/blob/master/tests/UnitTests.hs) are simple tests mostly related to directory and file path utilities. Then [`haskellings-tests`](https://github.com/MondayMorningHaskell/haskellings/blob/master/tests/Main.hs) focus on compiling and running code. You can run both suites with `stack test`. When you open a pull request, they tests will also be run on Circle CI.

### Linting and Formatting

As much as possible, use `hlint` to lint source and test code, and `stylish-haskell` for formatting. There is no need to apply these to user exercise files, which are bound to have parse errors.

You can install both of these tools with `stack install`:

```bash
>> stack install hlint
>> stack install stylish-haskell
```

For a complete lint and format run-through, use these commands. You can, of course, exclude files you haven't modified.

```bash
>> stylish-haskell -i src/Haskellings/*.hs src/Haskellings/Internal/*.hs app/Main.hs tests/Main.hs tests/UnitTests.hs
>> hlint src/Haskellings/*.hs src/Haskellings/Internal/*.hs app/Main.hs tests/Main.hs tests/UnitTests.hs
```

Running `stylish-haskell -i` will automatically fix issues in place. Linting issues from `hlint` are output to the terminal and must be fixed manually.

### Getting Approved

Once you've opened your pull request, you should add the "Ready for Review" label to request a review. Once your PR is "Approved" you can squash-merge it.

## Adding an Exercise

The first step in adding your exercise is to make the exercise file itself in the appropriate sub-directory of [exercises](https://github.com/MondayMorningHaskell/haskellings/tree/master/exercises). You should include a section at the top either with a "lesson" explaining the concept the user is supposed to implement. Or alternatively, you can link to the appropriate section of a useful online book or blog, such as [Monday Morning Haskell](https://mmhaskell.com), [Learn You a Haskell](http://www.learnyouahaskell.com), etc.

For some extra guidelines, recall then that there are three types of exercises: Compile-Only, Unit-Tested, and Executable, as you can see in the [code here](https://github.com/MondayMorningHaskell/haskellings/blob/master/src/ExerciseList.hs#L13-L17).

### Compile Only

A Compile-Only exercise should have a module name at the top that matches the exercise name (e.g. `module Expressions where` for `Expressions.hs`). After that, the only guideline is that your Haskell code should not compile in such a way that the user must fix it. For example, you can use `???` for data declarations or type signatures.

### Unit Tested

Most exercises that involve function implementations should be unit-tested. These *do not* need a module declaration at the top (it will be `Main`, which is the case by default). Instead you should include the following imports:

```haskell
import Test.Tasty
import Test.Tasty.HUnit
```

Then at the bottom you should write a `main` function that will run a series of test cases using the Tasty-HUnit library. These should exercise the functions the user filled in. You don't need to create different test groups for each function, and you don't need to go overboard with test cases. Usually 2-5 cases per function is enough. Here is an example:

```haskell
main :: IO ()
main = defaultMain $ testGroup "Functions1" $
  [ testCase "Subtract 1" $ subtract7 13 @?= 6
  , testCase "Subtract 2" $ subtract7 27 @?= 20
  , testCase "Twelve" $ twelve @?= 12
  , testCase "MultiplyProductBy5 1" $ multiplyProductBy5 5 5 @?= 125
  , testCase "MultiplyProductBy5 2" $ multiplyProductBy5 1 10 @?= 50
  , testCase "MultiplyProductBy5 3" $ multiplyProductBy5 2 22 @?= 220
  , testCase "Sixty" $ sixty @?= 60
  ]
```

### Executable Exercises

If your exercise involves terminal or file system interactions, you probably want to make it an `Executable` exercise. In this kind of exercise the user will write `IO` functions and possibly even modify the `main` function itself. For these exercises, you define just a single "test case". This test case consists of a list of strings to provide as terminal input to the program, and a predicate function on a list of strings. The predicate will evaluate the lines of output from the user's program when the sample inputs are passed, and assess whether they have completed the exercise. Place these expressions in the [`ExecutableExercises` module](https://github.com/MondayMorningHaskell/haskellings/blob/master/src/ExecutableExercises.hs).

```haskell
io1Inputs :: [String]
...

io1Predicate :: [String] -> Bool
...
```

Many times, the predicate will be that exact output is expected. But if the output depends on the state of the user's file system, you might be more flexible.

You should include the inputs your test passes and the expected output at the bottom of the exercise file. Here's an example:

```
-- IO1.hs

-- TODO: Get the user's name from the terminal, and then
--       greet them by printing "Hello" plus their name!
main :: IO ()
main = ???

{-

Sample Input:

John

Sample Output:

What is your name?
Hello, John!

-}
```

Then the input and predicate expressions might look like:

```haskell
io1Inputs :: [String]
io1Inputs = ["John"]

io1Predicate :: [String] -> Bool
io1Predicate = (==) ["What is your name?", "Hello, John!"]
```

### Guidelines for All Exercises

1. Your exercise should include a comment `-- I AM NOT DONE` so the Watcher doesn't skip it prematurely.
2. Your exercise should not compile initially, even if there are unit tests (i.e. prefer using `???` to `undefined` for functions the user should fill in). These prevents the possibibility that someone using the Watcher sees a large number of failing test cases and needs to scroll up just to figure out which exercise they are working on.
3. You exercise should include a `TODO` comment where the user is supposed to start working.
4. You can import other library modules you need at the top of the files. But if they are not already in one of the packages included by the [`haskellings` library](https://github.com/MondayMorningHaskell/haskellings/blob/master/haskellings.cabal#L35), you should add that dependency. This ensures the import will be visible in the Package DB used by GHC when running the exercise.

### Adding it to the Exercise List

When your exercise is complete, you can add it to the list of exercises in the [ExerciseList module](https://github.com/MondayMorningHaskell/haskellings/blob/master/src/ExerciseList.hs). You'll need to create an appropriate [`ExerciseInfo` object](https://github.com/MondayMorningHaskell/haskellings/blob/master/src/ExerciseList.hs#L31-L36) and add it in the appropriate order in the [`allExercises` list](https://github.com/MondayMorningHaskell/haskellings/blob/master/src/ExerciseList.hs#L38).

Creating this object should be pretty straightforward, following the examples already there, but here's a summary of the fields:

1. `exerciseName` - The name of the exercise module (should be the file name, but without ".hs")
2. `exerciseDirectory` - The sub-directory below `/exercises` where the file is located
3. `exerciseType` - See [this data type](https://github.com/MondayMorningHaskell/haskellings/blob/master/src/ExerciseList.hs#L13-L17). Either `CompileOnly`, `UnitTested`, or `Executable`. If the latter, make sure to refer to the sample input and output predicate you made above.
4. `exerciseHint` - A string to display as the "hint" for the exercise.

After you've taken these steps, you should be able to run `stack build` and then run your exercise! Please take the time to fill in your sample solution to verify that all the test cases work as expected.

Once you've done this, you should be ready to [Open a Pull Request](#opening-a-pull-request)! In your PR description, please include the sample solution you would provide for the exercise so reviewers can test it!

### Exercise Ideas

Here are some concepts that are still "Beginner-level" that Haskellings does not yet cover:

1. Advanced Containers (Sets, Maps, Hash Sets, Hash Maps, Arrays, etc.)
2. IO: Random numbers, basic concurrency
3. Cool functions, especially around lists: `zipWith, curry, uncurry, concatMap` and similar.
4. More details around function application and composition (`($), (.)`)
