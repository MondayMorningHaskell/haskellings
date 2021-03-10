# Haskellings

Welcome to Haskellings, an automated Haskell tutorial program (inspired by [Rustlings](https://www.github.com/rust-lang/rustlings)). This program has dozens of small exercises where you can read about the basic ideas of the Haskell language and then implement some simple functions.

## Getting Started

To use Haskellings, you must first have Haskell installed on your machine. The simplest way to do this is to install the [Haskell Platform](https://haskell.org/platform).

Next, you'll need to clone this repository and then build the code using `stack`.

```bash
>> git clone https://github.com/MondayMorningHaskell/haskellings.git
>> stack build
```

You can then install the `haskellings` executable to your local path using `stack install`. This will let you run `haskellings` from anywhere on your system:

```bash
>> stack install
>> haskellings run Types1
...
```

You can also run any `haskellings` command from the project directory using `stack exec` like so:

```bash
>> stack exec -- haskellings run Types1
```

## Running Exercises

Within the [exercises directory](https://github.com/MondayMorningHaskell/haskellings/tree/master/exercises), you can find sub-directories containing all the different exercise modules. You can run individual exercises with the `haskellings run` command combined with the name of the fine (without the extension).

```bash
>> haskellings run Types1
```

This will run the [`Types1`](https://github.com/MondayMorningHaskell/haskellings/blob/master/exercises/basics/Types1.hs) exercise.

Initially, exercises will not compile. You'll want to open the file, read the information at the top, and then fill in the code as indicated by the `TODO` comments. You'll usually be replacing the question marks `???` with your function implementations and type declarations.

You can re-run the exercise, and the output will let you know when you're done!

If you're stuck, each exercise also has a hint you can see by running `haskellings hint`:

```bash
>> haskellings hint Types1
Fill in appropriate type signatures for the expressions at the bottom.
```

Sometimes you might want to compile and run code you've written for one function without filling in the rest. When this happens, you can usually replace the question marks `???` for function implementations with the phrase `undefined`. Using `undefined` lets your code compile, though that particular function won't run:

```haskell
-- BEFORE:
subtract7 :: Int -> Int
subtract7 = ???

multiplyBy7 :: Int -> Int
multiplyBy7 = ???

-- AFTER:
subtract7 :: Int -> Int
subtract7 x = x - 7

multiplyBy7 :: Int -> Int
multiplyBy7 = undefined
```

For example, after making the above change, you'll be able to run the tests for the `subtract7` function. You'll still see errors while running the tests for `multiplyBy7`, but they will compile. Sometimes the implementation will start as `undefined` rather than `???`, in which case it's still your job to replace it!

## Using the Watcher

The simplest way to ensure that you're doing exercises _in order_ is to use the Watcher. You can invoke this with the command `haskellings watch`.

When you save your changes to the file, the Watcher will automatically re-compile and re-run the exercise, so you can see if you've completed it!

Each exercise file has the following comment:

```haskell
-- I AM NOT DONE
```

After the exercise is passing, you must delete this comment from the file and re-save to indicate to the Watcher that you are ready to move on to the next exercise. It will then automatically load and run your next task!

You can also use hints within the Watcher! Just type 'hint' and press enter!

```bash
>> haskellings watch
Couldn't compile : Expressions.hs
... (compilation output)

hint
Fill in numeric values in place of '???' for expression3 and expression4.
```

## Types of Exercises

There are three different types of exercises.

### Compile-Only Exercises

These are the most basic exercises. You'll see these with the more basic concepts, especially around create data types. In order to "pass" the exercise, you only have to edit the code so that the file compiles.

### Unit Test Exercises

These represent the bulk of exercises. You'll see a `main` function at the bottom of the exercise file with obvious testing code. For these exercises, you'll need to fill in function definitions to get certain expected results. So pay attention to the instructions! Once the unit tests pass, you can move on!

### Executable Exercises

There are also a few _executable_ exercises. These are most prevalent when you are learning about the `IO` monad. In these exercises, you will fill in the definition of the `main` function near the bottom of the file, as well as other `IO` functions in the file. This will be runnable IO code that can print to the terminal and, if necessary, read user input.

When you use `haskellings run`, it will run your program on one set of possible inputs and check if the output matches what we expect.

If it does not, you can run the executable using `haskellings exec`:

```bash
haskellings exec IO1
```

You can run this entering your own custom inputs. But to see why it is not "passing", you can enter in the **Sample Inputs** at the bottom of the file (if there are any). You should then compare your output to the **Sample Output**. It should usually (but not always) be a direct match.

## Contributing

Haskellings is an open source project. If you've found a bug, have any suggestions for improvements or just want to help, let us know! Take a look at the [CONTRIBUTING](./CONTRIBUTING.md) doc to learn how!

## Configuration

Haskellings needs to be able to find the appropriate version of GHC to actually compile and run individual exercises. It also needs to find Stack package databases to enable dependencies. Right now, we only look for these in the default locations, such as `~/.stack` on Posix systems and `%LOCALAPPDATA%\\sr` on Windows. In the future we will allow the program to be configured to use different paths if your installation is different. See [this issue](https://github.com/MondayMorningHaskell/haskellings/issues/16).

## Other Beginner Material

At [Monday Morning Haskell](https://mmhaskell.com), one of our core principles is that Haskell shouldn't be as hard to learn as people think it is. Accordingly, we have a wealth of other materials to help you get started learning the language!

1. [Beginners Checklist](https://mmhaskell.com/beginners-checklist) - This guide will point you to some other resources that can help you kickstart your Haskell journey!
2. [Recursion Workbook](https://mmhaskell.com/workbook) - Haskellings has several different exercises to help you learn about recursion. But this workbook has 2 in-depth chapters on the topic and offers 10 more practice problems that will help you master this vital skill!
3. [Liftoff Series](https://mmhaskell.com/liftoff) - This series is another good starting point for your learning experience. It covers a lot of the same ground you'll cover in the Haskellings exercises, but offers a bit more depth and continuity. Plus it will help you get familiar with GHCI!
4. [Other Beginners Series](https://mmhaskell.com/monads) - There are also several other complete series aimed at beginners! Take a look at these to learn more about monads, unit testing, and data structures!
5. [Haskell From Scratch Course](https://academy.mondaymorninghaskell.com/p/haskell-from-scratch) - If you're ready for a more in-depth approach to Haskell foundations, this is the place to go. Our beginners course will illumine a lot more details about Haskell's basics for you. Through hours of video content, dozens more exercises, and a final project, you'll leave this course with the confidence to start your own Haskell projects.
