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

Initially, exercises will not compile. You'll want to open the file, read the information at the top, and then fill in the code as indicated by the `TODO` comments.

You can re-run the exercise, and the output will let you know when you're done!

If you're stuck, each exercise also has a hint you can see by running `haskellings hint`:

```bash
>> haskellings hint Types1
Fill in appropriate type signatures for the expressions at the bottom.
```

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
