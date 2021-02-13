# haskellings

This is our automated tutorial to teach beginners the basics of Haskell! It's still under construction. If you want to contribute, email me at james@mondaymorninghaskell.me!

## Quick Start

Clone the repository and run these commands!

```bash
>> git clone https://github.com/MondayMorningHaskell/haskellings && cd haskellings
>> stack install
>> haskellings configure
>> haskellings watch
```

## Using `watch`

## Running Individual Exercises

## Custom Configuration

The `configure` command creates a new `.haskellings` folder in your home directory with a single file,
`haskellings_config.yaml`. This file contains the paths of important files and directories for running `haskellings`.

By default, `haskellings` will look for the files in these locations:

1. a
2. b
3. c

If you have installed Stack/GHC in a custom way, 

To get started with Haskellings, you should first run the `haskellings configure` command from within the repository.
This command locates some important files and directories we need to access
It will create a `.haskellings` folder in your home directory that will con

## TODO
I almost want to make configuration a separate branch. I want to get the lessons up and I want to get users as quickly
as possible. That is the priority. Custom configuration is a more interesting problem, but it can wait. Windows/Mac
stuff should be resolved quickly. But I don't know that I need everything there? Or is it necessary after all? What's
the simplest thing I can do to make this work on windows? I have those custom functions. And fpBFS is the right approach.
