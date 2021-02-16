# Contributing to Haskellings

## Linting and Formatting

As much as possible, use `hlint` to lint source and test code, and `stylish-haskell` for formatting. There is no need to apply these to user exercise files, which are bound to have parse errors.

You can install both of these tools with `stack install`:

```bash
>> stack install hlint
>> stack install stylish-haskell
```

For a complete lint and format run-through, use these commands. You can, of course, exclude files you haven't modified.

```bash
>> stylish-haskell -i src/ app/Main.hs tests/Main.hs tests/UnitTests.hs
>> hlint src/ app/Main.hs tests/Main.hs tests/UnitTests.hs
```

Running `stylish-haskell -i` will automatically fix issues in place. Linting issues from `hlint` are output to the terminal and must be fixed manually.
