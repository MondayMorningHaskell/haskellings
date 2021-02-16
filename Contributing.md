# Contributing to Haskellings

## Linting

As much as possible, use `hlint` to lint source and test code. This excludes any user exercise files, which are bound to have parse errors. For now, you can run the full lint using four calls:

```bash
>> hlint src/
>> hlint app/
>> hlint tests/Main.hs
>> hlint tests/UnitTests.hs
```
