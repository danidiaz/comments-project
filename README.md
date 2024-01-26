# How to run

Before the first time:

```
sqlite3 db.sqlite < schema.sql
```

Then:

```
cabal build all
cabal run comments
```

# Useful while developing

```
ormolu --mode inplace $(git ls-files '*.hs')
```

# Links

- [sqlite ROWIDs and the INTEGER PRIMARY KEY](https://www.sqlite.org/lang_createtable.html#rowid)

- [Using sqlite3 in a shell script](https://www.sqlite.org/cli.html)
  
  > One way to use sqlite3 in a shell script is to use "echo" or "cat" to generate a sequence of commands in a file, then invoke sqlite3 while redirecting input from the generated command file. This works fine and is appropriate in many circumstances. But as an added convenience, sqlite3 allows a single SQL command to be entered on the command line as a second argument after the database name. 

-  [#sqlite serialized theading mode and prepared statements](https://hachyderm.io/@DiazCarrete/111823721851342109)

