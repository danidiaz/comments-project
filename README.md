# What's this

A trivial web application created to test the [cauldron](https://github.com/danidiaz/cauldron) dependency injection library.

# How to run

After cloning the project repository, checkout the submodules:

```
git submodule init
git submodule update
```

Before running the server for the first time:

```
sqlite3 db.sqlite < schema.sql
```

Then:

```
cabal build all
cabal run comments
```

And navigate to `http://localhost:8000/comments`.

# Useful while developing

```
ormolu --mode inplace $(git ls-files '*.hs')
```

# Some things to note

- There is not a central configuration record. Each component registers some configuration
  bean, which is parsed independently from the [raw JSON configuration](comments/lib/Bean/JsonConf.hs). 

- Beans often have an ["interface"](comments/lib/Comments/Repository.hs) module
which defines a record-of-functions type, and an
["implemementation"](comments/lib/Comments/Repository/Sqlite.hs) module
(potentially more than one) which provides a constructor record-of-functions.

  This is not a hard rule; for other beans the record-of-functions and the
  constructor are defined in the same module.

- Some bean constructors require the records-of-functions they return to be
parameterized by `ReaderT env IO`, but others (like
[Comments.Repository.Sqlite.make](comments/lib/Comments/Repository/Sqlite.hs))
could be polymorphic over the monad. But then I would have to specify
constraints like `MonadLog m`, `MonadIO m` and `MonadUnliftIO m` so I decided
to stay concrete for now.

- The env in `ReaderT env IO` is only used for storing request-dependent info,
particular to the request's servicing thread. Currently we store the Sqlite
`Connection` allocated to each request, which is set by the
[`Runner`](comments/lib/Comments/Runner.hs).

  Unlike in other application architectures, the reader's env is not used for storing
  static dependencies (like, say, a logger function). Those are instead passed as
  regular parameters of the bean constructor functions.

- Beans don't have to know about their own dependencies' dependencies. The `Runner`
  doesn't care that the `CommentsServer` uses a `CommentsRepository` for example. 

- At the [composition root](comments/lib/Comments/Cauldron.hs) of the application, all types are known. It's
  a good place to add extra logging (using a decorator) without touching the bean implementations.

  Because the composition root sits near the top of the module dependency chain,
  recompilations after having modified it should (?) be fast.

# Links

- [sqlite ROWIDs and the INTEGER PRIMARY KEY](https://www.sqlite.org/lang_createtable.html#rowid)

- [Using sqlite3 in a shell script](https://www.sqlite.org/cli.html)
  
  > One way to use sqlite3 in a shell script is to use "echo" or "cat" to generate a sequence of commands in a file, then invoke sqlite3 while redirecting input from the generated command file. This works fine and is appropriate in many circumstances. But as an added convenience, sqlite3 allows a single SQL command to be entered on the command line as a second argument after the database name. 

-  [#sqlite serialized theading mode and prepared statements](https://hachyderm.io/@DiazCarrete/111823721851342109)

