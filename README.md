# todo app

![](https://github.com/aherrmann/example_servant_elm_bazel/workflows/CI/badge.svg)

A minimal todo app implemented in [Haskell][haskell] using [Servant][servant]
on the backend, and in [Elm][elm] on the frontend, built with [Bazel][bazel]
using [`rules_haskell`][rules_haskell] and [`rules_elm`][rules_elm].

[haskell]: https://www.haskell.org/
[servant]: https://hackage.haskell.org/package/servant
[elm]: https://elm-lang.org/
[bazel]: https://bazel.build/
[rules_haskell]: https://haskell.build/
[rules_elm]: https://github.com/EdSchouten/rules_elm

## Requirements

You will need the [dependencies of `rules_haskell`][dependencies]
installed on your system to build.

On MacOS you also need to set the following environment variable:

```
export BAZEL_USE_CPP_ONLY_TOOLCHAIN=1
```

[dependencies]: https://rules-haskell.readthedocs.io/en/latest/haskell.html#before-you-begin

## Instructions

Execute the following command to build and start the app:

```
$ bazel run //:backend
```

Then browse to `http://localhost:8080` to use the app.

## Interactive Development

You can use [`bazel-watcher`][bazel-watcher] to continuously rebuild the
frontend whenever any of its sources change, like so.

```
$ ibazel build //frontend
```

You can use [`ghcid`][ghcid] to continuously reload the backend whenever
any of its sources change, like so.

```
$ ghcid --command="bazel run //backend:backend@ghci" \
    --test=Main.main \
    --setup=":set args --assets bazel-bin/frontend/frontend.tar" \
    --reload=bazel-bin/frontend/frontend.tar
```

This will load the Haskell code into a GHCi session and run the server in
the interpreter. It will reload the session and restart the server
whenever any of the Haskell source files or the frontend bundle changes.

Note, if the dependency graph changes, e.g. when you edit a `BUILD.bazel`
file, then you will have to restart `ghcid` itself.

[bazel-watcher]: https://github.com/bazelbuild/bazel-watcher#readme
[ghcid]: https://github.com/ndmitchell/ghcid#readme
