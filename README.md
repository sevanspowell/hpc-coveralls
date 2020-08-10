hpc-coveralls
=============

[![Build Status](http://img.shields.io/travis/guillaume-nargeot/hpc-coveralls/master.svg)](https://travis-ci.org/guillaume-nargeot/hpc-coveralls) [![Gitter chat](http://img.shields.io/badge/gitter-chat--room-brightgreen.svg)](https://gitter.im/guillaume-nargeot/hpc-coveralls) [![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)](https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29) [![Version on Hackage](https://img.shields.io/hackage/v/hpc-coveralls.svg)](http://hackage.haskell.org/package/hpc-coveralls) [![Stories in Progress](https://badge.waffle.io/guillaume-nargeot/hpc-coveralls.svg?label=in%20progress&title=In%20Progress)](https://waffle.io/guillaume-nargeot/hpc-coveralls)

hpc-coveralls converts and sends Haskell projects hpc code coverage to [coverall.io](http://coveralls.io/).

At the moment, only [Travis CI](http://travis-ci.org) has been tested, but hpc-coveralls should be compatible with other CI services (Check `HpcCoverallsMain` [source](https://github.com/guillaume-nargeot/hpc-coveralls/blob/master/src/HpcCoverallsMain.hs) for the list).

hpc-coveralls is still under development and any contributions are welcome!

# Usage

## Travis CI

Below is the simplest example of configuration for your project `.travis.yml`:
```yaml
language: haskell
ghc: 7.8
script:
  - cabal configure --enable-tests --enable-library-coverage && cabal build && cabal test
after_script:
  - cabal install hpc-coveralls
  - hpc-coveralls [options] [test-suite-names]
```

When building with Cabal 1.22 or a newer version, use the `--enable-coverage` flag instead of `--enable-library-coverage`.

If the build fails during the test phase with an error message starting by "hpc:", just replace the `cabal test` command by `run-cabal-test`, as in the following example:
```yaml
before_install:
  - cabal install hpc-coveralls
script:
  - cabal configure --enable-tests --enable-library-coverage && cabal build
  - run-cabal-test [options] [cabal-test-options]
after_script:
  - hpc-coveralls [options] [test-suite-names]
```

This will prevent the build to fail because of hpc related reasons, which are usually not fatal and should not affect the coverage data. Details are available in the next section.

You may also experience some issues related to your project dependencies, which can be solved by using the `--avoid-reinstalls`/`--force-reinstalls` flags.</br>
Another way to solve problems related dependencies is to install hpc-coveralls in a sandbox, as in the example below:
```yaml
after_script:
  - cabal sandbox init && cabal install hpc-coveralls
  - .cabal-sandbox/bin/hpc-coveralls [options] [test-suite-names]
```

For a real world example usage, please refer to [this-project](https://github.com/guillaume-nargeot/project-euler-haskell) `.travis.yml` file ([result on coveralls](https://coveralls.io/r/guillaume-nargeot/project-euler-haskell)).<br/>
Other real world examples can be found on this [wiki page](https://github.com/guillaume-nargeot/hpc-coveralls/wiki/List-of-repositories-using-hpc-coveralls) which contains a list of GitHub repositories using hpc-coveralls.

## The run-cabal-test command

Under certain conditions related to the project structure and the version of hpc, `cabal test` may output an error message and exit with the error code `1`, which would result in a build failure.<br/>

To prevent this from happening, hpc-coveralls provides the `run-cabal-test` command which runs `cabal test` and returns with `0` if the following regular expression never matches any line of the output:

```perl
/^Test suite .*: FAIL$/
```

Below are some of the conditions under which you will likely need to use `run-cabal-test`:
- when using GHC 7.6 (hpc 0.6 known issue)
- when using GHC 7.8 with multiple test suites covering the same module(s) (issue [#18](https://github.com/guillaume-nargeot/hpc-coveralls/issues/18)) (fixed in GHC 7.10)

### Options

The `--cabal-name` option can be used to specify a custom executable name instead of the default `cabal` when calling `cabal test`.<br/>
Below is an example which can be useful for projects with a Travis configuration based on [multi-ghc-travis](https://github.com/hvr/multi-ghc-travis):

```bash
run-cabal-test --cabal-name=cabal-1.20
```

## The hpc-coveralls command

This command parses the hpc generated output, converts its to Coveralls json format and finally sends it to coveralls.io over http.<br/>
Multiple test suites can be specified, in which case the coverage report will be made of the merged coverage data generated by the specified test suites.<br/>
For example, if your test suite are named `test1` and `test2`, use the command as follows:

```bash
hpc-coveralls test1 test2
```

### Options

#### --exclude-dir

The `--exclude-dir` option allows to exclude source files located under a given directory from the coverage report.<br/>
You can exclude source files located under the `test/` directory by using this option as in the following example:

```bash
hpc-coveralls --exclude-dir=test [test-suite-names]
```

You can specify multiple excluded folders by using the following example syntax:

```bash
hpc-coveralls --exclude-dir=test1 --exclude-dir=test2 [test-suite-names]
```

#### --coverage-mode

As Coveralls doesn't support partial-line coverage yet, hpc-coveralls currently converts hpc coverage data into line based coverage data, which is the only format supported at the moment.
The `--coverage-mode` option allows to configure how the coverage data is converted into Coveralls format, based on your needs.<br/>
Below are the two modes currently available, with an explanation of what the hit count values mean.

`--coverage-mode=AllowPartialLines` (default):
- `0` : the line is never hit,
- `1` : the line is partially covered,
- `2` : the line is fully covered.

Note that `AllowPartialLines` conversion mode follows the same convention as the one used by [cloverage](https://github.com/lshift/cloverage) coveralls output for Clojure projects code coverage.

`--coverage-mode=StrictlyFullLines`:
- `0` : the line is never hit or only partially covered,
- `1` : the line is fully covered.

Please also note that there is an [open issue](https://github.com/lemurheavy/coveralls-public/issues/216) on coveralls issue tracker in order to improve this (add support for partial line coverage).

#### --repo-token

This option allows to specify your repo token when sending the report to coveralls.io.

#### --display-report

This boolean option prints the raw json coverage report to be sent to coveralls.io.

#### --dont-send

This boolean option prevents hpc-coveralls from sending the coverage report to coveralls.io.
This option can be used together with `--display-report` for testing purpose.<br/>
For example, you can try various combinations of the other options and confirm the difference in the resulting report outputs.

#### --curl-verbose

This boolean option enables curl verbose mode and prints the raw json response received after posting the coverage report to coveralls.io.

#### --cabal-file

Use this option to specify the cabal file of the coverage report target package.
This might be required in some cases, especially when building with cabal >= 1.22 and ghc >= 7.10, although hpc-coveralls assumes the package cabal file to be the unique file of extension ".cabal" in the current directory if it exists.
For further details check [this issue](https://github.com/guillaume-nargeot/hpc-coveralls/issues/44).

#### --service-name

This option allows you to override the `service_name` value from the report sent to coveralls.io.
You will have to specify it for example when using Travis-pro as in the example below as there is currently no way to programmatically determine:
```bash
--service-name=travis-pro
```

#### --hpc-dir

This option allows you to manually specify the hpc data directory to use. The behaviour without this option is to attempt to find the hpc directory in the typical places in the current directory.
```bash
--hpc-dir=/dir/share/hpc
```

This directory should contain your hpc data (mix files and tix files) in the standard directory structure for hpc output, for example:

```bash
hpc
├── mix
│   ├── my-lib-0.1.0.0
│   │   └── my-lib-0.1.0.0-inplace
│   │       ├── My.Lib.A.mix
│   │       ├── My.Lib.B.mix
│   |       └── My.Lib.C.mix
│   ├── my-lib-test
│   │   ├── SomeSpec.mix
│   │   ├── SomeOtherSpec.mix
│       └── Main.mix
│   └── my-lib-test2
│       ├── SomeSpec2.mix
│       ├── SomeOtherSpec2.mix
│       └── Main.mix
└── tix
    ├── my-lib-0.1.0.0
    │   └── my-lib-0.1.0.0.tix
    ├── my-lib-test
    │   └── my-lib-test.tix
    └── my-lib-test2
        └── my-lib-test2.tix
```

### --package-dir

This option allows you to specify a number of directories to search for cabal and source files. This might, for example, be used in a "cabal.project" with multiple Haskell packages.

It will only be used if `--cabal-file` is not used.
```bash
--package-dir ./my-lib-1 --package-dir ./my-lib-2
```

# Limitations

Because of the way hpc works, coverage data is only generated for modules that are referenced directly or indirectly by the test suites.
As a result, the total package coverage computed by coveralls may be higher than what it really is.
An option will be added soon in order to allow specifying source folders to include in the total coverage computation.

# Contributing

hpc-coveralls is still under development and any contributions are welcome!

[Future Plans and Ideas](https://github.com/guillaume-nargeot/hpc-coveralls/wiki/Future-Plans-and-Ideas)

Please share your comments and suggestions on hpc-coveralls [Gitter channel](https://gitter.im/guillaume-nargeot/hpc-coveralls)!

# License

BSD3 ([tl;dr](https://tldrlegal.com/license/bsd-3-clause-license-(revised)))

# Notes

- HPC publication: http://ittc.ku.edu/~andygill/papers/Hpc07.pdf
