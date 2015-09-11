bumper
======

With this tool you may bump a version of your project with one command. Only requirement is to stick with semantic versioning described on [semver.org](http://semver.org).

In order to bump minor version 2.1.4 in your Setup.hs file call.

```bash
bumper -c 2.1.4 minor Setup.hs
```
More can be found in [examples][examples] section

Other features are:

* Tool supports replacing suffixes

```
1.1.0-alpha
3.4.9-rc3
0.6-beta
```
* and build numbers

```
4.2.0+b02
1.1+1b0fa9b
0.3.1-alpha+466d881
```

* You may also update many files with one command, as long as they share the version number.

```bash
bumper -c 1.0.1 major file1.txt file2.txt file3.txt
```

### Examples {#examples}

* bumping major version of 1.0.1:

```bash
bumper -c 1.0.1 major file.txt
```

* bumping minor version of 2.0.1:

```bash
bumper -c 2.0.1 minor file.txt
```

* bumping patch version of 2.1.1:

```bash
bumper -c 2.1.1 patch file.txt
```

* changing build number with minor version

```bash
```

* changing suffix with build number

```bash
```

### Installation

## Contact

Report issues via [github](https://github.com/inirudebwoy/bumper/issues). Pull requests welcomed.

# Changelog

## Version 0.1.0

* Added support for suffixes (i.e. 0.4.2-rc2, 1.2.3-alpha) and build numbers (i.e. 0.4.2-rc2+b34, 1.2.3-alpha+7de4309)
* Added support for bumping multiple files

## Version 0.0.1

* First release, version bumping from command line
