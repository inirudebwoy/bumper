bumper
======

With this tool you may bump a version of your project with one command. Only requirement is to stick with semantic versioning described on [semver.org](http://semver.org).

In order to bump minor version 2.1.4 in your Setup.hs file call.

```bash
bumper minor -c 2.1.4 Setup.hs
```
More can be found in [examples](#examples) section

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
bumper major -c 1.0.1 file1.txt file2.txt file3.txt
```

### <a name="examples"></a>Examples

* bumping major version of 1.0.1:

```bash
bumper major-c 1.0.1 file.txt
```

* bumping minor version of 2.0.1:

```bash
bumper minor -c 2.0.1 file.txt
```

* bumping patch version of 2.1.1:

```bash
bumper patch -c 2.1.1 file.txt
```

* changing build number with minor version

```bash
bumper minor -c 2.1.1 -b b79
```

* changing suffix with build number

```bash
bumper -b d49bb5c -s alpha
```

### <a name="installation"></a>Installation

### <a name="contact"></a>Contact

Report issues via [github](https://github.com/inirudebwoy/bumper/issues). Pull requests welcomed.

### <a name="changelog"></a>Changelog

* Version 0.1.0
    * Added support for suffixes (i.e. 0.4.2-rc2, 1.2.3-alpha) and build numbers (i.e. 0.4.2-rc2+b34, 1.2.3-alpha+7de4309)
    * Added support for bumping multiple files
* Version 0.0.1
    * First release, version bumping from command line
