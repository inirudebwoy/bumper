bumper
======

Intention of this tool is provide same functionality as [bumpversion](https://github.com/peritus/bumpversion) but written in Haskell.
With this tool you may bump a version of your project with one command. Only requirement is to stick with semantic versioning described on [semver.org](http://semver.org).

### Examples

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

### Installation

## Contact

# Changelog

## Version 0.0.1

* First release, version bumping from command line
