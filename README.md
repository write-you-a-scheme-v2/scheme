Write You a Scheme
==================
Shortcut build
```bash
$ ./build 
```
Shortcut repl 
```bash 
$ ./run
```
Building the reference implementation.

```bash
$ stack exec scheme
```

Debugging the reference implementation in GHCI.

```bash
$ stack repl 
```

Building the HTML and MediaWiki files for the tutorial text.

```bash
$ stack exec docs
```

The Wikipedia files are generated to ``output/docs/*.wiki``. To view the HTML
output.

```bash
$ firefox output/scheme.html
```
