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
Contributing
------------

Please feel free to contribute any improvements, corrects or ideas you have!
I would love to hear how you are using these ideas, or what you think about 
this project. Your help will make this project great!

License
-------

<img
src="http://mirrors.creativecommons.org/presskit/buttons/88x31/png/by-nc-sa.png"
width="140" alt="Artwork CC BY NC SA" />

This written work is licensed under a <a rel="license"
href="http://creativecommons.org/licenses/by-nc-sa/4.0/">Creative Commons
Attribution-NonCommercial-ShareAlike 4.0 International License</a>. You may
reproduce and edit this work with attribution for all non-commercial purposes.

The included source is released under the terms of the [MIT
License](http://opensource.org/licenses/MIT).
