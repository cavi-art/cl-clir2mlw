# cl-clir2mlw
CLIR2MLW is a parser which prints CLIR files as WhyML files.

# Requirements
- You need a Common Lisp runtime. (I only tested this with SBCL at the moment)

CLIR2MLW uses qlot for managing dependencies, so you need quicklisp
(qlot is automatically pulled by us). So you can either install
QuickLisp by hand, or have it managed by Roswell.

## Getting [qlot][qlot]
### Way #1, using Quicklisp
For that you need a working Quicklisp runtime.
If you do not have one, just type on your shell the following:

```bash
curl -O https://beta.quicklisp.org/quicklisp.lisp
curl -O https://beta.quicklisp.org/quicklisp.lisp.asc
gpg --verify quicklisp.lisp.asc quicklisp.lisp
sbcl --load quicklisp.lisp
```

This will start a lisp environment. From there, follow the instructions, and type:

```common-lisp
(quicklisp-quickstart:install)
(ql:add-to-init-file)
(quit)
```

Now, open again your lisp environment and type:

```common-lisp
(ql:quickload :qlot)
```

That's it.

### Way #2, using Roswell

[Roswell][ros] is a Common Lisp manager, installer and launcher. It's
similar to Stack for Haskell or other utilities.

It provides a nice interface for a lot of things.

You can take a look at how to install roswell in [its wiki][ros-install].

  [ros-install]: https://github.com/roswell/roswell/wiki/1.-Installation
  
Once you have Roswell installed, just type in your shell:

```bash
ros install qlot
```

# Usage

You can build an executable file, and then use it as any non-lisp
console binary. For building it, just type

```bash
sbcl --load binary.lisp
```

The binary's current usage is reproduced below. You can get this
information by executing `./clir2mlw --help`

```
Usage: clir2mlw [-hva] [+a] [OPTIONS] FILE

A CLIR parser to WhyML.
Options which immediately exit:
  -h, --help                  Print this help and exit.
  -v, --version               Print version number and exit.
Options controlling the output
  -(+)a, --no-auto-file-name[=yes/no] Choose automatically the file name. If an 
                              output file is not set, this sets the output file 
                              to the same as the input file, except the 
                              extension is replaced. Otherwise, the output is 
                              sent to stdout.
                              Fallback: yes
  --override-output-extension=STR Override the extension to save files with
                              Fallback: mlw
```


[qlot]: https://github.com/fukamachi/qlot
[ros]: https://github.com/roswell/roswell
