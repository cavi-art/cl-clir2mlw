clir2mlw
=========
CLIR2MLW is a parser which prints CLIR files as WhyML files.


Usage
=====

You can build an executable file, and then use it as any non-lisp
console binary.

Before building
---------------

Before building the binary you need to get the project's dependencies.
They are managed through QuickLisp and QLOT. The *best* way to get
this working is to install [Roswell][ros] and install sbcl from there
as your lisp implementation. Other configurations have not been
tested.

After you have Roswell, get the project's dependencies by typing:

```bash
ros run -L sbcl -l init.lisp
```

That should get the dependencies and execute the tests.


Building
--------

For building the binary, just type

```bash
ros run -L sbcl -l binary.lisp
```

The binary's current usage is reproduced below. You can get this
information by executing `./clir2mlw --help`

```text
./clir2mlw -h
Usage: clir2mlw [-hvas] [+a] [OPTIONS] FILE ...

A CLIR parser to WhyML.
Depending on -a, files have to be either pairs CLIR-FILE.CLIR CLIR-FILE.MLW or 
just a list of CLIR files, and the MLW paths are derived from the original 
names.
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
  -s, --to-stdout             Send the output of all files to stdout.
  --override-output-extension=STR Override the extension to save files with
                              Fallback: mlw
```

Requirements
============

- You need a Common Lisp runtime. (I only tested this with SBCL at the moment)

CLIR2MLW uses qlot for managing dependencies, so you need quicklisp
(qlot is automatically pulled by us). So you can either install
QuickLisp by hand, or have it managed by Roswell.

## Getting [qlot][qlot]

### Way #1, using Roswell

[Roswell][ros] is a Common Lisp manager, installer and launcher. It's
similar to Stack for Haskell or other utilities.

It provides a nice interface for a lot of things.

For example, Roswell automatically includes QuickLisp into the runtime
by default, so there is no need to require it elsewhere.

You can take a look at how to install roswell in [its wiki][ros-install].

  [ros-install]: https://github.com/roswell/roswell/wiki/1.-Installation
  
Once you have Roswell installed, just type in your shell:

```bash
ros install qlot
```

[qlot]: https://github.com/fukamachi/qlot
[ros]: https://github.com/roswell/roswell

### Way #2, using Quicklisp
For that you need a working QuickLisp runtime.
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


Contributing
============

This project encourages the [GitHub Flow][flow] for external
contributions. Please send any improvements you may find via GitHub a
Pull Request. You can also send them by email or any other means, and
they will end up being integrated here.

By sending a Pull Request you agree to publish your own code under the same 
license as the one stated in the repository.

  [flow]: https://guides.github.com/introduction/flow/

Acknowledgements
================

This work is partially supported by
the Spanish MINECO project CAVI-ART (TIN2013-44742-C4-3-R),
Madrid regional project N-GREENS Software-CM (S2013/ICE-2731) and
UCM grant GR3/14-910502.

CAVI-ART stands for Computer Assisted ValIdation by Analysis, 
tRansformation and Testing.
