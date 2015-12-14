[![Build Status](https://travis-ci.org/luismbo/cl-travis.svg?branch=master)](https://travis-ci.org/luismbo/cl-travis)

Overview and quick start
------------------------

CL-TRAVIS helps you test your Common Lisp projects with [Travis][1]
and many different Lisp implementations: ABCL, Allegro CL, SBCL,
CMUCL, CCL and ECL.

Using it is simple, you don't even have to clone this repository:

1. Grab the sample [`.travis.yml`][2] file or one from the examples below
2. Modify it to test your own project
3. Push it to the root of your project's repository and
   [enable Travis][3] from your GitHub account.

Setting up target Lisps
-----------------------

In the first part of your `.travis.yml` file, use environment
variables to setup test environments. The most important one is
`LISP`. CL-TRAVIS uses it find out which Lisp implementation to
install for you. You can use additional variables and specify that
some environments are allowed to fail. See [Travis][3]'s instructions
for more possibilities.

```yaml
env:
  matrix:
    - LISP=allegro
    - "LISP=sbcl FOO=baz"
    - "LISP=ccl FOO=bar"

matrix:
  allow_failures:
    - env: LISP=allegro
```

Running tests
-------------

In the second part of the `.travis.yml` file, pull in any dependencies
of your project and use shell commands to run tests.

CL-TRAVIS ensures that [CIM][4] and Quicklisp are installed, which
means you can conveniently run Lisp code from shell-script snippets in
your `.travis.yml` files, using the CIM's `cl` command, grabbing any
dependencies via Quicklisp's `ql:quickload`.

Also, ASDF is set up to look for system definitions recursively within
your project repository and within the `~/lisp` directory, so
`ql:quickload` (or `asdf:load-system`) will find these before any
others.

Here's an example that tests [babel][5] against the bleeding edge 
versions of [trivial-features][6] and [alexandria][7] but grabs
its remaining dependencies through Quicklisp.

```yaml
install:
  - curl -L https://github.com/luismbo/cl-travis/raw/master/install.sh | sh
  - git clone --depth=1 git://github.com/trivial-features/trivial-features.git ~/lisp/trivial-features
  - git clone git://common-lisp.net/projects/alexandria/alexandria.git ~/lisp/alexandria

script:
  - cl -e '(ql:quickload :babel-tests)
           (unless (babel-tests:run)
             (uiop:quit 1))'
```

Examples
--------

Here's a list of `.travis.yml` files from various projects using CL-TRAVIS:
[CFFI](https://github.com/cffi/cffi/blob/master/.travis.yml),
[SLIME](https://github.com/slime/slime/blob/master/.travis.yml),
[Osicat](https://github.com/osicat/osicat/blob/master/.travis.yml),
[stumpwm](https://github.com/stumpwm/stumpwm/blob/master/.travis.yml),
[Babel](https://github.com/cl-babel/babel/blob/master/.travis.yml),
[trivial-garbage](https://github.com/trivial-garbage/trivial-garbage/blob/master/.travis.yml),
[trivial-features](https://github.com/trivial-features/trivial-features/blob/master/.travis.yml).

[1]: https://travis-ci.org
[2]: https://raw.githubusercontent.com/luismbo/cl-travis/master/.travis.yml
[3]: http://docs.travis-ci.com/user/getting-started/
[4]: https://github.com/KeenS/CIM
[5]: https://github.com/cl-babel/babel
[6]: https://github.com/trivial-garbage/trivial-garbage
[7]: http://common-lisp.net/projects/alexandria/
