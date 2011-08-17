TKYProf
========================

TKYProf is a web-based interactive visualizer for [GHC Time and Allocation Profiling Reports](http://www.haskell.org/ghc/dist/stable/docs/html/users_guide/prof-time-options.html). It helps you to find the performance bottlenecks of your code quickly.

![TKYProf](http://cdn-ak.f.st-hatena.com/images/fotolife/m/maoe/20110816/20110816185419.png)

Prerequistes
------------------------

* TKYProf uses some of HTML5 features, but does not support graceful downgrading. You need a modern browser supporting HTML5 and CSS3 for now.
* TKYProf is written in Haskell. Haskell Platform and Cabal are required.

How to use TKYProf
------------------------

1. `cabal install tkyprof` installs the executable `tkyprof`.
2. Run `tkyprof` on your terminal.
3. Access [http://localhost:3000/](http://localhost:3000/).
4. Drag and drop your profiling reports.
5. TKYProf draws a pretty chart.

![screenshot](http://cdn-ak.f.st-hatena.com/images/fotolife/m/maoe/20110817/20110817100841.png)

Developers
------------------------

This tool is written and maintained by Mitsutoshi Aoe, <maoe@foldr.in>.
