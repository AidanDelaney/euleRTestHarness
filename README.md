# euleR Test Harness

This project conducts a specific experiment to compare Euler diagram drawings drawn by the Venn/Euler and euleR algorithms.

## Dependency installation

The eulerTestHarness requires both R packages [venneuler](http://cran.r-project.org/web/packages/venneuler/) and [euleR](https://github.com/AidanDelaney/euleR).  venneuler is available from CRAN however, euleR is only available from git at the moment.

## How to run

There's a makefile.  If you have [GNU R](https://www.r-project.org/) installed, then the makefile should "just work"(tm) so:

```sh
$ make
```

If you don't use make, then try

```sh
$ R src/test.R
```

## Publications

  * None yet
