## Email Response from previous submission
   The Description field should not start with the package name,
     'This package' or similar.

Please fix and resubmit.

Also single quote software names such as 'RStudio' in the Description field.

Best,
Uwe Ligges

> This has been fixed now.

## R CMD check results
```
==> devtools::check(args = c('--as-cran'))

ℹ Updating simrel documentation
ℹ Loading simrel
Registered S3 method overwritten by 'DoE.base':
  method           from       
  factorize.factor conf.design
Writing NAMESPACE
Writing NAMESPACE
── Building ────────────────────────────────────────────────────────────────────────────────────── simrel ──
Setting env vars:
• CFLAGS    : -Wall -pedantic -fdiagnostics-color=always
• CXXFLAGS  : -Wall -pedantic -fdiagnostics-color=always
• CXX11FLAGS: -Wall -pedantic -fdiagnostics-color=always
────────────────────────────────────────────────────────────────────────────────────────────────────────────
✓  checking for file ‘/Data/Dropbox/Personal/R-Packages/simrel/DESCRIPTION’ ...
─  preparing ‘simrel’:
✓  checking DESCRIPTION meta-information ...
─  installing the package to build vignettes
✓  creating vignettes (11.6s)
─  checking for LF line-endings in source and make files and shell scripts
─  checking for empty or unneeded directories
─  building ‘simrel_2.1.0.tar.gz’
   
── Checking ────────────────────────────────────────────────────────────────────────────────────── simrel ──
Setting env vars:
• _R_CHECK_CRAN_INCOMING_USE_ASPELL_: TRUE
• _R_CHECK_CRAN_INCOMING_REMOTE_    : FALSE
• _R_CHECK_CRAN_INCOMING_           : FALSE
• _R_CHECK_FORCE_SUGGESTS_          : FALSE
• NOT_CRAN                          : true
── R CMD check ─────────────────────────────────────────────────────────────────
─  using log directory ‘/Data/Dropbox/Personal/R-Packages/simrel.Rcheck’
─  using R version 4.1.1 (2021-08-10)
─  using platform: x86_64-pc-linux-gnu (64-bit)
─  using session charset: UTF-8
─  using options ‘--no-manual --as-cran’
✓  checking for file ‘simrel/DESCRIPTION’
─  this is package ‘simrel’ version ‘2.1.0’
─  package encoding: UTF-8
✓  checking package namespace information ...
✓  checking package dependencies (3.5s)
✓  checking if this is a source package
✓  checking if there is a namespace
✓  checking for executable files ...
✓  checking for hidden files and directories
✓  checking for portable file names
✓  checking for sufficient/correct file permissions ...
✓  checking whether package ‘simrel’ can be installed (6s)
✓  checking installed package size ...
✓  checking package directory ...
✓  checking for future file timestamps (10s)
✓  checking ‘build’ directory
✓  checking DESCRIPTION meta-information ...
✓  checking top-level files
✓  checking for left-over files
✓  checking index information ...
✓  checking package subdirectories ...
✓  checking R files for non-ASCII characters ...
✓  checking R files for syntax errors ...
✓  checking whether the package can be loaded (1.2s)
✓  checking whether the package can be loaded with stated dependencies (1s)
✓  checking whether the package can be unloaded cleanly (1s)
✓  checking whether the namespace can be loaded with stated dependencies (1.1s)
✓  checking whether the namespace can be unloaded cleanly (1.1s)
✓  checking loading without being on the library search path (1.1s)
✓  checking dependencies in R code (1.3s)
✓  checking S3 generic/method consistency (2.2s)
✓  checking replacement functions (1.1s)
✓  checking foreign function calls (1.3s)
✓  checking R code for possible problems (10.1s)
✓  checking Rd files ...
✓  checking Rd metadata ...
✓  checking Rd line widths ...
✓  checking Rd cross-references ...
✓  checking for missing documentation entries (1.1s)
✓  checking for code/documentation mismatches (3.3s)
✓  checking Rd \usage sections (2.5s)
✓  checking Rd contents ...
✓  checking for unstated dependencies in examples ...
✓  checking installed files from ‘inst/doc’ ...
✓  checking files in ‘vignettes’ ...
✓  checking examples (7.2s)
✓  checking for unstated dependencies in ‘tests’ ...
─  checking tests ...
✓  Running ‘testthat.R’ (2.9s)
✓  checking for unstated dependencies in vignettes (3.1s)
✓  checking package vignettes in ‘inst/doc’ ...
✓  checking re-building of vignette outputs (5.3s)
✓  checking for non-standard things in the check directory
✓  checking for detritus in the temp directory
   
   
── R CMD check results ─────────────────────────────────────── simrel 2.1.0 ────
Duration: 1m 7.7s

0 errors ✓ | 0 warnings ✓ | 0 notes ✓

R CMD check succeeded
```