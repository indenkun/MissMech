This `MissMech` package was originally maintained by Mortaza Jamshidian and was once removed from the CRAN repository.

We are now resubmitting it with permission from the original maintainer.

Mortaza Jamshidian should have emailed CRAN about the change of maintainer.

## RESUBMISSION

```
We still see one commented code line in:
Examples in comments in:
       TestMCARNormality.Rd
```

Fixed: Commented out code that was overlooked has been reverted to normal code. All remaining comments are for example separators and explanations, not for commenting out code.

## Test environments

* local R installation, R 4.3.2
* Windows Server 2022, R-devel, 64 bit (on r-hub)
* Ubuntu Linux 20.04.1 LTS, R-release, GCC (on r-hub)
* Fedora Linux, R-devel, clang, gfortran (on r-hub)

## R CMD check results

* There is a NOTE that is found on all platform;

```
* checking CRAN incoming feasibility ...
 Maintainer: 'Mao Kobayashi <kobamao.jp@gmail.com>'
  
  New submission
  
  Package was archived on CRAN
  CRAN repository db overrides:
   X-CRAN-Comment: Archived on 2020-10-08 as check problems were not corrected in time.
```

This is an unavoidable message because it is a message associated with a change of maintainer.

* There is a NOTE refers to possibly mis-spelled words in DESCRIPTION.

```
Possibly misspelled words in DESCRIPTION:
  Homoscedasticity (3:16, 17:45)
  Jalal (16:53)
  Jamshidian (16:38)
  MCAR (15:64, 17:121)
  MissMech (17:10)
```

These are not misspellings. Acronyms are preceded by the full spelling.

* These are 3 NOTEs.

```
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
```

```
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```

```
* checking for non-standard things in the check directory ... NOTE
```

This issue does not seem to be caused by the package itself.
