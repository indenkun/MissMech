## Test environments

* local R installation, R 4.3.3
* Windows Server 2022, R-devel, 64 bit (on r-hub)
* Ubuntu Linux 20.04.1 LTS, R-release, GCC (on r-hub)
* Fedora Linux, R-devel, clang, gfortran (on r-hub)

## R CMD check results

* There are 2 NOTE that is found on only Windows Server.

```
* checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  ''NULL''
  
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```

This issue does not seem to be caused by the package itself.

* There is a NOTE that is found on Linux Environment (on r-hub).

```
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
```

This issue does not seem to be caused by the package itself.

## Reverse Dependency Checking

There are no packages with reverse dependencies.