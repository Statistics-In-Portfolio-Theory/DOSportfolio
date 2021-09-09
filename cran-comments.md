## Submission information

This is a (third) resubmission due to an issue and a misunderstanding in the
README.md which appear from Rmd->md conversion where an http link was created.

The change has now been made in the `.bib` file from `@misc{...}` to 
`@article{...}`. The url is now hardcoded and parses correctly from 
`.Rmd -> .md` in the introduction vignette and README.


## Test environments

* local Windows10x64 Version 21H1
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit (r-hub)
* Ubuntu Linux 20.04.1 LTS, R-release, GCC (r-hub)
* Fedora Linux, R-devel, clang, gfortran (r-hub)
* Debian Linux, R-devel, clang, ISO-8859-15 locale (r-hub)

## R CMD check results
There were no ERRORs or WARNINGs though one common NOTE relating to the 
surnames of the authors on most platforms (all except Debian Linux). Below
in an excerpt of the NOTEs

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Erik Thorsén <erik.thorsen@math.su.se>’

New submission

Possibly misspelled words in DESCRIPTION:
  Bodnar (25:24)
  Parolya (25:32)
  Thorsén (25:45)
  nonoverlapping (27:43)

## Downstream dependencies
There are currently no downstream dependencies for this package.
