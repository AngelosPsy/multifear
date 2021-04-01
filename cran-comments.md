Big thanks to Julia Haider for checking the package. Following the comments, I did the following:

* Reduced the length of the title to 41 characters
* I did not include in the description the words such as "This package", package name, title or similar. No such mention was done in the first release but I was repeating the title so I changed that as well.
* I included a reference in the used methods in the DESCRIPTION file as follows: The methods are described in Lonsdorf, Gerlicher, Klingelhofer-Jens, & Krypotos <doi:10.31234/osf.io/2z6pd>.
* I added values to the .Rd files of the following functions: 
      chop_css.Rd
      combine_cs.Rd
      exclusion_criteria.Rd
* For clariy, and to avoid confusion, I increased the package version to 0.1.1.
## Test environments
* local R installation, R 4.0.3
* ubuntu 20.04 (release and devel, on github actions)
* macOS-latest (release)
* win-builder (devel on github actions)

## R CMD check results

0 errors | 0 warnings | 1 note

Maintainer: 'Angelos-Miltiadis Krypotos <amkrypotos@gmail.com>'

New submission

Possibly mis-spelled words in DESCRIPTION:
  Gerlicher (11:448)
  Jens (11:472)
  Klingelhofer (11:459)
  Krypotos (11:480)
  Lonsdorf (11:438)
  Multiverse (3:8)
  multiverse (11:71)
  
The words are not mis-spelled as they refer to authors' names and the name of the type of the statistical analyses the package is performing.  
  
