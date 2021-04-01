Big thanks to Julia Haider for checking the package. Following the comments, I did the following:

- Reduced the length of the title to 41 characters
- I did not include in the description the words such as "This package", package name, title or similar. No such mention was done in the first release but I was repeating the title so I changed that as well.
- I included a reference in the used methods in the DESCRIPTION file as follows: The methods are described in Lonsdorf, Gerlicher, Klingelhofer-Jens, & Krypotos <doi:10.31234/osf.io/2z6pd>.
- I added values to the .Rd files of the following functions: 
      chop_css.Rd: \value
      combine_cs.Rd: \value
      exclusion_criteria.Rd: \value

## Test environments
* local R installation, R 4.0.3
* ubuntu 16.04 (on travis-ci), R 4.0.3
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
