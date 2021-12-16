# gpbonus

*An R package*

The goal of gpbonus is to compute student grade points with a bonus by minimizing the absolute difference between the fixed significant level 0.05 and a p-value of the Kolmogorov-Smirnov test which compares a distribution of grade points with the fixed normal distribution.

## Example

These are examples which shows you how to use the package:

``` r
gp <- c(43, 72, 58, 60, 27, 51, 69, 51, 60, 66, 65, 53, 69, 81, 61, 75, 54, 33, 47, 49, 54, 40)
new.gp <- gpbonus::gp_bonus(gp)
print(new.gp)
freq <- gpbonus::gp_summary(new.gp$new.grade.points)
print(freq)
```

``` r
gp <- c(72, "E", 51, 69, "WF", 81, 61, 75, 54, "W")
new.gp <- gpbonus::gp_bonus(gp)
print(new.gp)
freq <- gpbonus::gp_summary(new.gp$new.grade.points)
print(freq)
```

## Installation

``` r
devtools::install_github("galaamn/gpbonus")
```

## How to Developed The Package

1. Create a new package by using RStudio.
2. Open "Project Options" and go to "Build Tools". Check "Generate Documentation with Roxygen".
3. Add GPLv3 license with the devtools::use_gpl3_license() function.
4. Edit DESCRIPTION file.
5. Rename "R/hello.R" to "R/functions.R".
6. Declare new functions and write documentation in the "R/functions.R" file.
7. Check and fix errors by using commands: 'Install and Restart' and 'Check Package'.
8. Prepare for package testing by using devtools::use_testthat() function and writing tests. After that test and fix functions by using the 'Test Package' command.
9. devtools::use_readme_md() and edit the README.md file.
10. Build a source or a binary package.

Some useful keyboard shortcuts for package authoring:

- Document: 'Ctrl + Shift + D'
- Install and Restart: 'Ctrl + Shift + B'
- Check Package: 'Ctrl + Shift + E'
- Test Package: 'Ctrl + Shift + T'

## Author

© 2018 MAKHGAL Ganbold [www.galaa.mn](http://galaa.mn/)
