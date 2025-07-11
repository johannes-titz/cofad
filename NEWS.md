# cofad (development version)

# cofad 0.3.3

* report correct df for t test

# cofad 0.3.2

* change dv name for akan

# cofad 0.3.1

* fix github action for joss paper
* fix some typos
* add install instructions for cran
* remove rtools instructions (only relevant for dev version)
* change order of condition variable for akan data set
* add Maraver 2021 data set

# cofad 0.3.0

* several bugs were fixed including rare occasions, where the order of factors was not treated correctly
* included a helper function to calculate differences between two sets of lambdas for a competing contrast analysis, including documentation
* added data examples
* remove plyr as dependency
* spell check
* change maintainer to Johannes Titz, change order of authors, add contributors Mirka Henninger and Simone Malejka
* improve summary functions, content and display
* modify shiny GUI to use normal elements due to instability with moving elements (sortable)
* migrate to shinytest2
* include test for aggregated function, make it work with summary
* deduplicate code (reuse between for mixed)
* update summary for mixed

# cofad 0.2.1

* small improvements in documentation, references and paper for the official
publication at journal of open source software

# cofad 0.2.0

* Added a `NEWS.md` file to track changes to the package.
* Added Shiny GUI.
* Improved structure of the package.
* Fixed Bug with 0-variance conditions.
* Improved README.
* Improved examples.
* Improved documentation.
* Added and documented data sets.
* Added function for aggregated data.
