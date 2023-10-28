Assignment B-1: Making a function
================
Eully Ao

## Introduction

In this assignment, I will be creating, documenting, and testing a
function. There are 4 exercises:

1.  Making a function
2.  Documenting the function
3.  Examples of using the function
4.  Testing the function

To begin, load packages required for this assignment.

``` r
library(tidyverse)
library(roxygen2)
library(datateachr)
library(testthat)
```

## Exercise 1 & 2

In the first exercise, I will be creating the function. I would like to
create a function that, when used with a dataframe with samples,
produces a new dataframe that has summary statistics for a given
variable with the dataset, grouped by another variable. While I only
used this once previously, I still think it could be useful for future
analyses. The calculated statistics I will include in this function are
range, mean, median, and standard deviation. In the second exercises, I
will be documenting the function using `roxygen2` tags.

References: [curly
curly](https://dcl-prog.stanford.edu/tidy-eval-basics.htmlhttps://dcl-prog.stanford.edu/tidy-eval-basics.html),
[if else](https://www.datamentor.io/r-programming/if-else-statement),
[documenting](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html)

``` r
#' Calculate Summary Statistics
#'
#' 'calc_summary_stats' calculates summary statistics for a given variable within a dataset, 
#' grouped by another variable.
#'
#' @param data A data frame containing the dataset.
#' @param grouping_variable A character string specifying the variable by which the data 
#' should be grouped, therefore named grouping_variable.
#' @param data_variable A character string specifying the variable for which summary 
#' statistics will be calculated. The variable is wthin a dataset, thus named data_variable.
#'
#' @return A data frame containing the summary statistics range, mean, median, 
#' and standard deviation.

calc_summary_stats <- function(data, grouping_variable, data_variable) { 
  if (data %>%  pull({{data_variable}}) %>% is.numeric()) {
    summary_stats <- data %>%
    group_by({{grouping_variable}}) %>%
    summarise( 
      range = max({{data_variable}}) - min({{data_variable}}),
      mean = mean({{data_variable}}, na.rm = TRUE),
      median = median({{data_variable}}, na.rm = TRUE),
      std_dev = sd({{data_variable}}, na.rm = TRUE))
    return(summary_stats)
  } else {
    stop ("The specificed 'data_variable' must contain numeric values.")
  }
}
```

## Exercise 3

In this example, I will demonstrate the usage of `calc_summary_stats`
with a few examples.

First, using the `vancouver_trees` dataset from the package
`datateachr`, I will group the trees by their species, indicated by
`species_name`, and calculate the summary statistics for the variable
`diameter` using the function. Then, I will produce a dataframe named
`trees_summary` with the results.

``` r
# load dataset as dataframe
vancouver_trees <- vancouver_trees
head(vancouver_trees)
```

    ## # A tibble: 6 × 20
    ##   tree_id civic_number std_street genus_name species_name cultivar_name  
    ##     <dbl>        <dbl> <chr>      <chr>      <chr>        <chr>          
    ## 1  149556          494 W 58TH AV  ULMUS      AMERICANA    BRANDON        
    ## 2  149563          450 W 58TH AV  ZELKOVA    SERRATA      <NA>           
    ## 3  149579         4994 WINDSOR ST STYRAX     JAPONICA     <NA>           
    ## 4  149590          858 E 39TH AV  FRAXINUS   AMERICANA    AUTUMN APPLAUSE
    ## 5  149604         5032 WINDSOR ST ACER       CAMPESTRE    <NA>           
    ## 6  149616          585 W 61ST AV  PYRUS      CALLERYANA   CHANTICLEER    
    ## # ℹ 14 more variables: common_name <chr>, assigned <chr>, root_barrier <chr>,
    ## #   plant_area <chr>, on_street_block <dbl>, on_street <chr>,
    ## #   neighbourhood_name <chr>, street_side_name <chr>, height_range_id <dbl>,
    ## #   diameter <dbl>, curb <chr>, date_planted <date>, longitude <dbl>,
    ## #   latitude <dbl>

``` r
# try function
trees_diameter_summary <- calc_summary_stats(data = vancouver_trees, grouping_variable = species_name, data_variable = diameter)

# view results
head(trees_diameter_summary)
```

    ## # A tibble: 6 × 5
    ##   species_name   range  mean median std_dev
    ##   <chr>          <dbl> <dbl>  <dbl>   <dbl>
    ## 1 ABIES           33   12.9    12      7.51
    ## 2 ACERIFOLIA   X  55   20.8    19     11.8 
    ## 3 ACUMINATA       26   10.9     7      9.91
    ## 4 ACUTISSIMA      34    8.87    8.5    4.33
    ## 5 AILANTHIFOLIA   16   32      34      6.44
    ## 6 ALBA            38.5 19.4    20.2   14.4

Next, using the `iris` dataset built into R, I will group the samples by
species, indicated by `Species`, and calculate the summary statistics
for the variable `Sepal.Length` using the function. Then, I will produce
a dataframe named `sepal_length_summary` with the results.

``` r
# load dataset as dataframe
iris <- iris
head(iris)
```

    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ## 1          5.1         3.5          1.4         0.2  setosa
    ## 2          4.9         3.0          1.4         0.2  setosa
    ## 3          4.7         3.2          1.3         0.2  setosa
    ## 4          4.6         3.1          1.5         0.2  setosa
    ## 5          5.0         3.6          1.4         0.2  setosa
    ## 6          5.4         3.9          1.7         0.4  setosa

``` r
# try function
sepal_length_summary <- calc_summary_stats(data = iris, grouping_variable = Species, data_variable = Sepal.Length)

# view results
head(sepal_length_summary)
```

    ## # A tibble: 3 × 5
    ##   Species    range  mean median std_dev
    ##   <fct>      <dbl> <dbl>  <dbl>   <dbl>
    ## 1 setosa       1.5  5.01    5     0.352
    ## 2 versicolor   2.1  5.94    5.9   0.516
    ## 3 virginica    3    6.59    6.5   0.636

Next, using the `PlantGrowth` dataset built into R, I will group the
samples by their treatment group, indicated by `group`, and calculate
the summary statistics for the variable `weight` using the function.
Then, I will produce a dataframe named `plant_weight_summary` with the
results.

``` r
# load dataset as dataframe
PlantGrowth <- PlantGrowth
head(PlantGrowth)
```

    ##   weight group
    ## 1   4.17  ctrl
    ## 2   5.58  ctrl
    ## 3   5.18  ctrl
    ## 4   6.11  ctrl
    ## 5   4.50  ctrl
    ## 6   4.61  ctrl

``` r
# try function
plant_weight_summary <- calc_summary_stats(data = PlantGrowth, grouping_variable = group, data_variable = weight)

# view results
head(plant_weight_summary)
```

    ## # A tibble: 3 × 5
    ##   group range  mean median std_dev
    ##   <fct> <dbl> <dbl>  <dbl>   <dbl>
    ## 1 ctrl   1.94  5.03   5.15   0.583
    ## 2 trt1   2.44  4.66   4.55   0.794
    ## 3 trt2   1.39  5.53   5.44   0.443

Now, to demonstrate an error, I will use the `vancouver_trees` dataset
to try to calculate summary statistics for non-numeric values, such as
those found in the column `neighbourhood_name`.

``` r
# is neighbourhood_name a numeric variable? 
is.numeric(vancouver_trees$neighbourhood_name)
```

    ## [1] FALSE

``` r
# try function, using neighbourhood_name as data_variable
trees_error <- calc_summary_stats(data = vancouver_trees, grouping_variable = species, data_variable = neighbourhood_name)
```

    ## Error in calc_summary_stats(data = vancouver_trees, grouping_variable = species, : The specificed 'data_variable' must contain numeric values.

Because the values found in `neighbourhood_name` are not numeric, the
error message “The specified ‘data_variable’ must contain numeric
values” pops up.

## Exercise 4

In this exercises, I will be testing the function using the `expect_()`
function from the `testthat` package.

First, I will test the function with a vector with no NA’s. I will
create a small one for this example. The test checks that the result
`result` is a dataframe using `expect_is`. Using `expect_equal`, it will
check that the result has 5 columns (one for `data_variable` and one
each for range, mean, median, and standard deviation), and the variables
are grouped correctly. It will also checks whether the values for each
of those statistics are correct, with tolerance for numbers that are not
exactly the same to be considered equal due to decimals.

``` r
# create test using a dataframe that has no NA's
test_that("calc_summary_stats with vector containing no NA's", {
  data <- data.frame(
    grouping_variable = c("A", "B", "A", "B"),
    data_variable = c(5, 10, 15, 20)
  )
  result <- calc_summary_stats(data, grouping_variable, data_variable)
  
  expect_is(result, "data.frame") # check that the resulting object is a dataframe
  expect_equal(ncol(result), 5) # check that there are 5 columns
  expect_equal(result$grouping_variable, c("A", "B")) # check that the samples are grouped correctly
  expect_equal(result$range, c(10, 10)) # check range 
  expect_equal(result$mean, c(10, 15)) # check mean 
  expect_equal(result$median, c(10, 15)) # check median
  expect_equal(result$std_dev, c(7.07, 7.07), tolerance = 1e5)
  
})
```

    ## Test passed 🎉

Next, I will test the function with a vector with NA’s. Again, I will
create a small one for this example and run a similar test as the
previous example.

``` r
# create test using a dataframe that has no NA's
test_that("calc_summary_stats with vector with NA's", {
  data <- data.frame(
    grouping_variable = c("A", "B", "A", "B"),
    data_variable = c(5, 10, NA, 20)
  )
  result <- calc_summary_stats(data, grouping_variable, data_variable)
  
  expect_is(result, "data.frame") # check that the resulting object is a dataframe
  expect_equal(ncol(result), 5) # check that there are 5 columns
  expect_equal(result$grouping_variable, c("A", "B")) # check that the samples are grouped correctly
  expect_equal(result$range, c(NA, 10)) # check range 
  expect_equal(result$mean, c(5, 15)) # check mean 
  expect_equal(result$median, c(5, 15)) # check median
  expect_equal(result$std_dev, c(NA, 7.07), tolerance = 1e5)
  
})
```

    ## Test passed 🎉

Finally, I will also check that the function will not work if the values
are not numeric by using `expect_error`.

``` r
# create test using a dataframe that has non numeric values
test_that("calc_summary_stats with non-numeric data_variable", {
  data <- data.frame(
    grouping_variable = c("A", "B", "A", "B"),
    data_variable = c("five", "ten", "fifteen", "twenty")
  )

  expect_error(calc_summary_stats(data, grouping_variable, data_variable))
})
```

    ## Test passed 🌈

All together, the tests show that the function works as expected.
