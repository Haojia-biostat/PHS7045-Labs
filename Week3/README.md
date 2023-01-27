Lab 03 - Functions and data.table
================

# Learning goals

- Used advanced features of functions in R.
- Use the `merge()` function to join two datasets.
- Deal with missings and data imputation data.
- Identify relevant observations using `quantile()`.
- Practice your GitHub skills.

# Lab description

For this lab, we will deal with the meteorological dataset downloaded
from the NOAA, the `met`. We will use `data.table` to answer some
questions regarding the `met` data set, and practice our Git+GitHub
skills.

This markdown document should be rendered using `gfm` document.

# Part 1: Setup the Git project and the GitHub repository

1.  Go to your documents (or wherever you are planning to store the
    data) in your computer, and create a folder for this project, for
    example, “PHS7045-labs”

2.  In that folder, save [this
    template](https://raw.githubusercontent.com/UofUEpiBio/PHS7045-advanced-programming/main/labs/03-functions-and-datatable/03-functions-and-datatable.qmd)
    as “README.qmd.” This will be the markdown file where all the magic
    will happen.

3.  Go to your GitHub account and create a new repository, hopefully of
    the same name this folder has, i.e., “PHS7045-labs”.

4.  Initialize the Git project, add the “README.qmd” file, and make your
    first commit.

5.  Add the repo you just created on GitHub.com to the list of remotes,
    and push your commit to `origin` while setting the upstream.

Most of the steps can be done using the command line:

``` sh
# Step 1
cd ~/Documents
mkdir PHS7045-labs
cd PHS7045-labs

# Step 2
wget https://raw.githubusercontent.com/UofUEpiBio/PHS7045-advanced-programming/main/labs/03-functions-and-datatable/03-functions-and-datatable.qmd
mv 03-functions-and-datatable.qmd README.qmd

# Step 3
# Happens on github

# Step 4
git init
git add README.Rmd
git commit -m "First commit"

# Step 5
git remote add origin git@github.com:[username]/PHS7045
git push -u origin master
```

You can also complete the steps in R (replace with your paths/username
when needed)

``` r
# Step 1
setwd("~/Documents")
dir.create("PHS7045-labs")
setwd("PHS7045-labs")

# Step 2
download.file(
"https://raw.githubusercontent.com/UofUEpiBio/PHS7045-advanced-programming/main/labs/03-functions-and-datatable/03-functions-and-datatable.qmd",
destfile = "README.qmd"
)

# Step 3: Happens on Github

# Step 4
system("git init && git add README.Rmd")
system('git commit -m "First commit"')

# Step 5
system("git remote add origin git@github.com:[username]/PHS7045-labs")
system("git push -u origin master")
```

Once you are done setting up the project, you can now start working on
the lab.

# Part 2: Advanced functions

## Question 1: **Ellipsis**

Write a function using the ellipsis argument (`...`) with the goal of
(i) retrieving the list of arguments passed to it, (ii) printing
information about them using `str()`, and (iii) printing the environment
where they belong and the address of the object in memory using
`data.table::address()`.

``` r
get_ellips_arg <- function(...) {
  arguments <- list(...)
  lapply(arguments, str)
  # print(environment(arguments))
  lapply(arguments, \(x) print(data.table::address(x)))
  invisible()
}
get_ellips_arg(123, pi = pi)
```

     num 123
     num 3.14
    [1] "0000020384ced100"
    [1] "000002038782f2d8"

Knit the document, commit your changes, and push them to GitHub.

## Question 2: **Lazy evaluation**

A concept we did not review was lazy evaluation. Write a function with
two arguments (`a` and `b`) that only uses one of them as an integer,
and then call the function passing the following arguments
`(1, this_stuff)`

``` r
lazy_eval <- function(a, b) print(a)
lazy_eval(1, this_stuff)
```

    [1] 1

Knit the document, commit your changes, and push them to GitHub.

## Question 3: **Putting all together**

Write a function that fits a linear regression model and saves the
result to the global environment using the `assign()` function. The name
of the output must be passed as a symbol using lazy evaluation.

``` r
assign_fit <- function(x, y, fit_name = nobody_cares) {
  assign("fit", lm(y~x), envir = .GlobalEnv)
}
set.seed(7045)
y <- rnorm(100)
x <- runif(100)
assign_fit(x = x, y = y)
getAnywhere(fit)
```

    A single object matching 'fit' was found
    It was found in the following places
      .GlobalEnv
    with value


    Call:
    lm(formula = y ~ x)

    Coefficients:
    (Intercept)            x  
         0.4409      -0.5394  

Knit the document, commit your changes, and push them to GitHub.

# Part 3: Data.table

## Setup in R

1.  Load the `data.table` (and the `dtplyr` and `dplyr` packages if you
    plan to work with those).

``` r
library(data.table)
```

2.  Load the met data from
    https://raw.githubusercontent.com/USCbiostats/data-science-data/master/02_met/met_all.gz,
    and the station data. For the latter, you can use the code we used
    during the lecture to pre-process the stations’ data:

``` r
met <- fread("https://raw.githubusercontent.com/USCbiostats/data-science-data/master/02_met/met_all.gz")
str(met)
```

    Classes 'data.table' and 'data.frame':  2377343 obs. of  30 variables:
     $ USAFID           : int  690150 690150 690150 690150 690150 690150 690150 690150 690150 690150 ...
     $ WBAN             : int  93121 93121 93121 93121 93121 93121 93121 93121 93121 93121 ...
     $ year             : int  2019 2019 2019 2019 2019 2019 2019 2019 2019 2019 ...
     $ month            : int  8 8 8 8 8 8 8 8 8 8 ...
     $ day              : int  1 1 1 1 1 1 1 1 1 1 ...
     $ hour             : int  0 1 2 3 4 5 6 7 8 9 ...
     $ min              : int  56 56 56 56 56 56 56 56 56 56 ...
     $ lat              : num  34.3 34.3 34.3 34.3 34.3 34.3 34.3 34.3 34.3 34.3 ...
     $ lon              : num  -116 -116 -116 -116 -116 ...
     $ elev             : int  696 696 696 696 696 696 696 696 696 696 ...
     $ wind.dir         : int  220 230 230 210 120 NA 320 10 320 350 ...
     $ wind.dir.qc      : chr  "5" "5" "5" "5" ...
     $ wind.type.code   : chr  "N" "N" "N" "N" ...
     $ wind.sp          : num  5.7 8.2 6.7 5.1 2.1 0 1.5 2.1 2.6 1.5 ...
     $ wind.sp.qc       : chr  "5" "5" "5" "5" ...
     $ ceiling.ht       : int  22000 22000 22000 22000 22000 22000 22000 22000 22000 22000 ...
     $ ceiling.ht.qc    : int  5 5 5 5 5 5 5 5 5 5 ...
     $ ceiling.ht.method: chr  "9" "9" "9" "9" ...
     $ sky.cond         : chr  "N" "N" "N" "N" ...
     $ vis.dist         : int  16093 16093 16093 16093 16093 16093 16093 16093 16093 16093 ...
     $ vis.dist.qc      : chr  "5" "5" "5" "5" ...
     $ vis.var          : chr  "N" "N" "N" "N" ...
     $ vis.var.qc       : chr  "5" "5" "5" "5" ...
     $ temp             : num  37.2 35.6 34.4 33.3 32.8 31.1 29.4 28.9 27.2 26.7 ...
     $ temp.qc          : chr  "5" "5" "5" "5" ...
     $ dew.point        : num  10.6 10.6 7.2 5 5 5.6 6.1 6.7 7.8 7.8 ...
     $ dew.point.qc     : chr  "5" "5" "5" "5" ...
     $ atm.press        : num  1010 1010 1011 1012 1013 ...
     $ atm.press.qc     : int  5 5 5 5 5 5 5 5 5 5 ...
     $ rh               : num  19.9 21.8 18.5 16.9 17.4 ...
     - attr(*, ".internal.selfref")=<externalptr> 

``` r
# Download the data
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]

str(stations)
```

    Classes 'data.table' and 'data.frame':  26181 obs. of  3 variables:
     $ USAF : int  7018 7026 7070 8260 8268 8307 8411 8414 8415 8418 ...
     $ CTRY : chr  NA "AF" "AF" NA ...
     $ STATE: chr  NA NA NA NA ...
     - attr(*, ".internal.selfref")=<externalptr> 

3.  Merge the data as we did during the lecture.

``` r
length(unique(met[,USAFID]))
```

    [1] 1595

``` r
table(met[,USAFID] %in% stations[,USAF])
```


       TRUE 
    2377343 

``` r
met <- merge(met, stations, by.x = "USAFID", by.y = "USAF", all.x = T)
head(met[, list(USAFID, WBAN, STATE)])
```

       USAFID  WBAN STATE
    1: 690150 93121    CA
    2: 690150 93121    CA
    3: 690150 93121    CA
    4: 690150 93121    CA
    5: 690150 93121    CA
    6: 690150 93121    CA

## Question 1: Representative station for the US

What is the median station in terms of temperature, wind speed, and
atmospheric pressure? Look for the three weather stations that best
represent the continental US using the `quantile()` function. Do these
three coincide?

``` r
weathers <- c("temp", "wind.sp", "atm.press")
# check the distribution of weathers
lapply(met[,..weathers], summary)
```

    $temp
       Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     -40.00   19.60   23.50   23.59   27.80   56.00   60089 

    $wind.sp
       Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
       0.00    0.00    2.10    2.46    3.60   36.00   79693 

    $atm.press
       Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      960.5  1011.8  1014.1  1014.2  1016.4  1059.9 1666274 

``` r
# calculate median temperature, wind speed, and atmospheric pressure by station
med_weather_stations <- met[, lapply(.SD, median, na.rm = T), by = USAFID, .SDcols = weathers]
# median of medians
sapply(med_weather_stations[, ..weathers], quantile, probs = 0.5, na.rm = T)
```

         temp.50%   wind.sp.50% atm.press.50% 
             23.0           2.6        1014.6 

``` r
# locate the station with the value closest to each of the medians
med_weather_stations[which.min(abs(temp - 23.0)), USAFID]
```

    [1] 720277

``` r
med_weather_stations[which.min(abs(wind.sp - 2.6)), USAFID]
```

    [1] 720113

``` r
med_weather_stations[which.min(abs(atm.press - 1014.6)), USAFID]
```

    [1] 720394

Knit the document, commit your changes, and Save it on GitHub. Don’t
forget to add `README.md` to the tree, the first time you render it.

## Question 2: Representative station per state

Identify what the most representative (the median) station per state is.
Instead of looking at one variable at a time, look at the euclidean
distance. If multiple stations show in the median, select the one at the
lowest latitude.

Knit the doc and save it on GitHub.

## (optional) Question 3: In the middle?

For each state, identify the closest station to the mid-point of the
state. Combining these with the stations you identified in the previous
question, use `leaflet()` to visualize all \~100 points in the same
figure, applying different colors for those identified in this question.

Knit the doc and save it on GitHub.

## (optional) Question 4: Means of means

Using the `quantile()` function, generate a summary table that shows the
number of states included, average temperature, wind speed, and
atmospheric pressure by the variable “average temperature level,” which
you’ll need to create.

Start by computing the states’ average temperature. Use that measurement
to classify them according to the following criteria:

- low: temp \< 20
- Mid: temp \>= 20 and temp \< 25
- High: temp \>= 25

Once you are done with that, you can compute the following:

- Number of entries (records),
- Number of NA entries,
- Number of stations,
- Number of states included, and
- Mean temperature, wind speed, and atmospheric pressure.

All by the levels described before.

Knit the document, commit your changes, and push them to GitHub. If
you’d like, you can take this time to include the link of [the issue of
the
week](https://github.com/UofUEpiBio/PHS7045-advanced-programming/issues/5)
so that you let us know when you are done, e.g.,

``` bash
git commit -a -m "Finalizing lab 3 https://github.com/UofUEpiBio/PHS7045-advanced-programming/issues/5"
```
