# SLAM (development version)

* Removed 4 `data_SLAM_*` datasets and replaced them with 2 simplified ones:
`gluc` and `nmr`. These only contain cohorts 1-3 and they have both census and
survival information to make examples shorter.
* `add_delta()` was renamed to `mutate_delta()`
* All examples were migrated from the Roxygen documentation to their own R
files. 

# SLAM 0.2.0

* New Functions
    - `plot_long()` Function takes in data, and plots points and lines
longitudinally, using loess (you can change span, but it is set to default)
You can also choose to show the local maximum for analytical purposes. Thank
you Remy!
    - `plot_bar()` Function takes in a database, and plots points based on 3                  thank you Remy!
variables - y-axis, x-axis, and grouping. Function calculates standard
deviations and means by group and point on x-axis. Only y-axis needs to be
numeric, but if you want to ensure the x-axis are ordered correctly, I
suggest ordering it before plotting. Thank you Remy! 
    - `plot_mfreq()`
* Refactored Functions
    - `add_delta()`
    - `merge_difdate()`
    - `merge_diftime()`
* Features Added
    - `NEWS.md` 
    - `README.md`

# SLAM 0.1.0

* Added `add_delta()` function. This function allows you to take lagged or 
leading rolling differences for numeric values. It will calculate the leading or 
lagging differences based only on non-NA observations. Also the user can specify
an integer value for the size of the shift window. Thank you Jorge for this 
function.
* Fixed bug in `merge_diftime()`. The bug caused the second date to be a numeric 
value instead of a date has been fixed. Thank you Jorge for pointing this out 
and providing a solution.
* missForest is no longer a package import; it is now suggested. This makes 
R < 4.1 users be able to install the SLAM package easier because the current 
missForest version is only available for R >= 4.1 users. However, this means you 
may have to manually install the missForest package if you would like to use the 
function `impute_mf()`. This can be done using the install.packages command. 

# SLAM 0.0.3

* Updated Author

# SLAM 0.0.2

* Fix bug in `surv_tmerge()` that replicated the age for every measurement of 
each subject.

# SLAM 0.0.1

* First release 
* Included 5 functions:
    1. `surv_tmerge()` - creates a dataframe for repeated measures cox analysis
    2. `surv_cox()` - creates a cox model
    3. `surv_gethr()` - extracts hazard ratios from your cox model
    4. `merge_diftime()` - merges two datasets that has different measurement times
    5. `impute_mf()` - imputes missing values in a dataframe using missForest package
(CAUTION: not tested)
* Included 4 dataframes:
    1. `data_SLAM_census` - SLAM census info
    2. `data_SLAM_surv` - SLAM survival info
    3. `data_SLAM_gluc` - SLAM glucose info
    4. `data_SLAM_nmr` - SLAM nmr info
