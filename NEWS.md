# SLAM (development version)

# SLAM 0.1.0.9001

* refactored `add_delta()`, `merge_difdate()`, `merge_diftime()`
* longplot and histoplot from Remy were added
* `longplot()` renamed to `plot_long()`
* `histoplot()` renamed to `plot_bar()`
* `plot_mfreq()` added

# SLAM 0.1.0.9000

* Added a `NEWS.md` file to track changes to the package.
* Added installation advice to `README.md`
* Added authors to functions and references to datasets.
* Added first version of `merge_difdate()`. This function was built
on the data.table. So it should be faster than `merge_diftime()`.
* Fixed `merge_diftime()` and `merge_difdate()` bug if two dates are provided. 

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
