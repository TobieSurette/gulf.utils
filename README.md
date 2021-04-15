## `gulf` Packages:

Function          | Description
----------------- | -------------------------------
`gulf.utils`      | Core utility functions.
`gulf.metadata`   | Functions to assign or extract metadata associated with data objects.
`gulf.data`       | Functions to access, manipulate and summarize sGSL data sets.
`gulf.trawl.data` | Provide access to trawl probe data from sGSL Science surveys.
`gulf.graphics`   | Data plotting functions.
`gulf.spatial`    | Functions to perform spatial plots and access spatial data from the sGSL.
`gulf.manage`     | Functions to manage sGSL data and 'gulf' packages.

### Installation:

Packages from the `gulf` series of packages can be installed directly from GitHub by running the following code from R:

```
library(devtools)
install_github("TobieSurette/gulf.utils")
install.gulf()
```

# `gulf.utils` package

The `gulf.utils` contains a core set of utility functions used in various 'gulf' packages. Various 'gulf' packages are available, each containing specific functions to aid in accessing, manipulating and analyzing Southern Gulf of Saint Lawrence (sGSL) data sets.

## Clearing functions:

Function | Description
-------- | --------------------------
`clm`    | Clear varibles from memory
`clg`    | Clear graphics windows
`clc`    | Clear the **R** console

## Date and time functions:

Function.   | Description
----------- | --------------------------------------------------
`date`      | Create, reformat or extract date objects.
`time`      | Create, reformat or extract time objects.
`julian`    | Convert date to julian day (i.e. day of the year).
`year`      | Extract year from date.
`month`     | Extract month from date.
`day`       | Extract day from date.
`week`      | Convert date to week of the year.
`time2sec`  | Convert time to seconds.
`time2min`  | Convert time to minutes.
`time2hour` | Convert time to hours.
`time2day`  | Convert time to days.

## Data frame functions:

These are functions which were added to extend `data.frame` objects' functionality:

Function   | Description
---------- | ---------------------------------------------------------------------------------------
`deflate`  | Shrink data frame by removing empty fields and attaching singular fields as attributes. 
`describe` | Prints a summary of data frame contents.
`import`   | Import data fields from one data frame to another.
`inflate`  | Opposite of `deflate`.
`match`    | Return the row indices of matching entries between two data frames.
`sort`     | Sort a data frame.
`squeeze`  | Remove empty column fields from a data frame. 

## Text functions:

These are functions to format, correct or translate text.

Function    | Description
---------   | -----------------------------------------------------------------------------------
`deblank`   | Remove leading, trailing and repeating blanks in a string.
`en2fr`     | Translate common fisheries science words and short phrases from english to french.
`fr2en`     | Translate common fisheries science words and short phrases from french to english.
`language`  | Language options.
`lexicon`   | Return the set of words found in text.
`spelling`  | Correct word spelling.
`translate` | Dictionary translation of common fisheries terms. 

## Data export functions:

Function | Description
-------- | -----------------------------
`excel`  | Send data object to MS Excel.
