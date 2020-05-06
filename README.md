# gulf.utils

Set of utility functions for the southern Gulf of Saint Lawrence. 

## Function overview:

### Clearing functions:

Function | Description
-------- | -------------
`clm`    | Clear varibles from memory
`clg`    | Clear graphics windows
`clc`    | Clear the **R** console

### Date and time functions:

Function.   | Description
----------- | -------------
`date`      | Create, reformat or extract date objects.
`time`      | Create, reformat or extract time objects.
`time2sec`  | Convert time to seconds.
`time2min`  | Convert time to minutes.
`time2hour` | Convert time to hours.
`time2day`  | Convert time to days.
`week`      | Convert time to week of the year.
`julian`    | Convert time to julian day (i.e. day of the year).

### Data frame functions:

These are functions which were added to extend `data.frame` objects' functionality:

Function | Description
-------- | -------------
`sort`   | Sort a data frame.
`match`  | Return the row indices of matching entries between two data frames.

### Data export

Function | Description
-------- | -------------
`excel`  | Send data object to MS Excel.

### Metadata functions:

These are functions to define or extract various types of metadata associated with a data object.

Function      | Description
------------- | -------------
`key`         | Set or extract an index key for a data table.
`description` | Set or extract a text description(s) for a data table or variable field.
`format`      | Set or extract the format defintion of a variable field.
`units`       | Set or extract the units of a variable field.
