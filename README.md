# exforParser - R package

The functionality of this package allows to extract 
the information from EXFOR entries given as character
strings and put it into a nested list that can be 
easily handled in R. It is also possible to convert
EXFOR entries to JSON objects that can be conveniently
imported in other high-level languages such as Python.

## Requirements

Being an R package, it requires the R interpreter and
depends on the Rcpp package.

## Installation

The package can be installed from the command line via
```
git clone https://github.com/gschnabel/exforParser.git
R CMD INSTALL exforParser
```

## Basic usage

The following code snippet shows how to load an
entry from a file: 
```
library(exforParser)

# read an EXFOR entry from a file into a character string
# exforFilePath can be replaced by a hard-coded string pointing to
# some other file containing an EXFOR entry
exforFilePath <- system.file("extdata", "entry_20917.txt", package="exforParser")
# An alternative EXFOR entry for exploration: 
# exforFilePath <- system.file("extdata", "entry_22871.txt", package="exforParser")
exforEntry <- paste0(readLines(exforFilePath), collapse="\n")

# extract the information from the character string
# and convert it to a nested list 
exforList <- parseEntry(exforEntry)
```

The variable `exforList` is a nested list which gives convenient
access to the various fields of the EXFOR entry.
Here are some examplary commands that show how information
about an EXFOR entry can be retrieved:

```
# show names of the top-level elements
names(exforList)
# show number of subentries in the entry
length(exforList$SUBENT)
# show the first subentry
exforList$SUBENT[[1]]
# show the author information
exforList$SUBENT[[1]]$BIB$AUTHOR
# show the detector used
exforList$SUBENT[[1]]$BIB$DETECTOR
# show the column names of the DATA table
exforList$SUBENT[[2]]$DATA$DESCR
# show the units of the columns in the DATA table
exforList$SUBENT[[2]]$DATA$UNIT
# show the DATA table itself
exforList$SUBENT[[2]]$DATA$TABLE
```

If the data should be exported to another programming 
language, the JSON format can be used as intermediate
format:

```
jsonString <- convToJSON(exforList)
cat(jsonString, file="testfile.json")
# file is created in the working directory
# check with the command 'getwd()' to figure out
# the current working directory
```

It is also possible to export only a few subentries
of the EXFOR entry:
```
jsonString <- convToJSON(exforList$SUBENT[1:3])
cat(jsonString, file="testfile.json")
```

For working with the EXFOR data, it may be convenient to 
add the general information in the first subentry
to another subentry of the same EXFOR entry.
Another simplification is the inclusion of the information
in the COMMON blocks to the DATA block, even if this 
means duplication of information.
Finally, it facilitates processing if units are standardized,
i.e. all energies given as MeV and cross sections given as 
millibarn.
The mentioned transformations can be performed using 
function `transformSubent`:

```
transfSubent <- transformSubent(exforList$SUBENT[[1]], exforList$SUBENT[[2]])
```


