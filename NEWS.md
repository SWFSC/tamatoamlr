# tamatoamlr 1.0.0.9000 (in development)

* Renamed package to `tamatoamlr`

* Package now depends on v1.1+ of the `amlrian` package (previously `amlrDatabases`)

* Updated licence to Apache 2.0 to match current NMFS recommendations

* Added `dbplyr` to Imports to explicitly require it for database work

* Update `extract.R` and various modules to build queries first, and then collect from the database. [#8]

* Updated various tab functionality, especially AFS DCC, cape-wide pup census, tag resights, and MMPA takes

* Updated `afs_capewide_pup` functions: renamed functions including changing to 'cwp_' prefix; added `cwp_loc_agg` to aggregate across locations in a consistent fashion; bug fixes (#10)

* Extracted `dcc` functions from Shiny app and made them generalized functions for processing DCC download data. 

* Added DCC sample data [#5]

* Added tests for `dcc`, `csphoc`, and `cwp` functions [e.g., #6]

* Added "Punta Las Torres" and "Penguin Colonies" to variable `csphoc.core.location.groups`

* Updated `tamatoa` to only allow pre-defined connections via a filedsn argument

* Added `mult_date_filter` for filtering census data frames for the closest records from each season to a given date.


# amlrPinnipeds 0.5.0

* Requires [amlrDatabases](https://github.com/us-amlr/amlrDatabases) >= v0.7.0. Specifically, `amlrDatabases::mod_output_server` now does not take a `parent` argument

* Updated afs_capewide_pup_census module to handle new SQL table structures, and added new plots and displays for single and multiple seasons

* Added `afs_capewide_pup` functions

* Renamed `csphoc_complete_aggregated` to `complete_csphoc` for consistency with other function names. `complete_csphoc` now does not do its own factor logic before using `complete`. In addition, users now only pass the counts data frame to the function.


# amlrPinnipeds 0.4.0

* amlrPinnipeds now depends on the following additional packages: [hms](https://github.com/tidyverse/hms), [forcats](https://github.com/tidyverse/forcats), and [stringi](https://github.com/gagolews/stringi)

* Updated `mutate_location` to keep in step with name column in beaches table

* `mutate_location` now uses `stringi::stri_escape_unicode` to escape all non-ASCII characters

* Added `csphoc.core.location.groups` variable to define core census locations for CS-PHOC data paper

* Added 'census_counts' functions `sum_count` and `total_count` to handle common situations when working with US AMLR Pinniped census data

* Added `csphoc_complete_aggregated` to complete aggregated US AMLR CS-PHOC data, by header ID and species


# amlrPinnipeds 0.3.0

* Requires [amlrDatabases](https://github.com/us-amlr/amlrDatabases) >= v0.6.0

* Added Captures tab and associated functionality from [@karensnyder](https://github.com/karensnyder)

* Added `mutate_location` for standardizing Cape Shirreff locations 

* Added `arrange_tag` for nicely sorting pinniped tag numbers

* Added extract (`tbl_...`) functions for consistent extraction of data from the database

* Added lots of additional functionality to Tamatoa during the 2022-23 field season, including: 

  - Phocid Census tab; specifically to work with new database table structure
  - AFS Study Beach Census tab
  - AFS Capewide Pup Census +/- five percent check
  - DCC tab: process DCC files from current season


# amlrPinnipeds 0.2.0

* Update database connection management to keep up with amlrDatabases v0.4.0

* Views now must have 'location' and 'location_group' variables

* Currently only the phocid census tab is displayed; other module files have been temporarily removed to facilitate testing and package checking


# amlrPinnipeds 0.1.0

* Initial release, 2021/22 field season
