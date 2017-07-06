
iformr
======

Overview
--------

This package is intended to provide a collection of user-friendly tools to leverage the power of the iFormBuilder API. It is in an early-development state. Additional functions will be added as time allows.

iFormBuilder <https://www.iformbuilder.com/> provides a platform for mobile data collection. The platform provides a number of user-friendly tools to build and distribute mobile forms, assign users, edit data, and retrieve data from the cloud. Unfortunately, the point-and-click web interface can become a major bottleneck as data volume increases, or you try to push the limits of form design.

Certain tasks, such a preloading large option-lists with newly collected data, are often impractical unless they can be automated. Fortunately, the platform also includes an excellent *Application Programming Interface* (API) <http://docs.iformbuilder.apiary.io/#>.

Just about anything that can be done using the iFormBuilder web interface can be done infinitely faster using the API. However, for a majority of users, requesting access tokens or learning the correct syntax to navigate the API can be an insurmountable hurdle. The functions in this repository aim to lower the threshold so that biologists, or field staff with only minimal programming experience will feel comfortable with the API. The aim is to make form building, data collection, and data retrieval much more efficient.

Please see the setup instructions and example files for notes on usage.

This package was inspired by the Hadley Wickham's httr package. In particular the *oauth-server-side.R* file at: <https://github.com/r-lib/httr/blob/master/R/oauth-server-side.R>. The *hex\_to\_raw()* function from Jeroen Ooms openssl package has also been incorporated.

Installation
------------

There was a bug in R-3.4.0 that prevented installation of this package. You will need to install R-3.4.1 or higher

``` r
# Install the development version from GitHub:
# install.packages("devtools")
devtools::install_github("arestrom/iformr")
```
