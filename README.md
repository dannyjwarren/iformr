
# iformr

## Overview

This package provides a collection of tools to leverage the power of the
iFormBuilder API. The package is in a developmental state. Additional
functions will be added as time allows. Contributions are always
welcome. A big thanks to Bill DeVoe of the Maine Department of Marine
Resources for several contributions. Please see function documentation
for author credits.

iFormBuilder <https://www.iformbuilder.com/> provides a commercial
platform for mobile data collection. The platform includes a number of
web-based tools to build and distribute mobile forms, assign users, edit
data, and retrieve data from the cloud. Unfortunately, the
point-and-click web interface can become a major bottleneck as data
volume increases, or you try to push the limits of form design.

Certain tasks, such a preloading large option-lists with newly collected
data, are often impractical unless they can be automated. Fortunately,
the platform also includes an excellent *Application Programming
Interface* (API) <http://docs.iformbuilder.apiary.io/#>.

Just about anything that can be done using the iFormBuilder web
interface can be done infinitely faster using the API. However, for a
majority of users, requesting access tokens, or learning the correct
syntax to navigate the API can be an insurmountable hurdle. The
functions in this repository aim to lower the threshold so that
biologists, or field staff with only limited R programming experience
will feel comfortable working with the API. The aim is to make form
building, data collection, and data retrieval more efficient.

This package was inspired by the Hadley Wickham’s httr package. In
particular the *oauth-server-side.R* file at:
<https://github.com/r-lib/httr/blob/master/R/oauth-server-side.R>.
Encoding and signing JSON web tokens now relies on the `jose` package by
Jeroen Ooms <https://github.com/jeroen/jose>. Thanks to Danny Warren
(WDFW) for identifying issues where requests for access tokens would
occasionaly fail. That issue should now be fixed by using the `jose`
package to sign and encode JSON web tokens.

## Installation

You will need to install R-3.4.1 or higher.

``` r
# Install the development version from GitHub:
# install.packages("remotes")
remotes::install_github("dannyjwarren/iformr")
```

## Storing API connection credentials

In order to use the functions in this package you **must** first get a
`client_key` and `client_secret` for your profile from iFormBuilder. If
you have a dedicated server this can be done by logging in to your
iFormBuilder server as a *server admin* and clicking on the *Server
Admin* tab. Select *Manage Profiles*, highlight the row for the desired
profile then click *Manage*. Next select *Server Admin* \> *API Apps* \>
*New Client* and follow the directions to create a new client. You will
be asked to assign the API application to an existing *Username*. It is
good practice to create a separate, dedicated *user* for any API
requests. After creating the new client, a new *Client Key* and *Client
Secret* will be posted to the *API Applications* table.

If you do not have dedicated server you will need to contact
iFormBuilder, or check their webpage, for directions on how to obtain a
`client_key` and `client_secret`.

To add the `client_key` and `client_secret` to your `.Renviron` file you
will first need to create, or locate, your `.Renviron` file. In your R
console, enter:

``` r
normalizePath("~/")
```

The path to your `.Renviron` file will be displayed. Use RStudio to
either create a new `.Renviron` file if it does not exist, or open your
existing `.Renviron` file from the specified location. The `.Renviron`
is a simple text file and will look similar to:

    MyProfileNameClientKey=eb7411c1525c486ae7a64727965c3e042eab7389
    MyProfileNameClientSecret=471dbbe5061f4c1db9141c48cc1d0d5a96e60cc6

Simply add your new `client_key` and `client_secret` as in the example
above. The *client\_key\_name* is the section before the equals sign.
The `client_key` is the value following the equals sign. The same
pattern is used for the `client secret`. You can enter any convenient
name for the name portion. These names are what you will enter as
arguments to the `get_iform_access_token()` function to request an
`access_token`.

Make sure there is an extra space at the bottom of the last entry in the
`.Renviron` file before saving, or R will silently fail to read the
entries. You will need to close RStudio and then reopen after saving.
Afterwards, every time you open RStudio the environment variables will
be available in your session.

Detailed instructions on how to add credentials to your `.Renviron` file
can be found in the Appendix section of:
<https://cran.r-project.org/web/packages/httr/vignettes/api-packages.html>

Obtaining an `access_token` is a prerequisite for any requests made to
the iFormBuilder API.

## Request limits and usage notes

Please see the introduction section of the iFormBuilder
[API](http://docs.iformbuilder.apiary.io/#reference/user-resource/user)
for general notes on API usage and limits.

There are limits to the number of items you can retrieve per request.
You can typically request up to `1000` records of data from individual
forms or subforms. The same `1000` record limit applies to requests for
items in option lists. Other request are limited to `100` items. For
example, if you need to retrieve a list of the names of all option lists
in your profile, you can only retrieve `100` in each request. This is
more fully explained in the link above. If you hit these limits you can
simply make additional requests and set the `offset` parameter to skip
previously returned records.

The introduction section provides examples of how to specify the
*fields* you need to return from any given request, and how to set
request *parameters*. **Please read** the [API
introduction](http://docs.iformbuilder.apiary.io/#reference/user-resource/user)
before proceeding.

## Building mobile forms

Contributions by Bill DeVoe (<William.DeVoe@maine.gov>) provide a set of
tools to generate the basic structure of mobile forms directly from R
dataframes. See the `create_page()`, `copy_page()`, `rename_page()`, and
`data2form()` functions. By programatically generating mobile forms from
data, the otherwise tedious task of creating mobile forms by hand can be
standardized and made considerably more reliable and efficient.

For those just beginning to learn form design, [Mobile forms for salmon
surveys](https://arestrom.github.io/mf4ss/) may serve as a useful
starting point. This online text steps through the manual process of
form design using the standard iFormBuilder toolset.
