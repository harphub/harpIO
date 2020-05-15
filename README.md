
<!-- README.md is generated from README.Rmd. Please edit that file -->

<style>
  body{
    text-align: justify;
  }
</style>

# harpIO <a href=#><img src='man/figures/harp_logo_dark.svg' align="right" height="131.5" style="margin-left:30px" /></a>

**harpIO** provides a set of read and write functions for harp plus the
option to transform gridded data to point data, cross sections or other
grids at read time. **harpIO** is able to read grib1, grib2, FA, NetCDF
and vfld files from weather forecast / climate models; station
observations from vobs files, radar / satellite observations from HDF5
files and in house databases at selected meteorological institutes. Some
of these formats require support packages to be installed and you will
be given advice on which packages to install when you attempt to read
those data.

## Installation

``` r
install.packages("remotes")
remotes::install_github("andrew-MET/harpIO")
```

### System libraries

**harpIO** uses the [meteogrid](https://github.com/adeckmyn/meteogrid)
package for interpolation and the geogrid class for storing gridded
data. This package makes use of the [PROJ4](https://proj4.org) library
for handling projections. If you have the PROJ libraries installed in a
standard location (e.g. /usr/local) meteogrid will install without
problems. However, if the PROJ libraries are in a non standard location,
you need to tell the install function where they are:

``` r
remotes::install_github(
  "andrew-MET/harpIO",
  configure.args = c(
    meteogrid = "--with-proj-lib=/path/to/proj/lib --with-proj-include=/path/to/proj/include"
  )
)
```

## Reading, transforming and writing forecast data

The main workhorse for **harpIO** is `read_forecast()`. This function
can read multiple files from multiple sources of both gridded and point
data. See the article [Reading forcast model
data](articles/read_raw_forecast.html) for more information.

For gridded data you have the option to transform the data to point
data, another grid, or a vertical cross section. This is done with
`transformation` and `transformation_opts` arguments. Interpolation to
points is done by providing a data frame of latitiude and longitude
locations, regridding is done by providing a definition of the grid to
which to regrid (and if necessary reproject) the data, and a cross
section is obtained by providing the latitude and longitude of the end
points. See the article [Transforming forecast model
data](articles/transformations.html) for more information.
