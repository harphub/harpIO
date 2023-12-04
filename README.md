
<!-- README.md is generated from README.Rmd. Please edit that file -->

# harpIO <img src='man/figures/harp_logo_dark.svg' align="right" width ="80" />

{harpIO} provides a set of read and write functions for harp plus the
option to transform gridded data to point data, cross sections or other
grids at read time. {harpIO} is able to read *grib1*, *grib2*, *FA*,
*NetCDF* and *vfld* files from weather forecast / climate models;
station observations from *vobs* files, radar / satellite observations
from *HDF5* files and in the future from in-house databases at selected
meteorological institutes. Some of these formats require support
packages to be installed and you will be given advice on which packages
to install when you attempt to read those data.

## Installation

``` r
install.packages("remotes")
remotes::install_github("harphub/harpIO")
```

### System libraries

{harpIO} uses the [{meteogrid}](https://github.com/harphub/meteogrid)
package for interpolation and the geogrid class for storing gridded
data. This package makes use of the [PROJ4](https://proj4.org) library
for handling projections. If you do not have PROJ4 installed you can
install with:

``` bash
sudo apt-get install libproj-dev
```

If you do not have sudo rights, speak to your system administrator.

If you have the PROJ libraries installed in a standard location
(e.g. /usr/local) meteogrid will install without problems. However, if
the PROJ libraries are in a non standard location, you need to tell the
install function where they are:

``` r
remotes::install_github(
  "harphub/harpIO",
  configure.args = c(
    meteogrid = "--with-proj-lib=/path/to/proj/lib --with-proj-include=/path/to/proj/include"
  )
)
```

Alternatively you can set environment variables

``` bash
export PROJ_LIB_PATH=/path/to/proj/lib
export PROJ_INCLUDE_PATH=/path/to/proj/include
```

If you include these environment variables in your .bashrc file, or
equivalent, you won’t need to worry about it when you wish to install an
update to meteogrid.

Or you can set compiler and linker options in the file
\$HOME/.R/Makevars

``` bash
CPPFLAGS=-I/path/to/proj/include
LDFLAGS=-L/path/to/proj/lib -Wl,-rpath,/path/to/proj/lib
```

In this case you only have to set them once and not worry about it when
you wish to install an update to meteogrid… However, there is a danger
that setting Makevars explicitly can impact the installation of other
packages, so in general it is good practice to remove or rename
\$HOME/.R/Makevars after successful installation.

When setting environment variables or creating a Makevars file, R must
be restarted for the changes to take effect before running
`remotes::install_github("harphub/harpIO")`.

## Reading, transforming and writing forecast data

The main workhorse for {harpIO} is `read_forecast()`. This function can
read multiple files from multiple sources of both gridded and point
data. See the articles [Reading forcast model
data](articles/read_raw_forecast.html) and [Options for file
formats](articles/file_options.html) for more information.

For gridded data you have the option to transform the data to point
data, another grid, or a vertical cross section. This is done with the
`transformation` and `transformation_opts` arguments. Interpolation to
points is done by providing a data frame of latitiude and longitude
locations, regridding is done by providing a definition of the grid to
which to regrid (and if necessary reproject) the data, and a cross
section is obtained by providing the latitude and longitude of the end
points and reading 3 dimensional data. See the article [Transforming
forecast model data](articles/transformations.html) for more
information.

When data are interpolated to points it is possible to save the result
to [SQLite](https://www.sqlite.org/) files. These are portable database
files that allow fast access to the data with the ability to select and
filter what you wish to read. You can tell `read_forecast()` to output
point data to SQLite files by setting the argument
`output_file_opts = sqlite_opts(path = "/path/to/output")`. These data
can then be read with `read_point_forecast()`. For gridded data there is
generally no advantage to outputting to another format, so this option
is only available if data are either point data to begin with, or
`transformation = "interpolate"`.

## Reading observation data

For point observations, {harpIO} can currently only read from *vobs*
files, as produced by the
[HIRLAM](http://hirlam.org/index.php/hirlam-programme-53) consortium,
and write them out to SQLite files using `read_obs()` for faster access.
The SQLite files can then be read with `read_point_obs()`. For gridded
observations, data can be read from *NetCDF*, *grib* and some *HDF5*
files using `read_grid()` (note that you need to install the “ncdf4”
package from CRAN to read NetCDF files, you need to install the
[“Rgrib2”](https://harphub.github.io/Rgrib2) package with
`remotes::install_github("harphub/Rgrib2")` to read grib files and you
need to install the “hdf5r” package from CRAN to read HDF5 files).

## A note about HDF5 files

The [hdf5r](https://cran.r-project.org/web/packages/hdf5r/index.html)
package is needed to read HDF5 files. Installation of the package
requires the HDF5 system library. If this library not available in
e.g. /usr/local/lib or /usr/lib or similar then the location of the HDF5
library needs to be specified. Typically this can be done with

``` r
install.packages(
  "hdf5r",
  configure.args = "--with-hdf5=/path/to/hdf5"
)
```

However, in some cases the appropriate link flags might no be set
properly. In this case you should temporarily create a `~/.R/Makevars`
file with the following content:

``` bash
PKG_LIBS = -L/path/to/hdf5/lib -Wl,-rpath,/path/to/hdf5/lib -lhdf5hl_fortran -lhdf5_hl_cpp -lhdf5_hl -lhdf5_fortran -lhdf5_cpp -lhdf5 -lm
```

then install hdf5r and remove the `~/.R/Makevars` file.

On the **ECMWF Atos** platform you can put the following in
`~/.R/Makevars`

``` bash
PKG_LIBS = $(HDF5_LIB)
```

And then

``` bash
module load hdf5
module load R
R
> install.packages("hdf5r")
> file.remove("~/.R/Makevars")
```
