# harpIO 0.2.2

* HOTFIX Released 2nd February 2024

* New parameters added to OBSOUL reading.

* Partial fix of bug that resulted in errors when extracting domain information
from WRF output files - care should be taken when reading data from WRF files 
as this is not fully tested.

# harpIO 0.2.1

* HOTFIX Released 5th December 2023

* This version includes some improvements for reading NetCDF files and bug fixes

### Bug fixes

* Fix a bug whereby `read_point_obs()` failed when not all accumulation periods
are available in the SQLite file

* Add "ws" to list of known parameters in `parse_harp_parameter()`

### NetCDF improvements

* When no projection string is available, get the projection information from
CF compliant attributes

* Get dimension names from variables. In general this means that dimension names
no longer need to be set via `netcdf_opts()`

### Documentation improvements

* Vignette added to describing how to read point observations from .csv files

* Extra note added to help users install the `hsf5r` package


# harpIO 0.2.0

* This version was released in November 2023

* It includes many internal changes to make your experience smoother and 
includes more informative error messages. 

### Breaking changes 

* Arguments `start_date`, `end_date`, and `by` to read functions are deprecated 
and replaced by `dttm`, to be used together with `seq_dttm()` to generate a 
sequence of date-time strings. Note that you can still use the old arguments, 
but will be periodically warned to change.

* New column names in outputs to read functions

|Old name|New name|
|:-------|:-------|
|fcdate|fcst_dttm|
|validdate|valid_dttm|
|leadtime|lead_time|

* `bind_fcst()` is deprecated. `bind()` should be used instead. 

* `get_filenames()` is deprecated. `generate_filenames()` should be used instead.

* `msub()` is deprecated. `psub()` should be used instead.

* `read_det_interpolate()` and `read_eps_interpolate()` are defunct. 
`read_forecast(..., transformation = "interpolate")` should be used instead. 

* `read_obs_convert()` is defunct. `read_obs(..., transformation = "...")` 
should be used instead.

### Selected new features

* Parameters are defined via the internal data list `harp_params`. This list 
includes parameter name substitutions for different file formats and the 
possibility to apply a function to specific parameters at read time. 

* The parameter list can be added to or modified with `add_param_def()` and 
`modify_param_def()` respectively. 

* All data read in by harp read functions attain a class - the print method 
for each class tells you what type of dataset it is. One exception is for 
`read_obs()` and `read_point_obs()`, which both return basic tibbles (data 
frames).

* `use_grib_stepRange()` is added as a helper to select grib messages via
`grib_opts(param_find = list(param = use_grib_stepRange(...)))`. This is useful 
for determining between accumulated and instantaneous variables with the same
grib shortName. `{lead_time}` can be used to take the current lead time.  


# harpIO 0.0.9

* This is the version that is basically unchanged since late 2021 / early 2022.

* It was officially tagged v0.0.9 in November 2023

# harpIO 0.0.0.9168

### Update of grib and FA handling
This version of harp requires the newest version of meteogrid >= 3.8.5 to work. For reading grib files Rgrib2 >= 
1.4.0 to work. It should be noted that Rgrib2 1.4.0 is not backwards compatible with older versions of harpIO.


# harpIO 0.0.0.9160

### Possible breaking changes

* Default file name template changed for from paste0("fctable_", fcst_type) to "fctable" in `read_point_forecast()`
  * For fcst_type = "eps" the default template was "fctable_eps". This separated each lead time out into a separate file and was consistent with files output from `read_eps_interpolate()` and consisent with HARPv2. `read_eps_interpolate()` will soon be deprecated and we recommend using `read_forecast()` instead with `output_file_opts = sqlite_opts(...)` where the default template is "fctable", which combines all lead times into a single file. `read_point_forecast()` is now consistent with `read_forecast()` in using "fctable" as the default template. To get the previous functionality you need to set `file_template = "fctable_eps"` in the call to `read_point_forecast()`. For deterministic data this change has no impact.  
