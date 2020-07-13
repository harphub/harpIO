# harpIO 0.0.0.9160

## Possible breaking changes

* Default file name template changed for from paste0("fctable_", fcst_type) to "fctable" in `read_point_forecast()`
  * For fcst_type = "eps" the default template was "fctable_eps". This separated each lead time out into a separate file and was consistent with files output from `read_eps_interpolate()` and consisent with HARPv2. `read_eps_interpolate()` will soon be deprecated and we recommend using `read_forecast()` instead with `output_file_opts = sqlite_opts(...)` where the default template is "fctable", which combines all lead times into a single file. `read_point_forecast()` is now consistent with `read_forecast()` in using "fctable" as the default template. To get the previous functionality you need to set `file_template = "fctable_eps"` in the call to `read_point_forecast()`. For deterministic data this change has no impact.  
