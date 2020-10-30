# Histogram operator

##### Description

The `histogram_operator` allows one to create 1D or 2D histograms.

##### Usage

Input projection|.
---|---
`y-axis`        | numeric, measurement
`x-axis`        | numeric, second measurement for 2D histogram, optional

Input parameters|.
---|---
`n_bins`        | number of bins

Output relations|.
---|---
`x_bin`        | x bin
`y_bin`        | y bin
`count`        | count of values in the bin
`density`        | proportion of values in the bin

##### Details

The operator bins the values over the x and y axes and computes the frequency of values in each bin. The computation is done per cell.

##### See Also

[density_operator](https://github.com/tercen/density_operator)

