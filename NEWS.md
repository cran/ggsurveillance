# ggsurveillance 0.5.1

-   Bug fix: Fix corner cases for the `fill_gaps` option in `bin_by_date()`

# ggsurveillance 0.5.0

## New Features

-   `bin_by_date()`: New `tidyverse`-compatible function for flexible date-based aggregation (binning). This function was previously internal to `geom_epicurve()` and `stat_bin_date()`.
    -   It includes a simpler and faster `fill_gaps` argument to automatically fill gaps in a time series with 0s. 
-   `guide_axis_nested_date()`: New axis guide for creating nested date labels for hierarchical time periods (e.g., year > month > day). This feature is powered by the `{legendry}` package.
-   `label_power10()`: New `ggplot2`-compatible labeling function to format numbers in scientific notation with powers of 10 (e.g., $2 \times 10^5$).
-   `geom_epicurve_text()` and `geom_epicurve_point()`: New geoms to easily add text annotations or points to cases in epidemic curves created with `geom_epicurve()`.

## Breaking Changes

-   `scale_y_cases_5er()` now defaults to starting at 0, providing more intuitive and accurate case count visualizations. The previous behaviour can be restored by setting `scale_y_cases_5er(limits = NULL)`.

# ggsurveillance 0.4.0

-   `geom_bar_diverging()` for diverging bar charts, including:
    -   `stat_diverging()` for easy labeling of these charts
    -   `scale_x_continuous_diverging()`: Creates symmetric diverging scales
    -   `geom_area_diverging()` for continuous variables.
    -   `geom_col_range()`: The underlying geom which creates bars from `x`, `ymin` and `ymax` (or flipped).
-   `geom_label_last_value()` for labeling of the last value of a time series (like `geom_line()`)
    -   `stat_last_value()` pulls the coordinates of the last value. E.g. can be used to add a point to the end of the line.
    -   `geom_label_last_value_repel()`: `ggrepel` versions for crowded plots with multiple lines
-   New dataset: `population_german_states`
-   `label_skip()` for skipping axis labels, e.g. only label every second tick
-   re-export `label_date()` and `label_date_short()` from scales for date labels with a custom locale.
-   Improvements and bug fixes

# ggsurveillance 0.3.0

-   new `ggplot2` theme modification helpers:
    -   `theme_mod_rotate_x_axis_labels()`: rotate axis labels
    -   `theme_mod_legend_position()`: legend positioning
    -   `theme_mod_remove_minor_grid()` : remove minor panel grid lines (x, y or both) or all grind lines
-   `geom_vline_year()` now also supports year_breaks based on weeks. Since weeks don't fall on the same date every year.
-   `tsibble` now optional dependency to improve speed of first install
-   add `plotly` compatibility for `geom_epicurve()` and `geom_epigantt()`
-   `scale_y_cases_5er()`: rename to `min.n` (from `n.min`) for correct `base::pretty()` compatibility
-   Bug fixes and minor improvements

# ggsurveillance 0.2.0

-   Update `geom_epigantt()`: Add auto-scaling for linewidth and update documentation
-   Add `scale_y_discrete_reverse()`
-   New dataset of a fictional hospital outbreak
-   `geometric_mean()`: Add an option to disable warnings
-   Minor fixes

# ggsurveillance 0.1.2

-   Documentation improvements and bug fixes for `geom_epicurve()`

# ggsurveillance 0.1.1

-   First release
