# gentools

![gentools](gentools.png)

The goal of gentools is to make it easier to work with genomics lab data. The package provides
functions to create a table, plot and a report from imported genomics lab data.

## Installation

You can install this package with the following code:

``` r
if(!require(devtools)){
  install.packages("devtools")
}
devtools::install_github("acolorado1/gentools")
```

## Example

Here are some basic examples demonstrating some functionalities of gentools package.

``` r
library(gentools)

# Import genomics dataset
import_data("filepath")

# Give summary statistics for the dataset gendata
genomic_summary(data = gendata, log2 = "log2.fold_change.", sig = "significant")

# Create a volcano plot from given dataset
genomic_plot(plot.type = "volcano", data = gendata, gene.n = "gene",
             log2 = "log2.fold_change.", pval = "p_value")

```

