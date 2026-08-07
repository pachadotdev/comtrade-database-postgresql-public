#!/bin/bash

export COMTRADE_HS_DIGITS=6
export COMTRADE_YEARS="1986:2023"

# Compute weights
Rscript 03-01-compute-lt-weights.r h0 h1
Rscript 03-01-compute-lt-weights.r h1 h2
Rscript 03-01-compute-lt-weights.r h2 h3
Rscript 03-01-compute-lt-weights.r h3 h4
Rscript 03-01-compute-lt-weights.r h4 h5
Rscript 03-01-compute-lt-weights.r h5 h6
Rscript 03-01-compute-lt-weights.r s1 s2
Rscript 03-01-compute-lt-weights.r s2 s3
Rscript 03-01-compute-lt-weights.r s3 s4
Rscript 03-01-compute-lt-weights.r s3 h0

# Or just run all weights in one go
# Rscript 03-01-compute-lt-weights.r

# Then convert all years to h3 (HS 2007)
Rscript 03-02-convert-flows.r h3
