# flowsr
This repo generates the files needed to prepare the data for circular plots of flight flows
as described in the [circflows repo](https://github.com/espinielli/circflows).

A description of what is performed is available in [ifr_flows.pdf](ifr_flows.pdf).

For an example of the plots, please visit:

* [flows of flights in/out of Europe](http://ansperformance.eu/studies/flows-extra/)
* [flows of flights within of Europe](http://ansperformance.eu/studies/flows-intra/)

Once a new city-pair flows file, `data/raw/ifr_flows_2001-2015.csv`, is made available
it is enough to knitr `ifr_flows.Rmd` to generate a new `ifr_flows.pdf` and all the files
`data/*-viz.csv`.
