[![License: AGPL v3](https://img.shields.io/badge/License-AGPL%20v3-blue.svg)](https://www.gnu.org/licenses/agpl-3.0)

# Kolam Package
Suite of Tools for Creating, Analyzing and Visualizing Kolams

A set of tools for processing and studying kolams. This kolam package should 	enable a way to create, draw and analyze kolam drawings. The functionality includes possibilities to extract information on kolam from a transcribed kolam in form of a .yaml file, the ability to plot kolams given a sequence of gestures or 	to extract information from a kolam object and the ability to compare kolam drawings. See the Github page for more information, documentation and examples.

# Install
In order to be able to run the code, you will need to have an up-to-date version of [R](https://www.r-project.org/) installed on your computer.
```
# Install devtools package if necessary
if(!"devtools" %in% rownames(installed.packages())) install.packages("devtools")

library(devtools)
devtools::install_github("nhtran93/kolam")
```

## Authors

* **[N.-Han Tran](https://www.eva.mpg.de/ecology/staff/han-tran/index.html)**
* [Bret A. Beheim](https://www.babeheim.com/)

## License

This project is licensed under the GNU AGPLv3 License - see the [LICENSE.md](LICENSE.md) file for details.
