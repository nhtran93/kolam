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

# Examples
```
# Plot a kolam with one loop
plotLoop(c("o4", "o2", "o1", "o4", "o3", "o1", "o1", "o1",
           "o4", "o3", "o1", "o1", "o4", "o3", "o2", "o1"),
           delay = 0.0001, headingStart = 135)

# Plot a kolam with multiple loops
plotLoop(c("o1", "o3", "o4", "o1", "o1","o3", "o4", "o1", "o1", "o2",
           "o1", "o3", "o4", "o1", "o1","o3", "o4", "o1", "o1", "o2",
           "o1", "o3", "o4", "o1", "o1","o3", "o4", "o1", "o1", "o2",
           "o1", "o3", "o4", "o1", "o1","o3", "o4", "o1", "o1", "o2"),
           xStart = 0, yStart = 0.5, headingStart = 45)
plotLoop(c("c3"), -1, 0, 45, add = TRUE)

# Plot kolam with different colour for each loop
plotLoop(c("o1", "o3", "o4", "o1", "o1","o3", "o4", "o1", "o1", "o2",
           "o1", "o3", "o4", "o1", "o1","o3", "o4", "o1", "o1", "o2",
           "o1", "o3", "o4", "o1", "o1","o3", "o4", "o1", "o1", "o2",
           "o1", "o3", "o4", "o1", "o1","o3", "o4", "o1", "o1", "o2"),
           xStart = 0, yStart = 0.5, headingStart = 45)
plotLoop(c("c1"), xStart = -1, yStart = -0.5,
         headingStart = 45, add = TRUE, col = "blue")
         
# Turn off the arrow that indicates the starting position
plotLoop(c("o1", "o3", "o4", "o1", "o1","o3", "o4", "o1", "o1", "o2",
           "o1", "o3", "o4", "o1", "o1","o3", "o4", "o1", "o1", "o2",
           "o1", "o3", "o4", "o1", "o1","o3", "o4", "o1", "o1", "o2",
           "o1", "o3", "o4", "o1", "o1","o3", "o4", "o1", "o1", "o2"),
         xStart = 0, yStart = 0.5, headingStart = 45, 
         arrow = FALSE)
plotLoop(c("c1"), xStart = -1, yStart = -0.5,
         headingStart = 45, add = TRUE, col = "blue", 
         arrow = FALSE)

# Plot kolam drawings using the provided dataframe
plotKolam(kolamObject[[1]])

# Make the 'pulli' (dots) smaller and turn off the arrows
plotKolam(kolamObject[[2]], arrow = FALSE, cex = 0.4)

# Turn off all the loop colouring
plotKolam(kolamObject[[4]], arrow = FALSE, cex = 0.4, loop_col = FALSE)

```

## Authors

* **[N.-Han Tran](https://www.eva.mpg.de/ecology/staff/han-tran/index.html)**
* [Bret A. Beheim](https://www.babeheim.com/)
* [Timothy Waring](https://timwaring.info/)

## License

This project is licensed under the GNU AGPLv3 License - see the [LICENSE.md](LICENSE.md) file for details.
