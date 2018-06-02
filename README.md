# ProlifAnalysis
Development version of the R package ProlifAnalysis for non-linear analysis of stem cell proliferation curves.

To install the development version of ProlifAnalysis run the following commands from within R:

```{r}
library(devtools)
install_github('aejensen/ProlifAnalysis')
```

# Example
```{r}
library(ProlifAnalysis)
data(confluencyExample)

mL5 <- estimateL5(confluencyExample$x, confluencyExample$y)
summary(mL5)

plot(mL5)
plot(mL5, type="velocity")
plot(mL5, type="acceleration")
```

# Running the shiny APP!
```{r}
library(ProlifAnalysis)

runShinyProliferation()
```
