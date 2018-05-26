# ProlifAnalysis
Development version of the R package ProlifAnalysis for analysis of stem cell proliferation

To install the development version of TruncComp run the following commands from within R

```{r}
library(devtools)
install_github('aejensen/ProlifAnalysis')
```

# Example
```{r}
library(ProlifAnalysis)
data(confluencyExample)

out <- estimateL5(confluencyExample$x, confluencyExample$y)
summary(out)
plot(out)
plot(out, type="velocity")
plot(out, type="acceleration")
```
