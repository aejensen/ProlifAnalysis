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

mL5 <- estimateL5(confluencyExample$x, confluencyExample$y)
summary(mL5)

mSpline <- estimateSpline(confluencyExample$x, confluencyExample$y)

par(mfrow=c(2,3))
plot(mL5)
plot(mL5, type="velocity")
plot(mL5, type="acceleration")
plot(mSpline)
plot(mSpline, type="velocity")
plot(mSpline, type="acceleration")
```
