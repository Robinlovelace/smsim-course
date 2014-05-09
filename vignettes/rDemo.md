# Example session to demonstrate R's capabilities

R can behave just like any calculator - it understands numbers!


```r
1
```

```
## [1] 1
```

```r

1 + 1
```

```
## [1] 2
```

```r

(1 + 5)/3
```

```
## [1] 2
```

```r
# now try some of your own calculations
```


R also allows collections of numbers to be operated on at once.
Collections of text and numbers can be assigned names - these are *objects*.


```r
c(1, 2, 3)/2  # note that each number is divided by 2 - it knows what you mean
```

```
## [1] 0.5 1.0 1.5
```

```r

x <- c(1, 2, 3)  # assign the numbers to an object
x * 5
```

```
## [1]  5 10 15
```

```r
x^3
```

```
## [1]  1  8 27
```

```r
x * x
```

```
## [1] 1 4 9
```

```r

y <- c(4, 2, 1)
plot(x, y)  # very basic plotting
lines(x, y)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 


R is very versatile. It can do almost *anything* involving numbers and
text. However, it was designed to work with fairly large datasets. Let's load one:


```r
# navigate to data - modify this depending on your dataset:
df <- read.csv("~/repos/smsim-course/data/cakeMap/ind.csv")
head(df)  # this asks R to print the top rows of the file
```

```
##   NCakes Car Sex NSSEC8 ageband4
## 1    3-5   2   1      7    25-34
## 2    1-2   2   2      2    55-64
## 3    1-2   1   2      2    45-54
## 4     6+   1   1      5    45-54
## 5    1-2   2   1      2    45-54
## 6    1-2   1   2      2    45-54
```

```r
summary(df$Car)  # note the use of the $ symbol to specify columns
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    1.00    1.00    1.00    1.19    1.00    2.00
```

```r
summary(df$ageband4)  # note the output is different - this is because of the type of data:
```

```
## 16-24 25-34 35-44 45-54 55-64 65-74 
##   101   166   171   198   159   121
```

```r
class(df$Car)  # this was recognised by R as numeric integer data
```

```
## [1] "integer"
```

```r
class(df$ageband4)  # this is factor data
```

```
## [1] "factor"
```


Let's explore the data a little - can we identify a link between cake consumption and class?


```r
table(df$NCakes, df$NSSEC8)
```

```
##         
##          1.1 1.2  2  3  4  5  6  7  8 97
##   <1       5   2 23 11  4  8 26  8  7  1
##   1-2      6  15 55 27 17 14 45 37 16  5
##   3-5     11  18 54 31 19 32 48 30 14  3
##   6+       9  14 55 22 23 32 46 29 25  0
##   rarely   2   3 10  5  4  8 12 16  8  1
```


Perhaps, but let's recategorise the cake data so it's numeric:


```r
library(car)  # load a new package - try install.packages('car') if this fails
levels(df$NCakes) <- c(0.5, 1.5, 4, 10, 0.1)
df$NCakes <- as.numeric(as.character(df$NCakes))
class(df$NCakes)
```

```
## [1] "numeric"
```

```r
class(df$NSSEC8)
```

```
## [1] "numeric"
```

```r
df$NSSEC8 <- paste0("ns", df$NSSEC8)
aggregate(df$NCakes ~ df$NSSEC8, mean, data = df)
```

```
##    df$NSSEC8 df$NCakes
## 1      ns1.1     4.415
## 2      ns1.2     4.535
## 3        ns2     4.371
## 4        ns3     4.068
## 5        ns4     4.984
## 6        ns5     5.040
## 7        ns6     4.145
## 8        ns7     3.926
## 9        ns8     4.776
## 10      ns97     2.010
```


How does cake consumption vary by age? Again, R can tell us:


```r
aggregate(df$NCakes ~ df$ageband4, mean, data = df)
```

```
##   df$ageband4 df$NCakes
## 1       16-24     4.411
## 2       25-34     3.979
## 3       35-44     4.285
## 4       45-54     4.007
## 5       55-64     4.399
## 6       65-74     5.525
```



This ability to use numbers in a general purpose way and
handle large datasets makes R extremely flexible.
The flexibility also means R is hard - this is why we need tutorials
like this one to see how it works.

