'Spatial microsimulation' in R: allocating individuals to zones
========================================================

This reproducible code shows how to use IPF to generate lists of individuals for individual zones using IPF.
The input datasets are 3 constraint variables and an individual level dataset.
We will work through the entire process to show how spatial microsimulation can be done in R.

## Loading the input data
The first stage is to load the input data, from an Excel spreadsheet in this case.


```r
# install.packages('gdata') # install gdata package if not already installed
library(gdata)  # load package for reading data
```

```
## gdata: read.xls support for 'XLS' (Excel 97-2004) files ENABLED.
## 
## gdata: read.xls support for 'XLSX' (Excel 2007+) files ENABLED.
## 
## Attaching package: 'gdata'
## 
## The following object is masked from 'package:utils':
## 
##     object.size
## 
## The following object is masked from 'package:stats':
## 
##     nobs
```

```r
ind <- read.xls("msim.xlsx", sheet = "SAMPLING")
head(ind)
```

```
##   ID GENDER        AGE ETHNICITY
## 1  1   MALE  AGED0TO16     WHITE
## 2  2   MALE  AGED0TO16     MIXED
## 3  3   MALE  AGED0TO16     ASIAN
## 4  4   MALE  AGED0TO16     BLACK
## 5  5   MALE  AGED0TO16     OTHER
## 6  6   MALE AGED17TO45     WHITE
```

```r

# load the constraints and check the totals add up
con1 <- read.xls("msim.xlsx", "GENDER")[-1]
sum(con1)
```

```
## [1] 840
```

```r
con2 <- read.xls("msim.xlsx", "AGE")[-1]
sum(con2)
```

```
## [1] 840
```

```r
con3 <- read.xls("msim.xlsx", "ETHNICITY")[-1]
sum(con3)
```

```
## [1] 840
```

```r
num.cons <- 3  # n. constraints - can set automatically: length(which(grepl('con[1-9]',ls()))))
cons <- cbind(con1, con2, con3)
cat.names <- names(cons)
```


## Converting the individual level data into a 0/1 "model matrix" for aggregation

In order to compare individual level data with aggregate constraints,
the aggregate data must be converted into a flat form, with counts
for each of the categorise in the dataset:


```r
# creating 0/1 matrix representation of ind. data
gender.cat <- model.matrix(~ind$GENDER - 1)
age.cat <- model.matrix(~ind$AGE - 1)
eth.cat <- model.matrix(~ind$ETHNICITY - 1)
ind.cat <- cbind(gender.cat, age.cat, eth.cat)
names(ind.cat) <- cat.names
```



## Create the weight matrix and initialise
IPF *reweights* individuals for each zone (A to C in this case).
For this we must first create a weight matrix.
In fact, this will be a *weight array*, with a 2D matrix for each
constraint, allowing for easy iteration through the constraints.


```r
weights <- array(dim = c(nrow(ind), nrow(cons), num.cons + 1))
weights[, , 1] <- 1  # sets initial weights to 1
```


## Create aggregated output matrix and add values from individual inputs
This stage creates an array for the aggregated outputs.
Notice that `ind.agg` has the same dimension as `cons`,
allowing for direct comparison between the two.
This is the key to IPF!


```r
ind.agg <- array(dim = c(nrow(cons), ncol(cons), num.cons + 1))
for (i in 1:nrow(cons)) {
    ind.agg[i, , 1] <- colSums(ind.cat) * weights[1, i, 1]
}
ind.agg[, , 1]  # look at what we've created - individual level data comparable w. cons
```

```
##      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11]
## [1,]   20   20   10   10   10   10    8    8    8     8     8
## [2,]   20   20   10   10   10   10    8    8    8     8     8
## [3,]   20   20   10   10   10   10    8    8    8     8     8
```





# The IPF part #############

Now that all the data and objects used for the model have been set-up,
we are ready to run the model. We constrain by one constraint at a time.


```r
# Re-weighting for constraint 1 via IPF
for (j in 1:nrow(cons)) {
    for (i in 1:ncol(con1)) {
        weights[which(ind.cat[, i] == 1), j, 2] <- con1[j, i]/ind.agg[j, i, 
            1]
    }
}
for (i in 1:nrow(cons)) {
    # convert con1 weights back into aggregates
    ind.agg[i, , 2] <- colSums(ind.cat * weights[, i, 1] * weights[, i, 2])
}
# test results for first row (not necessary for model)
ind.agg[1, , 2] - cons[1, ]  # should be zero for age/sex
```

```
##   MALE FEMALE AGED0TO16 AGED17TO45 AGED46TO64 AGED65OLDER WHITE MIXED
## 1    0      0      12.5      -37.5      -17.5        42.5  -150    40
##   ASIAN BLACK OTHER
## 1    40    30    40
```

```r
cor(as.numeric(as.vector(ind.agg[, , 2])), as.numeric(as.matrix(cons)))  # how good is the correlation (fit)
```

```
## [1] 0.5527
```


## Second constraint

```r
for (j in 1:nrow(cons)) {
    for (i in 1:ncol(con2) + ncol(con1)) {
        weights[which(ind.cat[, i] == 1), j, 3] <- cons[j, i]/ind.agg[j, i, 
            2]
    }
}
for (i in 1:nrow(cons)) {
    # convert con2 back into aggregate
    ind.agg[i, , 3] <- colSums(ind.cat * weights[, i, 1] * weights[, i, 2] * 
        weights[, i, 3])
}
ind.agg[1, , 3] - cons[1, ]  # should be close to zero for new constraint
```

```
##   MALE FEMALE AGED0TO16 AGED17TO45 AGED46TO64 AGED65OLDER WHITE MIXED
## 1    0      0         0          0          0           0  -150    40
##   ASIAN BLACK OTHER
## 1    40    30    40
```

```r
cor(as.numeric(as.vector(ind.agg[, , 3])), as.numeric(as.matrix(cons)))  # how good is the correlation (fit)
```

```
## [1] 0.6944
```


## Third constraint

```r
for (j in 1:nrow(cons)) {
    for (i in 1:ncol(con3) + ncol(con1) + ncol(con2)) {
        weights[which(ind.cat[, i] == 1), j, 4] <- cons[j, i]/ind.agg[j, i, 
            3]
    }
}
for (i in 1:nrow(cons)) {
    # convert con3 back into aggregate
    ind.agg[i, , 4] <- colSums(ind.cat * weights[, i, 1] * weights[, i, 2] * 
        weights[, i, 3] * weights[, i, 4])
}
ind.agg[1:3, , 4] - cons[1:3, ]  # test the result
```

```
##   MALE FEMALE AGED0TO16 AGED17TO45 AGED46TO64 AGED65OLDER WHITE MIXED
## 1    0      0         0          0          0           0     0     0
## 2    0      0         0          0          0           0     0     0
## 3    0      0         0          0          0           0     0     0
##   ASIAN BLACK OTHER
## 1     0     0     0
## 2     0     0     0
## 3     0     0     0
```


## Improvements in model fit

Notice that the fit of the model improves from one constraint to the next.
What is the final model fit?


```r
cor(as.numeric(as.vector(ind.agg[, , 4])), as.numeric(as.matrix(cons)))  # how good is the correlation (fit)
```

```
## [1] 1
```

```r
# you get a perfect fit between constraint data and results of model why?
# because of the final weights:
fw <- weights[, , 1] * weights[, , 2] * weights[, , 3] * weights[, , 4]
head(fw)  # cols are zones, rows are individuals
```

```
##      [,1]   [,2]   [,3]
## [1,]  1.2 0.3516 0.9877
## [2,]  1.2 1.7578 1.3169
## [3,] 24.0 7.0312 4.9383
## [4,]  1.2 1.7578 1.3169
## [5,]  2.4 0.3516 0.3292
## [6,]  2.4 1.7578 7.4074
```


# Integerisation phase ###################

We have allocated weights to the individuals for a good (perfect)
model fit. These are the *maximum likelihood* or *maximum entropy* values
to match the individuals with the zones.
The final stage is to convert this *weight matrix* into a list of individuals for each zone.

## Setting up objects for the integerisation phase


```r
intall <- ints <- as.list(1:nrow(cons))  # Names of integer indices (ints), and integer populations (intall) in ordered list
intagg <- cons * 0  # Aggregate area stats - set to 0 to avoid confusion
f <- floor(fw)  # truncated weights
d <- fw - f

set.seed(0)  # Include this line to ensure repeatable results
```


## Integerisation loop
Here we sample individuals based on their weights.
This is the Truncate Replicate Sample (TRS) method described by
Lovelace and Ballas (2011).


```r
for (i in 1:nrow(cons)){
  if(max(f[,i]) == 0) f[which.max(fw[,i]),i] <- 1 # ensures model will run in case max(i5.w5 < 1) thanks to Eveline van Leeuwen
  ints[[i]] <- rep(which(fw[,i] > 0), f[,i])
  s <- sample(which(fw[,i] > 0), size = sum(con1[i,]) - sum(f[,i]) , # sample using decimal weights to 'top up' selection
              prob=d[,i], replace = F) 
  ints[[i]] <- c(ints[[i]], s) # add the sampled population to the one selected from truncated weights
  intall[[i]] <- ind[ints[[i]],] # Pulls all other data from index
  source("areaCat.R") # save the aggregate data
  intagg[i,] <- colSums(area.cat) 
}
```


## The results

What is the result of this?
A list of individuals for each zone. Let's take a look at all of the model output.


```r
intall
```

```
## [[1]]
##       ID GENDER         AGE ETHNICITY
## 1      1   MALE   AGED0TO16     WHITE
## 2      2   MALE   AGED0TO16     MIXED
## 3      3   MALE   AGED0TO16     ASIAN
## 3.1    3   MALE   AGED0TO16     ASIAN
## 3.2    3   MALE   AGED0TO16     ASIAN
## 3.3    3   MALE   AGED0TO16     ASIAN
## 3.4    3   MALE   AGED0TO16     ASIAN
## 3.5    3   MALE   AGED0TO16     ASIAN
## 3.6    3   MALE   AGED0TO16     ASIAN
## 3.7    3   MALE   AGED0TO16     ASIAN
## 3.8    3   MALE   AGED0TO16     ASIAN
## 3.9    3   MALE   AGED0TO16     ASIAN
## 3.10   3   MALE   AGED0TO16     ASIAN
## 3.11   3   MALE   AGED0TO16     ASIAN
## 3.12   3   MALE   AGED0TO16     ASIAN
## 3.13   3   MALE   AGED0TO16     ASIAN
## 3.14   3   MALE   AGED0TO16     ASIAN
## 3.15   3   MALE   AGED0TO16     ASIAN
## 3.16   3   MALE   AGED0TO16     ASIAN
## 3.17   3   MALE   AGED0TO16     ASIAN
## 3.18   3   MALE   AGED0TO16     ASIAN
## 3.19   3   MALE   AGED0TO16     ASIAN
## 3.20   3   MALE   AGED0TO16     ASIAN
## 3.21   3   MALE   AGED0TO16     ASIAN
## 3.22   3   MALE   AGED0TO16     ASIAN
## 3.23   3   MALE   AGED0TO16     ASIAN
## 4      4   MALE   AGED0TO16     BLACK
## 5      5   MALE   AGED0TO16     OTHER
## 5.1    5   MALE   AGED0TO16     OTHER
## 6      6   MALE  AGED17TO45     WHITE
## 6.1    6   MALE  AGED17TO45     WHITE
## 7      7   MALE  AGED17TO45     MIXED
## 7.1    7   MALE  AGED17TO45     MIXED
## 8      8   MALE  AGED17TO45     ASIAN
## 8.1    8   MALE  AGED17TO45     ASIAN
## 8.2    8   MALE  AGED17TO45     ASIAN
## 8.3    8   MALE  AGED17TO45     ASIAN
## 8.4    8   MALE  AGED17TO45     ASIAN
## 8.5    8   MALE  AGED17TO45     ASIAN
## 8.6    8   MALE  AGED17TO45     ASIAN
## 8.7    8   MALE  AGED17TO45     ASIAN
## 8.8    8   MALE  AGED17TO45     ASIAN
## 8.9    8   MALE  AGED17TO45     ASIAN
## 8.10   8   MALE  AGED17TO45     ASIAN
## 8.11   8   MALE  AGED17TO45     ASIAN
## 8.12   8   MALE  AGED17TO45     ASIAN
## 8.13   8   MALE  AGED17TO45     ASIAN
## 8.14   8   MALE  AGED17TO45     ASIAN
## 8.15   8   MALE  AGED17TO45     ASIAN
## 8.16   8   MALE  AGED17TO45     ASIAN
## 8.17   8   MALE  AGED17TO45     ASIAN
## 8.18   8   MALE  AGED17TO45     ASIAN
## 8.19   8   MALE  AGED17TO45     ASIAN
## 8.20   8   MALE  AGED17TO45     ASIAN
## 8.21   8   MALE  AGED17TO45     ASIAN
## 8.22   8   MALE  AGED17TO45     ASIAN
## 8.23   8   MALE  AGED17TO45     ASIAN
## 8.24   8   MALE  AGED17TO45     ASIAN
## 8.25   8   MALE  AGED17TO45     ASIAN
## 8.26   8   MALE  AGED17TO45     ASIAN
## 8.27   8   MALE  AGED17TO45     ASIAN
## 8.28   8   MALE  AGED17TO45     ASIAN
## 8.29   8   MALE  AGED17TO45     ASIAN
## 8.30   8   MALE  AGED17TO45     ASIAN
## 8.31   8   MALE  AGED17TO45     ASIAN
## 8.32   8   MALE  AGED17TO45     ASIAN
## 8.33   8   MALE  AGED17TO45     ASIAN
## 8.34   8   MALE  AGED17TO45     ASIAN
## 8.35   8   MALE  AGED17TO45     ASIAN
## 8.36   8   MALE  AGED17TO45     ASIAN
## 8.37   8   MALE  AGED17TO45     ASIAN
## 8.38   8   MALE  AGED17TO45     ASIAN
## 8.39   8   MALE  AGED17TO45     ASIAN
## 8.40   8   MALE  AGED17TO45     ASIAN
## 8.41   8   MALE  AGED17TO45     ASIAN
## 8.42   8   MALE  AGED17TO45     ASIAN
## 8.43   8   MALE  AGED17TO45     ASIAN
## 8.44   8   MALE  AGED17TO45     ASIAN
## 8.45   8   MALE  AGED17TO45     ASIAN
## 8.46   8   MALE  AGED17TO45     ASIAN
## 8.47   8   MALE  AGED17TO45     ASIAN
## 9      9   MALE  AGED17TO45     BLACK
## 9.1    9   MALE  AGED17TO45     BLACK
## 10    10   MALE  AGED17TO45     OTHER
## 10.1  10   MALE  AGED17TO45     OTHER
## 10.2  10   MALE  AGED17TO45     OTHER
## 10.3  10   MALE  AGED17TO45     OTHER
## 11    11   MALE  AGED46TO64     WHITE
## 12    12   MALE  AGED46TO64     MIXED
## 13    13   MALE  AGED46TO64     ASIAN
## 13.1  13   MALE  AGED46TO64     ASIAN
## 13.2  13   MALE  AGED46TO64     ASIAN
## 13.3  13   MALE  AGED46TO64     ASIAN
## 13.4  13   MALE  AGED46TO64     ASIAN
## 13.5  13   MALE  AGED46TO64     ASIAN
## 13.6  13   MALE  AGED46TO64     ASIAN
## 13.7  13   MALE  AGED46TO64     ASIAN
## 13.8  13   MALE  AGED46TO64     ASIAN
## 13.9  13   MALE  AGED46TO64     ASIAN
## 13.10 13   MALE  AGED46TO64     ASIAN
## 13.11 13   MALE  AGED46TO64     ASIAN
## 13.12 13   MALE  AGED46TO64     ASIAN
## 13.13 13   MALE  AGED46TO64     ASIAN
## 13.14 13   MALE  AGED46TO64     ASIAN
## 13.15 13   MALE  AGED46TO64     ASIAN
## 13.16 13   MALE  AGED46TO64     ASIAN
## 13.17 13   MALE  AGED46TO64     ASIAN
## 13.18 13   MALE  AGED46TO64     ASIAN
## 13.19 13   MALE  AGED46TO64     ASIAN
## 13.20 13   MALE  AGED46TO64     ASIAN
## 13.21 13   MALE  AGED46TO64     ASIAN
## 13.22 13   MALE  AGED46TO64     ASIAN
## 13.23 13   MALE  AGED46TO64     ASIAN
## 13.24 13   MALE  AGED46TO64     ASIAN
## 13.25 13   MALE  AGED46TO64     ASIAN
## 13.26 13   MALE  AGED46TO64     ASIAN
## 13.27 13   MALE  AGED46TO64     ASIAN
## 13.28 13   MALE  AGED46TO64     ASIAN
## 13.29 13   MALE  AGED46TO64     ASIAN
## 13.30 13   MALE  AGED46TO64     ASIAN
## 13.31 13   MALE  AGED46TO64     ASIAN
## 13.32 13   MALE  AGED46TO64     ASIAN
## 13.33 13   MALE  AGED46TO64     ASIAN
## 13.34 13   MALE  AGED46TO64     ASIAN
## 13.35 13   MALE  AGED46TO64     ASIAN
## 13.36 13   MALE  AGED46TO64     ASIAN
## 13.37 13   MALE  AGED46TO64     ASIAN
## 14    14   MALE  AGED46TO64     BLACK
## 15    15   MALE  AGED46TO64     OTHER
## 15.1  15   MALE  AGED46TO64     OTHER
## 15.2  15   MALE  AGED46TO64     OTHER
## 18    18   MALE AGED65OLDER     ASIAN
## 18.1  18   MALE AGED65OLDER     ASIAN
## 18.2  18   MALE AGED65OLDER     ASIAN
## 18.3  18   MALE AGED65OLDER     ASIAN
## 18.4  18   MALE AGED65OLDER     ASIAN
## 18.5  18   MALE AGED65OLDER     ASIAN
## 18.6  18   MALE AGED65OLDER     ASIAN
## 18.7  18   MALE AGED65OLDER     ASIAN
## 18.8  18   MALE AGED65OLDER     ASIAN
## 23    23 FEMALE   AGED0TO16     ASIAN
## 23.1  23 FEMALE   AGED0TO16     ASIAN
## 23.2  23 FEMALE   AGED0TO16     ASIAN
## 23.3  23 FEMALE   AGED0TO16     ASIAN
## 23.4  23 FEMALE   AGED0TO16     ASIAN
## 23.5  23 FEMALE   AGED0TO16     ASIAN
## 23.6  23 FEMALE   AGED0TO16     ASIAN
## 23.7  23 FEMALE   AGED0TO16     ASIAN
## 23.8  23 FEMALE   AGED0TO16     ASIAN
## 23.9  23 FEMALE   AGED0TO16     ASIAN
## 23.10 23 FEMALE   AGED0TO16     ASIAN
## 23.11 23 FEMALE   AGED0TO16     ASIAN
## 23.12 23 FEMALE   AGED0TO16     ASIAN
## 23.13 23 FEMALE   AGED0TO16     ASIAN
## 23.14 23 FEMALE   AGED0TO16     ASIAN
## 23.15 23 FEMALE   AGED0TO16     ASIAN
## 25    25 FEMALE   AGED0TO16     OTHER
## 26    26 FEMALE  AGED17TO45     WHITE
## 27    27 FEMALE  AGED17TO45     MIXED
## 28    28 FEMALE  AGED17TO45     ASIAN
## 28.1  28 FEMALE  AGED17TO45     ASIAN
## 28.2  28 FEMALE  AGED17TO45     ASIAN
## 28.3  28 FEMALE  AGED17TO45     ASIAN
## 28.4  28 FEMALE  AGED17TO45     ASIAN
## 28.5  28 FEMALE  AGED17TO45     ASIAN
## 28.6  28 FEMALE  AGED17TO45     ASIAN
## 28.7  28 FEMALE  AGED17TO45     ASIAN
## 28.8  28 FEMALE  AGED17TO45     ASIAN
## 28.9  28 FEMALE  AGED17TO45     ASIAN
## 28.10 28 FEMALE  AGED17TO45     ASIAN
## 28.11 28 FEMALE  AGED17TO45     ASIAN
## 28.12 28 FEMALE  AGED17TO45     ASIAN
## 28.13 28 FEMALE  AGED17TO45     ASIAN
## 28.14 28 FEMALE  AGED17TO45     ASIAN
## 28.15 28 FEMALE  AGED17TO45     ASIAN
## 28.16 28 FEMALE  AGED17TO45     ASIAN
## 28.17 28 FEMALE  AGED17TO45     ASIAN
## 28.18 28 FEMALE  AGED17TO45     ASIAN
## 28.19 28 FEMALE  AGED17TO45     ASIAN
## 28.20 28 FEMALE  AGED17TO45     ASIAN
## 28.21 28 FEMALE  AGED17TO45     ASIAN
## 28.22 28 FEMALE  AGED17TO45     ASIAN
## 28.23 28 FEMALE  AGED17TO45     ASIAN
## 28.24 28 FEMALE  AGED17TO45     ASIAN
## 28.25 28 FEMALE  AGED17TO45     ASIAN
## 28.26 28 FEMALE  AGED17TO45     ASIAN
## 28.27 28 FEMALE  AGED17TO45     ASIAN
## 28.28 28 FEMALE  AGED17TO45     ASIAN
## 28.29 28 FEMALE  AGED17TO45     ASIAN
## 28.30 28 FEMALE  AGED17TO45     ASIAN
## 28.31 28 FEMALE  AGED17TO45     ASIAN
## 29    29 FEMALE  AGED17TO45     BLACK
## 30    30 FEMALE  AGED17TO45     OTHER
## 30.1  30 FEMALE  AGED17TO45     OTHER
## 30.2  30 FEMALE  AGED17TO45     OTHER
## 31    31 FEMALE  AGED46TO64     WHITE
## 32    32 FEMALE  AGED46TO64     MIXED
## 33    33 FEMALE  AGED46TO64     ASIAN
## 33.1  33 FEMALE  AGED46TO64     ASIAN
## 33.2  33 FEMALE  AGED46TO64     ASIAN
## 33.3  33 FEMALE  AGED46TO64     ASIAN
## 33.4  33 FEMALE  AGED46TO64     ASIAN
## 33.5  33 FEMALE  AGED46TO64     ASIAN
## 33.6  33 FEMALE  AGED46TO64     ASIAN
## 33.7  33 FEMALE  AGED46TO64     ASIAN
## 33.8  33 FEMALE  AGED46TO64     ASIAN
## 33.9  33 FEMALE  AGED46TO64     ASIAN
## 33.10 33 FEMALE  AGED46TO64     ASIAN
## 33.11 33 FEMALE  AGED46TO64     ASIAN
## 33.12 33 FEMALE  AGED46TO64     ASIAN
## 33.13 33 FEMALE  AGED46TO64     ASIAN
## 33.14 33 FEMALE  AGED46TO64     ASIAN
## 33.15 33 FEMALE  AGED46TO64     ASIAN
## 33.16 33 FEMALE  AGED46TO64     ASIAN
## 33.17 33 FEMALE  AGED46TO64     ASIAN
## 33.18 33 FEMALE  AGED46TO64     ASIAN
## 33.19 33 FEMALE  AGED46TO64     ASIAN
## 33.20 33 FEMALE  AGED46TO64     ASIAN
## 33.21 33 FEMALE  AGED46TO64     ASIAN
## 33.22 33 FEMALE  AGED46TO64     ASIAN
## 33.23 33 FEMALE  AGED46TO64     ASIAN
## 33.24 33 FEMALE  AGED46TO64     ASIAN
## 34    34 FEMALE  AGED46TO64     BLACK
## 35    35 FEMALE  AGED46TO64     OTHER
## 35.1  35 FEMALE  AGED46TO64     OTHER
## 38    38 FEMALE AGED65OLDER     ASIAN
## 38.1  38 FEMALE AGED65OLDER     ASIAN
## 38.2  38 FEMALE AGED65OLDER     ASIAN
## 38.3  38 FEMALE AGED65OLDER     ASIAN
## 38.4  38 FEMALE AGED65OLDER     ASIAN
## 38.5  38 FEMALE AGED65OLDER     ASIAN
## 37    37 FEMALE AGED65OLDER     MIXED
## 10.4  10   MALE  AGED17TO45     OTHER
## 21    21 FEMALE   AGED0TO16     WHITE
## 29.1  29 FEMALE  AGED17TO45     BLACK
## 31.1  31 FEMALE  AGED46TO64     WHITE
## 12.1  12   MALE  AGED46TO64     MIXED
## 36    36 FEMALE AGED65OLDER     WHITE
## 32.1  32 FEMALE  AGED46TO64     MIXED
## 19    19   MALE AGED65OLDER     BLACK
## 35.2  35 FEMALE  AGED46TO64     OTHER
## 20    20   MALE AGED65OLDER     OTHER
## 15.3  15   MALE  AGED46TO64     OTHER
## 24    24 FEMALE   AGED0TO16     BLACK
## 38.6  38 FEMALE AGED65OLDER     ASIAN
## 27.1  27 FEMALE  AGED17TO45     MIXED
## 5.2    5   MALE   AGED0TO16     OTHER
## 26.1  26 FEMALE  AGED17TO45     WHITE
## 6.2    6   MALE  AGED17TO45     WHITE
## 2.1    2   MALE   AGED0TO16     MIXED
## 
## [[2]]
##       ID GENDER         AGE ETHNICITY
## 2      2   MALE   AGED0TO16     MIXED
## 3      3   MALE   AGED0TO16     ASIAN
## 3.1    3   MALE   AGED0TO16     ASIAN
## 3.2    3   MALE   AGED0TO16     ASIAN
## 3.3    3   MALE   AGED0TO16     ASIAN
## 3.4    3   MALE   AGED0TO16     ASIAN
## 3.5    3   MALE   AGED0TO16     ASIAN
## 3.6    3   MALE   AGED0TO16     ASIAN
## 4      4   MALE   AGED0TO16     BLACK
## 6      6   MALE  AGED17TO45     WHITE
## 7      7   MALE  AGED17TO45     MIXED
## 7.1    7   MALE  AGED17TO45     MIXED
## 7.2    7   MALE  AGED17TO45     MIXED
## 7.3    7   MALE  AGED17TO45     MIXED
## 7.4    7   MALE  AGED17TO45     MIXED
## 7.5    7   MALE  AGED17TO45     MIXED
## 7.6    7   MALE  AGED17TO45     MIXED
## 7.7    7   MALE  AGED17TO45     MIXED
## 8      8   MALE  AGED17TO45     ASIAN
## 8.1    8   MALE  AGED17TO45     ASIAN
## 8.2    8   MALE  AGED17TO45     ASIAN
## 8.3    8   MALE  AGED17TO45     ASIAN
## 8.4    8   MALE  AGED17TO45     ASIAN
## 8.5    8   MALE  AGED17TO45     ASIAN
## 8.6    8   MALE  AGED17TO45     ASIAN
## 8.7    8   MALE  AGED17TO45     ASIAN
## 8.8    8   MALE  AGED17TO45     ASIAN
## 8.9    8   MALE  AGED17TO45     ASIAN
## 8.10   8   MALE  AGED17TO45     ASIAN
## 8.11   8   MALE  AGED17TO45     ASIAN
## 8.12   8   MALE  AGED17TO45     ASIAN
## 8.13   8   MALE  AGED17TO45     ASIAN
## 8.14   8   MALE  AGED17TO45     ASIAN
## 8.15   8   MALE  AGED17TO45     ASIAN
## 8.16   8   MALE  AGED17TO45     ASIAN
## 8.17   8   MALE  AGED17TO45     ASIAN
## 8.18   8   MALE  AGED17TO45     ASIAN
## 8.19   8   MALE  AGED17TO45     ASIAN
## 8.20   8   MALE  AGED17TO45     ASIAN
## 8.21   8   MALE  AGED17TO45     ASIAN
## 8.22   8   MALE  AGED17TO45     ASIAN
## 8.23   8   MALE  AGED17TO45     ASIAN
## 8.24   8   MALE  AGED17TO45     ASIAN
## 8.25   8   MALE  AGED17TO45     ASIAN
## 8.26   8   MALE  AGED17TO45     ASIAN
## 8.27   8   MALE  AGED17TO45     ASIAN
## 8.28   8   MALE  AGED17TO45     ASIAN
## 8.29   8   MALE  AGED17TO45     ASIAN
## 8.30   8   MALE  AGED17TO45     ASIAN
## 8.31   8   MALE  AGED17TO45     ASIAN
## 8.32   8   MALE  AGED17TO45     ASIAN
## 8.33   8   MALE  AGED17TO45     ASIAN
## 8.34   8   MALE  AGED17TO45     ASIAN
## 9      9   MALE  AGED17TO45     BLACK
## 9.1    9   MALE  AGED17TO45     BLACK
## 9.2    9   MALE  AGED17TO45     BLACK
## 9.3    9   MALE  AGED17TO45     BLACK
## 9.4    9   MALE  AGED17TO45     BLACK
## 9.5    9   MALE  AGED17TO45     BLACK
## 9.6    9   MALE  AGED17TO45     BLACK
## 9.7    9   MALE  AGED17TO45     BLACK
## 10    10   MALE  AGED17TO45     OTHER
## 11    11   MALE  AGED46TO64     WHITE
## 12    12   MALE  AGED46TO64     MIXED
## 12.1  12   MALE  AGED46TO64     MIXED
## 12.2  12   MALE  AGED46TO64     MIXED
## 12.3  12   MALE  AGED46TO64     MIXED
## 12.4  12   MALE  AGED46TO64     MIXED
## 13    13   MALE  AGED46TO64     ASIAN
## 13.1  13   MALE  AGED46TO64     ASIAN
## 13.2  13   MALE  AGED46TO64     ASIAN
## 13.3  13   MALE  AGED46TO64     ASIAN
## 13.4  13   MALE  AGED46TO64     ASIAN
## 13.5  13   MALE  AGED46TO64     ASIAN
## 13.6  13   MALE  AGED46TO64     ASIAN
## 13.7  13   MALE  AGED46TO64     ASIAN
## 13.8  13   MALE  AGED46TO64     ASIAN
## 13.9  13   MALE  AGED46TO64     ASIAN
## 13.10 13   MALE  AGED46TO64     ASIAN
## 13.11 13   MALE  AGED46TO64     ASIAN
## 13.12 13   MALE  AGED46TO64     ASIAN
## 13.13 13   MALE  AGED46TO64     ASIAN
## 13.14 13   MALE  AGED46TO64     ASIAN
## 13.15 13   MALE  AGED46TO64     ASIAN
## 13.16 13   MALE  AGED46TO64     ASIAN
## 13.17 13   MALE  AGED46TO64     ASIAN
## 13.18 13   MALE  AGED46TO64     ASIAN
## 13.19 13   MALE  AGED46TO64     ASIAN
## 13.20 13   MALE  AGED46TO64     ASIAN
## 13.21 13   MALE  AGED46TO64     ASIAN
## 14    14   MALE  AGED46TO64     BLACK
## 14.1  14   MALE  AGED46TO64     BLACK
## 14.2  14   MALE  AGED46TO64     BLACK
## 14.3  14   MALE  AGED46TO64     BLACK
## 14.4  14   MALE  AGED46TO64     BLACK
## 15    15   MALE  AGED46TO64     OTHER
## 17    17   MALE AGED65OLDER     MIXED
## 17.1  17   MALE AGED65OLDER     MIXED
## 18    18   MALE AGED65OLDER     ASIAN
## 18.1  18   MALE AGED65OLDER     ASIAN
## 18.2  18   MALE AGED65OLDER     ASIAN
## 18.3  18   MALE AGED65OLDER     ASIAN
## 18.4  18   MALE AGED65OLDER     ASIAN
## 18.5  18   MALE AGED65OLDER     ASIAN
## 18.6  18   MALE AGED65OLDER     ASIAN
## 18.7  18   MALE AGED65OLDER     ASIAN
## 18.8  18   MALE AGED65OLDER     ASIAN
## 18.9  18   MALE AGED65OLDER     ASIAN
## 19    19   MALE AGED65OLDER     BLACK
## 19.1  19   MALE AGED65OLDER     BLACK
## 22    22 FEMALE   AGED0TO16     MIXED
## 22.1  22 FEMALE   AGED0TO16     MIXED
## 23    23 FEMALE   AGED0TO16     ASIAN
## 23.1  23 FEMALE   AGED0TO16     ASIAN
## 23.2  23 FEMALE   AGED0TO16     ASIAN
## 23.3  23 FEMALE   AGED0TO16     ASIAN
## 23.4  23 FEMALE   AGED0TO16     ASIAN
## 23.5  23 FEMALE   AGED0TO16     ASIAN
## 23.6  23 FEMALE   AGED0TO16     ASIAN
## 23.7  23 FEMALE   AGED0TO16     ASIAN
## 23.8  23 FEMALE   AGED0TO16     ASIAN
## 23.9  23 FEMALE   AGED0TO16     ASIAN
## 23.10 23 FEMALE   AGED0TO16     ASIAN
## 24    24 FEMALE   AGED0TO16     BLACK
## 24.1  24 FEMALE   AGED0TO16     BLACK
## 26    26 FEMALE  AGED17TO45     WHITE
## 26.1  26 FEMALE  AGED17TO45     WHITE
## 27    27 FEMALE  AGED17TO45     MIXED
## 27.1  27 FEMALE  AGED17TO45     MIXED
## 27.2  27 FEMALE  AGED17TO45     MIXED
## 27.3  27 FEMALE  AGED17TO45     MIXED
## 27.4  27 FEMALE  AGED17TO45     MIXED
## 27.5  27 FEMALE  AGED17TO45     MIXED
## 27.6  27 FEMALE  AGED17TO45     MIXED
## 27.7  27 FEMALE  AGED17TO45     MIXED
## 27.8  27 FEMALE  AGED17TO45     MIXED
## 27.9  27 FEMALE  AGED17TO45     MIXED
## 27.10 27 FEMALE  AGED17TO45     MIXED
## 27.11 27 FEMALE  AGED17TO45     MIXED
## 27.12 27 FEMALE  AGED17TO45     MIXED
## 27.13 27 FEMALE  AGED17TO45     MIXED
## 28    28 FEMALE  AGED17TO45     ASIAN
## 28.1  28 FEMALE  AGED17TO45     ASIAN
## 28.2  28 FEMALE  AGED17TO45     ASIAN
## 28.3  28 FEMALE  AGED17TO45     ASIAN
## 28.4  28 FEMALE  AGED17TO45     ASIAN
## 28.5  28 FEMALE  AGED17TO45     ASIAN
## 28.6  28 FEMALE  AGED17TO45     ASIAN
## 28.7  28 FEMALE  AGED17TO45     ASIAN
## 28.8  28 FEMALE  AGED17TO45     ASIAN
## 28.9  28 FEMALE  AGED17TO45     ASIAN
## 28.10 28 FEMALE  AGED17TO45     ASIAN
## 28.11 28 FEMALE  AGED17TO45     ASIAN
## 28.12 28 FEMALE  AGED17TO45     ASIAN
## 28.13 28 FEMALE  AGED17TO45     ASIAN
## 28.14 28 FEMALE  AGED17TO45     ASIAN
## 28.15 28 FEMALE  AGED17TO45     ASIAN
## 28.16 28 FEMALE  AGED17TO45     ASIAN
## 28.17 28 FEMALE  AGED17TO45     ASIAN
## 28.18 28 FEMALE  AGED17TO45     ASIAN
## 28.19 28 FEMALE  AGED17TO45     ASIAN
## 28.20 28 FEMALE  AGED17TO45     ASIAN
## 28.21 28 FEMALE  AGED17TO45     ASIAN
## 28.22 28 FEMALE  AGED17TO45     ASIAN
## 28.23 28 FEMALE  AGED17TO45     ASIAN
## 28.24 28 FEMALE  AGED17TO45     ASIAN
## 28.25 28 FEMALE  AGED17TO45     ASIAN
## 28.26 28 FEMALE  AGED17TO45     ASIAN
## 28.27 28 FEMALE  AGED17TO45     ASIAN
## 28.28 28 FEMALE  AGED17TO45     ASIAN
## 28.29 28 FEMALE  AGED17TO45     ASIAN
## 28.30 28 FEMALE  AGED17TO45     ASIAN
## 28.31 28 FEMALE  AGED17TO45     ASIAN
## 28.32 28 FEMALE  AGED17TO45     ASIAN
## 28.33 28 FEMALE  AGED17TO45     ASIAN
## 28.34 28 FEMALE  AGED17TO45     ASIAN
## 28.35 28 FEMALE  AGED17TO45     ASIAN
## 28.36 28 FEMALE  AGED17TO45     ASIAN
## 28.37 28 FEMALE  AGED17TO45     ASIAN
## 28.38 28 FEMALE  AGED17TO45     ASIAN
## 28.39 28 FEMALE  AGED17TO45     ASIAN
## 28.40 28 FEMALE  AGED17TO45     ASIAN
## 28.41 28 FEMALE  AGED17TO45     ASIAN
## 28.42 28 FEMALE  AGED17TO45     ASIAN
## 28.43 28 FEMALE  AGED17TO45     ASIAN
## 28.44 28 FEMALE  AGED17TO45     ASIAN
## 28.45 28 FEMALE  AGED17TO45     ASIAN
## 28.46 28 FEMALE  AGED17TO45     ASIAN
## 28.47 28 FEMALE  AGED17TO45     ASIAN
## 28.48 28 FEMALE  AGED17TO45     ASIAN
## 28.49 28 FEMALE  AGED17TO45     ASIAN
## 28.50 28 FEMALE  AGED17TO45     ASIAN
## 28.51 28 FEMALE  AGED17TO45     ASIAN
## 28.52 28 FEMALE  AGED17TO45     ASIAN
## 28.53 28 FEMALE  AGED17TO45     ASIAN
## 28.54 28 FEMALE  AGED17TO45     ASIAN
## 28.55 28 FEMALE  AGED17TO45     ASIAN
## 28.56 28 FEMALE  AGED17TO45     ASIAN
## 28.57 28 FEMALE  AGED17TO45     ASIAN
## 29    29 FEMALE  AGED17TO45     BLACK
## 29.1  29 FEMALE  AGED17TO45     BLACK
## 29.2  29 FEMALE  AGED17TO45     BLACK
## 29.3  29 FEMALE  AGED17TO45     BLACK
## 29.4  29 FEMALE  AGED17TO45     BLACK
## 29.5  29 FEMALE  AGED17TO45     BLACK
## 29.6  29 FEMALE  AGED17TO45     BLACK
## 29.7  29 FEMALE  AGED17TO45     BLACK
## 29.8  29 FEMALE  AGED17TO45     BLACK
## 29.9  29 FEMALE  AGED17TO45     BLACK
## 29.10 29 FEMALE  AGED17TO45     BLACK
## 29.11 29 FEMALE  AGED17TO45     BLACK
## 29.12 29 FEMALE  AGED17TO45     BLACK
## 29.13 29 FEMALE  AGED17TO45     BLACK
## 30    30 FEMALE  AGED17TO45     OTHER
## 30.1  30 FEMALE  AGED17TO45     OTHER
## 31    31 FEMALE  AGED46TO64     WHITE
## 32    32 FEMALE  AGED46TO64     MIXED
## 32.1  32 FEMALE  AGED46TO64     MIXED
## 32.2  32 FEMALE  AGED46TO64     MIXED
## 32.3  32 FEMALE  AGED46TO64     MIXED
## 32.4  32 FEMALE  AGED46TO64     MIXED
## 32.5  32 FEMALE  AGED46TO64     MIXED
## 32.6  32 FEMALE  AGED46TO64     MIXED
## 32.7  32 FEMALE  AGED46TO64     MIXED
## 32.8  32 FEMALE  AGED46TO64     MIXED
## 33    33 FEMALE  AGED46TO64     ASIAN
## 33.1  33 FEMALE  AGED46TO64     ASIAN
## 33.2  33 FEMALE  AGED46TO64     ASIAN
## 33.3  33 FEMALE  AGED46TO64     ASIAN
## 33.4  33 FEMALE  AGED46TO64     ASIAN
## 33.5  33 FEMALE  AGED46TO64     ASIAN
## 33.6  33 FEMALE  AGED46TO64     ASIAN
## 33.7  33 FEMALE  AGED46TO64     ASIAN
## 33.8  33 FEMALE  AGED46TO64     ASIAN
## 33.9  33 FEMALE  AGED46TO64     ASIAN
## 33.10 33 FEMALE  AGED46TO64     ASIAN
## 33.11 33 FEMALE  AGED46TO64     ASIAN
## 33.12 33 FEMALE  AGED46TO64     ASIAN
## 33.13 33 FEMALE  AGED46TO64     ASIAN
## 33.14 33 FEMALE  AGED46TO64     ASIAN
## 33.15 33 FEMALE  AGED46TO64     ASIAN
## 33.16 33 FEMALE  AGED46TO64     ASIAN
## 33.17 33 FEMALE  AGED46TO64     ASIAN
## 33.18 33 FEMALE  AGED46TO64     ASIAN
## 33.19 33 FEMALE  AGED46TO64     ASIAN
## 33.20 33 FEMALE  AGED46TO64     ASIAN
## 33.21 33 FEMALE  AGED46TO64     ASIAN
## 33.22 33 FEMALE  AGED46TO64     ASIAN
## 33.23 33 FEMALE  AGED46TO64     ASIAN
## 33.24 33 FEMALE  AGED46TO64     ASIAN
## 33.25 33 FEMALE  AGED46TO64     ASIAN
## 33.26 33 FEMALE  AGED46TO64     ASIAN
## 33.27 33 FEMALE  AGED46TO64     ASIAN
## 33.28 33 FEMALE  AGED46TO64     ASIAN
## 33.29 33 FEMALE  AGED46TO64     ASIAN
## 33.30 33 FEMALE  AGED46TO64     ASIAN
## 33.31 33 FEMALE  AGED46TO64     ASIAN
## 33.32 33 FEMALE  AGED46TO64     ASIAN
## 33.33 33 FEMALE  AGED46TO64     ASIAN
## 33.34 33 FEMALE  AGED46TO64     ASIAN
## 33.35 33 FEMALE  AGED46TO64     ASIAN
## 33.36 33 FEMALE  AGED46TO64     ASIAN
## 34    34 FEMALE  AGED46TO64     BLACK
## 34.1  34 FEMALE  AGED46TO64     BLACK
## 34.2  34 FEMALE  AGED46TO64     BLACK
## 34.3  34 FEMALE  AGED46TO64     BLACK
## 34.4  34 FEMALE  AGED46TO64     BLACK
## 34.5  34 FEMALE  AGED46TO64     BLACK
## 34.6  34 FEMALE  AGED46TO64     BLACK
## 34.7  34 FEMALE  AGED46TO64     BLACK
## 34.8  34 FEMALE  AGED46TO64     BLACK
## 35    35 FEMALE  AGED46TO64     OTHER
## 37    37 FEMALE AGED65OLDER     MIXED
## 37.1  37 FEMALE AGED65OLDER     MIXED
## 37.2  37 FEMALE AGED65OLDER     MIXED
## 37.3  37 FEMALE AGED65OLDER     MIXED
## 38    38 FEMALE AGED65OLDER     ASIAN
## 38.1  38 FEMALE AGED65OLDER     ASIAN
## 38.2  38 FEMALE AGED65OLDER     ASIAN
## 38.3  38 FEMALE AGED65OLDER     ASIAN
## 38.4  38 FEMALE AGED65OLDER     ASIAN
## 38.5  38 FEMALE AGED65OLDER     ASIAN
## 38.6  38 FEMALE AGED65OLDER     ASIAN
## 38.7  38 FEMALE AGED65OLDER     ASIAN
## 38.8  38 FEMALE AGED65OLDER     ASIAN
## 38.9  38 FEMALE AGED65OLDER     ASIAN
## 38.10 38 FEMALE AGED65OLDER     ASIAN
## 38.11 38 FEMALE AGED65OLDER     ASIAN
## 38.12 38 FEMALE AGED65OLDER     ASIAN
## 38.13 38 FEMALE AGED65OLDER     ASIAN
## 38.14 38 FEMALE AGED65OLDER     ASIAN
## 38.15 38 FEMALE AGED65OLDER     ASIAN
## 38.16 38 FEMALE AGED65OLDER     ASIAN
## 39    39 FEMALE AGED65OLDER     BLACK
## 39.1  39 FEMALE AGED65OLDER     BLACK
## 39.2  39 FEMALE AGED65OLDER     BLACK
## 39.3  39 FEMALE AGED65OLDER     BLACK
## 9.8    9   MALE  AGED17TO45     BLACK
## 12.5  12   MALE  AGED46TO64     MIXED
## 5      5   MALE   AGED0TO16     OTHER
## 40    40 FEMALE AGED65OLDER     OTHER
## 19.2  19   MALE AGED65OLDER     BLACK
## 22.2  22 FEMALE   AGED0TO16     MIXED
## 31.1  31 FEMALE  AGED46TO64     WHITE
## 2.1    2   MALE   AGED0TO16     MIXED
## 26.2  26 FEMALE  AGED17TO45     WHITE
## 4.1    4   MALE   AGED0TO16     BLACK
## 37.4  37 FEMALE AGED65OLDER     MIXED
## 10.1  10   MALE  AGED17TO45     OTHER
## 29.14 29 FEMALE  AGED17TO45     BLACK
## 25    25 FEMALE   AGED0TO16     OTHER
## 27.14 27 FEMALE  AGED17TO45     MIXED
## 36    36 FEMALE AGED65OLDER     WHITE
## 39.4  39 FEMALE AGED65OLDER     BLACK
## 14.5  14   MALE  AGED46TO64     BLACK
## 16    16   MALE AGED65OLDER     WHITE
## 24.2  24 FEMALE   AGED0TO16     BLACK
## 18.10 18   MALE AGED65OLDER     ASIAN
## 23.11 23 FEMALE   AGED0TO16     ASIAN
## 1      1   MALE   AGED0TO16     WHITE
## 
## [[3]]
##       ID GENDER         AGE ETHNICITY
## 2      2   MALE   AGED0TO16     MIXED
## 3      3   MALE   AGED0TO16     ASIAN
## 3.1    3   MALE   AGED0TO16     ASIAN
## 3.2    3   MALE   AGED0TO16     ASIAN
## 3.3    3   MALE   AGED0TO16     ASIAN
## 4      4   MALE   AGED0TO16     BLACK
## 6      6   MALE  AGED17TO45     WHITE
## 6.1    6   MALE  AGED17TO45     WHITE
## 6.2    6   MALE  AGED17TO45     WHITE
## 6.3    6   MALE  AGED17TO45     WHITE
## 6.4    6   MALE  AGED17TO45     WHITE
## 6.5    6   MALE  AGED17TO45     WHITE
## 6.6    6   MALE  AGED17TO45     WHITE
## 7      7   MALE  AGED17TO45     MIXED
## 7.1    7   MALE  AGED17TO45     MIXED
## 7.2    7   MALE  AGED17TO45     MIXED
## 7.3    7   MALE  AGED17TO45     MIXED
## 7.4    7   MALE  AGED17TO45     MIXED
## 7.5    7   MALE  AGED17TO45     MIXED
## 7.6    7   MALE  AGED17TO45     MIXED
## 7.7    7   MALE  AGED17TO45     MIXED
## 7.8    7   MALE  AGED17TO45     MIXED
## 8      8   MALE  AGED17TO45     ASIAN
## 8.1    8   MALE  AGED17TO45     ASIAN
## 8.2    8   MALE  AGED17TO45     ASIAN
## 8.3    8   MALE  AGED17TO45     ASIAN
## 8.4    8   MALE  AGED17TO45     ASIAN
## 8.5    8   MALE  AGED17TO45     ASIAN
## 8.6    8   MALE  AGED17TO45     ASIAN
## 8.7    8   MALE  AGED17TO45     ASIAN
## 8.8    8   MALE  AGED17TO45     ASIAN
## 8.9    8   MALE  AGED17TO45     ASIAN
## 8.10   8   MALE  AGED17TO45     ASIAN
## 8.11   8   MALE  AGED17TO45     ASIAN
## 8.12   8   MALE  AGED17TO45     ASIAN
## 8.13   8   MALE  AGED17TO45     ASIAN
## 8.14   8   MALE  AGED17TO45     ASIAN
## 8.15   8   MALE  AGED17TO45     ASIAN
## 8.16   8   MALE  AGED17TO45     ASIAN
## 8.17   8   MALE  AGED17TO45     ASIAN
## 8.18   8   MALE  AGED17TO45     ASIAN
## 8.19   8   MALE  AGED17TO45     ASIAN
## 8.20   8   MALE  AGED17TO45     ASIAN
## 8.21   8   MALE  AGED17TO45     ASIAN
## 8.22   8   MALE  AGED17TO45     ASIAN
## 8.23   8   MALE  AGED17TO45     ASIAN
## 8.24   8   MALE  AGED17TO45     ASIAN
## 8.25   8   MALE  AGED17TO45     ASIAN
## 8.26   8   MALE  AGED17TO45     ASIAN
## 8.27   8   MALE  AGED17TO45     ASIAN
## 8.28   8   MALE  AGED17TO45     ASIAN
## 8.29   8   MALE  AGED17TO45     ASIAN
## 8.30   8   MALE  AGED17TO45     ASIAN
## 8.31   8   MALE  AGED17TO45     ASIAN
## 8.32   8   MALE  AGED17TO45     ASIAN
## 8.33   8   MALE  AGED17TO45     ASIAN
## 8.34   8   MALE  AGED17TO45     ASIAN
## 8.35   8   MALE  AGED17TO45     ASIAN
## 8.36   8   MALE  AGED17TO45     ASIAN
## 9      9   MALE  AGED17TO45     BLACK
## 9.1    9   MALE  AGED17TO45     BLACK
## 9.2    9   MALE  AGED17TO45     BLACK
## 9.3    9   MALE  AGED17TO45     BLACK
## 9.4    9   MALE  AGED17TO45     BLACK
## 9.5    9   MALE  AGED17TO45     BLACK
## 9.6    9   MALE  AGED17TO45     BLACK
## 9.7    9   MALE  AGED17TO45     BLACK
## 9.8    9   MALE  AGED17TO45     BLACK
## 10    10   MALE  AGED17TO45     OTHER
## 10.1  10   MALE  AGED17TO45     OTHER
## 11    11   MALE  AGED46TO64     WHITE
## 11.1  11   MALE  AGED46TO64     WHITE
## 12    12   MALE  AGED46TO64     MIXED
## 12.1  12   MALE  AGED46TO64     MIXED
## 12.2  12   MALE  AGED46TO64     MIXED
## 13    13   MALE  AGED46TO64     ASIAN
## 13.1  13   MALE  AGED46TO64     ASIAN
## 13.2  13   MALE  AGED46TO64     ASIAN
## 13.3  13   MALE  AGED46TO64     ASIAN
## 13.4  13   MALE  AGED46TO64     ASIAN
## 13.5  13   MALE  AGED46TO64     ASIAN
## 13.6  13   MALE  AGED46TO64     ASIAN
## 13.7  13   MALE  AGED46TO64     ASIAN
## 13.8  13   MALE  AGED46TO64     ASIAN
## 13.9  13   MALE  AGED46TO64     ASIAN
## 13.10 13   MALE  AGED46TO64     ASIAN
## 13.11 13   MALE  AGED46TO64     ASIAN
## 14    14   MALE  AGED46TO64     BLACK
## 14.1  14   MALE  AGED46TO64     BLACK
## 14.2  14   MALE  AGED46TO64     BLACK
## 16    16   MALE AGED65OLDER     WHITE
## 16.1  16   MALE AGED65OLDER     WHITE
## 17    17   MALE AGED65OLDER     MIXED
## 17.1  17   MALE AGED65OLDER     MIXED
## 17.2  17   MALE AGED65OLDER     MIXED
## 18    18   MALE AGED65OLDER     ASIAN
## 18.1  18   MALE AGED65OLDER     ASIAN
## 18.2  18   MALE AGED65OLDER     ASIAN
## 18.3  18   MALE AGED65OLDER     ASIAN
## 18.4  18   MALE AGED65OLDER     ASIAN
## 18.5  18   MALE AGED65OLDER     ASIAN
## 18.6  18   MALE AGED65OLDER     ASIAN
## 18.7  18   MALE AGED65OLDER     ASIAN
## 18.8  18   MALE AGED65OLDER     ASIAN
## 18.9  18   MALE AGED65OLDER     ASIAN
## 18.10 18   MALE AGED65OLDER     ASIAN
## 18.11 18   MALE AGED65OLDER     ASIAN
## 19    19   MALE AGED65OLDER     BLACK
## 19.1  19   MALE AGED65OLDER     BLACK
## 19.2  19   MALE AGED65OLDER     BLACK
## 21    21 FEMALE   AGED0TO16     WHITE
## 22    22 FEMALE   AGED0TO16     MIXED
## 23    23 FEMALE   AGED0TO16     ASIAN
## 23.1  23 FEMALE   AGED0TO16     ASIAN
## 23.2  23 FEMALE   AGED0TO16     ASIAN
## 23.3  23 FEMALE   AGED0TO16     ASIAN
## 23.4  23 FEMALE   AGED0TO16     ASIAN
## 23.5  23 FEMALE   AGED0TO16     ASIAN
## 24    24 FEMALE   AGED0TO16     BLACK
## 26    26 FEMALE  AGED17TO45     WHITE
## 26.1  26 FEMALE  AGED17TO45     WHITE
## 26.2  26 FEMALE  AGED17TO45     WHITE
## 26.3  26 FEMALE  AGED17TO45     WHITE
## 26.4  26 FEMALE  AGED17TO45     WHITE
## 26.5  26 FEMALE  AGED17TO45     WHITE
## 26.6  26 FEMALE  AGED17TO45     WHITE
## 26.7  26 FEMALE  AGED17TO45     WHITE
## 26.8  26 FEMALE  AGED17TO45     WHITE
## 27    27 FEMALE  AGED17TO45     MIXED
## 27.1  27 FEMALE  AGED17TO45     MIXED
## 27.2  27 FEMALE  AGED17TO45     MIXED
## 27.3  27 FEMALE  AGED17TO45     MIXED
## 27.4  27 FEMALE  AGED17TO45     MIXED
## 27.5  27 FEMALE  AGED17TO45     MIXED
## 27.6  27 FEMALE  AGED17TO45     MIXED
## 27.7  27 FEMALE  AGED17TO45     MIXED
## 27.8  27 FEMALE  AGED17TO45     MIXED
## 27.9  27 FEMALE  AGED17TO45     MIXED
## 27.10 27 FEMALE  AGED17TO45     MIXED
## 27.11 27 FEMALE  AGED17TO45     MIXED
## 28    28 FEMALE  AGED17TO45     ASIAN
## 28.1  28 FEMALE  AGED17TO45     ASIAN
## 28.2  28 FEMALE  AGED17TO45     ASIAN
## 28.3  28 FEMALE  AGED17TO45     ASIAN
## 28.4  28 FEMALE  AGED17TO45     ASIAN
## 28.5  28 FEMALE  AGED17TO45     ASIAN
## 28.6  28 FEMALE  AGED17TO45     ASIAN
## 28.7  28 FEMALE  AGED17TO45     ASIAN
## 28.8  28 FEMALE  AGED17TO45     ASIAN
## 28.9  28 FEMALE  AGED17TO45     ASIAN
## 28.10 28 FEMALE  AGED17TO45     ASIAN
## 28.11 28 FEMALE  AGED17TO45     ASIAN
## 28.12 28 FEMALE  AGED17TO45     ASIAN
## 28.13 28 FEMALE  AGED17TO45     ASIAN
## 28.14 28 FEMALE  AGED17TO45     ASIAN
## 28.15 28 FEMALE  AGED17TO45     ASIAN
## 28.16 28 FEMALE  AGED17TO45     ASIAN
## 28.17 28 FEMALE  AGED17TO45     ASIAN
## 28.18 28 FEMALE  AGED17TO45     ASIAN
## 28.19 28 FEMALE  AGED17TO45     ASIAN
## 28.20 28 FEMALE  AGED17TO45     ASIAN
## 28.21 28 FEMALE  AGED17TO45     ASIAN
## 28.22 28 FEMALE  AGED17TO45     ASIAN
## 28.23 28 FEMALE  AGED17TO45     ASIAN
## 28.24 28 FEMALE  AGED17TO45     ASIAN
## 28.25 28 FEMALE  AGED17TO45     ASIAN
## 28.26 28 FEMALE  AGED17TO45     ASIAN
## 28.27 28 FEMALE  AGED17TO45     ASIAN
## 28.28 28 FEMALE  AGED17TO45     ASIAN
## 28.29 28 FEMALE  AGED17TO45     ASIAN
## 28.30 28 FEMALE  AGED17TO45     ASIAN
## 28.31 28 FEMALE  AGED17TO45     ASIAN
## 28.32 28 FEMALE  AGED17TO45     ASIAN
## 28.33 28 FEMALE  AGED17TO45     ASIAN
## 28.34 28 FEMALE  AGED17TO45     ASIAN
## 28.35 28 FEMALE  AGED17TO45     ASIAN
## 28.36 28 FEMALE  AGED17TO45     ASIAN
## 28.37 28 FEMALE  AGED17TO45     ASIAN
## 28.38 28 FEMALE  AGED17TO45     ASIAN
## 28.39 28 FEMALE  AGED17TO45     ASIAN
## 28.40 28 FEMALE  AGED17TO45     ASIAN
## 28.41 28 FEMALE  AGED17TO45     ASIAN
## 28.42 28 FEMALE  AGED17TO45     ASIAN
## 28.43 28 FEMALE  AGED17TO45     ASIAN
## 28.44 28 FEMALE  AGED17TO45     ASIAN
## 28.45 28 FEMALE  AGED17TO45     ASIAN
## 29    29 FEMALE  AGED17TO45     BLACK
## 29.1  29 FEMALE  AGED17TO45     BLACK
## 29.2  29 FEMALE  AGED17TO45     BLACK
## 29.3  29 FEMALE  AGED17TO45     BLACK
## 29.4  29 FEMALE  AGED17TO45     BLACK
## 29.5  29 FEMALE  AGED17TO45     BLACK
## 29.6  29 FEMALE  AGED17TO45     BLACK
## 29.7  29 FEMALE  AGED17TO45     BLACK
## 29.8  29 FEMALE  AGED17TO45     BLACK
## 29.9  29 FEMALE  AGED17TO45     BLACK
## 29.10 29 FEMALE  AGED17TO45     BLACK
## 29.11 29 FEMALE  AGED17TO45     BLACK
## 30    30 FEMALE  AGED17TO45     OTHER
## 30.1  30 FEMALE  AGED17TO45     OTHER
## 30.2  30 FEMALE  AGED17TO45     OTHER
## 31    31 FEMALE  AGED46TO64     WHITE
## 31.1  31 FEMALE  AGED46TO64     WHITE
## 31.2  31 FEMALE  AGED46TO64     WHITE
## 32    32 FEMALE  AGED46TO64     MIXED
## 32.1  32 FEMALE  AGED46TO64     MIXED
## 32.2  32 FEMALE  AGED46TO64     MIXED
## 32.3  32 FEMALE  AGED46TO64     MIXED
## 33    33 FEMALE  AGED46TO64     ASIAN
## 33.1  33 FEMALE  AGED46TO64     ASIAN
## 33.2  33 FEMALE  AGED46TO64     ASIAN
## 33.3  33 FEMALE  AGED46TO64     ASIAN
## 33.4  33 FEMALE  AGED46TO64     ASIAN
## 33.5  33 FEMALE  AGED46TO64     ASIAN
## 33.6  33 FEMALE  AGED46TO64     ASIAN
## 33.7  33 FEMALE  AGED46TO64     ASIAN
## 33.8  33 FEMALE  AGED46TO64     ASIAN
## 33.9  33 FEMALE  AGED46TO64     ASIAN
## 33.10 33 FEMALE  AGED46TO64     ASIAN
## 33.11 33 FEMALE  AGED46TO64     ASIAN
## 33.12 33 FEMALE  AGED46TO64     ASIAN
## 33.13 33 FEMALE  AGED46TO64     ASIAN
## 33.14 33 FEMALE  AGED46TO64     ASIAN
## 34    34 FEMALE  AGED46TO64     BLACK
## 34.1  34 FEMALE  AGED46TO64     BLACK
## 34.2  34 FEMALE  AGED46TO64     BLACK
## 34.3  34 FEMALE  AGED46TO64     BLACK
## 35    35 FEMALE  AGED46TO64     OTHER
## 36    36 FEMALE AGED65OLDER     WHITE
## 36.1  36 FEMALE AGED65OLDER     WHITE
## 36.2  36 FEMALE AGED65OLDER     WHITE
## 37    37 FEMALE AGED65OLDER     MIXED
## 37.1  37 FEMALE AGED65OLDER     MIXED
## 37.2  37 FEMALE AGED65OLDER     MIXED
## 37.3  37 FEMALE AGED65OLDER     MIXED
## 38    38 FEMALE AGED65OLDER     ASIAN
## 38.1  38 FEMALE AGED65OLDER     ASIAN
## 38.2  38 FEMALE AGED65OLDER     ASIAN
## 38.3  38 FEMALE AGED65OLDER     ASIAN
## 38.4  38 FEMALE AGED65OLDER     ASIAN
## 38.5  38 FEMALE AGED65OLDER     ASIAN
## 38.6  38 FEMALE AGED65OLDER     ASIAN
## 38.7  38 FEMALE AGED65OLDER     ASIAN
## 38.8  38 FEMALE AGED65OLDER     ASIAN
## 38.9  38 FEMALE AGED65OLDER     ASIAN
## 38.10 38 FEMALE AGED65OLDER     ASIAN
## 38.11 38 FEMALE AGED65OLDER     ASIAN
## 38.12 38 FEMALE AGED65OLDER     ASIAN
## 38.13 38 FEMALE AGED65OLDER     ASIAN
## 38.14 38 FEMALE AGED65OLDER     ASIAN
## 39    39 FEMALE AGED65OLDER     BLACK
## 39.1  39 FEMALE AGED65OLDER     BLACK
## 39.2  39 FEMALE AGED65OLDER     BLACK
## 39.3  39 FEMALE AGED65OLDER     BLACK
## 40    40 FEMALE AGED65OLDER     OTHER
## 6.7    6   MALE  AGED17TO45     WHITE
## 2.1    2   MALE   AGED0TO16     MIXED
## 10.2  10   MALE  AGED17TO45     OTHER
## 16.2  16   MALE AGED65OLDER     WHITE
## 28.46 28 FEMALE  AGED17TO45     ASIAN
## 1      1   MALE   AGED0TO16     WHITE
## 11.2  11   MALE  AGED46TO64     WHITE
## 5      5   MALE   AGED0TO16     OTHER
## 18.12 18   MALE AGED65OLDER     ASIAN
## 22.1  22 FEMALE   AGED0TO16     MIXED
## 26.9  26 FEMALE  AGED17TO45     WHITE
## 24.1  24 FEMALE   AGED0TO16     BLACK
## 9.9    9   MALE  AGED17TO45     BLACK
## 3.4    3   MALE   AGED0TO16     ASIAN
## 7.9    7   MALE  AGED17TO45     MIXED
```


## Rearranging the output into a single data frame
For ease of analysis, it is best to have all the output
individuals in a single data frame, with a new column added to show which
zone they belong to:


```r
intall.df <- cbind(intall[[1]], zone = 1)
head(intall.df)
```

```
##     ID GENDER       AGE ETHNICITY zone
## 1    1   MALE AGED0TO16     WHITE    1
## 2    2   MALE AGED0TO16     MIXED    1
## 3    3   MALE AGED0TO16     ASIAN    1
## 3.1  3   MALE AGED0TO16     ASIAN    1
## 3.2  3   MALE AGED0TO16     ASIAN    1
## 3.3  3   MALE AGED0TO16     ASIAN    1
```

```r
for (i in 2:nrow(cons)) {
    # run for all zones with 1:length(intall)
    intall.df <- rbind(intall.df, cbind(intall[[i]], zone = i))
}
summary(intall.df[intall.df$zone == 1, ])  # test the output
```

```
##        ID          GENDER             AGE      ETHNICITY        zone  
##  Min.   : 1.0   FEMALE:102   AGED0TO16  : 50   ASIAN:199   Min.   :1  
##  1st Qu.: 8.0   MALE  :148   AGED17TO45 :101   BLACK:  9   1st Qu.:1  
##  Median :13.0                AGED46TO64 : 79   MIXED: 11   Median :1  
##  Mean   :17.6                AGED65OLDER: 20   OTHER: 20   Mean   :1  
##  3rd Qu.:28.0                                  WHITE: 11   3rd Qu.:1  
##  Max.   :38.0                                              Max.   :1
```

```r
summary(intall.df[intall.df$zone == 3, ])  # test the output
```

```
##        ID          GENDER             AGE      ETHNICITY        zone  
##  Min.   : 1.0   FEMALE:149   AGED0TO16  : 21   ASIAN:150   Min.   :3  
##  1st Qu.: 9.0   MALE  :121   AGED17TO45 :152   BLACK: 39   1st Qu.:3  
##  Median :26.0                AGED46TO64 : 48   MIXED: 40   Median :3  
##  Mean   :21.2                AGED65OLDER: 49   OTHER:  9   Mean   :3  
##  3rd Qu.:29.0                                  WHITE: 32   3rd Qu.:3  
##  Max.   :40.0                                              Max.   :3
```

```r
summary(intall.df)
```

```
##        ID          GENDER             AGE      ETHNICITY        zone     
##  Min.   : 1.0   FEMALE:450   AGED0TO16  :103   ASIAN:548   Min.   :1.00  
##  1st Qu.: 9.0   MALE  :390   AGED17TO45 :401   BLACK:100   1st Qu.:1.00  
##  Median :23.0                AGED46TO64 :221   MIXED:101   Median :2.00  
##  Mean   :20.8                AGED65OLDER:115   OTHER: 38   Mean   :2.02  
##  3rd Qu.:29.0                                  WHITE: 53   3rd Qu.:3.00  
##  Max.   :40.0                                              Max.   :3.00
```


## The impact of integerisation on model fit
Integerisation usually introduces some error. Let's see how much:


```r
cor(as.numeric(as.matrix(intagg)), as.numeric(as.matrix(cons)))
```

```
## [1] 0.9998
```


The answer is NOT A LOT! The integerisation strategy is good at selecting
appropriate individuals for each zone.


