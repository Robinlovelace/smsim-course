# Equalising constraint variable populations

IPF relies on constraint variables that have equal populations.
This makes sense: each constraint variable should related directly
to the same target population. Problems can emerge, for example, if one
constraint variable (e.g. travel to work) reports information only for a subset
of the population (people with a job), whereas another (e.g. all people by age
and sex) is about a larger or smaller group of people. In this case,
we would make a decision about what the 'true' target population is
and set all other variables equal to this. So, if we are focussed on
people in employment, we would first remove all people not of employable
age (e.g. below 16 and over 65) from the age variable.
The next stage is to 'equalise' the variables, setting the total
to constraint population.
If not, it's like trying to constrain individuals
by age and sex to number of cars per household: the two constraints are
about different things. More practically, the resulting weights from
IPF will not converge unless all of the constraint variables have the
same total population.

The code in this vignette describes how to equalise the constraint
variables to a single 'dominant' variable. A judgement must be made
and justified about which variable should be used to determine the
population. This stage of 'equalisation' can change the results of
spatial microsimulation and there is a danger that the process will
lead to incorrect constraints, so it important to mention it in the
methodology, as part of a drive towards reproducible, transparent and
rigorous research.

We use an example that is used to generate the constraint data
for the 'cakeMap' example. What follows is basically an expansion on
'[load-all.R](https://github.com/Robinlovelace/smsim-course/blob/master/../data/cakeMap/load-all.R)'.
This is real code, performing a real job in that it converts the
constraints with variable populations into equalised constraints,
all of which have the same total population.

# The worked example

First we read in the constraint variables, which have already
been pre-processed from the raw data to rename the categories
and ensure that only the columns that are needed are selected.
The code that does this pre-processing is contained in
files entitled 'process*.R'. The file to process the raw
geographic age/sex variable, for example, is found in
'[process-age.R](https://github.com/Robinlovelace/smsim-course/blob/master/../data/cakeMap/process-age.R)'
in the ../data/cakeMap folder of smsim-course repository.
This is the origin of 'con1.csv' to 'con3.csv', which we load below.



```r
getwd()  # should be in the smsim-course folder
```

```
## [1] "/nfs/foe-fs-01_users/georl/repos/smsim-course/vignettes"
```

```r
con1 <- read.csv("../data/cakeMap/con1.csv")  # age/sex variable
con2 <- read.csv("../data/cakeMap/con2.csv")  # no car / car
con3 <- read.csv("../data/cakeMap/con3.csv")  # ns-sec
names(con1)
```

```
##  [1] "m16_24" "m25_34" "m35_44" "m45_54" "m55_64" "m65_74" "f16_24"
##  [8] "f25_34" "f35_44" "f45_54" "f55_64" "f65_74"
```

```r
names(con2)
```

```
## [1] "X922452" "X271399"
```

```r
names(con3)
```

```
##  [1] "X1.1"  "X1.2"  "X2"    "X3"    "X4"    "X5"    "X6"    "X7"   
##  [9] "X8"    "Other"
```


Taking a look at con2 - car ownership - it is clear that some
additional pre-processing is needed. This is done in the following code:


```r
con2 <- data.frame(cbind(con2[, 1] - con2[, 2], con2[, 2]))
names(con2) <- c("Car", "NoCar")
head(con2)
```

```
##    Car NoCar
## 1 5651  1134
## 2 6431  1792
## 3 6458  1239
## 4 4574  1772
## 5 3813  3311
## 6 3344  2412
```


# Checking the populations

Let's check whether or not the constraint variables add-up,
simply by summing their total populations:


```r
sum(con1)
```

```
## [1] 1623800
```

```r
sum(con2)
```

```
## [1] 922452
```

```r
sum(con3)
```

```
## [1] 1904260
```

```r
c(sum(con1), sum(con2), sum(con3))/sum(con1)  # how much the values deviate from expected
```

```
## [1] 1.0000 0.5681 1.1727
```


The above code shows that there is a wide range of total populations
for each of the constraints. `con2` is highly problematic here
as it contains less than 60% of the population of the
target variable that defines the total for each zone.
In practice we would need to think carefully about this:
`con2` is in fact a household-level variable, explaining the
low number. It does not relate to the number of cars that
people *have access to* so should not really be equalised in this
way.

For the purposes of demonstrating the code, however, we will
press on. Next is the equalisation stage:


```r
con.pop <- rowSums(con1)
con1 <- round(con1 * con.pop/rowSums(con1))
con2 <- round(con2 * con.pop/rowSums(con2))
con3 <- round(con3 * con.pop/rowSums(con3))
```


This code first sets the constraint population (`con.pop`)
equal to the row sums for constraint 1.
In the subsequent 3 lines, the numbers in each cell are
*equalised*. This works by multiplying the values in
each cell by the constraint population and then dividing by
the total population in each zone according to the constraint
variable in question. Thus cells in zones with low populations
increase; cells in zones with populations higher than expected decrease.
Critically, the *proportions* between the different cell numbers stay the same.

`round` is used to ensure the results are integers for ease of understanding
the input dataset and prevent confusion in this cakeMap example.
It is not needed: IPF works equally well on non-integer cells and total
populations.

# Check the results make sense

Do the results of this equalisation phase make sense?
The following R queries check to ensure that this is the case:


```r
sum(con1); sum(con2); sum(con3); # all the numbers should be equal - this is close enough!
```

```
## [1] 1623797
```

```r

# bind all the data frames together
all.msim <- cbind(con1 
                  ,con2
                  ,con3
                  )

which(all.msim == 0) 
```

```
## integer(0)
```

```r
range(all.msim) # range of values - there are no zeros
```

```
## [1]    35 17381
```

```r
mean(con.pop) # average number of individuals in each zone
```

```
## [1] 13095
```

```r

# in case there are zeros, set just above 1 to avoid subsequent problems
con1[con1 == 0] <- con2[con2 == 0] <- con3[con3 == 0] <- 0.0001   
# previous step avoids zero values (aren't any in this case...)

head(all.msim)
```

```
##   m16_24 m25_34 m35_44 m45_54 m55_64 m65_74 f16_24 f25_34 f35_44 f45_54
## 1    671    771   1033   1160   1165    772    679    760   1053   1283
## 2    887   1254   1217   1344   1229    752    832   1169   1255   1369
## 3    883    864   1195   1382   1170    878    811    962   1285   1493
## 4    976   1085   1184   1073    852    482   1021   1202   1034   1177
## 5   1368   1764   1528    978    768    462   1421   1771   1296   1004
## 6   1552   1847   1425    908    688    349   1572   1893   1414    887
##   f55_64 f65_74   Car NoCar X1.1 X1.2   X2   X3   X4  X5   X6   X7   X8
## 1   1139    859  9449  1896  347 1068 2772 1731 1132 657 1173  760  288
## 2   1228    886 10497  2925  397 1313 3264 1978 1128 735 1285 1041  378
## 3   1276    933 11018  2114  355 1001 2999 1894 1261 797 1483 1060  366
## 4    793    587  8264  3202  143  398 1583 1455  803 791 1587 1265  886
## 5    765    485  7285  6325   83  253  971  961  803 706 1638 1749 2106
## 6    626    355  7852  5664   48  173  717  696  844 517 1305 1339 2605
##   Other
## 1  1417
## 2  1902
## 3  1915
## 4  2556
## 5  4341
## 6  5270
```

```r

category.labels <- names(all.msim) # define the category variables we're working with
```




# Save the final output



```r
write.csv(all.msim, "../data/cakeMap/cons.csv", row.names = F)
```





