# comparison between trs and pp integerisation methods
# see Lovelace and Ballas (2011) on 'Truncate, replicate sample' for context
# run after cMap.R, pp-integerise.R and trs-integerise.R, uncommenting this: intagg.* <- intagg

sum(abs(cons - ind.agg[,,4])) / sum(cons)
sum(abs(cons - intagg.pp)) / sum(cons)
sum(abs(cons - intagg.trs)) / sum(cons)