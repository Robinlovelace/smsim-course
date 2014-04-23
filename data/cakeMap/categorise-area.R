# create new age/sex variable
AS <- paste0(intall[[i]]$Sex, intall[[i]]$ageband4)
unique(AS)

# matrix for constraint 1 - age/sex
m1 <- model.matrix(~AS-1)

# matrix for con2 (car ownership)
intall[[i]]$Car <- as.character(intall[[i]]$Car)
m2 <- model.matrix(~intall[[i]]$Car-1)

# matrix for con3 (nssec)
intall[[i]]$NSSEC8 <- as.character(intall[[i]]$NSSEC8)
m3 <- model.matrix(~intall[[i]]$NSSEC8-1)

# Polishing up
area.cat <- data.frame(cbind(m1, m2, m3))
names(ind.cat) <- category.labels
