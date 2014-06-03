# creating 0/1 matrix representation of ind. data
gender.cat <- model.matrix(~intall[[i]]$GENDER -1 )
age.cat <- model.matrix(~intall[[i]]$AGE -1)
eth.cat <- model.matrix(~intall[[i]]$ETHNICITY - 1)
area.cat <-  cbind(gender.cat, age.cat, eth.cat)
names(area.cat) <- cat.names