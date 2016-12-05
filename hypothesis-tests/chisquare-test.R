tbl = read.ftable("datafiles/flight.csv", sep=",", skip=1, row.var.names="TicketType", col.vars=list("FlightType" = c("Domestic", "International")))
chisq.test(tbl)