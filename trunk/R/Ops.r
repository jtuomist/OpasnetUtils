# SETMETHOD OPS ######### Arithmetic operations of ovariables: first they are merged by index columns,
### then the operation is performed for the Result.x and Result.y columns.
### If one of the expressions is numeric, it is first transformed to ovariable.
setMethod(
	f = "Ops", 
	signature = signature(e1 = "ovariable", e2 = "ovariable"), 
	definition = function(e1, e2) {
		out <- merge(e1, e2)@output
		
		test1 <- paste(e1@name, "Result", sep = "") %in% colnames(out)
		test2 <- paste(e2@name, "Result", sep = "") %in% colnames(out)
		
		rescol1 <- ifelse(test1, paste(e1@name, "Result", sep = ""), "Result")
		rescol2 <- ifelse(test2, paste(e2@name, "Result", sep = ""), "Result")
		
		if (!(test1 & test2)) {
			rescol1 <- "Result.x"
			rescol2 <- "Result.y"
		}
		
		out$Result <- callGeneric(out[[rescol1]], out[[rescol2]])
		
		out <- new(
			"ovariable",
			dependencies = data.frame(Name = c(e1@name, e2@name)),
			output = out[, !colnames(out) %in% c(rescol1, rescol2)]
		)
		out <- CheckMarginals(out)
		return(out)
	}
)

setMethod(
	f = "Ops", 
	signature = signature(e1 = "ovariable", e2 = "numeric"), 
	definition = function(e1, e2) {
		e2 <- make.ovariable(e2)
		out <- callGeneric(e1, e2)
		return(out)
	}
)

setMethod(
	f = "Ops", 
	signature = signature(e1 = "numeric", e2 = "ovariable"), 
	definition = function(e1, e2) {
		e1 <- make.ovariable(e1)
		out <- callGeneric(e2, e1)
		return(out)
	}
)