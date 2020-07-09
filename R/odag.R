##### Makes a directed acyclic graph (DAG) out of all ovariables and their dependencies in the global environment.
##### Requires igraph package.
### ideas of additional atteibutes: decision indices are shown as specific colour on vertex border. 
## voi analysis results are shown as the size of vertex. Can this be retrieved from somewhere automatically? Only if there is a specific object eg voi. 
## good functions for layout: layout.fruchterman.reingold() layout.reingold.tilford() layout.lgl()
## This function is outdated and depreciated. Use functions from 
## code Op_en3861/makeGraph and others from [[Insight network]] instead.

odag <- function(plotting = TRUE, ...) {
	edg <- data.frame()
	ova <- character()
	dec <- character()
	deci <- character()
	for(i in ls(envir = .GlobalEnv)) { # Find all ovariables
		if("ovariable" %in% class(get(i))) {
			ova <- c(ova, i)
		}
		if("odecision" %in% class(get(i))) {
			dec <- c(dec, i)
		}
	}
	for(i in ova) { # Find all edges between ovariables
		obj <- get(i)
		if(nrow(obj@dependencies) > 0) {
			edg <- rbind(edg, data.frame(
							from = obj@dependencies$Name,
							to = obj@name,
							weight = 1 # Not sure yet what should be used as weight.
					))
		}
	}
	if (length(edg) == 0) {
		warning("No edges found!")
		return()
	}
	dag <- graph.data.frame(edg, directed = TRUE) # Create directed acyclic graph
	for(i in dec) {
		deci <- union(deci, get(i)@dectable$Decision)
	}
	for(i in ova) { #Add attributes for vertices
		obj <- get(i)
		if(!i %in% V(dag)$name) dag <- dag + vertex(obj@name)
		V(dag)$Size[V(dag)$name == i] <- nrow(obj@output)
		ind <- colnames(obj@output)[
				obj@marginal &
						!grepl("Source$", colnames(obj@output)) &
						! colnames(obj@output) %in% deci
		]
		decii <- deci[deci %in% colnames(obj@output)]
		if(length(ind) > 0) V(dag)$Indices[V(dag)$name == i] <- paste(ind, collapse = ", ")
		if(length(decii) > 0) V(dag)$Decisions[V(dag)$name == i] <- paste(decii, collapse = ", ")
	}
	V(dag)$Size[is.na(V(dag)$Size)] <- 1
	V(dag)$Size[V(dag)$Size == 0] <- 1
	V(dag)$Indices[is.na(V(dag)$Indices)] <- ""
	V(dag)$Decisions[is.na(V(dag)$Decisions)] <- ""
	
	if(plotting) {
		plot(
				dag, 
				vertex.label.cex = 0.7, 
				vertex.size = log(V(dag)$Size)+2, # Vertex size is (non-linearly) relative to rows in output.
				vertex.color = ifelse(nchar(V(dag)$Decisions) > 0, "Red", "SkyBlue2"),
				layout = layout.fruchterman.reingold
		)
	} else {
		return(dag) 
	}
}
