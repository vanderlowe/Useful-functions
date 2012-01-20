library(RMySQL)

stderr <- function(x) {
	sqrt(var(x)/length(x))
}

exists.object <- function(obj) {
	return(exists(as.character(substitute(obj))))
}

mean.na <- function(x) {
	mean(x, na.rm = T)
}

round.mean <- function(x, digits = 0) {
	round(mean.na(x), digits = d)
}

round.sd <- function(x, digits = 0) {
	round(sd(x, na.rm = T), digits = d)
}


# Plotting
error.bar <- function(x, y, upper, lower = upper, length = 0.1, ...){
	# The x-values can be obtained by assigning the return value of a barplot to a variable
	# e.g. barx <- barplot(stuff)
	x <- as.vector(x)
	y <- as.vector(y)
	upper <- as.vector(upper)
	lower <- as.vector(lower)
		
	if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
		stop("vectors must be same length")
	arrows(x, y + upper, x, y - lower, angle = 90, code = 3, length = length, ...)
}

# APA-style reporting
apa.dec <- function(x, leading.zero = F) {
	value = formatC(x, digits = 2, format = "f")
	if (no.zero) {
		return(gsub("^0","", value))
	} else {
		return(value)
	}
}

apa.n <- function(var) {
	return(sprintf("\\\\emph{n}~=~%i", nrow(var)))
}

apa.N <- function(var) {
	return(sprintf("\\\\emph{N}~=~%i", nrow(var)))
}

apa.meansd <- function(x, include.x = FALSE) {
	x.bar = mean(x, na.rm = T)
	x.sd = sd(x, na.rm = T)
	if (include.x) {x.bar.str = "\\\\bar{x}~=~"} else {x.bar.str = ""}
	return(sprintf("$%s%.2f, (\\\\emph{SD}~=~%.2f$)",x.bar.str, x.bar, x.sd))
}

apa.mean <- function(x, digits = 2, with.x = F) {
	x.bar = format(mean(x, na.rm = T), digits = digits, nsmall = digits)
	if (with.x) {prefix = "M~=~"} else {prefix = ""}
	return(paste("$",prefix,x.bar,"$", sep = ""))
}

apa.sd <- function(x) {
	x.sd = sd(x, na.rm = T)
	return(sprintf("$\\\\emph{SD}~=~%.2f$", x.sd))
}

cor.table <- function(x) {
	# Code yanked from http://myowelt.blogspot.com/ ("Beautiful Correlation Tables in R" posted on 2008-04-09)

	require(Hmisc)
	require(xtable)
	x <- as.matrix(x) 
	R <- rcorr(x)$r 
	p <- rcorr(x)$P 

	## define notions for significance levels; spacing is important.
	mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))

	## trunctuate the matrix that holds the correlations to two decimal
	R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 

	## build a new matrix that includes the correlations with their apropriate stars 
	Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
	diag(Rnew) <- paste(diag(R), " ", sep="") 
	rownames(Rnew) <- colnames(x)
	if (length(colnames(x)) > 3) {
		colnames(Rnew) <- (1:length(colnames(x)))
		rownames(Rnew) <- paste(1:length(colnames(x)), colnames(x), sep =". ")
	} else {
		colnames(Rnew) <- paste(colnames(x), "", sep="")
	}
	

	## remove upper triangle
	Rnew <- as.matrix(Rnew)
	Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
	Rnew <- as.data.frame(Rnew, stringsAsFactors = FALSE) 

	## remove last column and return the matrix (which is now a data frame)
	Rnew <- cbind(Rnew[1:length(Rnew)-1])
	return(Rnew)
}


prettySort <- function(x, digits = 2, ...) {
	# Print nice looking factor tables
	
	x = factor.table(x$loadings, ...)

	# convert to numeric
	for (c in 2:ncol(x)) {
		x[,c] = as.numeric(gsub(" ", "", x[,c]))
	}
	col.order = paste("-x[,",2:ncol(x),"]", sep = "", collapse = ", ")
	col.order = paste("x = x[order(", col.order,"),]", sep = "", collappse = "")
	#x = x[order(parse(text = col.order)),]
	eval(parse(text = col.order))

	# convert to character
	for (c in 2:ncol(x)) {
		x[,c] = format(x[,c], digits = digits)
		x[,c] = as.character(gsub("NA","", x[,c]))
	}
	
	return(x)
}




# Functions for calculations
################################################################################################

# Functions to work with data frames
################################################################################################
grepcols <- function(pattern, dataset) {
	vars = names(dataset)[grep(pattern,names(dataset))]
	results = data.frame(data.by.name(dataset,vars[1]))
	
	for (i in 2:length(vars)) {
		results = cbind(results, data.by.name(dataset,vars[i]))
	}
	names(results) <- vars
	return(results)
}

names.num <- function(dataset, debug = F) {
	r = character(0)
	
	for (i in 1:ncol(dataset)) {
		if (class(dataset[,i]) == "integer" | class(dataset[,i]) == "numeric") {
			if (debug) {cat(names(dataset)[i],"\n")}
			thisSD <- try(sd(dataset[,i], na.rm = T))
			if (class(thisSD) == "try-error" | is.na(thisSD)) {next} else {
				# print(names(dataset)[i])
				if (thisSD > 0) {r = c(r, names(dataset)[i])}
			}
		}
	}
	return(r)
}

names.fac <- function(dataset) {
	r = character(0)
	
	for (i in 1:ncol(dataset)) {
		if (class(dataset[,i]) == "factor") {
			r = c(r, names(dataset)[i])
		}
	}
	return(r)
}

names.grep <- function(pattern, dataset) {
	vars = names(dataset)[grep(pattern,names(dataset))]
	return(vars)
}

name.by.i <- function(dataset,i) {
	return(names(dataset)[i])
}

i.by.name <- function(dataset,name) {
	return(which(names(dataset) == name))
}

data.by.name <- function(dataset,col.name) {
	i = which(names(dataset) == col.name)
	return(dataset[,i])
}

n.by.factor <- function(dataset,col.name,factor) {
	i = which(names(dataset) == col.name)
	return(length(which(dataset[,i] == factor)))
}

onlyNumbers <- function(x) {
	x = gsub("(N|n)one","0", x)
	x = gsub("^-$", NA, x)
	teststring = gsub("NA|\\d+", "", paste(x, collapse = ""))
	if (teststring == "") {return(TRUE)} else {return(FALSE)}
}

factorize <- function(df, max.factors = 10, debug = FALSE) {
	for (i in 1:ncol(df)) {
		if (class(df[,i]) == "character") {
			if (debug) {cat(names(df)[i])}
			if(length(unique(df[,i])) <= max.factors) {
				df[,i] <- as.factor(df[,i])
			}
			if (debug) {cat("\n")}
		}
	}
	return(df)
}

numerize <- function(data) {
	for (c in 1:ncol(data)) {
		if (onlyNumbers(data[,c])) {
			data[,c] <- as.numeric(as.character(data[,c]))
		}
	}
	
	return(data)
}

# Functions to create intercorrelation networks
################################################################################################

cor.net <- function(dataset, included.vars = NULL, excluded.vars = NULL, focus = NULL, full = TRUE, sig = 0.05) {
	# Set up a data frame to hold the results
	results <- data.frame(var1=character(0),var2=character(0),estimate=numeric(0),p.value=numeric(0), stringsAsFactors = FALSE)
	
	# Subtract excluded vars (given as arguments) from analysis
	if (is.null(included.vars)) {
		cor.variables = setdiff(names.num(dataset),excluded.vars)
	}
	else
	{
		cor.variables = included.vars
	}
	
	# If given, make one variable the focus against which all other vars are correlated
	if (is.null(focus)) {cor.target = cor.variables} else {cor.target = focus}
	
	i = 1
	for (var1 in cor.variables) {
		for (var2 in cor.target) {
			if (var1 == var2) {
				# Vars are the same, no need to do anything
			}
			else
			{
				cat(var1," - ",var2,"\n")
				r = try(cor.test(data.by.name(dataset, var1),data.by.name(dataset, var2)), silent = T)
				
				if (class(r) == "try-error" || is.na(r)) {next} else {
					if (r$p.value <= sig) {
						results[i,] <- c(var1,var2,round(r$estimate, digits=2),round(r$p.value, digits=3))
						i = i + 1
					}
				}
			}
		}
	}
	
	# Do you also wish to calculate intercorrelations among the vars that correlated with the focus variable?
	if (is.null(focus) != TRUE & full == TRUE) {
		cat("Calculating the rest...")
		i = nrow(results) + 1
		for (var1 in unique(results$var1)) {
			for (var2 in unique(results$var1)) {
				if (var1 == var2) {
					# Vars are the same, no need to do anything
				}
				else
				{
					cat(var1," - ",var2,"\n")
					r = try(cor.test(data.by.name(dataset, var1),data.by.name(dataset, var2)), silent = T)
					
					if (class(r) == "try-error" || is.na(r)) {next} else {
						if (r$p.value <= sig) {
							results[i,] <- c(var1,var2,round(r$estimate, digits=2),round(r$p.value, digits=3))
							i = i + 1
						}
					}
				}
			}
		}
	}
	return(results)
}

view <- function(var, data = cor.df) {
	r = data[data$var1 == var,]
	pos = r[r$estimate > 0,]
	neg = r[r$estimate < 0,]
	
	pos = pos[order(pos$estimate, decreasing = T),]
	neg = neg[order(neg$estimate),]
	return(rbind(pos,neg))
}

# Functions to visualize intercorrelation networks
################################################################################################

make.net <- function(cor.df) {
	net = graph.data.frame(cor.df, directed = FALSE)
	E(net)$color <- "black"
	E(net)[estimate < 0]$color <- "red"
	E(net)[estimate > 0]$color <- "green"
	return(net)
}

corPlot <- function(net,title="") {
	plot(net,
	vertex.label=V(net)$name,
	layout = layout.kamada.kawai,
	edge.width = scale(abs(as.numeric(E(net)$estimate)))+2,
	edge.label = round(as.numeric(E(net)$estimate),digits=2),
	edge.label.family="Helvetica",
	edge.label.cex=0.6,vertex.size=5,
	edge.label.color="black",
	vertex.shape="none",
	vertex.label.family="Helvetica",
	vertex.label.cex=0.75,
	vertex.size=5,
	main=title
	)
}

networkColors <- function(net) {

}

# Functions to do analyses on large number of variables
################################################################################################

kitchenSinkRegression <- function(target.var, as = "dependent", add.formula = "", exclude = NULL, var.threshold = 0, sig.threshold = 0.05, data = NULL, debug = F) {
	if(is.null(data)) {stop("You must provide a data")}
	exclude = c(exclude, strsplit(add.formula,"(\\+|\\*)")[[1]])
	numvars = setdiff(names.num(data), union(exclude, target.var))
	results = data.frame(var1 = NA, R2 = NA, p = NA, sig.terms = NA, formula = NA)
	row = 1
	for (var in numvars) {
		if (debug) print(var)
		if (nrow(na.omit(data[var])) == 0) {next}
		if (debug) {cat(var, "\n")}
		if (as != "dependent" & as != "predictor") {stop("You must specify mode as either 'dependent' or 'predictor'.")}
		if (as == "dependent") {
			lm.str <- sprintf("fit <- lm(%s ~ %s%s, data = %s)", target.var, var, add.formula, as.character(substitute(data)))
		}
		if (as == "predictor") {
			lm.str <- sprintf("fit <- lm(%s ~ %s%s, data = %s)", var, target.var, add.formula, as.character(substitute(data)))
		}
		eval(parse(text = toString(lm.str)))
		r <- summary(fit)
		r.p_value <- pf(r$fstatistic[1], r$fstatistic[2], r$fstatistic[3],lower.tail=FALSE)
		#print(paste("foo",var,r.p_value))
		if (r.p_value < sig.threshold) {
			coeffs <- r$coefficients[2:nrow(r$coefficients),4]
			if (length(coeffs[coeffs < sig.threshold]) >= var.threshold) {
				#print(r)
				results[row,] <- c(var, round(r$r.squared, digits = 3), round(r.p_value[[1]], digits = 3), length(which(r$coefficients[2:length(r$coefficients[,4]),4] < 0.05)), sprintf("summary(%s(%s, data = %s))", as.character(r$call[1]), as.character(r$call[2]), as.character(r$call[3])))
				row = row + 1
			}
		}			
	}
	return(results[order(results$R2),])
}

kitchenSinkAnova <- function(target.var, add.formula = "", dataset, sig = 0.05, ...) {
	numvars = names.num(dataset)
	results = data.frame(vars = NA, p = NA)
	row = 1
	for (var in numvars) {
		try(if (debug) {print(var)}, silent = T)
		aov.str <- sprintf("fit <- aov(%s ~ %s%s, data = dataset)", var, target.var, add.formula)
		eval(parse(text = toString(aov.str)))
		r <- summary(fit)
		r.p_value <- r[[1]][ "Pr(>F)" ][[1]][1]

		if (r.p_value <= sig) {
			print("-------------------------------------------------")
			print(fit$call)
			print(r)
			results[row,] = c(as.character(fit$call[[2]][2]),round(r.p_value, digits = 3))
			row = row + 1
		}			
	}
	return(results)
}

mediators <- function(dv, ivs = NULL, exclude = c(), correlations = cor.df, z = 1.96, d) {
	library(multilevel)
	results = list()
	ex = c(exclude, dv)
	if (is.null(ivs)) {ivs = setdiff(correlations[correlations$var2 == dv, ]$var1, ex)}

	for (iv in ivs) {
		potential.ms = setdiff(correlations[correlations$var1 == iv, ]$var2, ex)
		ms = intersect(ivs, potential.ms)
		for (m in ms) {
			s = sobel(d[iv][[1]],d[m][[1]],d[dv][[1]])
			if (abs(s$z.value) >= z) {
				print("--------------------------------")
				print(paste("IV:",iv,"M:",m,"DV:",dv))
				s$pred = iv; s$med = m ;s$dv = s$dv
				results = c(results, s)
			}
		}
	}
	return(results)
}

make.df <- function(vars, dataset) {
	results = data.frame(data.by.name(dataset, vars[1]))
	
	if (length(vars) > 1) {
		for (var in vars[2:length(vars)]) {
			results = cbind(results, data.by.name(dataset, var))
		}
	}
	names(results) <- vars
	return(results)
}

# Functions to help in data visualization
################################################################################################

panel.cor <- function(x, y, digits=2, prefix="r = ", cex.cor)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
    
    test <- cor.test(x,y)
    # borrowed from printCoefmat
    Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                  symbols = c("***", "**", "*", ".", " "))
    
    text(0.5, 0.5, txt, cex = cex * 0.5)
    text(.8, .55, Signif, cex = cex * 0.4)
}

# Functions for working with LaTeX
################################################################################################

cleanSnw <- function(filename) {
	text = readLines(filename, warn = F)
	
	r.code = c()
	within.block = FALSE
	for (line in text) {
		if (grepl("^(\\s+|)<<", line)) {
			within.block = TRUE
			next
		}
		
		if (grepl("^@$", line)) {
			within.block = FALSE
			next
		}
		
		if (within.block) {
			r.code = c(r.code, line)
		}
	}
	return(r.code)
}

SnwSource <- function(filename) {
	text = cleanSnw(filename)
	writeLines(text, "/tmp/temp.R")
	source("/tmp/temp.R")
	unlink("/tmp/temp.R")
}