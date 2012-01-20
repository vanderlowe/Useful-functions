
p.value <- function(i, sig.stars = F, p.notation = T) {
	if (i < 0.001) {
		str = "0.001"
		prefix = "\\\\emph{p}~<~"
	} else {
		if (i < 0.01) {str = sprintf("%.3f", i)} else {str = sprintf("%.2f", i)}
		prefix = "\\\\emph{p}~=~"
	}
	
	if (p.notation) {
		str = paste(prefix, str, sep = "")
	}
	
	stars = ""
	if (sig.stars) {
		if (i <= 0.05) {stars = "*"}
		if (i <= 0.01) {stars = "**"}
		if (i <= 0.001) {stars = "***"}
	}
	
	str = gsub("0\\.",".", str)
	return(paste(str, stars, sep = ""))
}

report <- function(obj) {
	UseMethod("report")
}

report.aov <- function(obj) {
	theSummary = summary(obj)
	F = round(unlist(theSummary)["F value1"][[1]], 2)
	btw.df = unlist(theSummary)["Df1"][[1]]
	within.df = unlist(theSummary)["Df2"][[1]]
	p = unlist(theSummary)["Pr(>F)1"][[1]]
	p.value = p.value(p)
	if (p < 0.001) {p.value = "\\\\emph{p} < 0.001"}
	str = sprintf("\\\\emph{F}(%i,%i)~=~%.2f, %s", btw.df, within.df, F, p.value)
	return(str)
}

report.lm <- function(obj) {
	theSummary = summary(obj)
	r2 = round(theSummary$r.squared, digits = 2)
	fstat = theSummary$fstatistic
	f = fstat[1]
	df1 = fstat[2]
	df2 = fstat[3]
	p = pf(fstat[1], fstat[2], fstat[3], lower.tail=FALSE)
	p.value = p.value(p)
	if (p < 0.001) {p.value = "\\\\emph{p}~<~0.001"}
	str = sprintf("\\\\emph{$R^2$}~=~%.2f, \\\\emph{F}(%i,~%i)~=~%.2f, %s", r2, df1, df2, f, p.value)
	return(str)
}

report.htest <- function(obj) {
	r = round(obj$estimate, digits = 2)
	n = as.numeric(unlist(obj)["parameter.df"][[1]]) + 2
	p = p.value(obj$p.value)
	str = sprintf("\\\\emph{r}~=~%.2f, \\\\emph{n}~=~%i, %s", r, n, p)
	return(str)
}

report.t <- function(obj) {
	df = obj$parameter
	str = sprintf("\\\\emph{t}(%i)~=~%.2f,", df, obj$statistic)
	return(paste(str,p.value(obj$p.value)))
}

predictor <- function(model, i) {
	s = summary(model)
	predictors = coef(s)
	thisPredictor = predictors[i+1,]
	class(thisPredictor) <- "predictor"
	return(thisPredictor)
}

report.predictor <- function(predictor) {
	b = round(predictor[[1]], digits = 2)
	p = round(predictor[[4]], digits = 2)
	p.value = p.value(p)

	str = sprintf("$\\\\beta$~=~%.2f, %s", b, p.value)
	return(str)
}