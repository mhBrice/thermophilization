temperature.trend <- function(climate_var, year_measured, plot_id) 
{
  n = length(unique(plot_id))
  res = matrix(NA,n,4)
  #rownames(res) = paste("Site", unique(plot_id), sep=".")
  colnames(res)= c("slope","Rsquare","adjRsquare","p-value")
  for(i in 1:n) {
    curr.plot <- unique(plot_id)[i]
    sample.year <- year_measured[plot_id == curr.plot]
    sample.clim <- climate_var[plot_id == curr.plot]
    re <- lm(sample.clim ~ sample.year)
    res[i,1] = summary(re)$coefficients[2,1]
    res[i,2] = summary(re)$r.squared
    res[i,3] = summary(re)$adj.r.squared
    f.vector = summary(re)$fstatistic
    res[i,4] = pf(f.vector[1],f.vector[2],f.vector[3], lower.tail=FALSE)
  }
  res <- cbind.data.frame(plot_id = unique(plot_id), res)
}

