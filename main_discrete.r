unif_prior = function(len) {
  return( (integer(len) + 1) / len )
}
# partition unit interval into n-1 equal subintervals
unit_part = function(n) {
  return(0:n / n)
}

hypo_postr = function(hypo_prior, likelihood) {
  # hypos = length(hypo_prior)
  postrodds = hypo_prior * likelihood
  
  # normalize the odds
  return( postrodds / sum(postrodds) )
}

postrdf = function(priordf, likelihood) {
  return( data.frame(
    params = priordf$params,
    probs = hypo_postr(
      priordf$probs,
      likelihood
      )
    ) )
}

binomupdate = function(priordf, n, k) {
  likelihood = dbinom(x=k, size=n, prob=priordf$params)
  return( postrdf(priordf, likelihood) )
}

# example
# df = data.frame( params = unit_part(100), probs = unif_prior(101) )
# df2 = binomupdate(df, 50, 35)
