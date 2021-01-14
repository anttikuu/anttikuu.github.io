confumatHelper <- function(confumat){
  halls <- rownames(confumat)
  Reference <- factor(rep(halls, times = c(sum(confumat[1,]),sum(confumat[2,]),sum(confumat[3,]),sum(confumat[4,]))), levels = halls)
  Response <- factor(
    c(
      rep(halls, times = c(confumat[1,1], confumat[1,2], confumat[1,3], confumat[1,4])),
      rep(halls, times = c(confumat[2,1], confumat[2,2], confumat[2,3], confumat[2,4])),
      rep(halls, times = c(confumat[3,1], confumat[3,2], confumat[3,3], confumat[3,4])),
      rep(halls, times = c(confumat[4,1], confumat[4,2], confumat[4,3], confumat[4,4]))),
    levels = halls
  )
  # OBS!!! this order of response and reference is needed in CARET confusion matrix function
  xtab <- table(Response, Reference)
  return(xtab)
}