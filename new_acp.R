centrer_reduire = function(y, d) {
  d = d
  y = y
  v = round(t(y) %*% d %*% y, 3)
  d_1_sur_s = round(cbind(solve(diag(sqrt(
    diag(v)
  )))), 3)
  g = t(t(y) %*% d %*% cbind(rep(1, length = nrow(y))))
  #print("centre")
  #print(g)
  #print("transposée de l'indicatrice fois g")
  print(cbind(rep(1, length = nrow(y))) %*% g)
  y = round(y - (cbind(rep(1, length = nrow(
    y
  ))) %*% g), 3) # Centrer
  #print("Centrée")
  print(y)
  y = y %*% d_1_sur_s
  print("reduite")
  return (y)
}
composantes_principales = function(y, m, u) {
  c = y %*% m %*% u
  return (c)
  
}
contributions = function(d, c, values) {
  cdp = apply(d, 1, sum) * apply(c * c, 1, sum)
  contrib1 = (1 / values[1]) %*% cdp
  contrib2 = (1 / values[2]) %*% cdp
  return (cbind(contrib1, contrib2))
}
qualites_de_representations = function(c) {
  qr_axe_1 = cbind(c[, c(1)] * c[, c(1)]) * diag(solve(diag(apply(c * c, 1, sum))))
  qr_axe_2 = cbind(apply(c[, c(1, 2)] * c[, c(1, 2)], 1, sum) * diag(solve(diag(apply(
    c * c, 1, sum
  )))))
}
ACP = function(x, d) {
  if (TRUE) {
    if (nrow(d) == ncol(d) &
        sum(diag(d)) == 1 & identical(diag(diag(d)), d)) {
      y = centrer_reduire(x, d)
      m = diag(rep(1, ncol(y)))
      variance = t(y) %*% d %*% y
      propres = eigen(variance %*% m)
      print("Variance covariance")
      print(variance)
      
      nb = length(propres$values[propres$values > 0])
      
      values = propres$values[c(1:nb)]
      vectors = propres$vectors[, c(1:nb)]
      print("valuees")
      print(values)
      print("vecteurs propres")
      print(vectors)
      c = composantes_principales(x, m, vectors)
      
      contrib = contributions(d, c, values)
      
      qualites = qualites_de_representations(c)
      
      result = list(
        "mainAxis" = c,
        "contributions" = contrib,
        "qualities" = qualites,
        "centre_reduite" = y,
        "valeurs_propres" = values,
        "vecteurs_propres" = vectors,
        "variance_covariance" = variance
      )
      return (result)
    }
  }
}


x = matrix(
  data = c(0, 1, 2, 3, 4, 4, 3, 2, 1, 0, 0, 1, 3, 1, 0),
  nrow = 5,
  ncol = 3
)
print(x)
d = diag(rep(1 / 5, 5))

result = ACP(x, d)
