inverse = function(x){
  if(is.vector(x)){
    result = c(1)
    for(i in 1:length(x)){
      if(x[i] == 0){
        result[i] = 0
      }
      else{
        result[i] = 1/(x[i])
      }
    }
    return (result)
  }
}
centrer_reduire = function(x, d){
  
  g = cbind(t(x)%*%d%*%cbind(rep(1, length=nrow(x))))
  y = round(x - cbind(rep(1, length=nrow(x))) %*% t(g), 3) # Centrer
  
  print("Centrée")
  print(y)

  v = round(t(y)%*%d%*%y, 3)
  print("v")
  print(v)

  d_1_sur_s = diag(inverse(sqrt(diag(v))))
  
  z = y%*%d_1_sur_s

  return (list(z=z, v=v))
}
acp = function(x, d){
  # Si les vérifications suivantes ne sont pas toutes respectées, on sort du programme:
  if(TRUE){
    if(nrow(d) == ncol(d) & sum(diag(d)) == 1 & identical(diag(diag(d)), d)){ # 2) d est un matrice diagonale dont la somme des poids donne 1
      m = matrix(data = c(1), nrow = ncol(x), ncol = ncol(x)) # 1)
      variance = matrix(data = c(1) , ncol(x), ncol(x))
      if (!(identical(apply(x, 1, sum), rep(0, length=nrow(x))))){ # Teste si x n'est pas centré
        list = centrer_reduire(x, d)
        z = list$z
        v = list$v
        print("Matrice centreé et réduite")
        print(z)
        print("Matrice des variances_covariances")
        print(v)
        print("Matrice des métriques")
        m = diag(rep(1, ncol(x))) # La matrice identité
        print(m)
      }
      else{
        print("Matrice des variances_covariances")
        variance = t(x)%*%d%*%x # matrice des variance-covariance
        print("Matrice des métriques")
        m = diag(inverse(diag(variance))) # m recoit D1/s au carré
        print(m)
      }
      propres = eigen(variance%*%m)
      nb = length(propres$values[propres$values > 0.001]) # Le nombre de valeurs propres positives
      print("Le valeurs propres positives non nulles")
      values = propres$values[c(1:nb)]
      print(values)
      print("Les vecteurs")
      vectors = propres$vectors[,c(1:nb)]
      print(vectors)
      print("Les composantes princiales")
      c = x%*%m%*%vectors
      print(c)
      #y_approx = 
      # Ici, on crée la carte, qui peut etre un sev de dim 1 ou 2.
      carte = c[1]
      if(is.vector(c) == TRUE){
        # On fait la meme chose que hors du if sauf qu'ici, on ne dessine pas de cercle de corrélation
      }
      carte = c[c(1,2)]
      # Calculer les contributions des individus aux axes
      cdp = apply(d, 1, sum)*apply(c*c, 1, sum)
      print("Les contributions sur le premier axe")
      contrib1 = (1/values[1])%*%cdp
      print(contrib1)
      print("Les contributions sur le deuxième axe")
      contrib2 = (1/values[2])%*%cdp
      print(contrib2)
      # Calcuer les qualités de représentation des individus
      qr_axe_1 = cbind(c[,c(1)]*c[,c(1)]) * diag(inverse(apply(c*c, 1, sum)))
      qr_axe_2 = cbind(apply(c[,c(1, 2)]*c[,c(1, 2)], 1, sum) * diag(inverse(apply(c*c, 1, sum))))
      print("Les qualités de représentation sur le premier axe")
      print(qr_axe_1)
      print("Les qualités de représentation sur le deuxième axe")
      print(qr_axe_2)
      # Dessiner le cercle de corrélation
      # Dessiner la boite à moustache.
      #draw.circle(qr_axe_1, qr_axe_2,radius,nv=100,border=NULL,col=NA,lty=1,density=NULL,
         #         angle=45,lwd=1)
    }
    else{
      print("La matrice d doit etre diagonale avec sa trace egale à 1.")
    }
  }
}

x = matrix(data = c(0,1,2,3,4,4,3,2,1,0,0,1,3,1,0), nrow = 5, ncol = 3)
print(x)
d = diag(rep(1/5, 5))

acp(x, d)