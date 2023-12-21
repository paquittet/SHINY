
# Fonction ressource K ----------------------------------------------------
k_f <- function(x, x0, k0, lambda) {
  
  k <- k0 - lambda*(x - x0)^2
  
  return(k)
  
}



# Fonction compétition a --------------------------------------------------
a_f <- function(x1, x2, sigma) {
  
  a <- exp(-(1/2) * ((x1 - x2)^2)/ sigma) 
  
  return(a)
  
}



# Fitness d'invasion ------------------------------------------------------
fitness_f <- function(x){
  x1 <- x[1]
  x2 <- x[2]
  r = 1
  s <- r*(1 - (a_f(x1, x2) * (k_f(x1) / k_f(x2))))
  return(s)
}


# Fonction contourplot ----------------------------------------------------
filled.contour.modif <-
  function (x = seq(0, 1, length.out = nrow(z)),
            y = seq(0, 1, length.out = ncol(z)),
            z,
            xlim = range(x, finite = TRUE),
            ylim = range(y, finite = TRUE),
            zlim = range(z, finite = TRUE),
            levels = pretty(zlim, nlevels),
            nlevels = 20,
            col = NULL,
            plot.title,
            plot.axes,
            key.title,
            key.axes,
            asp = NA,
            xaxs = "i",
            yaxs = "i",
            las = 1,
            axes = TRUE,
            frame.plot = axes,
            ...)

  {
    if (missing(z)) {
      if (!missing(x)) {
        if (is.list(x)) {
          z <- x$z
          y <- x$y
          x <- x$x
        }
        else {
          z <- x
          x <- seq.int(0, 1, length.out = nrow(z))
        }
      }
      else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
      y <- x$y
      x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
      stop("increasing 'x' and 'y' values expected")
    plot.new()
    plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i", 
                yaxs = "i")
    if (missing(key.axes)) {
      if (axes) 
        axis(4)
    }
    else key.axes
    if (!missing(key.title)) 
      key.title
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
    .filled.contour(x, y, z, levels, col)
    if (missing(plot.axes)) {
      if (axes) {
        title(main = "", xlab = "", ylab = "")
        Axis(x, side = 1)
        Axis(y, side = 2)
      }
    }
    else plot.axes
    if (frame.plot) 
      box()
    if (missing(plot.title)) 
      title(...)
    else plot.title
    invisible()
  }



# MATRICE M ---------------------------------------------------------------
M_matrix <- function(N = N, xmin = xmin, xmax = xmax, sigma = sigma, k0 = k0, lambda = lambda){
  # DESCRIPTION 
  #' @param N le nombre de phénotypes en compétition
  #' @param xmin valeur minimum du phénotype
  #' @param xmax valeur maximum du phénotype
  #' @param x la valeur des traits pour chacun de ces phénotypes
  #' @param sigma argument sigma de la fonction d'intensité de coefficient a(xi, xj) 
  #' @param k0  valeur optimale d'utilisation de ressource de la fonction K(xN)
  #' @param lambda forme de la courbe d'utilisation de ressource de la fonction K(xN)
  #' 
  #' @return La matrice M contenant les valeurs de a(xi, xj) / a(xi) [voir ci-dessus]
  
  
  # FONCTION
  x = seq(xmin, xmax, length.out = N)  # On répartit uniformément N fois les valeurs de trait entre xmin et xmax
  x0 = 1 # On fixe x0 à 1.
  
  # Matrice des traits D
  X = matrix(data = x, nrow = N, ncol = N, byrow = T)  # matrice des traits x1, ..., xN
  D = X - t(X)  # Matrice des xi - xj
  
  # Matrice de compétition A
  A = exp(-(D^2)/(2*(sigma)^2))  # Matrice des coefficients de compétition inter-traits
  
  # Matrice B contenant 1/KN
  K <- c()
  epsilon = 10^(-6)  # Pour ne pas avoir de 0 dans la matrice B
  for(trait in x){   # Valeur de K(xN) par trait
    K <- c(K, max( 0 + epsilon, k0 - lambda*((trait - x0))^2))
  } 
  
  B_trans <-  matrix(data = K, nrow = N, ncol = N, byrow = T)  # Matrice K(xN)
  B <-  apply(X = B_trans, MARGIN = 2, FUN = function(x) 1/x)  # 1 / Matrice K(xN)
  
  # Matrice M
  M = A * B
  
  return(M)
}



# FONCTION DE COMPETITION -------------------------------------------------
LV_trait <- function(t, n, params){
  # DESCRIPTION
  #' @param t le temps
  #' @param n la proportion initiale des phénotypes dans la population
  #' @param params liste des arguments : [M] la matrice M issue de M_matrix() et [r] le(s) taux de croissance des populations
  
  
  # FONCTION
  # Initialisaton
  r <- params[[1]]
  M <- params[[2]]
  
  # Fixe à 0 les N trop petits
  epsilon = 10^(-8)
  n[n<epsilon] <- 0
  
  # Sortie
  dn <-  r * n * (1 - n %*% M)
  
  return(list(dn))
}






# ARBRE -------------------------------------------------------------------
sortie_raster <-
  function(nT = nT, caption = caption) {
    # DESCRIPTION
    #' @param nT objet contenant la sortie de la fonction de mutation_function()
    #' @param caption titre de la caption du graphique
    
    
    
    # PREPARATION DES DONNES POUR LA VISUALISATION
    # On supprime les colonnes vides
    nT_gg <- as.data.frame(nT)
    col_0 <- which(colSums(nT_gg) == 0)
    if (length(col_0) != 0) {
      nT_gg <- nT_gg[-col_0]
    }
    
    # On convertit en proportion par scénario pour une meilleure lisibilité
    nT_gg <-
      as.data.frame(t(apply(
        nT_gg,
        MARGIN = 1,
        FUN = function(x)
          x / sum(x)
      )))
    
    # On renomme les colonnes
    col_nb <- ifelse(ncol(nT_gg) == 0, N, ncol(nT_gg))
    colnames(nT_gg) <- 1:col_nb
    
    # Ajout colonne "temps"
    nT_gg$temps <- 1:evol_time
    
    # On ajuste le format pour ggplot2
    nT_gg <-  nT_gg %>%
      pivot_longer(cols = 1:col_nb,
                   values_to = "dens",
                   names_to = "phenotype")
    
    # On binarise pour la représentation
    nT_gg$dens <- ifelse(nT_gg$dens > seuil, 1, 0)
    
    # Ajout colonne trait associé au phénotype
    nT_gg$trait <- rep(X[1:col_nb], evol_time)
    
    
    
    # RASTER PLOT
    ggplot(data = nT_gg, mapping = aes(trait, temps,  z = dens)) +
      geom_contour(color = "black") +
      scale_fill_gradient2(low = "white" ,
                           high = "black") +
      theme_classic() +
      geom_vline(xintercept = x0,
                 col = "red",
                 lty = 2) +
      labs(caption = caption) +
      theme(
        line = element_blank(),
        plot.caption = element_text(hjust = 0.5, size = 14),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        legend.position = "none",
        text = element_text(size = 15)
      ) +
      xlim(c(0, 2)) +
      xlab("Valeur du trait") +
      ylab("Temps évolutif")
  }
