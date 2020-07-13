compare_identical <- function(targetKolam, referenceKolam){
  cp1 <- extractControlpoints(targetKolam)
  cp2 <- extractControlpoints(referenceKolam)


  out <- c()

  # are the number of loops identical between both kolams?
  if (length(cp1) == length(cp2) & targetKolam$nLoops == referenceKolam$nLoops) {
    out <- c(out, TRUE)

    # are the number of gestures identical for each loop between both kolams
    if (lengths(cp1) == lengths(cp2)) {
      out <- c(out, TRUE)
    }

    # The order of the gestures does not matter. But for identity the gestures must appear
    for (i in 1:length(cp1)) {
      out <- c(out, all(cp1[[i]] %in% cp2[[i]]))
    }

    # The angles can only differ between specific values, otherwise the kolam is not identical, but chiral.
    if (!targetKolam$initialization$x_init %% 1 == 0.5 & targetKolam$initialization$y_init %% 1 == 0.5) {
        if (targetKolam$initialization$heading_init == referenceKolam$initialization$heading_init) {
          out <- c(out, TRUE)
        }
        else{
          if (targetKolam$initialization$heading_init == 45) {
            out <- c(out, referenceKolam$initialization$heading_init == 225)
          }
          else{
            out <- c(out, referenceKolam$initialization$heading_init == 135)
          }
        }
    }
  }

  return(all(out))
}


compare_chiral <- function(targetKolam, referenceKolam){
  cp1 <- extractControlpoints(targetKolam)
  cp2 <- extractControlpoints(referenceKolam)


  out <- c()

  # are the number of loops identical between both kolams?
  if (length(cp1) == length(cp2) & targetKolam$nLoops == referenceKolam$nLoops) {
    out <- c(out, TRUE)

    # are the number of gestures identical for each loop between both kolams
    if (lengths(cp1) == lengths(cp2)) {
      out <- c(out, TRUE)
    }

    # The order of the gestures does not matter. But for identity the gestures must appear
    for (i in 1:length(cp1)) {
      out <- c(out, all(cp1[[i]] %in% cp2[[i]]))
    }

    # The angles can only differ between specific values for orthogonal gestures, otherwise the kolam is not identical, but chiral.
    if (!targetKolam$initialization$x_init %% 1 == 0.5 & targetKolam$initialization$y_init %% 1 == 0.5) {
        if (targetKolam$initialization$heading_init == 135| targetKolam$initialization$heading_init == 315) {
          out <- c(out, referenceKolam$initialization$heading_init == 225 | referenceKolam$initialization$heading_init == 45)
        }

        if (targetKolam$initialization$heading_init == 45| targetKolam$initialization$heading_init == 225) {
          out <- c(out,referenceKolam$initialization$heading_init == 135 | referenceKolam$initialization$heading_init == 315)
        }
    }

  }

  return(all(out))
}


compare_rotate  <- function(targetKolam, referenceKolam){
  cp1 <- extractControlpoints(targetKolam)
  cp2 <- extractControlpoints(referenceKolam)

  out <- c()
}
