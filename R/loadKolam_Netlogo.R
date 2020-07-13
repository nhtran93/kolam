loadKolam <- function(data){
  nLoops <- data$loops
  # The matrix size is the value encoded in Netlogo and used to translate the history output from Netlogo into a coordinate system (the system we are using here in R). The calculation goes as follows: data$history/data _matrix_size = coordinate values. The coordinate values then need to be rounded to the next 0.5 decimal value.
  nMatrix <- data$matrix_size
  hash <- ifelse(test = length(data$png_hash) == 0,
                 yes = data$kolam_png_hash, data$png_hash)
  pulli <- ifelse(length(data$pulli) == 0, data$size, data$pulli)
  codes <- data$kolam_codes
  notes <- data$notes

  if (nLoops == 0 | is.character(nLoops)) {
    stop("There is an error in the data. There are no loops to draw.")
  }

  if (nLoops == 1) {
    x <- stringr::str_extract(pattern = regex("(?<=Oldx)(\\s|\\d|\\.)+"), string = data$history)
    x <- gsub("\\s+", ",", gsub("^\\s+|\\s+$", "", x))
    x <- round_any(as.numeric(unlist(strsplit(x = x, split = ",", fixed = T)))/data$matrix_size, 0.5)

    y <- stringr::str_extract(pattern = regex("(?<=Oldy)(\\s|\\d|\\.)+"), string = data$history)
    y <- gsub("\\s+", ",", gsub("^\\s+|\\s+$", "", y))
    y <- round_any(as.numeric(unlist(strsplit(x = y, split = ",", fixed = T)))/data$matrix_size, 0.5)

    h <- stringr::str_extract(pattern = regex("(?<=Oldh)(\\s|\\d|\\.)+"), string = data$history)
    h <- gsub("\\s+", ",", gsub("^\\s+|\\s+$", "", h))
    h <- as.numeric(unlist(strsplit(x = h, split = ",", fixed = T)))

    df <- data.frame(cbind(x, y, h))

    if(df[1, "x"] == 0 | df[1, "y"] == 0) {
      df <- df[2:nrow(df), ]
    }

    initialization <- data.frame(x_init = df[1, "x"], y_init = df[1, "y"], heading_init = df[1, "h"], loop = 1)

    sequence <- gsub("\\[", replacement = "", x = data$sequence)
    sequence <- gsub("\\]", replacement = "", x = sequence)
    sequence <- gsub("\\\"", replacement = "", x = sequence)
    sequence <- gsub("\\s", replacement = ",", x = sequence)
    sequence <- as.character(unlist(strsplit(x = sequence, split = ",", fixed = T)))
    sequence <- tolower(sequence[sequence != ""])
    sequences <- list(loop1 = sequence)
  return(list(sequences = sequences, df = df, nLoops = nLoops, nMatrix = nMatrix, hash = hash, pulli = pulli, initialization = initialization, kolam_codes = codes, notes = notes))
  }
  else{

    # X, Y coordinates & heading
    x <- stringr::str_extract(pattern = regex("(?<=Oldx)(\\s|\\d|\\.)+"), string = data$history)
    x <- gsub("\\s+", ",", gsub("^\\s+|\\s+$", "", x))
    x <- round_any(as.numeric(unlist(strsplit(x = x, split = ",", fixed = T)))/data$matrix_size, 0.5)

    y <- stringr::str_extract(pattern = regex("(?<=Oldy)(\\s|\\d|\\.)+"), string = data$history)
    y <- gsub("\\s+", ",", gsub("^\\s+|\\s+$", "", y))
    y <- round_any(as.numeric(unlist(strsplit(x = y, split = ",", fixed = T)))/data$matrix_size, 0.5)

    h <- stringr::str_extract(pattern = regex("(?<=Oldh)(\\s|\\d|\\.)+"), string = data$history)
    h <- gsub("\\s+", ",", gsub("^\\s+|\\s+$", "", h))
    h <-as.numeric(unlist(strsplit(x = h, split = ",", fixed = T)))

    df <- data.frame(cbind(x, y, h))

    if(df[1, "x"] == 0 | df[1, "y"] == 0) {
      df <- df[2:nrow(df), ]
    }

    # Sequence
    sequence <- gsub("\\s+", ",", gsub("^\\s+|\\s+$", "", data$sequence))
    sequence <- gsub("\\[", replacement = "", x = sequence)
    sequence <- gsub("\\\"", replacement = "", x = sequence)
    sequence <- gsub("\\]", replacement = ",1", x = sequence)
    sequence <- tolower(as.character(unlist(strsplit(x = sequence, split = ",", fixed = T))))
    sequence <- sequence[sequence != ""]
    sequence <- sequence[1:(length(sequence)-1)]

    if (length(which(sequence == 1)) != nLoops) {
      stop("Something is going on wrong in the sequences.")
    }

    sequences <- list()
    index <- c()
    for (i in 1:nLoops){
      s <- sequence[1:which(sequence == 1)[1]-1]
      sequence <- sequence[-c(1:which(sequence == 1)[1])]
      sequences[[paste0("loop", i, sep = "")]] <- s
      index <- append(index, rep(i, length(s)))
    }

    df$loop <- index

    initialization <- data.frame(df[!duplicated(df$loop), ])
    rownames(initialization) <- NULL
    colnames(initialization) <- c("x_init", "y_init", "heading_init", "loop")

    return(list(sequences = sequences, df = df, nLoops = nLoops, nMatrix = nMatrix, pulli = pulli, hash = hash, initialization = initialization, kolam_codes = codes, notes = notes))
  }
}
