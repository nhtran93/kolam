extract_sequence <- function(untransformed_sequence, unique_count = TRUE, transitional_one = FALSE){
  sequence <- gsub("\\s+", ",", gsub("^\\s+|\\s+$", "", untransformed_sequence))
  sequence <- gsub("\\[", replacement = "", x = sequence)
  sequence <- gsub("\\\"", replacement = "", x = sequence)
  sequence <- gsub("\\]", replacement = ",1", x = sequence)
  sequence <- tolower(as.character(unlist(strsplit(x = sequence, split = ",", fixed = T))))
  sequence <- sequence[sequence != ""]
  sequence <- sequence[1:(length(sequence)-1)]
  sequence <- sequence[-which(sequence == "1")]

  if (!unique_count) {
    sequence <- gsub(pattern = "d1l", replacement = "d1b", x = sequence)
    sequence <- gsub(pattern = "d1r", replacement = "d1b", x = sequence)
    sequence <- gsub(pattern = "d2l", replacement = "d2b", x = sequence)
    sequence <- gsub(pattern = "d2r", replacement = "d2b", x = sequence)
    sequence <- gsub(pattern = "d3l", replacement = "d3b", x = sequence)
    sequence <- gsub(pattern = "d3r", replacement = "d3b", x = sequence)
    sequence <- gsub(pattern = "d4l", replacement = "d4b", x = sequence)
    sequence <- gsub(pattern = "d4r", replacement = "d4b", x = sequence)

    if (transitional_one) {
      sequence <- gsub(pattern = "t1l", replacement = "t1", x = sequence)
      sequence <- gsub(pattern = "t1r", replacement = "t1", x = sequence)
      sequence <- gsub(pattern = "t2l", replacement = "t2", x = sequence)
      sequence <- gsub(pattern = "t2r", replacement = "t2", x = sequence)
      sequence <- gsub(pattern = "t3l", replacement = "t3", x = sequence)
      sequence <- gsub(pattern = "t3r", replacement = "t3", x = sequence)
      sequence <- gsub(pattern = "t4l", replacement = "t4", x = sequence)
      sequence <- gsub(pattern = "t4r", replacement = "t4", x = sequence)
    }
    else{
      sequence <- gsub(pattern = "t1l", replacement = "t1b", x = sequence)
      sequence <- gsub(pattern = "t1r", replacement = "t1b", x = sequence)
      sequence <- gsub(pattern = "t2l", replacement = "t2b", x = sequence)
      sequence <- gsub(pattern = "t2r", replacement = "t2b", x = sequence)
      sequence <- gsub(pattern = "t3l", replacement = "t3b", x = sequence)
      sequence <- gsub(pattern = "t3r", replacement = "t3b", x = sequence)
      sequence <- gsub(pattern = "t4l", replacement = "t4b", x = sequence)
      sequence <- gsub(pattern = "t4r", replacement = "t4b", x = sequence)
    }
  }

  return(sequence)
}
