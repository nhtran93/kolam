extract_geometric_switches <- function(sequence, sequential = FALSE){
  transitional_gestures <- c("t1", "t2", "t3", "t4", "t1r", "t1l", "t2r", "t2l", "t3r", "t3l", "t4r", "t4l")
  idx <- which(sequence %in% transitional_gestures)
  
  if (sequential) {
    # Switches are counted for every not sequential transitional gesture. Each counted transitional gesture as 2 space switches (one before and one after).
    # But because I am using the diffence to measure whether transitional gestures are sequentially occuring, it is importance to add a 1 for the gesture.
    n_switches <- (length(which(diff(idx) > 1)) + 1)*2
  }
  else{
    n_switches <- length(idx)
  }
  
  return(n_switches)
}
