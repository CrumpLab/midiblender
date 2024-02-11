#' Get Feature Probabilities
#'
#' This function calculates the probability of each feature as a proportion of total. It sums up all the values in the MIDI matrix, calculates the summary for each column(feature), and finally computes the probability of each feature by dividing the column summary by the total summary.
#'
#' @param midi_matrix A numeric note by time feature matrix.
#'
#' @return A numeric vector containing the probabilities of each feature. The length of the vector is equal to the number of features (columns) in the input MIDI matrix.
#'
#' @examples
#' \dontrun{
#' }
#' @export
get_feature_probs <- function(midi_matrix){
  total_sum <- sum(midi_matrix)
  feature_sum <- colSums(midi_matrix)
  feature_probs <- feature_sum/total_sum
  return(feature_probs)
}


#' Compute the mean note density of a midi matrix
#'
#' This function takes a midi matrix and returns the rounded mean of the sum of rows. Note that the round function is used to round to the nearest whole number.
#'
#' @param midi_matrix A numerical matrix where the of rows represents the MIDI notes, and the sum of each row represents the note density.
#'
#' @return A rounded integer that represents the mean note density.
#'
#' @export
get_mean_note_density <- function(midi_matrix){
  return(round(mean(rowSums(midi_matrix))))
}

#' Create New Features Based on Probabilities
#'
#' Input a sequence of probabilities and get back a new sequence based on a binomial distribution and these probabilities. Any resulting value greater than 1 in the new sequence will be set to 1.
#'
#' @param probs Numeric vector, a sequence of probabilities between 0 and 1 used to generate the new sequence.
#' @param density Integer, the number of trials (default is 6).
#' @param exmaples Inger, the number of examples to generate (default is 1)
#'
#' @return If example is 1, a sequence same length as probs but with values following a binomial distribution based on the inputted probabilities, with any values greater than 1 set to 1. If example is > 1, a matrix of sequences, with the number of columns the same length as the probabilities, and sequences corresponding to rows.
#'
#' @export
new_features_from_probs <- function(probs, density = 6, examples = 1) {

  if(examples == 1){
  # generate new vector based on probabilities
  new_sequence <- stats::rbinom(n = length(probs),
                         size = density,
                         prob = probs)

  # ensure any element greater than 1 is set to 1
  new_sequence[new_sequence > 1] <- 1

  return(new_sequence)

  }

 if(examples > 1){

  new_sequences <- t(replicate(examples,stats::rbinom(n = length(probs),
                                           size = density,
                                           prob = probs)))

  new_sequences[new_sequences > 1] <-1
  return(new_sequences)

 }

}

#' Convert feature vector to matrix
#'
#' @description
#' This function takes in a feature vector and the number of notes, and returns a matrix. Each note is a row of the matrix.
#'
#' @param vec A vector of features. The length of the vector must be divisible by the number of notes.
#' @param num_notes An integer specifying the number of notes. This would be the number of rows in the returned matrix.
#'
#' @return
#' A matrix with num_notes rows and length(vec)/num_notes columns.
#'
#' @export
feature_vector_to_matrix <- function(vec,num_notes){
  if(is.vector(vec)){
    return(
      matrix(vec,
           nrow=num_notes,
           ncol=length(vec)/num_notes,
           byrow = F)
    )
  }

  if(is.matrix(vec)){
    return(
      #make assumptions and turn it into a vector
      matrix(c(t(vec)),
             nrow=num_notes,
             ncol=length(vec)/num_notes,
             byrow = F)
    )
  }


}

#' Bresenham's algorithm for Euclidean rhythm generation
#'
#' The algorithm evenly distributes a specified number of beats in a total specified number of steps, returning a binary vector where 1s indicate the occurrence of a beat.
#'
#' @param beats Integer, number of beats to distribute.
#' @param steps Integer, total number of steps across which the beats are to be distributed.
#' @param start Integer, the starting value in sequence. Default is 1.
#'
#' @return A numerical vector of size `steps` containing 1s where beats are present and 0s elsewhere.
#'
#' @details
#' Converted to R from discussions of Euclidean Algorithms in this post <https://medium.com/code-music-noise/euclidean-rhythms-391d879494df>. See also Toussaint, Godfried. 2005. “The Euclidean Algorithm Generates Traditional Musical Rhythms.” In, 4756. <https://archive.bridgesmathart.org/2005/bridges2005-47.html>
#'
#' @export
#'
#' @examples
#' \dontrun{
#' bresenham_euclidean(5, 16)
#' bresenham_euclidean(7, 20, 2)
#' }

bresenham_euclidean <- function(beats, steps, start = 1) {
  previous <- start
  pattern <- c()

  for (i in 0:(steps-1)) {
    xVal <- floor((beats / steps) * (i))
    pattern <- c(pattern, ifelse(xVal == previous, 0, 1))
    previous <- xVal
  }

  return(pattern)
}
