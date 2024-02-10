#' Mangle Positive Timing Function
#'
#' This function adds random jitter to a time column in a midi_df and ensures all times are positive.
#' It samples jitter values from a uniform distribution, and then adds these to the original
#' time series. Any negative times resulting from the jitter are set to 0.
#'
#' This function can be used with `dplyr` to modify a `midi_df`.
#'
#' @param x A numeric vector representing time points.
#' @param ticks_amount The range of the uniform distribution from which to sample jitter. Default is 5.
#'
#' @return A numeric vector with the same length as x, representing the 'mangled' time series.
#'
#' @details
#' The result is similar to "humanizing" the timing. Depending on the amount of jitter, small random ticks are added to all timing messages. This function does not jitter any simulatenous messages, so chords do not become spread out as triads.
#'
#' @examples
#' \dontrun{
#' mario <- midi_to_object("all_overworld.mid") #import midi
#' list2env(mario, .GlobalEnv) #send object to global environment
#'
#' midi_df %>%
#' mutate(time = case_when(
#'   meta == FALSE ~ mangle_positive_timing(time, ticks_amount = 5),
#'   meta == TRUE ~ time)
#'   ) -> copy_df
#'
#' # update miditapyr df
#' miditapyr_object$midi_frame_unnested$update_unnested_mf(copy_df)
#'
#' # write midi file to disk
#' miditapyr_object$write_file("jitter_mario.mid")
#' }
#' @export
#'
mangle_positive_timing <- function(x, ticks_amount = 5) {

  # sample random jitter values from uniform distribution
  # range set by ticks_amount
  jitter_vector <- round(runif(length(x),
                               min = -1 * ticks_amount,
                               max = ticks_amount))
  # add time vectors
  merged_time <- x + jitter_vector

  # eliminate negative time
  merged_time[merged_time < 0] <- 0
  return(merged_time)

}
