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
#' @export
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

#' Transpose MIDI Data
#'
#' This function changes the note values in a MIDI data frame by a certain number of semitones.
#' It also ensures that the note values stay within the MIDI range (0-127) and mutes
#' any notes that fall outside this range.
#'
#' @param midi_df a midi dataframe, usually imported from `midi_to_object()`.
#' @param semitones Numeric. The number of semitones to transpose the `note` column. If the `meta` column is `FALSE`, the function will add or subtract this number from the original note value. Default is 0, which means no transposition will occur.
#'
#' @return A data frame with the same structure as `midi_df`, but with the `note` and
#'   `velocity` columns modified according to the transposition and MIDI range rules.
#'   Any notes that fall outside the 0-127 range will have their velocity set to 0 (muted),
#'   and their note value will be set to 127.
#'
#' @export
#' @examples
#' \dontrun{
#' # import midi
#' mario <- midi_to_object("all_overworld.mid")
#' list2env(mario, .GlobalEnv) #send objects to Global environment
#'
#' # apply transpose
#' copy_df <- mangle_transpose(midi_df, -10)
#'
#' # update miditapyr df
#' miditapyr_object$midi_frame_unnested$update_unnested_mf(copy_df)
#' #write midi file to disk
#' miditapyr_object$write_file("transpose_mario.mid")
#' }
mangle_transpose <- function(midi_df, semitones = 0){

  midi_df %>%
    # add or subtract semitones to note column
    dplyr::mutate(note = dplyr::case_when(
      meta == FALSE ~ note+semitones,
      meta == TRUE ~ note)) %>%
    # set velocity for out of range notes to 0
    dplyr::mutate(velocity = ifelse(note > 127 | note < 0, 0, velocity)) %>%
    # make sure notes fall in 0-127 range
    dplyr::mutate(note = ifelse(note > 127 | note < 0, 127, note)) -> copy_df

  return(copy_df)
}

#' Randomly offset individual notes by a vector of semitones
#'
#' This function randomly changes individual note values in a MIDI data frame by a certain number of semitones.
#' It also ensures that the note values stay within the MIDI range (0-127) and mutes
#' any notes that fall outside this range.
#'
#' @param midi_df a midi dataframe, usually imported from `midi_to_object()`.
#' @param semitones numeric vector. default is c(2,7). The function automatically appends negative values as well.
#' @param keep_sam integer, number of 0s to add in semitpones vector, when this value is selected a given note is not shifted
#'
#' @return A data frame with the same structure as `midi_df`, but with the `note` and
#'   `velocity` columns modified according to the randomly sampled note offsets.
#'   Any notes that fall outside the 0-127 range will have their velocity set to 0 (muted),
#'   and their note value will be set to 127.
#'
#' @export
#' @examples
#' \dontrun{
#' # import midi
#' mario <- midi_to_object("all_overworld.mid")
#' list2env(mario, .GlobalEnv)
#'
#' # make sure midi_df has a tempo
#' midi_df <- midiblender::set_midi_tempo_midi_df(midi_df)
#'
#' # apply random note offsets
#' copy_df <- mangle_note_wiggler(midi_df, c(4,7), keep_same = 2)
#'
#' # update miditapyr df
#' miditapyr_object$midi_frame_unnested$update_unnested_mf(copy_df)
#'
#' #write midi file to disk
#' miditapyr_object$write_file("note_wiggler_mario.mid")
#' }
mangle_note_wiggler <- function(midi_df, semitones = c(2,7), keep_same = 2){

  # this function needs the midi_df to have a tempo

  # get meta messages and split them
  meta_messages_df <- midiblender::get_midi_meta_df(midi_df)
  meta_messages_split <- midiblender::split_meta_df(meta_messages_df)

  # get note_on and note_off messages
  body_messages <- midi_df %>%
    filter(meta == FALSE,
           type %in% c('note_on','note_off'))

  # add pyramidi timing
  body_messages_extend <- pyramidi::tab_measures(body_messages,
                                                 ticks_per_beat,
                                                 columns_to_add = c("m", "b", "t", "time"))
  # pivot wider to isolate individual notes per row
  body_messages_wide <-
    pyramidi::pivot_wide_notes(body_messages_extend)

  # define vector of semitones to sample from for note wigggling
  sampling_vector <- c(semitones, -1 * semitones, rep(0, keep_same))

  # Main note wiggler loop
  # goes through each note and randomly wiggles it or not
  repeat_collector <- c()
  for(i in 1:dim(body_messages_wide)[1]){

    # do this on the first row
    if(i == 1){
      # sample replacer note
      new_note <- body_messages_wide$note[i]+ sample(sampling_vector,1)
      # replace note
      body_messages_wide$note[i] <- new_note
      # track repeats so there are no duplicate note_on messages in time
      repeat_collector <- new_note
    }

    # do this for remaining rows
    if(i > 1){
      # sample a replacer note
      new_note <- body_messages_wide$note[i]+ sample(sampling_vector,1)

      # if note_on time is not the same as previous
      # sample new note and replace
      if(body_messages_wide$ticks_note_on[i] != body_messages_wide$ticks_note_on[i-1]){
        body_messages_wide$note[i] <- new_note
        #reset collector
        repeat_collector <- new_note
      }

      # if note_on time is same as previous
      if(body_messages_wide$ticks_note_on[i] == body_messages_wide$ticks_note_on[i-1]){
        # only add sample note if it is different
        if(new_note %in% repeat_collector == FALSE){
          body_messages_wide$note[i] <- new_note
          # add note to collector
          repeat_collector <- c(repeat_collector,new_note)
        }
      }
    }

  }

  # this might not be strictly necessary
  body_messages_wide <- body_messages_wide %>%
    mutate(i_note = 1:dplyr::n())

  # had issues with pivot longer from pyramidi, had to roll my own, works for now
  body_messages_long <- body_messages_wide %>%
    dplyr::select(c("i_track", "channel", "note", "i_note","time_note_on","time_note_off","ticks_note_on","ticks_note_off","velocity_note_on","velocity_note_off")) %>%
    tidyr::pivot_longer(c("ticks_note_on","ticks_note_off"),names_to = "type", values_to = "total_time") %>%
    dplyr::arrange(total_time) %>%
    mutate(time = total_time - lag(total_time, default = 0),
           velocity = velocity_note_on) %>%
    # set velocity to zero for notes outside of range
    dplyr::mutate(velocity = ifelse(note > 127 | note < 0, 0, velocity)) %>%
    # make sure notes are between 0 and 127
    dplyr::mutate(note = ifelse(note > 127 | note < 0, 127, note))

  #update body_messages with new notes and velocity
  body_messages$note <- body_messages_long$note
  body_messages$velocity <- body_messages_long$velocity

  copy_df <- rbind(meta_messages_split[[1]],
                   body_messages,
                   meta_messages_split[[2]])


  return(copy_df)
}
