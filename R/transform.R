#' copy single track df within midi_df
#'
#' Copy the midi_df, add cumulative time column, keep only note_on messages
#'
#' @param midi_df A dataframe representing a midi file returned from `midi_to_object()`.
#' @param track_num An integer indicating track number to select. Default is 0.
#'
#' @return A filtered data frame containing messages for the selected track
#'
#' @details
#' This function filters an imported midi_df for a specific track and makes a copy of midi_df. The imported midi_df comes from the midi_to_object() function, and represents the output of pyramidi::miditapyr$unnest_midi(message_list_df). I make copies of this sometimes out of caution. Currently, it is used to preserve the meta messages from the imported midi files.
#'
#' @examples
#' \dontrun{
#' copy_track_0 = copy_midi_df_track(midi_df, track_num = 0)
#' }
#' @export
copy_midi_df_track <- function(midi_df, track_num = 0) {
  # Select a particular track from the midi file
  midi_df %>%
    dplyr::filter(i_track == track_num)

}

#' copy_and_extend_midi_df
#'
#' Copy the midi_df, add cumulative time column, keep only note_on messages
#'
#' @param midi_df A dataframe representing a midi file returned from `midi_to_object()`. This dataframe must contain columns named 'i_track', 'type', and 'time', where 'i_track' is the track number, 'type' indicates the type of event (e.g., "note_on" or "note_off"), and 'time' is the time point of the event.
#' @param track_num An integer indicating the track number to be processed. Default is 0.
#'
#' @return A modified dataframe with an additional 'total_time' column and filtered only for "note_on" type.
#'
#' @details
#' This function filters and modifies an input midi dataframe, midi_df provided by midi_to_object. It first filters the data to keep rows from the specified track_num and both "note_on" and "note_off". Then adds a total_time column which accumulates the time variable. Lastly, it filters again to return only "note_on" rows.
#'
#' The general workflow is to make a copy of midi_df, which comes from pyramidi::miditapyr$unnest_midi(message_list_df), and then modify the copy with additional columns useful transforming MIDI messages with R data-wranglind techniques. The modified copy is then stripped of the additional columns, and returned to a state where it can be imported back into the miditapyr_object, which can be updated with the new midi information using miditapyr_object$midi_frame_unnested$update_unnested_mf(), and then written to a file using miditapyr_object$write_file("rando_mario.mid").
#'
#' @examples
#' \dontrun{
#' modified_midi_df <- copy_and_extend_midi_df(midi_df, track_num = 0)
#' }
#' @export

copy_and_extend_midi_df <- function(midi_df, track_num = 0) {
  # Select a particular track from the midi file
  copy_df <- midi_df %>%
    dplyr::filter(i_track == track_num,
           type %in% c("note_on", "note_off") == TRUE) %>%
    # add column to sum cumulative time
    dplyr::mutate(total_time = cumsum(time)) %>%
    # return only note_on
    dplyr::filter(type == "note_on")

}

#' Create a dataframe with helpful timing metrics from a midi_df
#'
#' @param df A dataframe containing at least a 'time' column and a 'total_time' column. Usually the df from `copy_and_extend_midi_df()`
#' @param ticks_per_beat Integer specifying the number of ticks per beat
#' @param bars Integer specifying the number of bars. If this parameter is NULL, it will compute it assuming 4/4 meter. Default is NULL.
#' @param smallest_tick Numeric. This is the smallest unit of time (in beats) represented in the output dataframe. If NULL, it will compute the smallest observed unit in the 'time' column. Default is NULL.
#'
#' @return A dataframe where each row represents one 'tick' (i.e., fundamental temporal unit), and columns represent the 'tick' timestamp (time_steps), its corresponding bar number (bars), and its step within the bar (bar_steps).
#'
#' @details
#'
#' MIDI files contain minimal timing information. This function computes additional timing metrics from a `midi_df` that are useful for later transformations.
#'
#' MIDI files and the the `midi_df` imported by `pyramidi::miditapyr$MidiFrames()` represent timing information in terms of relative time since the last message, not cumulative time. Messages in a MIDI file are ordered in chronological time. The first message sent occurs at tick 0. If the next message occurred 30 ticks later, its time would be recorded as 30. If the next message was 5 ticks later, its time would be recorded as 5. If another message was to be sent at the same time, it would be recorded in the next message with a time of 0, relative to the last message. MIDI files have a ticks per beat parameter. For example, one beat (or quarter note in 4/4 time) could have 96 MIDI ticks. It is possible to increase the resolution so that a single beat is given additional ticks.
#'
#' This function attempts to compute additional timing information in a new tibble, such as cumulative ticks, bars, and intervals within a bar. This tibble can be joined to a midi_df to aid with various transformation tasks.
#'
#'
#' @examples
#' \dontrun{
#' copy_track <- copy_and_extend_midi_df(midi_df, track_num = 0)
#' metric_tibble <- make_metric_tibble(copy_track, ticks_per_beat = 96, bars=48,smallest_tick=8)
#'
#' }
#' @export

make_metric_tibble <- function(df, ticks_per_beat, bars=NULL, smallest_tick=NULL ) {

  if(is.null(smallest_tick) == TRUE){
    # if not set by user
    # set smallest temporal unit to smallest observed unit in time column
    smallest_tick <- min(df$time[df$time > 0])
  }

  time_steps <- seq(0, max(df$total_time), smallest_tick)

  if(is.null(bars) == TRUE){
    # only 4/4 for now
    bars <- max(df$total_time)/(ticks_per_beat*4)
  }

  total_bars <- round(length(time_steps) / bars) + 1
  bar_vector <- rep(1:total_bars, each = bars)
  bar_steps <- rep(1:bars, total_bars)

  metric_tibble <- tibble::tibble(time_steps = time_steps,
                          bars = bar_vector[1:length(time_steps)],
                          bar_steps = bar_steps[1:length(time_steps)])
  return(metric_tibble)
}


#' Add Bars to Copy of Dataframe
#'
#' This function modifies a copy of the dataframe `df` by adding new columns related to time markers and bars from a `metric_tibble`. Rows in the dataframe are iterated upon to set values for these new columns based on matching `time_steps` in the `metric_tibble`.
#'
#' @param df A data frame that needs additional time marker and bar related data. The columns in the data frame can vary, but must contain a 'total_time' column. Usually received from `copy_and_extend_midi_df()`.
#' @param metric_tibble A tibble having columns 'time_steps', 'bars', and 'bar_steps'. This tibble provides the metrics that are used to add new marker and bar data to the data frame `df`. Created from `make_metric_tibble()`.
#'
#' @return A data frame which is a copy of the input data frame `df`, but with three additional columns: 'time_markers', 'bars', and 'bar_steps'. 'time_markers' is populated by the index of the match between the 'total_time' of `df` and 'time_steps' of `metric_tibble`. bars' and 'bar_steps' are populated from corresponding values in `metric_tibble` based on matching 'time_steps'.
#'
#' @examples
#' \dontrun{
#' copy_track <- copy_and_extend_midi_df(midi_df, track_num = 0)
#' metric_tibble <- make_metric_tibble(copy_track, ticks_per_beat = 96, bars=48,smallest_tick=8)
#' copy_track <- add_bars_to_copy_df(copy_track,metric_tibble)
#' }
#' @export

add_bars_to_copy_df <- function(df, metric_tibble) {

  # add new columns
  df <- df %>%
    dplyr::mutate(time_markers = 0,
           bars = 0,
           bar_steps = 0)

  for (i in 1:dim(df)[1]) {
    df$time_markers[i] <-
      which(metric_tibble$time_steps %in% df$total_time[i])
  }

  # get bar divisions, add them to df

  for (i in 1:dim(df)[1]) {
    get_timestep <- metric_tibble$time_steps[df$time_markers[i]]

    df$bars[i] <- metric_tibble %>%
      dplyr::filter(time_steps == get_timestep) %>%
      dplyr::pull(bars)

    df$bar_steps[i] <- metric_tibble %>%
      dplyr::filter(time_steps == get_timestep) %>%
      dplyr::pull(bar_steps)
  }

  return(df)
}



#' create_midi_matrix function
#'
#' @description This function creates a MIDI matrix for a given musical score.
#' @param df A data.frame containing the input MIDI notation. df has at least columns named 'bars', 'note', and 'bar_steps'. 'bars' indicates which bar the note occurs in, note' indicates the pitch of the note, bar_steps' shows the time position within the bar that the note occurs.
#' @param num_notes An integer determining the size of MIDI note bins. Default is 128.
#' @param intervals_per_bar An integer specifying the number of intervals to split each bar into for matrix preparation. Default is 48, which corresponds to 48th notes (or every quarter beat in a 4/4 signature).
#' @param separate A logical value indicating if the function should return a single concatenated matrix (FALSE) or a list of separate pitch, time, and pitch by time matrices (TRUE). Default is FALSE.
#' @return If separate = FALSE, a matrix of MIDI note data with rows representing bars, and columns representing 1) counts of notes in pitch bins, 2) counts of notes in time bins, and 3) a matrix 'flattened' to a vector of pitch by time data. If separate = TRUE, a list containing separate matrices for pitch, time, and pitch by time.
#' @examples
#'  \dontrun{
#' copy_track <- copy_and_extend_midi_df(midi_df, track_num = 0)
#' metric_tibble <- make_metric_tibble(copy_track, ticks_per_beat = 96, bars=48,smallest_tick=8)
#' copy_track <- add_bars_to_copy_df(copy_track,metric_tibble)
#' music_matrix <- create_midi_matrix(copy_track,128,48)
#' }
#' @export
create_midi_matrix <- function(df, num_notes = 128, intervals_per_bar = 48, separate=FALSE){

  #initialize matrix
  music_matrix <- matrix(0,
                         ncol = (intervals_per_bar+num_notes+(intervals_per_bar*num_notes)),
                         nrow = max(df$bars)
  )

  #loop to assign note_ons to
  for(i in 1:max(df$bars)) {

    # get the bar
    bar_midi <- df %>%
      dplyr::filter(bars == i)

    # make a temporary little matrix
    one_bar <- matrix(0,
                      nrow = num_notes,
                      ncol = intervals_per_bar)

    # assign 1s to note positions
    for (j in 1:dim(one_bar)[1]) {
      one_bar[bar_midi$note[j], bar_midi$bar_steps[j]] <- 1
    }

    # get summary vectors
    pitch_vector <- rowSums(one_bar)
    time_vector <- colSums(one_bar)
    pitch_by_time <- c(one_bar)

    #concatenate_vector
    music_vector <- c(pitch_vector, time_vector, pitch_by_time)

    # add to matrix
    music_matrix[i, ] <- music_vector

  }

  # return the whole matrix
  if(separate == FALSE) {
    return(music_matrix)
  }

  # or split the matrix and return a list of pitch, time, and pitch x time

  if(separate == TRUE){
    return(list(
      pitch_matrix = music_matrix[,1:num_notes],
      time_matrix = music_matrix[,(num_notes+1):(num_notes+intervals_per_bar)],
      pitch_by_time_matrix = music_matrix[,(num_notes+intervals_per_bar+1):dim(music_matrix)[2]]
    )
    )
  }

}

#' Convert a matrix representation of a MIDI sequence back to MIDI timings
#'
#' The function reads a binary matrix where pitch classes are on the rows and temporal locations on columns (1s represent note onsets, 0s represent absences of notes). It then converts this binary representation into MIDI timings, i.e. the time (in ticks) of note onsets and note offsets.
#'
#' @param midi_matrix A binary matrix, where pitch classes are on rows and temporal location are on columns. Each cell value should either be 1 (note onset) or 0 (notes absence).
#' @param smallest_time_unit Integer, specifying the smallest time unit (in MIDI ticks) that is used in the binary matrix representation. Default is 8 ticks.
#' @param note_off_length Numeric, specifying a fixed note duration in MIDI ticks that will be used for all the notes in the sequence. Default is 32 ticks.
#'
#' @return A dataframe with four columns: \code{note_num}, \code{note_id}, \code{type}, \code{time}.
#'         \code{note_num} is the MIDI pitch of each note. \code{note_id} is a unique identifier for each note.
#'         \code{type} whether the event is a note onset (\code{"note_on"}) or a note offset (\code{"note_off"}).
#'         \code{time} is the time (in MIDI ticks) since the last event.
#'
#' @examples
#' \dontrun{
#' }
#'
#' @export
matrix_to_midi_time <- function(midi_matrix, smallest_time_unit = 8, note_off_length = 32) {

  #get notes and times
  note_times <- which(midi_matrix == 1, arr.ind = T)
  colnames(note_times) <- c("note_num", "bar_step")
  note_times <- tibble::as_tibble(note_times)

  # convert back to midi time in ticks
  note_times <- note_times %>%
    dplyr::mutate(
      note_num = note_num -1,
      ticks = (bar_step * smallest_time_unit) - smallest_time_unit,
      note_id = 1:dplyr::n(),
      note_on = ticks,
      note_off = ticks + note_off_length
    ) %>%
    tidyr::pivot_longer(
      cols = c("note_on", "note_off"),
      names_to = "type",
      values_to = "cumulative_ticks"
    ) %>%
    dplyr::arrange(cumulative_ticks) %>%
    dplyr::mutate(time = cumulative_ticks - dplyr::lag(cumulative_ticks, default = 0))

}


#' get meta messages from a track_df
#'
#' @param track_df filtered midi_df with a selected i_track
#'
#' @return tibble of meta messages
#' @export
get_midi_meta_df <- function(track_df){
  return(track_df %>%
           dplyr::filter(meta ==TRUE))
}

#' set midi tempo in a meta messages df
#'
#' @param meta_messages_df a df of meta messages
#' @param update_tempo Integer, a midi tempo to update
#'
#' @return updated meta_messages_df
#' @export
set_midi_tempo_meta <- function(meta_messages_df,update_tempo=500000){
  # check if tempo actually exists, if not add it
  if("tempo" %in% names(meta_messages_df) == FALSE){
    return(
      meta_messages_df %>%
        dplyr::mutate(tempo = NaN) %>%
        dplyr::add_row(
          i_track = meta_messages_df$i_track[1],
          meta = TRUE,
          type = "set_tempo",
          tempo = update_tempo,
          .before = dim(meta_messages_df)[1]
        )
    )
  }

  # if tempo exists modify it
  if("tempo" %in% names(meta_messages_df) == TRUE){
    return(
      meta_messages_df %>%
        dplyr::mutate(tempo = ifelse(type == "set_tempo",update_tempo,tempo))
    )
  }
}


#' set midi tempo in a midi_df
#'
#' @param midi_df a df returned by `midi_to_object` or a copy of one
#' @param update_tempo Integer, a midi tempo to update
#'
#' @return updated meta_messages_df
#' @export
set_midi_tempo_midi_df <- function(midi_df,update_tempo=500000){
  # check if tempo actually exists, if not add it
  if("tempo" %in% names(midi_df) == FALSE){
    return(
      midi_df %>%
        dplyr::mutate(tempo = NaN) %>%
        dplyr::add_row(
          i_track = midi_df$i_track[1],
          meta = TRUE,
          type = "set_tempo",
          tempo = update_tempo,
          .after = 1
        )
    )
  }

  # if tempo exists modify it
  if("tempo" %in% names(midi_df) == TRUE){
    return(
      midi_df %>%
        dplyr::mutate(tempo = ifelse(type == "set_tempo",update_tempo,tempo))
    )
  }
}

#' Split a meta_df into list of top and end messages
#'
#' @param meta_messages_df meta_messages_df to split
#'
#' @return list of two tibbles
#' @export
#'
split_meta_df <- function(meta_messages_df){
  top_track_messages <- meta_messages_df %>%
    dplyr::filter(meta == TRUE) %>%
    dplyr::filter(type != "end_of_track" )

  end_track_message <- meta_messages_df %>%
    dplyr::filter(meta==TRUE) %>%
    dplyr::filter(type == "end_of_track" )

  return(list(top_track_messages,end_track_message))
}

#' matrix_to_midi_track function
#'
#' This function converts a MIDI time dataframe to a MIDI track dataframe, given a specific channel and velocity.
#' It subsequently writes note on/off, time, and note number messages.
#'
#' @param midi_time_df A dataframe containing MIDI time information.
#' @param split_meta_list A list of two data frames, the first contains metadata for the start of the MIDI track
#'    and the second contains metadata for the end of the MIDI track.
#' @param channel Integer value representing the MIDI channel. Default is 0.
#' @param velocity Integer value representing the velocity of the MIDI notes. Default is 64.
#'
#' @return An updated MIDI track data frame which includes note on/off, time, and note number messages.
#' @export
matrix_to_midi_track <- function(midi_time_df,split_meta_list, channel = 0, velocity = 64){

  new_midi_df <- split_meta_list[[1]]

  # write note on/off, time, and note number messages
  new_midi_df <- new_midi_df %>%
    dplyr::add_row(
      i_track = new_midi_df$i_track[1],
      meta = FALSE,
      channel = channel,
      velocity = velocity,
      type = midi_time_df$type,
      time = midi_time_df$time,
      note = midi_time_df$note_num) %>%
    dplyr::add_row(split_meta_list[[2]])

  return(new_midi_df)
}

#' @title Convert MIDI Data Frame to piano roll Matrix
#'
#' @description
#' This function takes a data frame obtained from a MIDI file and converts it to a binary matrix representation, with time frames as columns and MIDI note numbers (0-127) as rows.
#'
#' @param midi_df A data frame of MIDI events, from `midi_to_object()`
#' @param track The track number to be converted to a matrix. Default is 0.
#'
#' @return
#' A binary matrix where column indices represent time frames and row indices represent MIDI note numbers. Each entry in the matrix is 1 if the corresponding note is on at the corresponding time, and 0 otherwise.

#' @examples
#' \dontrun{
#' }
#' @export
midi_df_to_matrix <- function(midi_df, track = 0) {

  note_on_times <- midi_df %>%
    dplyr::filter(i_track == track,
                  type %in% c("note_on", "note_off")) %>%
    dplyr::mutate(total_time = cumsum(time)+1) %>%
    dplyr::filter(type == 'note_on') %>%
    dplyr::mutate(note = note+1)

  music_matrix <- matrix(0,
                         ncol = max(note_on_times$total_time),
                         nrow = 128)

  # assign 1s to note positions
  for (i in 1:dim(note_on_times)[1]) {
    music_matrix[note_on_times$note[i], note_on_times$total_time[i]] <- 1
  }

  return(music_matrix)

}

#' Reshape Piano Roll Matrix
#'
#' This function reshapes a piano roll matrix into a specified interval length. It pads with zero or crops the piano roll matrix to fit the desired length.
#'
#' @param piano_roll_matrix A matrix to be reshaped. The matrix rows represent the pitch (from 0 to 127) and the columns represents the time (ticks).
#' @param ticks_per_interval A number that specifies number of ticks per interval.
#'
#' @return A reshaped matrix with `num_intervals` rows and `(ticks_per_interval*128)` columns. Rows represent the time interval and columns represents the pitch.
#'
#' @examples
#' \dontrun{
#' }
#'
#' @export
reshape_piano_roll <- function(piano_roll_matrix,
                               ticks_per_interval){

  # get estimated number of intervals
  num_intervals <- round((dim(piano_roll_matrix)[2]-1)/ticks_per_interval)

  # determine how long the vector needs to be
  needed_vector_length <- num_intervals * (128 * ticks_per_interval)

  # concatenate piano roll matrix to vector
  roll_vector <- c(piano_roll_matrix)

  # checks to see if the vector is too short, then pad with 0s
  if(length(roll_vector) < needed_vector_length){
    # pad
    roll_vector[length(roll_vector):needed_vector_length] <- 0
  }

  # checks to see if the vector is too long, then crop
  if(length(roll_vector) > needed_vector_length){
    roll_vector <- roll_vector[1:needed_vector_length]
  }

  # construct the new matrix
  new_matrix <- matrix(roll_vector,
                       nrow = num_intervals,
                       ncol = (ticks_per_interval*128),
                       byrow=TRUE)
  return(new_matrix)
}
