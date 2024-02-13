#' @title Initialize a midi_df tibble with column headers
#'
#' @description
#' This function creates an empty tibble containing specific columns for a midi_df.
#'
#' @details
#' The columns include i_track (integer), meta (logical), type (character),
#' name (character), time (integer), numerator (integer), denominator (integer),
#' clocks_per_click (integer), notated_32nd_notes_per_beat (integer), program (integer),
#' channel (integer), control (integer), value (integer), note (integer), velocity (integer),
#' and tempo (integer).
#'
#' @return A tibble object with columns of different types.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' new_midi_df <- create_empty_midi_df()
#' }
#'
create_empty_midi_df <- function() {
  tibble::tibble(
    i_track = integer(),
    meta = logical(),
    type = character(),
    name = character(),
    time = integer(),
    numerator = integer(),
    denominator = integer(),
    clocks_per_click = integer(),
    notated_32nd_notes_per_beat = integer(),
    program = integer(),
    channel = integer(),
    control = integer(),
    value = integer(),
    note = integer(),
    velocity = integer(),
    tempo = integer()
  )
}


#' Add track name message to df
#'
#' Intended for use in a dplyr style pipe. Adds a track_name message to a meta header.
#'
#' @param meta_df midi_df to add
#' @param i_track integer, default = 0
#' @param type character, default = "track_name"
#' @param name character, default = ""
#' @param time integer, default = 0
#' @param ... send additional args to dplyr::add_row
#'
#' @return a new row
#' @export
#'
#' @examples
#' \dontrun{
#' new_midi_df <- create_empty_midi_df() %>%
#'   add_meta_track_name(name = "bass")
#' }
add_meta_track_name <- function(meta_df,
                                i_track = 0,
                                type = "track_name",
                                name = "",
                                time = 0,
                                ...){
  meta_df %>%
    dplyr::add_row(
      i_track = i_track,
      meta =  TRUE,
      type = type,
      name = name,
      time = time,
      ...
    )

}

#' Add set_tempo message to df
#'
#' Intended for use in a dplyr style pipe. Adds a set_tempo message to a meta header.
#'
#' @param meta_df midi_df to add
#' @param i_track integer, default = 0
#' @param type character, default = "set_tempo"
#' @param time integer, default = 0
#' @param tempo integer, default = 500000
#' @param ... send additional args to dplyr::add_row
#'
#' @return a new row
#' @export
#'
#' @examples
#' \dontrun{
#' new_midi_df <- create_empty_midi_df() %>%
#'   add_meta_track_name(name = "bass") %>%
#'   add_meta_tempo(tempo = 500000)
#' }
add_meta_tempo <- function(meta_df,
                           i_track = 0,
                           type = "set_tempo",
                           time = 0,
                           tempo = 500000,
                           ...) {

  meta_df %>%
    dplyr::add_row(
      i_track = i_track,
      meta = TRUE,
      type = type,
      time = time,
      tempo = tempo,
      ...
    )

}

#' Add time_signature message to df
#'
#' Intended for use in a dplyr style pipe. Adds a time_signature message to a meta header.
#'
#' @param meta_df midi_df to add
#' @param i_track integer, default = 0
#' @param type character, default = "time_signature"
#' @param time integer, default = 0
#' @param numerator integer, default = 4
#' @param denominator integer, default = 4
#' @param clocks_per_click integer, default = 36
#' @param notated_32nd_notes_per_beat integer, default = 8
#' @param ... send additional args to dplyr::add_row
#'
#' @return a new row
#' @export
#'
#' @examples
#' \dontrun{
#' new_midi_df <- create_empty_midi_df() %>%
#'   add_meta_track_name(name = "bass") %>%
#'   add_meta_tempo(tempo = 500000) %>%
#'   add_meta_time_sig()
#' }
add_meta_time_sig <- function(meta_df,
                              i_track = 0,
                              type = "time_signature",
                              time = 0,
                              numerator = 4,
                              denominator = 4,
                              clocks_per_click = 36,
                              notated_32nd_notes_per_beat = 8,
                              ...) {
  meta_df %>%
    dplyr::add_row(
      i_track = i_track,
      meta = TRUE,
      type = type,
      time = time,
      numerator = numerator,
      denominator = denominator,
      clocks_per_click = clocks_per_click,
      notated_32nd_notes_per_beat = notated_32nd_notes_per_beat,
      ...
    )

}

#' Add end_of_track message to df
#'
#' Intended for use in a dplyr style pipe. Adds an end_of_track message to a meta header.
#'
#' @param meta_df midi_df to add
#' @param i_track integer, default = 0
#' @param type character, default = "end_of_track"
#' @param time integer, default = 0
#' @param ... send additional args to dplyr::add_row
#'
#' @return a new row
#' @export
#'
#' @examples
#' \dontrun{
#' }
add_meta_end_of_track <- function(meta_df,
                                  i_track = 0,
                                  type = "end_of_track",
                                  time = 0,
                                  ...) {
  meta_df %>%
    dplyr::add_row(
      i_track = i_track,
      meta = TRUE,
      type = type,
      time = time,
      ...
    )

}

#' Add program_change message to df
#'
#' Intended for use in a dplyr style pipe. Adds a program_change message to a midi_df.
#'
#' @param df midi_df to add row to
#' @param i_track integer, default = 0
#' @param type character, default = "program_change"
#' @param time integer, default = 0
#' @param program integer, default = 0
#' @param channel integer, default = 0
#' @param ... send additional args to dplyr::add_row
#'
#' @return a new row
#' @export
#'
#' @examples
#' \dontrun{
#' }

add_program_change <- function(df,
                               i_track = 0,
                               type = "program_change",
                               time = 0,
                               program = 0,
                               channel = 0,
                               ...){
  df %>%
    dplyr::add_row(
      i_track = i_track,
      meta = FALSE,
      type = type,
      time = time,
      program = program,
      channel = channel,
      ...
    )

}

#' Add control_change message to df
#'
#' Intended for use in a dplyr style pipe. Adds a control_change message to a midi_df.
#'
#' @param df midi_df to add row to
#' @param i_track integer, default = 0
#' @param type character, default = "control_change"
#' @param time integer, default = 0
#' @param channel integer, default = 0
#' @param control integer, default = 0
#' @param value integer, default = 0
#' @param ... send additional args to dplyr::add_row
#'
#' @return a new row
#' @export
#'
#' @examples
#' \dontrun{
#' }
add_control_change <- function(df,
                               i_track = 0,
                               type = "control_change",
                               time = 0,
                               channel = 0,
                               control = 0,
                               value = 0,
                               ...){
  df %>%
    dplyr::add_row(
      i_track = i_track,
      meta = FALSE,
      type = type,
      time = time,
      channel = channel,
      control = control,
      value = value,
      ...
    )

}

#' Add a midi message to a midi_df
#'
#' Intended for use in a dplyr style pipe. Adds a midi message to a midi_df.
#'
#' @param df midi_df to add row to
#' @param i_track integer, default = 0
#' @param meta logical, default = FALSE
#' @param type character, default = "control_change"
#' @param name character, default = NA
#' @param time integer, default = 0
#' @param numerator integer, default = NaN
#' @param denominator integer, default = NaN
#' @param clocks_per_click integer, default = NaN
#' @param notated_32nd_notes_per_beat integer, default = NaN
#' @param program integer, default = NaN
#' @param channel integer, default = NaN
#' @param control integer, default = NaN
#' @param value integer, default = NaN
#' @param note integer, default = NaN
#' @param velocity integer, default = NaN
#' @param tempo integer, default = NaN
#' @param ... send additional args to dplyr::add_row
#'
#' @return a new row
#' @export
#'
#' @examples
#' \dontrun{
#' }
add_midi_message <- function(df,
                             i_track = 0,
                             meta = FALSE,
                             type = "",
                             name = NA,
                             time = 0,
                             numerator = NaN,
                             denominator = NaN,
                             clocks_per_click = NaN,
                             notated_32nd_notes_per_beat = NaN,
                             program = NaN,
                             channel = NaN,
                             control = NaN,
                             value = NaN,
                             note = NaN,
                             velocity = NaN,
                             tempo = NaN,
                             ...){
  df %>%
    dplyr::add_row(
      i_track = i_track,
      meta = meta,
      type = type,
      name = name,
      time = time,
      numerator = numerator,
      denominator = denominator,
      clocks_per_click = clocks_per_click,
      notated_32nd_notes_per_beat = notated_32nd_notes_per_beat,
      program = program,
      channel = channel,
      control = control,
      value = value,
      note = note,
      velocity = velocity,
      tempo = tempo,
      ...
    )

}

#' Add note_on message to df
#'
#' Intended for use in a dplyr style pipe. Adds a note_on message to a midi_df.
#'
#' @param df midi_df to add row to
#' @param i_track integer, default = 0
#' @param type character, default = "note_on"
#' @param time integer, default = 0
#' @param channel integer, default = 0
#' @param note integer, default = 60
#' @param velocity integer, default = 64
#' @param ... send additional args to dplyr::add_row
#'
#' @return a new row
#' @export
#'
#' @examples
#' \dontrun{
#' }
add_note_on <- function(df,
                               i_track = 0,
                               type = "note_on",
                               time = 0,
                               channel = 0,
                               note = 0,
                               velocity = 0,
                               ...){
  df %>%
    dplyr::add_row(
      i_track = i_track,
      meta = FALSE,
      type = type,
      time = time,
      channel = channel,
      note = note,
      velocity = velocity,
      ...
    )

}

#' Add note_off message to df
#'
#' Intended for use in a dplyr style pipe. Adds a note_off message to a midi_df.
#'
#' @param df midi_df to add row to
#' @param i_track integer, default = 0
#' @param type character, default = "note_off"
#' @param time integer, default = 0
#' @param channel integer, default = 0
#' @param note integer, default = 60
#' @param velocity integer, default = 64
#' @param ... send additional args to dplyr::add_row
#'
#' @return a new row
#' @export
#'
#' @examples
#' \dontrun{
#' }
add_note_off <- function(df,
                        i_track = 0,
                        type = "note_off",
                        time = 0,
                        channel = 0,
                        note = 0,
                        velocity = 0,
                        ...){
  df %>%
    dplyr::add_row(
      i_track = i_track,
      meta = FALSE,
      type = type,
      time = time,
      channel = channel,
      note = note,
      velocity = velocity,
      ...
    )

}


#' Create a tibble of midi note information
#'
#' @return tibble with columns for note names, octaves, and midi number
#' @export
#'
#' @examples
#' midi_notes <- midi_notes()
#' head(midi_notes)
midi_notes <- function(){
  midi_notes <- pyramidi::midi_defs %>%
    dplyr::rowwise() %>%
    dplyr::mutate(note_letter = unlist(strsplit(as.character(note_name), "-"))[1],
                  octave = unlist(strsplit(as.character(note_name), "-"))[2])

  midi_notes <-
    tibble::tibble(
      notes = rep(midi_notes[1:12, ]$note_letter, 11)[1:128],
      octaves = rep(-1:9, each = 12)[1:128],
      midi_number = 0:127
    )
}

