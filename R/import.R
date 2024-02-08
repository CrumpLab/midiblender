#' midi_to_object Function
#'
#' Import MIDI data and transform it into several useful R objects.
#'
#' @param file_path A character string specifying the path to the MIDI file.
#'
#' @return A list of MIDI import objects, which includes:
#' \itemize{
#'   \item \strong{miditapyr_object}: A miditapyr object representing the complete MIDI file.
#'   \item \strong{mido_object}: A mido object representing the MIDI file.
#'   \item \strong{message_list_df}: A data frame of MIDI messages.
#'   \item \strong{ticks_per_beat}: The number of ticks per beat.
#'   \item \strong{midi_df}: A data frame of MIDI messages, fully unnested.
#' }
#'
#' @details
#' This mostly wraps pyramidi functions that call miditapyr and mido.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   midi_objects <- midi_to_object("path_to_your_midi_file.mid")
#'   list2env(midi_objects, .GlobalEnv) # add to global environment
#' }
midi_to_object <- function(file_path) {
  #import midi using miditapyr
  miditapyr_object <- pyramidi::miditapyr$MidiFrames(file_path)

  #import using mido
  mido_object <- pyramidi::mido$MidiFile(file_path)

  # to R dataframe
  message_list_df <- pyramidi::miditapyr$frame_midi(mido_object)
  ticks_per_beat <- mido_object$ticks_per_beat

  # unnest the dataframe
  midi_df <- pyramidi::miditapyr$unnest_midi(message_list_df)

  return(
    midi_import_objects <- list(
      miditapyr_object = miditapyr_object,
      mido_object = mido_object,
      message_list_df = message_list_df,
      ticks_per_beat = ticks_per_beat,
      midi_df = midi_df
    )
  )
}
