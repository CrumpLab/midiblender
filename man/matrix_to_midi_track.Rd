% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform.R
\name{matrix_to_midi_track}
\alias{matrix_to_midi_track}
\title{matrix_to_midi_track function}
\usage{
matrix_to_midi_track(midi_time_df, split_meta_list, channel = 0, velocity = 64)
}
\arguments{
\item{midi_time_df}{A dataframe containing MIDI time information.}

\item{split_meta_list}{A list of two data frames, the first contains metadata for the start of the MIDI track
and the second contains metadata for the end of the MIDI track.}

\item{channel}{Integer value representing the MIDI channel. Default is 0.}

\item{velocity}{Integer value representing the velocity of the MIDI notes. Default is 64.}
}
\value{
An updated MIDI track data frame which includes note on/off, time, and note number messages.
}
\description{
This function converts a MIDI time dataframe to a MIDI track dataframe, given a specific channel and velocity.
It subsequently writes note on/off, time, and note number messages.
}
