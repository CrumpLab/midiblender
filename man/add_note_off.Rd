% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/midi-construct.R
\name{add_note_off}
\alias{add_note_off}
\title{Add note_off message to df}
\usage{
add_note_off(
  df,
  i_track = 0,
  type = "note_off",
  time = 0,
  channel = 0,
  note = 0,
  velocity = 0,
  ...
)
}
\arguments{
\item{df}{midi_df to add row to}

\item{i_track}{integer, default = 0}

\item{type}{character, default = "note_off"}

\item{time}{integer, default = 0}

\item{channel}{integer, default = 0}

\item{note}{integer, default = 60}

\item{velocity}{integer, default = 64}

\item{...}{send additional args to dplyr::add_row}
}
\value{
a new row
}
\description{
Intended for use in a dplyr style pipe. Adds a note_off message to a midi_df.
}
\examples{
\dontrun{
}
}
