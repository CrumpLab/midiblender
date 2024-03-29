% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/midi-construct.R
\name{add_note_on}
\alias{add_note_on}
\title{Add note_on message to df}
\usage{
add_note_on(
  df,
  i_track = 0,
  type = "note_on",
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

\item{type}{character, default = "note_on"}

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
Intended for use in a dplyr style pipe. Adds a note_on message to a midi_df.
}
\examples{
\dontrun{
}
}
