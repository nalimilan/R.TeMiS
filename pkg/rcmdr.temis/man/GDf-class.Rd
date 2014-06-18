\name{GDf-class}
\Rdversion{1.1}
\docType{class}
\alias{GDf-class}
\title{Class \code{"GDf"}}
\description{GUI editor for data frames}

\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{widget}:}{Object of class \code{ANY}.}
    \item{\code{block}:}{Object of class \code{ANY}.}
    \item{\code{head}:}{Object of class \code{ANY}.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{get_length()}:}{Get the number of columns in the data frames.}
    \item{\code{set_names(values, ...)}:}{Set column names.}
    \item{\code{focus_cell(i, j)}:}{Give focus to a given cell.}
    \item{\code{hide_row(i, hide)}:}{Hide a given row.}
    \item{\code{hide_column(j, hide)}:}{Hide a given column.}
    \item{\code{initialize(parent, items, ...)}:}{Initialize the widget with items.}
    \item{\code{set_items(value, i, j, ...)}:}{Set the value of cells.}
    \item{\code{get_names()}:}{Get column names.}
    \item{\code{init_widget(parent)}:}{Initialize the widget.}
    \item{\code{set_editable(j, value)}:}{Set whether a column can be edited.}
    \item{\code{sort_bycolumn(j, decreasing)}:}{Set the sorting column.}
    \item{\code{save_data(nm, where)}:}{Save contents to a data frame.}
  }
}
