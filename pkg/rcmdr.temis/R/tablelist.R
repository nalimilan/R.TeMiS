# Adapted from gWidgets2tcltk::tablelist.R
# https://github.com/jverzani/gWidgets2tcltk/blob/master/R/tablelist.R
# Copyright  2013 John Verzani. License: GPL ≥ 2.


## Events are: <<TablelistCellUpdated>> <<TablelistSelect>>

## Configure tbl
.tl_configure_columns <- function(tbl, nms) {
  .Tcl(sprintf("%s configure -columns {%s}",
               tbl$ID,
               paste(sprintf("0 {%s} left", nms), collapse="\n")
      ))
  sapply(seq_along(nms), function(j) .tl_set_column_editable(tbl, j))
}

## Load Data
## helper to load a row
.tl_insert_row <- function(tbl, row) {
  if(length(row) == 1 && grepl(" ", row))
    row <- paste("{", row, "}", sep="")
  tcl(tbl, "insert", "end", unlist(lapply(row, as.character)))
}

.tl_clear_data <- function(tbl) {
  tcl(tbl, "delete", "0", "end")
}

.tl_load_data <- function(tbl, items) {
  ## need to clear old first!
  .tl_clear_data(tbl)
  sapply(seq_len(nrow(items)), function(i)
         .tl_insert_row(tbl, items[i,,drop=TRUE]))
}

## return tcl cell index
.tl_get_cellindex <- function(tbl, i, j) {
  tcl(tbl, "cellindex", sprintf("%s, %s", i-1, j-1))
}


## Get Data
## get cell infor -- raw = text
.tl_get_cell_raw <- function(tbl, i, j) {
  raw <- tcl(tbl, "cellcget", .tl_get_cellindex(tbl, i, j), "-text")
  tclvalue(raw)
}

## returns text value for column -- must coerce to ...
.tl_get_column_raw1 <- function(tbl, j) {
  m <- .tl_no_rows(tbl)
  sapply(seq_len(m), function(i) .tl_get_cell_raw(tbl, i, j))
}

.tl_get_column_raw <- function(tbl, j) {
  tcl(tbl, "getcolumns", j-1)
}


## return character matrix
.tl_get_raw <- function(tbl) {
  do.call(cbind, lapply(seq_len(.tl_no_cols(tbl)), function(j) .tl_get_column_raw(tbl, j)))
}

## coerce
.coerce_raw <- function(x, values) UseMethod(".coerce_raw")
.coerce_raw.default <- function(x, values) as.character(values)
.coerce_raw.integer <- function(x, values) as.integer(values)
.coerce_raw.numeric <- function(x, values) as.numeric(values)
.coerce_raw.logical <- function(x, values) as.logical(values)
.coerce_raw.factor <- function(x, values) factor(as.character(values))


## names
.tl_set_column_name <- function(tbl, j, nm) {
  tcl(tbl, "columnconfigure", j-1, title=nm)
}

..tl_set_column_names <- function(tbl, nms) {
  for(j in seq_along(nms)) .tl_set_column_name(tbl, j, nms[j])
}


.tl_get_column_name <- function(tbl, j) {
  tail(as.character(tcl(tbl, "columnconfigure", j-1, title=NULL)), n=1)
}

..tl_get_column_names <- function(tbl) {
  sapply(seq_len(.tl_no_cols(tbl)), function(j) .tl_get_column_name(tbl, j))
}

## remove column
.tl_remove_column <- function(tbl, j) {
  tcl(tbl, "deletecolumns", j-1, j-1)
}


## sort by column
.tl_sort_bycolumn <- function(tbl, j, decreasing=FALSE) {
  dir <- if(decreasing) "decreasing" else "increasing"
  tcl(tbl, "sortbycolumn", j-1, sprintf("-%s", dir))
}


## size
.tl_no_rows <- function(tbl) as.numeric(tcl(tbl, "childcount", "root"))
.tl_no_cols <- function(tbl) as.numeric(tcl(tbl, "columncount"))


##
.tl_set_focus_on_cell <- function(tbl, i, j) {
  tcl(tbl, "see", sprintf("%s, %s", i-1, j-1))
}


## show/hide column
.tl_hide_row <- function(tbl, i, hide=TRUE) {
  hide <- if(hide) 1 else 0
  tcl(tbl, "rowconfigure", i-1, hide=hide)
}

.tl_hide_column <- function(tbl, j, hide=TRUE) {
  hide <- if(hide) 1 else 0
  tcl(tbl, "columnconfigure", j-1, hide=hide)
}

## toggle editabbility of column
.tl_set_column_editable <- function(tbl, j, editable=TRUE) {
  editable <- if(editable) "yes" else "no"
  tcl(tbl, "columnconfigure", j-1, editable=editable)
}
## names
.tl_set_column_name <- function(tbl, j, nm) {
  tcl(tbl, "columnconfigure", j-1, title=nm)
}

..tl_set_column_names <- function(tbl, nms) {
  for(j in seq_along(nms)) .tl_set_column_name(tbl, j, nms[j])
}


.tl_get_column_name <- function(tbl, j) {
  tail(as.character(tcl(tbl, "columnconfigure", j-1, title=NULL)), n=1)
}

..tl_get_column_names <- function(tbl) {
  sapply(seq_len(.tl_no_cols(tbl)), function(j) .tl_get_column_name(tbl, j))
}

## remove column
.tl_remove_column <- function(tbl, j) {
  tcl(tbl, "deletecolumns", j-1, j-1)
}


## sort by column
.tl_sort_bycolumn <- function(tbl, j, decreasing=FALSE) {
  dir <- if(decreasing) "decreasing" else "increasing"
  tcl(tbl, "sortbycolumn", j-1, sprintf("-%s", dir))
}


## size
.tl_no_rows <- function(tbl) as.numeric(tcl(tbl, "childcount", "root"))
.tl_no_cols <- function(tbl) as.numeric(tcl(tbl, "columncount"))


##
.tl_set_focus_on_cell <- function(tbl, i, j) {
  tcl(tbl, "see", sprintf("%s, %s", i-1, j-1))
}


## show/hide column
.tl_hide_row <- function(tbl, i, hide=TRUE) {
  hide <- if(hide) 1 else 0
  tcl(tbl, "rowconfigure", i-1, hide=hide)
}

.tl_hide_column <- function(tbl, j, hide=TRUE) {
  hide <- if(hide) 1 else 0
  tcl(tbl, "columnconfigure", j-1, hide=hide)
}

## toggle editabbility of column
.tl_set_column_editable <- function(tbl, j, editable=TRUE) {
  editable <- if(editable) "yes" else "no"
  tcl(tbl, "columnconfigure", j-1, editable=editable)
}


# Adapted from gdf.R
.tl_new <- function(parent) {
    if(!inherits(tclRequire("tablelist", warn = FALSE), "tclObj"))
        stop(.gettext("Tk widget 'tablelist' could not be loaded"))

    block <- ttkframe(parent)
#block <- parent
    xscr <- ttkscrollbar(block, orient="horizontal",
                         command=function(...) tkxview(widget,...))
    yscr <- ttkscrollbar(block, orient="vertical",
                         command=function(...) tkyview(widget,...))


    widget <- tkwidget(block, "tablelist::tablelist",
                       resizablecolumns=1,
                       xscrollcommand=function(...) tkset(xscr,...),
                       yscrollcommand=function(...) tkset(yscr,...))

    tcl(widget, "configure", selecttype="cell")

    tkgrid(widget, row=0, column=0, sticky="news")
    tkgrid(yscr, row=0, column=1, sticky="ns")
    tkgrid(xscr, row=1, column=0, sticky="ew")
    tkgrid.columnconfigure(block, 0, weight=1)
    tkgrid.rowconfigure(block, 0, weight=1)

    tcl("autoscroll::autoscroll", xscr)
    tcl("autoscroll::autoscroll", yscr)

    tkgrid(block, sticky="news")
    tkgrid.columnconfigure(top, 0, weight=1)
    tkgrid.rowconfigure(top, 0, weight=1)

    tkgrid.propagate(block, FALSE)

    widget
}

.tl_set_items <- function(widget, value, i,j,...) {
    if(!missing(i) || !missing(j)) {
      tmp <- .get_items(widget)
      tmp[i,j] <- value
      value <- tmp
    }
    .tl_load_data(widget, value)
}

.tl_save_data <- function(widget, nm, where) {
    assign(make.names(nm), .get_items(widget), where)
}

.get_dim <- function(widget) c(.tl_no_rows(widget), .tl_no_cols(widget))

..get_items <- function(widget, i, j, ...) {
    opar <- options("warn"); on.exit(options(opar))
    options(list(warn=-1)) # quiet for .coerce_raw
    d <- .get_dim(widget)
    l <- lapply(seq_len(d[2]), function(j) {
      .coerce_raw(head[[j]], .tl_get_column_raw(widget, j))
    })

    m <- structure(l,
                   .Names=..tl_get_column_names(widget),
                   row.names=seq_len(d[1]),
                   class="data.frame")

    m[i,j, ...]
}

##' Class to provide means to edit data frames
##'
##' The \code{GDf} class provides a means to edit a data frame. We
##' use the add on TK code provided by tablelist as the underlying
##' widget
##'
##' Simply click on a row and the editor pops up as a modal
##' dialog. The shortcut Shift+Enter will go onto the next case,
##' saving changes provided the auto save featuer is employed.
##'
##' There is no undo/redo support here. There is no support for
##' editing rownames (just lazy at the moment, holler if you would
##' like that). No support to change the dimensions of the data frame
##' or edit factors, ...
##' @rdname gWidgets2tcltk-package
.GDf <- setRefClass("GDf",
                    fields=list(
                      widget="ANY",
                      block="ANY",
                      head="ANY"
                      ),
                    methods=list(
                      initialize=function(parent, items, ...) {
                        if(!inherits(tclRequire("tablelist", warn = FALSE), "tclObj"))
                            stop(.gettext("Tk widget 'tablelist' could not be loaded"))

                        if(!inherits(tclRequire("autoscroll", warn = FALSE), "tclObj"))
                            stop(.gettext("Tk widget 'autoscroll' could not be loaded"))

                        ## what is
                        init_widget(parent)
                        items <- as.data.frame(items)
                        .tl_configure_columns(widget, names(items))

                        ## populate
                        set_items(value=items)
                        head <<- head(items, n=1) # store types

                        ## change handler is row updated
# handler_id <<- add_handler_changed(handler, action)
                      },
                      init_widget=function(parent) {
                        block <<- ttkframe(parent)
                        xscr <- ttkscrollbar(block, orient="horizontal",
                                             command=function(...) tkxview(widget,...))
                        yscr <- ttkscrollbar(block, orient="vertical",
                                             command=function(...) tkyview(widget,...))
                        

                        widget <<- tkwidget(block, "tablelist::tablelist",
                                            resizablecolumns=1,
                                            xscrollcommand=function(...) tkset(xscr,...),
                                            yscrollcommand=function(...) tkset(yscr,...))

                        tcl(widget, "configure", selecttype="cell")

                        tkgrid(widget, row=0, column=0, sticky="news")
                        tkgrid(yscr, row=0, column=1, sticky="ns")
                        tkgrid(xscr, row=1, column=0, sticky="ew")
                        tkgrid.columnconfigure(block, 0, weight=1)
                        tkgrid.rowconfigure(block, 0, weight=1)
                        
                        tcl("autoscroll::autoscroll", xscr)
                        tcl("autoscroll::autoscroll", yscr)

                        tkgrid.propagate(block, FALSE)
                      },
                      set_items=function(value, i,j,...) {
                        if(!missing(i) || !missing(j)) {
                          tmp <- .get_items()
                          tmp[i,j] <- value
                          value <- tmp
                        }
                        .tl_load_data(widget, value)
                      },
                      save_data=function(nm, where) {
                        "Save data set"
                        assign(make.names(nm), .get_items(), where)
                      },
                      .get_items=function(i, j, ...) {
                        opar <- options("warn"); on.exit(options(opar))
                        options(list(warn=-1)) # quiet for .coerce_raw
                        d <- .get_dim()
                        l <- lapply(seq_len(d[2]), function(j) {
                          .coerce_raw(head[[j]], .tl_get_column_raw(widget, j))
                        })

                        m <- structure(l,
                                       .Names=..tl_get_column_names(widget),
                                       row.names=seq_len(d[1]),
                                       class="data.frame")

                        m[i,j, ...]
                      },
                      get_names=function() ..tl_get_column_names(widget),
                      set_names=function(values, ...) ..tl_set_column_names(widget,values),
                      .get_dim=function() c(.tl_no_rows(widget), .tl_no_cols(widget)),
                      get_length=function() .get_dim()[2],
                      ## some extras
                      hide_row=function(i, hide=TRUE) .tl_hide_row(widget, i, hide),
                      hide_column=function(j, hide=TRUE) .tl_hide_column(widget, j, hide),
                      set_editable=function(j, value=TRUE) .tl_set_column_editable(widget, j, value),
                      focus_cell=function(i,j) tl_set_focus_on_cel(widget, i, j),
                      sort_bycolumn=function(j, decreasing=FALSE) {
                        .tl_sort_bycolumn(widget, j, decreasing)
                      }
                      ))

# Adapted from Rcmdr's selectActiveDataSet(). Copyright John Fox. License: GPL>=2.
.loadStemming <- function(){
	dataSets <- listDataSets()
	.activeDataSet <- ActiveDataSet()
	if (length(dataSets) == 0){
		Message(message=gettextRcmdr("There are no data sets from which to choose."),
				type="error")
		tkfocus(CommanderWindow())
		return()
	}
	initializeDialog(title=gettextRcmdr("Select Data Set"))
	dataSetsBox <- variableListBox(top, dataSets, title=gettextRcmdr("Data Sets (pick one)"))

	dset <- NULL

	onOK <- function(){
		dset <<- getSelection(dataSetsBox)
		closeDialog()
	}
	OKCancelHelp()
	tkgrid(getFrame(dataSetsBox), sticky="nw")
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix()

	dset
}

.editStemming <- function(df, row.names=NULL) {
    top <- tktoplevel()
    tkwm.geometry(top, tkwm.geometry(CommanderWindow()))

    if(!is.null(row.names)) {
        df <- cbind(rownames(df), df)
        names(df)[[1]] <- row.names
    }

    tl <- .GDf(top, df)
    tkgrid(tl$block, sticky="news")
    tkgrid.columnconfigure(top, 0, weight=1)
    tkgrid.rowconfigure(top, 0, weight=1)

    .env <- environment()

    onLoad <- function() {
        tkgrab.release(top)
        tkdestroy(top)

        .loadStemming()
    }

    onOK <- function() {
        tl$save_data("df", .env)
        rownames(.env$df) <- .env$df[[1]]
        .env$df <- .env$df[-1]

        tkgrab.release(top)
        tkdestroy(top)
        
    }

    loadButton <- buttonRcmdr(top, text=.gettext("Load stemming table"), command=onLoad)
    saveButton <- buttonRcmdr(top, text=.gettext("Save stemming table"), command=onSave)
    OKbutton <- buttonRcmdr(top, text=gettextRcmdr("OK"), width=12, command=onOK, default="active",
                            image="::image::okIcon", compound="left")
    tkgrid(loadButton, sticky="w")
    tkgrid(saveButton, sticky="w")
    tkgrid(OKbutton, sticky="e")

    tkgrab.set(top)
    tkwait.window(top)

    .env$df
}
