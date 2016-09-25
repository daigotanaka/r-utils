require(knitr)
render_caption <- 
function(caption) {
  paste('<p class="caption">', caption, "</p>", sep="")
}
 
knit_hooks$set(html.cap <- function(before, options, envir) {
    if(!before) {
      render_caption(options$html.cap)
    } else {
      # Do nothing (or set isTable flag to render_caption at the top?
    }
  }
)

require(R6)
Caption =
R6Class("Caption",
  public <- list(
    label_=c(),
    text_=c(),
    type_=NA,
    initialize=function(type="Figure") {
      self$type_ <- type
    },
    label=function(l, t=NULL) {
        index <- length(self$label_) + 1
        if (l %in% self$label_) {
            index <- which(self$label_ == l)
        } else {
            self$label_[index] <- l
        }
        if (!is.null(t)) {
            self$text_[index] <- t
        }
        which(l == self$label_)
    },
    text=function(l, t=NULL) {
        if (!l %in% self$label_) stop("No such label")
        index <- which(l == self$label_)
        if (!is.null(t)) {
            self$text_[index] <- t
        }
        paste(self$type_, " ", which(l == self$label_), ". ", self$text_[index], sep="")
    }
  )
)

Footnote =
R6Class("Footnote",
  public <- list(
    label_=c(),
    text_=c(),
    label=function(l, t=NULL) {
      self$update(l, t)
      writeLines(paste('<a href="#', l, '">',
        '<span id="', l, '_back"><sup>',
        which(l == self$label_), '</sup></span></a>', sep=""))
    },
    update=function(l, t=NULL) {
        index <- length(self$label_) + 1
        if (l %in% self$label_) {
            index <- which(self$label_ == l)
        } else {
            self$label_[index] <- l
        }
        if (!is.null(t)) {
            self$text_[index] <- t
        }
    },
    render=function(head="Notes") {
        writeLines('<div class="footnotes">')
        writeLines(head)
        writeLines('<ol>')
        items <- paste('<li id="', self$label_, '">', self$text_,
                      ' <a href="#', self$label_,'_back">&#8617;</a>', '</li>', sep="")
        writeLines(items)
        writeLines('</ol></div>')
    }
  )
)

numericToString = function(numbers, digits=2) {
    strings <- sprintf(paste("%.", digits, "f", sep=""), 
            round(numbers, digits=digits))
    return(strings)
}

markdownTableStrings = function(data, header=NULL, digits=2) {
    tableLines <- c()

    if (is.null(header)) {
        header <- names(data)
    }
    head <- paste("|",
                  paste(header, collapse="|"),
                  "|", sep="")

    format <- "|"
    for (col in 1:length(names(data))) {
        if (is.numeric(data[1, col])) {
            format <- paste(format, "----:|", sep="")
        } else {
            format <- paste(format, ":----|", sep="")
        }
    }

    row = c()
    for (r in 1:nrow(data)) {
        dataStr = c()
        for (col in 1:length(names(data))) {
            elem <- data[r, col]
            if (is.numeric(elem)) {
                dataStr = c(dataStr, numericToString(elem))
            } else if (is.factor(elem)) {
                dataStr = c(dataStr, as.character(elem))
            } else {
                dataStr = c(dataStr, elem)
            }
        }
        row[r] <- paste("|",
                        paste(dataStr, collapse="|"),
                        "|", sep="")
    }
    return (c(head, format, row))
}
