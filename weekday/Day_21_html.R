library(dbplyr)
translate_sql(sin(x) + tan(y))
translate_sql(x < 5 & !(y >= 5))
translate_sql(first %like% "Had*")
translate_sql(first %in% c("John", "Roger", "Robert"))
translate_sql(like == 7)

with_html(body(
  h1("A heading", id = "first"),
  p("Some text &", b("some bold text.")),
  img(src = "myimg.png", width = 100, height = 100)
))
html <- function(x) structure(x, class = "html")
print.html <- function(x, ...) {
  out <- paste0("<HTML> ", x)
  cat(paste(strwrap(out), collapse = "\n"), "\n", sep = "")
}
escape <- function(x) UseMethod("escape")
escape.html <- function(x) x
escape.character <- function(x) {
  x <- gsub("&", "&amp;", x)
  x <- gsub("<", "&lt;", x)
  x <- gsub(">", "&gt;", x)
  
  html(x)
}
escape.list <- function(x) {
  lapply(x, escape)
}

# Now we check that it works
escape("This is some text.")
escape("x > 1 & y < 2")
escape(escape("This is some text. 1 > 2"))
escape(html("<hr />"))

p("Some text.", b("some bold text"), class = "mypara")
names(c(a = 1, b = 2))
names(c(a = 1, 2))
names(c(1, 2))

named <- function(x) {
  if (is.null(names(x))) return(NULL)
  x[names(x) != ""]
}
unnamed <- function(x) {
  if (is.null(names(x))) return(x)
  x[names(x) == ""]
}

source("dsl-html-attributes.r", local = TRUE)

html_attributes <- function(list) {
  if (length(list) == 0) return("")
  
  attr <- map2_chr(names(list), list, html_attribute)
  paste0(" ", unlist(attr), collapse = "")
}
html_attribute <- function(name, value = NULL) {
  if (length(value) == 0) return(name) # for attributes with no value
  if (length(value) != 1) stop("`value` must be NULL or length 1")
  
  if (is.logical(value)) {
    # Convert T and F to true and false
    value <- tolower(value)
  } else {
    value <- escape_attr(value)
  }
  paste0(name, "='", value, "'")
}
escape_attr <- function(x) {
  x <- escape.character(x)
  x <- gsub("\'", '&#39;', x)
  x <- gsub("\"", '&quot;', x)
  x <- gsub("\r", '&#13;', x)
  x <- gsub("\n", '&#10;', x)
  x
}

p <- function(...) {
  args <- list(...)
  attribs <- html_attributes(named(args))
  children <- unlist(escape(unnamed(args)))
  
  html(paste0(
    "<p", attribs, ">",
    paste(children, collapse = ""),
    "</p>"
  ))
}

p("Some text")
p("Some text", id = "myid")
p("Some text", image = NULL)
p("Some text", class = "important", "data-value" = 10)

tag <- function(tag) {
  force(tag)
  function(...) {
    args <- list(...)
    attribs <- html_attributes(named(args))
    children <- unlist(escape(unnamed(args)))
    
    html(paste0(
      "<", tag, attribs, ">",
      paste(children, collapse = ""),
      "</", tag, ">"
    ))
  }
}
tag('p')('Some text')
p <- tag("p")
b <- tag("b")
i <- tag("i")
p("Some text.", b("Some bold text"), i("Some italic text"),
  class = "mypara")

void_tag <- function(tag) {
  force(tag)
  function(...) {
    args <- list(...)
    if (length(unnamed(args)) > 0) {
      stop("Tag ", tag, " can not have children", call. = FALSE)
    }
    attribs <- html_attributes(named(args))
    
    html(paste0("<", tag, attribs, " />"))
  }
}

img <- void_tag("img")
img(src = "myimage.png", width = 100, height = 100)

tags <- c("a", "abbr", "address", "article", "aside", "audio", 
          "b","bdi", "bdo", "blockquote", "body", "button", "canvas", 
          "caption","cite", "code", "colgroup", "data", "datalist", 
          "dd", "del","details", "dfn", "div", "dl", "dt", "em", 
          "eventsource","fieldset", "figcaption", "figure", "footer", 
          "form", "h1", "h2", "h3", "h4", "h5", "h6", "head", "header", 
          "hgroup", "html", "i","iframe", "ins", "kbd", "label", 
          "legend", "li", "mark", "map","menu", "meter", "nav", 
          "noscript", "object", "ol", "optgroup", "option", "output", 
          "p", "pre", "progress", "q", "ruby", "rp","rt", "s", "samp", 
          "script", "section", "select", "small", "span", "strong", 
          "style", "sub", "summary", "sup", "table", "tbody", "td", 
          "textarea", "tfoot", "th", "thead", "time", "title", "tr",
          "u", "ul", "var", "video")

void_tags <- c("area", "base", "br", "col", "command", "embed",
               "hr", "img", "input", "keygen", "link", "meta", "param", 
               "source", "track", "wbr")

tag_fs <- c(
  setNames(lapply(tags, tag), tags),
  setNames(lapply(void_tags, void_tag), void_tags)
)
tag_fs$p("Some text.", tag_fs$b("Some bold text"),
         tag_fs$i("Some italic text"))

with_html <- function(code) {
  eval(substitute(code), tag_fs)
}
with_html(body(
  h1("A heading", id = "first"),
  p("Some text &", b("some bold text.")),
  img(src = "myimg.png", width = 100, height = 100)
))
