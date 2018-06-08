make_function <- function(args, body, env = parent.frame()) {
  args <- as.pairlist(args)
  
  eval(call("function", args, body), env)
}
add <- make_function(alist(a = 1, b = 2), quote(a + b))
add(4, 3)
make_function(alist(a = , b = a), quote(a + b))
make_function(alist(a = , b = , ... =), quote(a + b))
adder <- function(x) {
  make_function(alist(y =), substitute({x + y}), parent.frame())
}
adder(10)

curve(sin(exp(4 * x)), n = 1000)
curve2 <- function(expr, xlim = c(0, 1), n = 100, 
                   env = parent.frame()) {
  f <- make_function(alist(x = ), substitute(expr), env)
  
  x <- seq(xlim[1], xlim[2], length = n)
  y <- f(x)
  
  plot(x, y, type = "l", ylab = deparse(substitute(expr)))
}
curve3 <- function(expr, xlim = c(0, 1), n = 100,
                   env = parent.frame()) {
  env2 <- new.env(parent = env)
  env2$x <- seq(xlim[1], xlim[2], length = n)
  
  y <- eval(substitute(expr), env2)
  plot(env2$x, y, type = "l", 
       ylab = deparse(substitute(expr)))
}

z <- quote(y <- x * 10)
deparse(z)
parse(text = deparse(z))
exp <- parse(text = c("
  x <- 4
  x
  5
"))
length(exp)
typeof(exp)
exp[[1]]

simple_source <- function(file, envir = new.env()) {
  stopifnot(file.exists(file))
  stopifnot(is.environment(envir))
  
  lines <- readLines(file, warn = FALSE)
  exprs <- parse(text = lines)
  
  n <- length(exprs)
  if (n == 0L) return(invisible())
  
  for (i in seq_len(n - 1)) {
    eval(exprs[i], envir)
  }
  invisible(eval(exprs[n], envir))
}

recurse_call <- function(x) {
  if (is.atomic(x)) {
    # Return a value
  } else if (is.name(x)) {
    # Return a value
  } else if (is.call(x)) {
    # Call recurse_call recursively
  } else if (is.pairlist(x)) {
    # Call recurse_call recursively
  } else {
    # User supplied incorrect input
    stop("Don't know how to handle type ", typeof(x), 
         call. = FALSE)
  }
}

logical_abbr <- function(x) {
  if (is.atomic(x)) {
    FALSE
  } else if (is.name(x)) {
    identical(x, quote(T)) || identical(x, quote(F))
  } else if (is.call(x) || is.pairlist(x)) {
    for (i in seq_along(x)) {
      if (logical_abbr(x[[i]])) return(TRUE)
    }
    FALSE
  } else {
    stop("Don't know how to handle type ", typeof(x), 
         call. = FALSE)
  }
}

logical_abbr(quote(TRUE))
logical_abbr(quote(T))
logical_abbr(quote(mean(x, na.rm = T)))
logical_abbr(quote(function(x, na.rm = T) FALSE))

find_assign <- function(x) {
  if (is.atomic(x) || is.name(x)) {
    NULL
  } else if (is.call(x)) {
    if (identical(x[[1]], quote(`<-`))) {
      x[[2]]
    } else {
      lapply(x, find_assign)
    }
  } else if (is.pairlist(x)) {
    lapply(x, find_assign)
  } else {
    stop("Don't know how to handle type ", typeof(x), 
         call. = FALSE)
  }
}
find_assign(quote(a <- 1))

find_assign(quote({
  a <- 1
  b <- 2
}))

find_assign2 <- function(x) {
  if (is.atomic(x) || is.name(x)) {
    character()
  } else if (is.call(x)) {
    if (identical(x[[1]], quote(`<-`))) {
      as.character(x[[2]])
    } else {
      unlist(lapply(x, find_assign2))
    }
  } else if (is.pairlist(x)) {
    unlist(lapply(x, find_assign2))
  } else {
    stop("Don't know how to handle type ", typeof(x), 
         call. = FALSE)
  }
}

find_assign2(quote({
  a <- 1
  b <- 2
  a <- 3
}))

find_assign2(quote({
  system.time(x <- print(y <- 5))
}))

find_assign3 <- function(x) {
  if (is.atomic(x) || is.name(x)) {
    character()
  } else if (is.call(x)) {
    if (identical(x[[1]], quote(`<-`))) {
      lhs <- as.character(x[[2]])
    } else {
      lhs <- character()
    }
    
    unique(c(lhs, unlist(lapply(x, find_assign3))))
  } else if (is.pairlist(x)) {
    unique(unlist(lapply(x, find_assign3)))
  } else {
    stop("Don't know how to handle type ", typeof(x), 
         call. = FALSE)
  }
}

find_assign3(quote({
  a <- 1
  b <- 2
  a <- 3
}))

find_assign3(quote({
  system.time(x <- print(y <- 5))
}))

find_assign3(quote({
  l <- list()
  l$a <- 5
  names(l) <- "b"
}))

find_assign4 <- function(x) {
  if (is.atomic(x) || is.name(x)) {
    character()
  } else if (is.call(x)) {
    if (identical(x[[1]], quote(`<-`)) && is.name(x[[2]])) {
      lhs <- as.character(x[[2]])
    } else {
      lhs <- character()
    }
    
    unique(c(lhs, unlist(lapply(x, find_assign4))))
  } else if (is.pairlist(x)) {
    unique(unlist(lapply(x, find_assign4)))
  } else {
    stop("Don't know how to handle type ", typeof(x), 
         call. = FALSE)
  }
}

find_assign4(quote({
  l <- list()
  l$a <- 5
  names(l) <- "b"
}))

a <- 1
b <- 3
bquote(a + b)
bquote(a + .(b))
bquote(.(a) + .(b))
bquote(.(a + b))

bquote2 <- function (x, where = parent.frame()) {
  if (is.atomic(x) || is.name(x)) {
    # Leave unchanged
    x
  } else if (is.call(x)) {
    if (identical(x[[1]], quote(.))) {
      # Call to .(), so evaluate
      eval(x[[2]], where)
    } else {
      # Otherwise apply recursively, turning result back into call
      as.call(lapply(x, bquote2, where = where))
    }
  } else if (is.pairlist(x)) {
    as.pairlist(lapply(x, bquote2, where = where))
  } else {
    # User supplied incorrect input
    stop("Don't know how to handle type ", typeof(x), 
         call. = FALSE)
  }
}

x <- 1
y <- 2
bquote2(quote(x == .(x)))
bquote2(quote(function(x = .(x)) {
  x + .(y)
}))

bquote2(quote(function(x = .(x)) {
  # This is a comment
  x +  # funky spacing
    .(y)
}))