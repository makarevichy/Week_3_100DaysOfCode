x <- quote(read.csv("important.csv", row.names = FALSE))
x[[1]]
is.name(x[[1]])
y <- quote(add(10)(20))
y[[1]]
is.call(y[[1]])
x <- quote(read.csv("important.csv", row.names = FALSE))
x[[2]]
x$row.names
names(x)
length(x) - 1
y <- quote(read.csv("important.csv", row.names = FALSE))
y$row.names <- TRUE
y$col.names <- FALSE
y
y[[2]] <- quote(paste0(filename, ".csv"))
y[[4]] <- NULL
y
y$sep <- ","
y
x[-3]
x[-1]
as.list(x[-1])
m1 <- quote(read.delim("data.txt", sep = "|"))
m2 <- quote(read.delim(s = "|", "data.txt"))
m3 <- quote(read.delim(file = "data.txt", , "|"))
standardise_call(m1); standardise_call(m2); standardise_call(m3)
call(":", 1, 10)
call("mean", quote(1:10), na.rm = TRUE)
as.call(list(quote(mean), quote(1:10)))

(a <- call("mean", 1:10))
(b <- call("mean", quote(1:10)))
identical(a, b)

is_valid_expression <- function(expr) {
  if (is.name(expr)) {
    return(TRUE)
  } else if (is.pairlist(expr) || is.call(expr)) {
    return(all(vapply(expr, is_valid_expression, logical(1))))
  } else if (is.atomic(expr) && length(expr) == 1L) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
is_valid_expression(a)
is_valid_expression(b)
is.call(a)
is.call(b)
do.call('mean', list(1:10))

do.call2 <- function(what, args, env = parent.frame()){
  cc <- as.call(c(as.name(what), as.list(args)))
  eval(cc, env)
}
do.call('mean', list(1:10))

(quote(f), a = 1, b = quote(mean(a)))
#> f(a = 1, b = mean(a))
call(quote('f'), a = 1, b = as.call(list(quote(mean), quote(a))))

make_call(quote(mean), list(quote(x), na.rm = TRUE))
#> mean(x, na.rm = TRUE)
make_call(quote(mean), quote(x), na.rm = TRUE)
#> mean(x, na.rm = TRUE)

make_call <- function(x, ...) {
  as.call(c(x, ...))
}

make_call(quote(mean), list(quote(x), na.rm = TRUE))
make_call(quote(mean), quote(x), na.rm = TRUE)
standardise_call(quote(mean.default(n = T, 1:10)))

#Capturing the current call
f <- function(abc = 1, def = 2, ghi = 3) {
  list(sys = sys.call(), match = match.call())
}
f(d = 2, 2)

mod <- lm(mpg ~ wt, data = mtcars)
update(mod, formula = . ~ . + cyl)

update_call <- function (object, formula., ...) {
  call <- object$call
  
  # Use update.formula to deal with formulas like . ~ .
  if (!missing(formula.)) {
    call$formula <- update.formula(formula(object), formula.)
  }
  
  modify_call(call, dots(...))
}
update_model <- function(object, formula., ...) {
  call <- update_call(object, formula., ...)
  eval(call, parent.frame())
}
update_model(mod, formula = . ~ . + cyl)

f <- function() {
  n <- 3
  lm(mpg ~ poly(wt, n), data = mtcars)
}
mod <- f()
update(mod, data = mtcars)
update_model <- function(object, formula., ...) {
  call <- update_call(object, formula., ...)
  eval(call, environment(formula(object)))
}
update_model(mod, data = mtcars)

write.csv <- function(...) {
  Call <- match.call(expand.dots = TRUE)
  for (arg in c("append", "col.names", "sep", "dec", "qmethod")) {
    if (!is.null(Call[[arg]])) {
      warning(gettextf("attempt to set '%s' ignored", arg))
    }
  }
  rn <- eval.parent(Call$row.names)
  Call$append <- NULL
  Call$col.names <- if (is.logical(rn) && !rn) TRUE else NA
  Call$sep <- ","
  Call$dec <- "."
  Call$qmethod <- "double"
  Call[[1L]] <- as.name("write.table")
  eval.parent(Call)
}
write.csv <- function(x, file = "", sep = ",", qmethod = "double", 
                      ...) {
  write.table(x = x, file = file, sep = sep, qmethod = qmethod, 
              ...)
}
modify_formula <- function(old, new) {
  old_lhs <- old[[2]]
  old_rhs <- old[[3]]
  if (length(new) == 2) {
    new_lhs <- old_lhs
    new_rhs <- new[[2]]
  } else {
    new_lhs <- new[[2]]
    new_rhs <- new[[3]]
  }
  
  new_lhs_ <- gsub(".", deparse(old_lhs), deparse(new_lhs), fixed = TRUE)
  new_rhs_ <- gsub(".", deparse(old_rhs), deparse(new_rhs), fixed = TRUE)
  as.formula(paste(new_lhs_, "~", new_rhs_))
}

modify_formula(y ~ x, ~ . + x2)
update.formula(y ~ x, ~ . + x2)
update.formula(y ~ x, log(.) ~ .)
modify_formula(y ~ x, log(.) ~ .)
update.formula(. ~ u+v, res ~ .)
modify_formula(. ~ u+v, res ~ .)

child_fn <- function() {
  message("I am the child_fn")
  message("My parent is ", sys.call(-1))
  message("My grandparent is ", sys.call(-2))
}

daddy_fn <- function() { child_fn() }
grandpa_fn <- function() { daddy_fn() }
grandpa_fn()