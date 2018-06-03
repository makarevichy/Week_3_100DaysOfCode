library(lattice)
xyplot(mpg ~ disp, data = mtcars)

x <- quote(mpg)
y <- quote(disp)
xyplot(x ~ y, data = mtcars)

a <- 1
b <- 2
substitute(a + b + z)

f <- function() {
  a <- 1
  b <- 2
  substitute(a + b + z)
}
f()

library(pryr)
a <- 1
b <- 2
subs(a + b + z)
subs(a + b, list(a = "y"))
subs(a + b, list(a = quote(y)))
subs(a + b, list(a = quote(y())))
subs(a + b, list("+" = quote(f)))
subs(a + b, list("+" = quote(`*`)))

x <- quote(mpg)
y <- quote(disp)
subs(xyplot(x ~ y, data = mtcars))
xyplot2 <- function(x, y, data = data) {
  substitute(xyplot(x ~ y, data = data))
}
xyplot2(mpg, disp, data = mtcars)

xyplot3 <- function(x, y, ...) {
  substitute(xyplot(x ~ y, ...))
}
xyplot3(mpg, disp, data = mtcars, col = "red", aspect = "xy")

x <- quote(a + b)
substitute(x, list(a = 1, b = 2))

substitute_q <- function(x, env) {
  call <- substitute(substitute(y, env), list(y = x))
  eval(call)
}

x <- quote(a + b)
substitute_q(x, list(a = 1, b = 2))

subs(a + b + c, list('+' = quote(`*`)))
subs(f(g(a, b), c), list(g = quote(`+`), f = quote(`*`)))
subs(f(a < b, c, d), list(f = quote(`if`)))

nl <- function(...) {
  dots <- pryr::named_dots(...)
  lapply(dots, eval, parent.frame())
}
nl(1, 2 + 2, mean(c(3, 5)))

#Expressions
x <- 4
y <- x * 10
y
z <- quote(y <- x * 10)
z
ast(y <- x * 10)
ast("a")
identical(1, quote(1))
identical("test", quote("test"))
ast(f())
ast(f(1, 2))
ast(f(a, b))
ast(f(g(), h(1, a)))
ast(if (x > 1) x else 1/x)
ast(function(x = 1, y) x)
ast(function(x = 1, y = x * 2) {x / y})
str(quote(a))
str(quote(a + b))
class_df <- substitute(class(df), list(df = data.frame(x = 10)))
class_df
eval(class_df)
pryr::ast(`if`(FALSE, "first",
               `if`(TRUE, "second",
                    `if`(TRUE, "third", "fourth"))))
# for ast(x + y %+% z)
# y %+% z will be calculated first and the result will be added to x
pryr::ast(x + y %+% z)

# for ast(x ^ y %+% z)
# x^y will be calculated first, and the result will be used as first argument of `%+%()`
pryr::ast(x ^ y %+% z)

f <- function(x) 10
formals(f)$x
is.name(formals(f)$x)
as.character(formals(f)$x)
missing_arg <- formals(f)$x
# Doesn't work!
is.name(missing_arg)

g <- function(x = 20, y) {
  x + y
}
formals(g) <- alist(x = , y = 10)
get2 <- function(cond){
  eval(as.name(cond))
}
get2('sum')
assign2 <- function(x, value){
  eval(substitute(x <- value,list(x = as.name(x), value = value)))
}
a <- 10
assign2('a', 20)