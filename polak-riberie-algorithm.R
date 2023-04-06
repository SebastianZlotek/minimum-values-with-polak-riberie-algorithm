#install.packages("numDeriv")
#install.packages('rgl')

fn1 <- function(x, y) {
  return(sin(3 * pi * x)^2 + (x - 1)^2 * (1 + sin(3 * pi * y)^2) + (y - 1)^2 * (1 + sin(2 * pi * y)^2))
} # Funkcja na dwoch zmiennych x,y potrzebna do dzialania wykresu

fn2 <- function(x) {
  return(sin(3 * pi * x[1])^2 + (x[1] - 1)^2 * (1 + sin(3 * pi * x[2])^2) + (x[2] - 1)^2 * (1 + sin(2 * pi * x[2])^2))
} # Funkcja na dwoch zmiennych x[1],x[2] potrzebna do dzialania algorytmu

# Algorytm Ternary
ternary <- function(f, lower, upper, tol) {
  f.lower <- f(lower)
  f.upper <- f(upper)
  while (abs(upper - lower) > 2 * tol) {
    x1 <- (2 * lower + upper) / 3
    f.x1 <- f(x1)
    x2 <- (lower + 2 * upper) / 3
    f.x2 <- f(x2)
    if (f.x1 < f.x2) {
      upper <- x2
      f.upper <- f.x2
    } else {
      lower <- x1
      f.lower <- f.x1
    }
  }
  return((upper + lower) / 2)
}

library(rgl) # Biblioteka do wykresow

library(numDeriv) # Biblioteka do funkcji grad

# Algorytm Polaka-Riberie'go
polak_riberie <- function(f, x, tol) {
  beta <- 1
  d <- -grad(f,x)
  i <- 0
  repeat {
    g <- function(a) {f(x + a * d)}
    step <- ternary(g,0,5,tol)
    new.x <- x + step * d
    if (dist(rbind(new.x,x)) < tol) {
      cat("Wektor: ", d, "\n Krok: ", step, 
          "\n Beta: ", beta,"\n Iteracja: ", i, "\n Punkt: ", new.x,"\n\n")
      return(new.x)
    }
    beta <- grad(f,new.x) %*% ((grad(f,new.x))-(grad(f,x))) / (grad(f,x) %*% grad(f,x)) 
             d <- -grad(f,new.x) + as.vector(beta) * d
             x <- new.x
             cat("Wektor: ", d, "\n Krok: ", step,
                 "\n Beta: ", beta,"\n Iteracja: ", i, "\n Punkt: ", new.x,"\n\n")
             i <- i + 1
  }
}

tol <- 1e-15 # Ustalam tolerancje

punkt <- c(10201,10201) # Ustalam punkt początkowy

polak_riberie(fn2, punkt, tol) # Wczytuje algorytm z danymi


### Przechodze do wizualizacji ###

x <- seq(-8, 8, by = 0.01) # Wybieram zakres x w jakim ma dzialac funkcja

y <- seq(-8, 8, by = 0.01) # Wybieram zakres y w jakim ma dzialac funkcja

wartosc <- outer(x, y, fn1) # Wartosci funkcji

persp3d(x, y, wartosc, col = "blue") # Wpisuje dane na wykres

rgl.light(x=5, y=5, z=5, ambient="white", diffuse="white") # Rozjasniam wykres

minimum_funkcji <- c(1,1,0) # Wpisuje do wektora minimum funkcji

rgl.spheres(minimum_funkcji, radius=3, color="red") # Zaznaczam minimum na wykresie



### Podaje mniejszy zakres ###

x <- seq(-2, 2, by = 0.01) # Wybieram mniejszy zakres x w jakim ma dzialac funkcja

y <- seq(-2, 2, by = 0.01) # Wybieram mniejszy zakres y w jakim ma dzialac funkcja

wartosc <- outer(x, y, fn1) # Wartosci funkcji

persp3d(x, y, wartosc, col = "blue") # Wpisuje dane na wykres


rgl.light(x=2, y=2, z=2, ambient="white", diffuse="white") # Rozjasniam wykres

minimum_funkcji <- c(1,1,0) # Wpisuje do wektora minimum funkcji

rgl.spheres(minimum_funkcji, radius=0.5, color="red") # Zaznaczam minimum na wykresie





























