Homework 2 on Newton’s methods
================
Leave your name and uni here
Due: 03/18/2020, Wednesday, by 1pm

# Problem 1

Design an optimization algorithm to find the minimum of the continuously
differentiable function \(f(x) =-e^{-1}\sin(x)\) on the closed interval
\([0,1.5]\). Write out your algorithm and implement it into .

# Answer: your answer starts here…

``` r
#R codes:

f = function(x){
  return(-sin(x)*exp(-x))
}


optimize(f, interval = c(0, 1.5))
```

    ## $minimum
    ## [1] 0.7854043
    ## 
    ## $objective
    ## [1] -0.3223969

``` r
curve(f, from = 0, to = 1.5)
```

<img src="homework-2-Optimization_files/figure-gfm/unnamed-chunk-2-1.png" width="90%" />

``` r
w = .618
a =  0
b = 1.5
tol = 1e-6
i = 0  # iteration index
x1 = (1 - w) * (b - a) + a
x2 = x1 + w * (b - a) * (1 - w)
res = c(a, b, x1, x2)

findmin = function() {
  while (abs(b - a) > tol) {
    i <- i + 1
    if (f(b) > f(a)) {
      b <- x2
      x1 <- (1 - w) * (b - a) + a
      x2 <- x1 + w * (b - a) * (1 - w)
    }
    else {
      a <- x1
      x1 <- x2
      x2 <- x1 + w * (b - a) * (1 - w)
    }
    res <- rbind(res, c(a, b, x1, x2))
  }

    return(res)
}

min = last(findmin())
```

The minimum according to the golden ratio search method is 0.785. This
closely approximates the result of optimize(), 0.785.

# Problem 2

The Poisson distribution is often used to model \`\`count’’ data — e.g.,
the number of events in a given time period.  
The Poisson regression model states that
\[Y_i \sim \textrm{Poisson}(\lambda_i),\] where
\[\log \lambda_i = \alpha + \beta x_i \] for some explanatory variable
\(x_i\). The question is how to estimate \(\alpha\) and \(\beta\) given
a set of independent data
\((x_1, Y_1), (x_2, Y_2), \ldots, (x_n, Y_n)\).

The Poisson distribution is given by
\[P(Y=y) = \frac{\lambda^y e^{-\lambda}}{y!}\] for \(\lambda > 0\).

# Answer: your answer starts here…

``` r
#R codes:
```

Consider the ABO blood type data, where you have
\(N_{\mbox{obs}} = (N_A,N_B,N_O,N_{AB}) = ( 26, 27, 42, 7)\).

# Answer: your answer starts here…

``` r
#R codes:
```
