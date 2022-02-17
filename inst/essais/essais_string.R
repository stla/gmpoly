p1 <- "-5 x^(0,1,0)-4 x^(2, 1, 1)"
p2 <- "  -  x^(3,4,5) + 5 x^(0,1,0)"
pol1 <- stringToPol(p1)
pol2 <- stringToPol(p2)
pol <- polynomialAdd(pol1, pol2)
polAsString(pol)

p1 <- "1/2 x^(0,1,0) + 4 x^(2, 1, 1)"
p2 <- "x^(3,4,5) + 5 x^(0,1,0)"
pol1 <- stringToPol(p1)
pol2 <- stringToPol(p2)
pol <- polynomialAdd(pol1, pol2)
polAsString(pol)

p1 <- "1/2 x^(0,1,0) + 4 x^(2, 1, 1)"
p2 <- "5 x^(0,0,0)"
pol1 <- stringToPol(p1)
pol2 <- stringToPol(p2)
pol <- polynomialMul(pol1, pol2)
polAsString(pol)

p1 <- "1/2 x^(2) + 4 x^(1)"
p2 <- "5 x^(3)"
pol1 <- stringToPol(p1)
pol2 <- stringToPol(p2)
pol <- polynomialMul(pol1, pol2)
polAsString(pol)
