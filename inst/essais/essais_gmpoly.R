library(gmpoly)

pol <- gmpoly("4 x^(2, 1, 1) + 1/2 x^(0,1,0)")
+pol
-pol
2 * pol
pol / 2
pol + 5
pol - 5
pol^2
pol1 <- gmpoly("2 x^(1,1) - 5/3 x^(0,1)")
pol2 <- gmpoly("-2 x^(1,1) + 3 x^(2,1)")
pol1 + pol2
pol1 * pol2

polAsString(stringToPol("4 x^(3,4) + 2"))
