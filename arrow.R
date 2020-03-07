# draw arrow in R plot

x = seq(-20,20,by=0.1)
y = sin(x)/x
plot(x,y, type = "l")
text(-15,0.7,expression(frac(sin(x),x)))
arrows(11,0.3,8,0.15, length=0.1)
text(11,0.45,"2nd\nmaximum") # \n means "newline"

# other
x = seq(-20,20,by=0.1)
y = sin(x)/x
plot(x,y, type = "l")
text(-15,0.7,expression(frac(sin(x),x)))
arrows(x0 = -9.5, y0 = 0.36698, x1 = -8, y1 = 0.12, length = 0.1)
text(-9.5,0.45,"2nd\nmaximum", col = 'red', cex = 0.8, font = 2) # \n means "newline"
grid()
