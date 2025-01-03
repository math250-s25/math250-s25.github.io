
A <- matrix(c(1, 0, -1, 1), nrow = 2)
v <- svd(A)$v
u <- svd(A)$u
d <- diag(svd(A)$d)



par(mfrow = c(1,3), pty="s")
plot(NA, NA, xlim = c(-2, 2), ylim = c(-2, 2), xlab = "", ylab = "",
     main = "v1 and v2")
arrows(0, 0, v[1, 1], v[2, 1], length = .02, col = "red")
arrows(0, 0, v[1, 2], v[2, 2], length = .02, col = "red")

plot(NA, NA, xlim = c(-2, 2), ylim = c(-2, 2), xlab = "", ylab = "",
     main = "scaled v1 and v2")
arrows(0, 0, (v %*% d)[1, 1], (v %*% d)[2, 1], length = .02, col = "green")
arrows(0, 0, (v %*% d)[1, 2], (v %*% d)[2, 2], length = .02, col = "green")

plot(NA, NA, xlim = c(-2, 2), ylim = c(-2, 2), xlab = "", ylab = "",
     main = "scaled and rotated v1 and v2")
arrows(0, 0, (A %*% v)[1, 1], (A %*% v)[2, 1], length = .02, col = "purple")
arrows(0, 0, (A %*% v)[1, 2], (A %*% v)[2, 2], length = .02, col = "purple")
arrows(0, 0, (u)[1, 1], (u)[2, 1], length = .02, col = "gold")
arrows(0, 0, (u)[1, 2], (u)[2, 2], length = .02, col = "gold")


# Check orthogonality
v[, 1] %*% v[, 2]
u[, 1] %*% u[, 2]

# compare trace A^T A and sum of squared singular values of A
sum(diag(A %*% t(A)))
sum(d ^ 2)

