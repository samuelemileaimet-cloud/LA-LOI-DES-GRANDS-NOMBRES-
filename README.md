# LA-LOI-DES-GRANDS-NOMBRES : application au capteur bruite (cas du thermometre)
''' r 
set.seed(2026)

#   paramtères du capteur

valeur_vraie <-25   # grandeur reelle (ex:25 degres C)
sigma        <-0.5  # ecart-type du bruit du capteur
biais        <-0    # erreur sysytématique (mettre 0.3 pour voir la limite)

simuler_capteur <-function(n, valeur_vraie,sigma, biais =0)
{
  valeur_vraie + biais + rnorm(n, mean = 0, sd = sigma)
}
moyenne_cumulée<- function(x) cumsum(x) / seq_along(x)

#  1. Convergence de la moyenne cumulée (5histoire)
n <- 5000
M <- sapply(1:5,function(i)moyenne_cumulée(simuler_capteur(n, valeur_vraie, sigma, biais)))
matplot(1:n,M, type = "l",lty = 1, log = "x",
        xlab = "Nombre de mesures n (echelle log)", ylab = "moyenne cumulée",
        main = "convergence de la moyenne d'un capteur bruite")
abline(h = valeur_vraie, lty = 2, lwd = 2)

#   2. La dispersion de la moyenne decroit en sigma / racine(n)

tailles <- c(10, 30, 100, 300, 1000, 3000)
n_rep <- 1000
obs   <- sapply(tailles, function(k)
      sd(replicate(n_rep, mean(simuler_capteur(k, valeur_vraie, sigma, biais )))))
disp <- data.frame(n = tailles, observe = obs, théorique = sigma / sqrt(tailles))
print(disp)
dev.new()
plot(disp$n, disp$observe, log = "xy",pch = 19,
     xlab = "nombre de mesures n", ylab = "Ecart-type de la moyenne",
     main = "dispersion de la moyenne : sigma / sqrt(n)")
lines(disp$n, disp$théorique, col = "steelblue", lwd = 2)

#   3. Combien de mesures faut-il ?

epsilon <- 0.05; delta <- 0.05
n_tchebychev <- ceiling(sigma^2 / (epsilon^2 * delta))
n_normale <- ceiling (((qnorm(1- delta / 2 )* sigma ) / epsilon )^2)
cat("\nPrecision +/-", epsilon, "avec un risque de", delta, ":\n",
    " Tchebychev      :", n_tchebychev, "mesures\n",
    " Approx. normale :", n_normale, "mesures\n")

#  4. Verification de la loi faible

moy <- replicate(5000, mean(simuler_capteur(n_normale, valeur_vraie, sigma)))
cat("P(|Mn - mu| >= epsilon) observee :", mean(abs(moy - valeur_vraie) >= epsilon),
    "\nBorne de Tchebychev              :",
    round(sigma^2 / (n_normale * epsilon^2), 3), "\n")

#  5. La limite : un biais n'est pas corrige par la moyenne
x <- simuler_capteur(1e5, valeur_vraie, sigma, biais = 0.3)
cat("\nAvec un biais de 0.3, la moyenne sur 1e5 mesures vaut", round(mean(x), 3),
    "(et non", valeur_vraie, ")\n")
'''

