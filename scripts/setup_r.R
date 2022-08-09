# boostrap renv
install.packages(c('remotes', 'renv'), repos = 'https://cran.rstudio.com/')

# install depedencies from renv (all apps share one renv.lock)
options(renv.consent = TRUE)
renv::restore(lockfile = 'wordbank-shiny/renv.lock', prompt = FALSE)
