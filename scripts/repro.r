unload <- function(pkg) {
	name <- paste0("package:", pkg)
	detach(name, unload = TRUE, character.only = TRUE)
}
reload <- function(pkg) {
	unload(pkg)
	library(pkg, character.only = TRUE)
}
reinstall <- function() {
	remotes::install_local(".", force = TRUE, upgrade = "never")
	reload("daveanalysis")
}

reinstall()
dave_config(log_level = 4, log_file = "test.log")
daveanalysis:::log_info("test msg")
