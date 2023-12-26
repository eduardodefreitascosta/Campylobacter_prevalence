
##Packages###

#Packages to be used
packages<-c("readxl","knitr","ggplot2","ggpubr","here",
            "tidyverse","grid","ggridges","ggthemes","extrafont","gridExtra",
            "ggridges","car","emmeans","lsmeans", "multcomp",
            "tidybayes","modelr","extraDistr","coda","rjags","patchwork")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))


dir.create(here("Figures"))
dir.create(here("Outputs"))


#Run the sample selection
source(here("Scripts","amostragem.R"))

#Run the descriptive and inferential analysis
source(here("Scripts","Campy.R"))
