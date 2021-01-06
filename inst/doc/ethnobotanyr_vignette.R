## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

load("ethnobotanydata.rda")
library(dplyr)
library(ethnobotanyR)
library(ggalluvial)
library(ggplot2)
library(ggridges)
library(magrittr)
# in case of rendering issues render with 
# rmarkdown::render('vignettes/ethnobotanyr_vignette.Rmd', output_file='ethnobotanyr_vignette.html', output_dir='vignettes')


## ---- echo= FALSE-------------------------------------------------------------
knitr::kable(head(ethnobotanydata), digits = 2, caption = "First six rows of the example ethnobotany data included with ethnobotanyR")

## ----URs----------------------------------------------------------------------
ethnobotanyR::URs(ethnobotanydata)

## ----URsum--------------------------------------------------------------------
ethnobotanyR::URsum(ethnobotanydata)

## ----CIs----------------------------------------------------------------------
ethnobotanyR::CIs(ethnobotanydata)

## ----FCs----------------------------------------------------------------------
ethnobotanyR::FCs(ethnobotanydata)

## ----NUs----------------------------------------------------------------------
ethnobotanyR::NUs(ethnobotanydata)

## ----RFCs---------------------------------------------------------------------
ethnobotanyR::RFCs(ethnobotanydata)

## ----RIs----------------------------------------------------------------------
ethnobotanyR::RIs(ethnobotanydata)

## ----UVs----------------------------------------------------------------------
ethnobotanyR::UVs(ethnobotanydata)

## ----CVe----------------------------------------------------------------------
ethnobotanyR::CVe(ethnobotanydata)

## ----FLs----------------------------------------------------------------------
ethnobotanyR::FLs(ethnobotanydata)

## ---- fig.width=7, fig.height=7-----------------------------------------------
URs_plot <- ethnobotanyR::Radial_plot(ethnobotanydata, ethnobotanyR::URs)

NUs_plot <- ethnobotanyR::Radial_plot(ethnobotanydata, ethnobotanyR::NUs)

FCs_plot <- ethnobotanyR::Radial_plot(ethnobotanydata, ethnobotanyR::FCs)

CIs_plot <- ethnobotanyR::Radial_plot(ethnobotanydata, ethnobotanyR::CIs)

cowplot::plot_grid(URs_plot, NUs_plot, FCs_plot, CIs_plot, 
    labels = c('URs', 'NUs', 'FCs', 'CIs'), 
    nrow = 2, 
    align="hv",
    label_size = 12)

## ---- fig.width=7, fig.height=7-----------------------------------------------
Chord_sp <- ethnobotanyR::ethnoChord(ethnobotanydata, by = "sp_name")

## ---- fig.width=7, fig.height=7-----------------------------------------------
Chord_informant <- ethnobotanyR::ethnoChord(ethnobotanydata, by = "informant")

## ---- fig.width=7, fig.height=7-----------------------------------------------
ethnobotanyR::ethno_alluvial(ethnobotanydata)

## ---- fig.width=7, fig.height=7-----------------------------------------------

# correct internal assignment for stat = "stratum" 
  StatStratum <- ggalluvial::StatStratum

ethnobotanyR::ethno_alluvial(ethnobotanydata, alpha = 0.2) + 
  ggplot2::theme(legend.position = "none") +  
             ggplot2::geom_label(stat = "stratum", 
                      ggplot2::aes(label = ggplot2::after_stat(stratum)))


## ----ethno_boot_uses----------------------------------------------------------
sp_a_data <- ethnobotanydata %>% filter(sp_name == "sp_a") 

sp_a_use <- ethno_boot(sp_a_data$Use_3, statistic = mean, n1 = 1000)

sp_b_data <- ethnobotanydata %>% filter(sp_name == "sp_b") 

sp_b_use <- ethno_boot(sp_b_data$Use_3, statistic = mean, n1 = 1000)


## ----ethno_boot_URs-----------------------------------------------------------
quantile(sp_a_use, c(0.05, 0.95))
quantile(sp_b_use, c(0.05, 0.95))

## ----fig.width=7, fig.height=7------------------------------------------------
boot_data <- data.frame(sp_a_use, sp_b_use)

ethno_boot_melt <- reshape2::melt(boot_data)

## ----plot_boot_ridges, fig.width=7, fig.height=4------------------------------
ggplot2::ggplot(ethno_boot_melt, aes(x = value, 
                y = variable, fill = variable)) +
                ggridges::geom_density_ridges() +
                ggridges::theme_ridges() + 
                theme(legend.position = "none") +
                labs(y= "", x = "Example Bayesian bootstraps of three use categories")

## ----ethno_sp_a---------------------------------------------------------------
ethno_sp_a <- dplyr::filter(ethnobotanydata, sp_name == "sp_a")

## ----answers------------------------------------------------------------------
  answers <- 2

## ----ethno_compet_sp_a--------------------------------------------------------
ethno_compet_sp_a <- dplyr::recode(ethno_sp_a$informant, 
    inform_a = 0.9,inform_b = 0.5,inform_c = 0.5,
    inform_d = 0.9, inform_e = 0.9, inform_f = 0.5,
    inform_g = 0.7,inform_h = 0.5,inform_i = 0.9,
    inform_j= 0.9, inform_eight = 0.9,inform_five = 0.6,
    inform_four = 0.5,inform_nine = 0.9, 
    inform_one = 0.5, inform_seven = 0.5,
    inform_six= 0.9, inform_ten = 0.9, 
    inform_three = 0.9, inform_two = 0.5)

## ----ethno_sp_a_bayes---------------------------------------------------------
ethno_sp_a_bayes <- ethnobotanyR::ethno_bayes_consensus(ethno_sp_a, 
                    answers = 2,
                    #here we keep the default normal distribution with `prior = -1`
                    prior_for_answers = ethno_compet_sp_a) 

## ----heatmap------------------------------------------------------------------
heatmap(ethno_sp_a_bayes)

## ----ethno_sp_a_rich----------------------------------------------------------
set.seed(123) #make random number reproducible
ethno_sp_a_rich <- data.frame(replicate(3,sample(0:10,20,rep=TRUE)))
  names(ethno_sp_a_rich) <- 
  gsub(x = names(ethno_sp_a_rich), 
  pattern = "X", replacement = "Use_")  
  ethno_sp_a_rich$informant <- sample(c('User_1', 'User_2'), 
  20, replace=TRUE)
  ethno_sp_a_rich$sp_name <- sample(c('sp_a'), 
  20, replace=TRUE)

## -----------------------------------------------------------------------------
ethno_compet_sp_a_rich <- 
          dplyr::recode(ethno_sp_a_rich$informant,
          User_1 = 0.9, User_2 = 0.5)

## ----fig.width=7, fig.height=7------------------------------------------------
ethno_sp_a_bayes <- ethnobotanyR::ethno_bayes_consensus(ethno_sp_a_rich,
       answers = 10, 
       prior_for_answers = ethno_compet_sp_a_rich, 
       prior=-1) #keep a normal prior in this example with -1

## ----fig.width=7, fig.height=7------------------------------------------------
ethno_sp_a_bayes_melt <-  ethno_sp_a_bayes %>%
  as.data.frame() %>%
  reshape2::melt()

## ----fig.width=7, fig.height=4------------------------------------------------
ggplot2::ggplot(ethno_sp_a_bayes_melt, aes(x = value, 
                y = variable, fill = variable)) +
                ggridges::geom_density_ridges() +
                ggridges::theme_ridges() + 
                theme(legend.position = "none")+
                labs(y= "", x = "Example ethno_bayes_consensus of use categories for sp_a")

