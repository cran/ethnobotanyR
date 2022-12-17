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


## -----------------------------------------------------------------------------
citation("ethnobotanyR")

## ---- echo= FALSE-------------------------------------------------------------
knitr::kable(head(ethnobotanydata), digits = 2, caption = "First six rows of the example ethnobotany data included with ethnobotanyR")

## ---- fig.width=7, fig.height=7, echo=FALSE-----------------------------------
Chord_sp <- ethnobotanyR::ethnoChord(ethnobotanydata, by = "sp_name")

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


