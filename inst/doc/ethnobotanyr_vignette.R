## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

load("ethnobotanydata.rda")

## ---- echo= FALSE--------------------------------------------------------
knitr::kable(head(ethnobotanydata), digits = 2, caption = "First six rows of the example ethnobotany data included with ethnobotanyR")

## ------------------------------------------------------------------------
ethnobotanyR::URs(ethnobotanydata)

## ------------------------------------------------------------------------
ethnobotanyR::URsum(ethnobotanydata)

## ------------------------------------------------------------------------
ethnobotanyR::CIs(ethnobotanydata)

## ------------------------------------------------------------------------
ethnobotanyR::FCs(ethnobotanydata)

## ------------------------------------------------------------------------
ethnobotanyR::NUs(ethnobotanydata)

## ------------------------------------------------------------------------
ethnobotanyR::RFCs(ethnobotanydata)

## ------------------------------------------------------------------------
ethnobotanyR::RIs(ethnobotanydata)

## ------------------------------------------------------------------------
ethnobotanyR::UVs(ethnobotanydata)

## ---- fig.width=7, fig.height=7------------------------------------------
ethnobotanyR::ethnoChord(ethnobotanydata)

## ---- fig.width=7, fig.height=7------------------------------------------
ethnobotanyR::ethnoChordUser(ethnobotanydata)

