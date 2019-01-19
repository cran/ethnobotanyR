## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
load("ethnobotanydata.rda")

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

