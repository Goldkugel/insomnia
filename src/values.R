library(stringr)
library(dplyr)
library(behavr)
library(tidyverse)
library(zeitgebr)
library(ggetho)
library(ggplot2)
library(ggpubr)

ZERO = 0
ONE = 1
TWO = 2
THREE = 3
FOUR = 4
FIVE = 5
TWENTY = 20

SEC_PER_MIN <- 60
MIN_PER_HOUR <- 60
HOUR_PER_DAY <- 24

SEC_PER_HOUR <- MIN_PER_HOUR * SEC_PER_MIN
SEC_PER_DAY <- SEC_PER_HOUR * HOUR_PER_DAY