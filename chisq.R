white_control <- 119 
non_white_control <- 410 -white_control
white_treatment <- 116
non_white_treatment <- 413-white_treatment

x <- matrix(c(white_control, white_treatment,  non_white_control, non_white_treatment), ncol =2, byrow = F)

chisq.test(x, correct = FALSE)$p.value

chisq.test(all_variables$White, all_variables$Group, correct = FALSE)$p.value



