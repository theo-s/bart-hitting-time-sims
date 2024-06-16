# If needed, install vthemes from Yu-Group website
# devtools::install_github("Yu-Group/vthemes")

library(ggplot2)
library(dplyr)
library(ggeasy)
library(vthemes)

use_saved_data = TRUE

all_plots = list()

iteration = 1
for (dgp in c("sum","high", "low", "piecewise", "tree", "lss")) {
  
  
  if (use_saved_data) {
    results <- read.csv(paste0("~/Desktop/bart-hitting-time-sims/results/cached_results/exp3_coverage_medium",dgp,"_rmse.csv"))
  } else {
    
    results <- data.frame(n = NA,
                          nchain = NA,
                          run = NA,
                          exp = 1,
                          rmse = NA,
                          cov = NA)
    
    for (file in dir("results_final/coverage6/")) {
      if (grepl(pattern = dgp, x = file)) {
        #print(file)
        f <- readRDS(file = paste0("results_final/coverage6/",file))
        
        for (iter in 1:3) {
          n_i = f[[1]]$n
          run_i = f[[1]]$run
          nchain_i =f[[1]]$nchain
          if (is.na(n_i) | is.na(run_i) | is.na(nchain_i)) {
            print(file)
            print(f)
          }
          if (iter == 1) {
            results <- rbind(results,
                             c(n_i, nchain_i, run_i, 1,
                               f[[iter]]$rmse, f[[iter]]$empirical_cov))
          } else if (iter == 2) {
            results <- rbind(results,
                             c(n_i, 10, run_i, 1,
                               f[[iter]]$rmse, f[[iter]]$coverage))
          } else if (iter == 3) {
            results <- rbind(results,
                             c(n_i, 20, run_i, 1,
                               f[[iter]]$rmse, f[[iter]]$coverage))
          }
          
        }
      }
    }
    #results = results %>% filter(nchain > 1)
    for (file in dir("results/coverage/")) {
      if (grepl(pattern = dgp, x = file)) {
        #print(file)
        f <- readRDS(file = paste0("results/coverage/",file))
        
        for (iter in 1:3) {
          n_i = f[[1]]$n
          run_i = f[[1]]$run
          nchain_i =f[[1]]$nchain
          if (is.na(n_i) | is.na(run_i) | is.na(nchain_i)) {
            print(file)
            print(f)
          }
          if (iter == 1) {
            # results <- rbind(results,
            #                  c(n_i, nchain_i, run_i, 1,
            #                    f[[iter]]$rmse, f[[iter]]$empirical_cov))
          } else if (iter == 2) {
            results <- rbind(results,
                             c(n_i, 2, run_i, 1,
                               f[[iter]]$rmse, f[[iter]]$coverage))
          } else if (iter == 3) {
            results <- rbind(results,
                             c(n_i, 5, run_i, 1,
                               f[[iter]]$rmse, f[[iter]]$coverage))
          }
          
        }
      }
    }
    
    write.csv(results, paste0("~/Desktop/bart-hitting-time-sims/results/cached_results/exp3_coverage_medium",dgp,"_rmse.csv"), row.names = FALSE)
  }
  
  if(dgp=="lss_") {
    dgp = "lss"
  }
  
  results[-1,] %>%
    group_by(n, nchain, exp) %>%
    summarise(reps_completed = length(unique(run)),
              max_rep = max(run)) -> num_runs
  print(dgp)
  print(num_runs, n=100)
  
  dgp_map =list("sum" = "Sum",
                "high" = "High",
                "low" = "Low",
                "piecewise" = "Piecewise Linear",
                "tree" = "Tree",
                "lss" = "Local Sparse Spiky")
  
  results[-1,] %>%
    filter(n>100) %>%
    filter(nchain < 20) %>% 
    group_by(n, nchain, exp) %>%
    summarise(mean_rmse = mean(rmse),
              mean_coverage = mean(cov),
              sd_coverage = sd(cov)/10) %>%
    dplyr::select(n, nchain, mean_coverage,sd_coverage) %>%
    mutate(nchain = as.factor(nchain)) %>%
    ggplot(aes(x = n, y = mean_coverage))+
    ggplot2::geom_point(aes(color = nchain)) +
    ggplot2::geom_line(aes(color = nchain)) +
    vthemes::theme_vmodern() +
    #vthemes::scale_color_vmodern(discrete = FALSE)+
    labs(y = "Empirical Coverage", title = paste0("",dgp_map[[dgp]]))+
    geom_errorbar(aes(ymin = mean_coverage - 1.96*sd_coverage, ymax = mean_coverage + 1.96*sd_coverage, color = nchain), width = 0)+
    scale_color_manual(values = c("1"="turquoise1", "2" = "steelblue1" , "5" = "steelblue3", "10"="royalblue1"),name = "Chains")+
    theme(axis.line = element_line(color='black'),
          panel.background = element_rect(fill = 'white', color = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          text = element_text(family = "Times"),
          panel.border = element_blank(),
          axis.title=element_text(size=12),
          axis.text=element_text(size=10))+
    scale_x_continuous(labels = function(x){return(paste0(as.character(x/1e3), "K"))})+
    scale_y_continuous(labels = function(x){return(paste0(as.character(x*100), "%"))})+
    geom_hline(yintercept = .95,linetype="dashed",
               color = "red") -> plot_i
  
  all_plots[[iteration]] = plot_i
  ggsave(plot_i,filename = paste0("~/Desktop/bart-hitting-time-sims/results/figures/exp3_medium/",dgp,"coverage_medium.pdf"), height = 2.5, width = 3)
  
  iteration <- iteration+1
}

library(gridExtra)

p_final = grid.arrange(all_plots[[1]]+theme(legend.position ="none"),
                       all_plots[[2]]+theme(legend.position ="none",axis.title.y=element_blank()),
                       all_plots[[3]]+theme(axis.title.y=element_blank()),
                       all_plots[[4]]+theme(legend.position ="none"),
                       all_plots[[5]]+theme(legend.position ="none",axis.title.y=element_blank()),
                       all_plots[[6]]+theme(axis.title.y=element_blank()), 
                       widths =c(4,4,5.5),
                       ncol = 3)
ggsave(p_final,filename = paste0("~/Desktop/bart-hitting-time-sims/results/figures/exp3_medium/all_coverages_medium.pdf"), height = 5, width = 8)
