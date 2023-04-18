library(tidyverse)
library(lavaan)
library(semTools)
library(furrr)




readRDS("N:/durable/projects/scq_efa/CFA/data_dx_NPRupdate.rds")
source("N:/durable/projects/scq_efa/rename_items.r")

######### MI - sex ##########
#sex 1 = male, 2 = female

#model
scq_5 <- '
Idosyncratic_speech =~ NN152 + NN153 + NN154 + NN155 + NN156

Repetitive_and_restrictive_behaviors =~ NN159 + NN157 + NN160 + NN161 + NN162 + NN163 + NN164 + NN165 + NN166 + NN167

Social_reciprocity =~ NN151 + NN158  +  NN168 + NN169 + NN175 + NN176  + NN177 + NN178 + NN179 + NN180 + NN182 + NN185 + NN186 + NN187 + NN189

Social_initiative =~  NN170 + NN171 + NN172 + NN173  + NN181

Play =~ NN183 + NN184 + NN188'



#running strong/strict invariance - hashed out strict first go round
mi.test <- list(strong = c("thresholds","loadings","intercepts"), # new parameters held equal for each level of invairance
                 strict = "residuals")

#code based on https://rdrr.io/cran/semTools/man/measEq.syntax.html
mi.list <- list()
for (i in 0:length(mi.test)) { #for each item in list (strong/strict) from 0-length of list (3) run the model with specified constraints 
  if (i == 0L) { #when 0 run configural model
    meq.label <- "configural"
    group.equal <- ""
    long.equal <- ""
  } else {
    meq.label <- names(mi.test)[i]
    group.equal <- unlist(mi.test[1:i]) # run specified constrains for level of invairance and all above
  }
  mi.list[[meq.label]] <- measEq.syntax(configural.model = scq_5,
                                         data = data_dx,
                                         ordered = c(grep("NN",colnames(data), value = TRUE)), 
                                         parameterization = "theta",
                                         ID.fac = "std.lv",
                                         ID.cat = "Wu.Estabrook.2016",
                                         group = "SEX",
                                         group.equal = group.equal,
                                        estimator = "WLSMV",
                                         missing = "pairwise",
                                         return.fit = TRUE)
}



mi.results <- compareFit(mi.list)

mod_ind <- modificationindices(mi.list$strict) %>%
  filter(op == "~~") 


#partial based on ModI in strict

mi.test_partial <- list(strict = "residuals")

mi.list_p <- list()
for (i in 0:length(mi.test_partial)) {
  if (i == 0L) {
    meq.label <- "configural"
    group.equal <- ""
    long.equal <- ""
  } else {
    meq.label <- names(mi.test_partial)[i]
    group.equal <- unlist(mi.test_partial[1:i])
  }
  mi.list_p[[meq.label]] <- measEq.syntax(configural.model = scq_5,
                                        data = data_dx,
                                        ordered = c(grep("NN",colnames(data), value = TRUE)),
                                        parameterization = "theta",
                                        ID.fac = "std.lv",
                                        ID.cat = "Wu.Estabrook.2016",
                                        group = "SEX",
                                        group.equal = group.equal,
                                        group.partial = c("NN162 ~~ NN162"), #freed prameter 
                                        estimator = "WLSMV",
                                        missing = "pairwise",
                                        return.fit = TRUE)
}

fitmeasures(mi.list_p$strict)

#########MI - autism dx group ##########


preg_ids <- data_dx %>%
  filter(autismDX_new == 0) %>%
  .$BARNid #ids of those without an autism dx in NPR, combination of preg_id and BARN_NR

n_asdDX <-  data_dx %>%
  filter(autismDX_new == 1) %>%
  nrow() #n of those with a dx in NPR

#creating subsample dataframe for autism dx MI 

set.seed(555)
random_preg_list <- as.list(c(seq(1:100))) #setting up list for 100 random subsets. Randomly sampled in next line from the nodx group
subset_ids <- map(random_preg_list, ~sample(preg_ids, n_asdDX*6)) #6:1 nodx:dx was the most even without converange issue in the no dx group, listing nodx ids for each subset
subsample_data <- tibble(subset_ids)
subsample_data <- subsample_data %>% 
  mutate(dataset = map(subset_ids, ~filter(data_dx, (autismDX_new == 1) | (BARNid %in% .x)))) #subseting data with random sample from nodx group and all of dx indv. included 

saveRDS(subsample_data, "subsample_data_NPRupdate_6x.rds")

##function to run MI## 

run_MI <- function(data){
scq_5 <- '
Idosyncratic_speech =~ NN152 + NN153 + NN154 + NN155 + NN156

Repetitive_and_restrictive_behaviors =~ NN159 + NN157 + NN160 + NN161 + NN162 + NN163 + NN164 + NN165 + NN166 + NN167

Social_reciprocity =~ NN151 + NN158  +  NN168 + NN169 + NN175 + NN176  + NN177 + NN178 + NN179 + NN180 + NN182 + NN185 + NN186 + NN187 + NN189

Social_initiative =~  NN170 + NN171 + NN172 + NN173  + NN181

Play =~ NN183 + NN184 + NN188'
  
mi.test_dx <- list(strong = c("thresholds","loadings","intercepts"),
                     strict = "residuals")
  
  mi.list_dx <- list()
  for (i in 0:length(mi.test_dx)) {
    if (i == 0L) {
      meq.label <- "configural"
      group.equal <- ""
      long.equal <- ""
    } else {
      meq.label <- names(mi.test_dx)[i]
      group.equal <- unlist(mi.test_dx[1:i])
    }
    mi.list_dx[[meq.label]] <- measEq.syntax(configural.model = scq_7,
                                             data = data,
                                             ordered = c(grep("NN",colnames(data), value = TRUE)),
                                             parameterization = "theta",
                                             ID.fac = "std.lv",
                                             ID.cat = "Wu.Estabrook.2016",
                                             group = "autismDX",
                                             group.equal = group.equal,
                                             missing = "pairwise",
                                             return.fit = TRUE)

    

     }
  
  #pulling out results
  fit_configural <- fitmeasures(mi.list_dx$configural, fit.measures = c("cfi.scaled","tli.scaled", "srmr.scaled", "rmsea.scaled", "mfi", "chisq.scaled", "df.scaled"))
  fit_strong <- fitmeasures(mi.list_dx$strong, fit.measures = c("cfi.scaled","tli.scaled", "srmr.scaled", "rmsea.scaled", "mfi", "chisq.scaled", "df.scaled"))
  fit_means <- fitmeasures(mi.list_dx$means, fit.measures = c("cfi.scaled","tli.scaled", "srmr.scaled", "rmsea.scaled", "mfi", "chisq.scaled", "df.scaled"))
  fit_strict <- fitmeasures(mi.list_dx$strict, fit.measures = c("cfi.scaled","tli.scaled", "srmr.scaled", "rmsea.scaled", "mfi", "chisq.scaled", "df.scaled"))
  fit_configural_all <- fitmeasures(mi.list_dx$configural)
  fit_strong_all <- fitmeasures(mi.list_dx$strong)
  fit_means_all <- fitmeasures(mi.list_dx$means)
  fit_strict_all <- fitmeasures(mi.list_dx$strict)
  
  fit.measures <- mget(ls(pattern= "fit_"))
  return(fit.measures)
}
run_MI_safe <- safely(run_MI)

###run models in parallel on cluster###

plan(multicore, workers = 25)#change based on cluster 
subsample_results <- subsample_data %>%
  mutate(results = future_map(dataset, run_MI_safe)) %>%
  select(c("subset_ids", "results"))

####for output - back in rstudio ##### 
subsample_results <- as_tibble(readRDS("Z:/projects/SCQ_MI/subsample_results_nomeans_new6x.rds")) %>%
  mutate(cfi_config = as.double(map_if(results, ~!is.null(.x),~as.double(.x[[1]]$fit_configural_all[["cfi.scaled"]]), .else = ~append(as.double(getElement(.x, 1)), NA_real_))),
         mfi_config = as.double(map_if(results, ~!is.null(.x),~as.double(.x[[1]]$fit_configural[["mfi"]]), .else = ~append(as.double(getElement(.x, 1)), NA_real_))),
         rmsea_config= as.double(map_if(results, ~!is.null(.x),~as.double(.x[[1]]$fit_configural_all[["rmsea.scaled"]]), .else = ~append(as.double(getElement(.x, 1)), NA_real_))),
         tli_config = as.double(map_if(results, ~!is.null(.x),~as.double(.x[[1]]$fit_configural_all[["tli.scaled"]]), .else = ~append(as.double(getElement(.x, 1)), NA_real_))),
         cfi_strong = as.double(map_if(results, ~!is.null(.x),~as.double(.x[[1]]$fit_strong_all[["cfi.scaled"]]), .else = ~append(as.double(getElement(.x, 1)), NA_real_))),
         mfi_strong = as.double(map_if(results, ~!is.null(.x),~as.double(.x[[1]]$fit_strong[["mfi"]]), .else = ~append(as.double(getElement(.x, 1)), NA_real_))),
         tli_strong = as.double(map_if(results, ~!is.null(.x),~as.double(.x[[1]]$fit_strong_all[["tli.scaled"]]), .else = ~append(as.double(getElement(.x, 1)), NA_real_))),
         rmsea_strong= as.double(map_if(results, ~!is.null(.x),~as.double(.x[[1]]$fit_strong_all[["rmsea.scaled"]]), .else = ~append(as.double(getElement(.x, 1)), NA_real_))),
         delta_cfi_strongMI = cfi_strong - cfi_config,
         delta_mfi_strongMI = mfi_strong - mfi_config,
         cfi_strict = as.double(map_if(results, ~!is.null(.x),~as.double(.x[[1]]$fit_strict_all[["cfi.scaled"]]), .else = ~append(as.double(getElement(.x, 1)), NA_real_))),
         mfi_strict = as.double(map_if(results, ~!is.null(.x),~as.double(.x[[1]]$fit_strict_all[["mfi"]]), .else = ~append(as.double(getElement(.x, 1)), NA_real_))),
         delta_cfi_strictMI = cfi_strict - cfi_strong,
         delta_mfi_strictMI = mfi_strict - mfi_strong,
        rmsea_strict= as.double(map_if(results, ~!is.null(.x),~as.double(.x[[1]]$fit_strict_all[["rmsea.scaled"]]), .else = ~append(as.double(getElement(.x, 1)), NA_real_))),
        tli_strict = as.double(map_if(results, ~!is.null(.x),~as.double(.x[[1]]$fit_strict_all[["tli.scaled"]]), .else = ~append(as.double(getElement(.x, 1)), NA_real_))),
        
  )
  


##### full sample for interpretation of parameter estimates


mi.test_dx <- list(strong = c("thresholds","loadings","intercepts"))

mi.list_dx <- list()
for (i in 0:length(mi.test_dx)) {
  if (i == 0L) {
    meq.label <- "configural"
    group.equal <- ""
    long.equal <- ""
  } else {
    meq.label <- names(mi.test_dx)[i]
    group.equal <- unlist(mi.test_dx[1:i])
  }
  mi.list_dx[[meq.label]] <- measEq.syntax(configural.model = scq_5,
                                        data = data_dx,
                                        ordered = c(grep("NN",colnames(data), value = TRUE)),
                                        parameterization = "theta",
                                        ID.fac = "std.lv",
                                        ID.cat = "Wu.Estabrook.2016",
                                        group = "autismDX_new",
                                        group.equal = group.equal,
                                        missing = "pairwise",
                                        estimator = "WLSMV",
                                        return.fit = TRUE)
}

