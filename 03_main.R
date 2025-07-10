# Date: July 2024
# Author: Raphaël Benerradi

# Clean version of the code used in the manuscript in a single file


# Import packages ---------------------------------------------------------

library(tidyverse)
library(jsonlite)

library(ggplot2)
library(ggtext) # for element_markdown
library(ggrepel) # for labels on points in graphs
library(ggvegan) # to plot the anosim test # available using command devtools::install_github("gavinsimpson/ggvegan")

library(vegan) # for diversity computations
library(car) # for Anova type II
library(multcomp) # for Tukey tests
library(lme4) # for lm with random effects
library(indicspecies) # for indicator species analysis


rm(list = ls())

set.seed(1234)


# Parameters of the execution and the input -------------------------------

metadata_exe = list(
  "data_input" = "data/01_b_surveys-clean_data.csv", # Survey data
  "trait_data" = "data/02_b_traits-clean_data.RData", # Trait data
  "do_we_save_plots" = TRUE, # If TRUE, we save the plots in main_folder_fig
  "types_plots" = c("pdf", "png"), # Types of plots to save
  # "main_folder_fig" = paste0("out/", substr(gsub(':', '', gsub(' ', '_', gsub('-', '', Sys.time()))), 1, 15), "/") # Folder to save the figures
  "main_folder_fig" = "out/full_analysis/" # Folder to save the figures
)

metadata_input = list(
  "col_name_site" = "cfb_plot_num", # Name of the column with the sites
  "col_name_type" = "type_plot", # Name of the column with the types of plots
  "col_name_day" = "day", # Name of the column with the date of the visit
  "order_types" = c("forest", "path", "road", "edge") # Order of the types of plots (especially for the figures)
)

# # Species included in the analysis (we include all the species)
# metadata_input[["species_selected"]] = c(
#   "Black Redstart", "Black Woodpecker", "Blue Tit", "Carrion Crow", "Coal Tit", 
#   "Common Buzzard", "Common Chaffinch", "Common Chiffchaff", "Common Raven",
#   "Common Redstart", "Crested Tit", "Dunnock", "Eurasian Blackbird", 
#   "Eurasian Blackcap", "Eurasian Bullfinch", "Eurasian Jay", "Eurasian Magpie",
#   "Eurasian Nuthatch", "Eurasian Siskin", "Eurasian Skylark", 
#   "Eurasian Treecreeper", "Eurasian Wren", "European Robin", "Fieldfare", 
#   "Firecrest", "Garden Warbler", "Goldcrest", "Goldfinch", 
#   "Great Spotted Woodpecker", "Great Tit", "Green Woodpecker", "Greenfinch", 
#   "Hawfinch", "Hooded Crow", "Long-tailed Tit", "Marsh Tit", "Mistle Thrush", 
#   "Red Crossbill", "Rook", "Short-toed Treecreeper", "Song Thrush", 
#   "Spotted Flycatcher", "Willow Tit", "Willow Warbler", "Wood Pigeon", 
#   "Yellowhammer"
# )
metadata_input[["species_selected"]] = 
  read.table(metadata_exe$data_input, header=TRUE, sep = ",", check.names = FALSE) %>% 
  dplyr::select(-visit_id, -cfb_plot_num, -round, -day, -time_visit, -type_plot, -observer) %>% 
  colnames()

# Define the palettes for the figures
# palette_types_subplots = scales::pal_hue()(length(metadata_input$order_types))
palette_types_subplots = c("#009E73", "#56B4E9", "#E69F00", "#CC79A7")
names(palette_types_subplots) = metadata_input$order_types


# Prepare folders and save parameters -------------------------------------

# Create the folders to save the outputs
if ( !dir.exists(metadata_exe$main_folder_fig) ){
  dir.create(metadata_exe$main_folder_fig)
}
if (metadata_exe$do_we_save_plots) {
  for (type_plot in metadata_exe$types_plots){
    if ( !dir.exists(paste0(metadata_exe$main_folder_fig, type_plot, "/")) ){
      dir.create(paste0(metadata_exe$main_folder_fig, type_plot, "/"))
    }
  }
  if ( !dir.exists(paste0(metadata_exe$main_folder_fig, "csv/")) ){
    dir.create(paste0(metadata_exe$main_folder_fig, "csv/"))
  }
}

# Save parameters (of execution and input) in json files
write(
  toJSON(metadata_exe, pretty = TRUE, auto_unbox = TRUE),
  file = paste0(metadata_exe$main_folder_fig, "metadata_exe_", 
                gsub("/[^/]*$|^[^/]*/", "", metadata_exe$main_folder_fig, perl=T),
                ".json")
)
write(
  toJSON(metadata_input, pretty = TRUE, auto_unbox = TRUE),
  file = paste0(metadata_exe$main_folder_fig, "metadata_input_", 
                gsub("/[^/]*$|^[^/]*/", "", metadata_exe$main_folder_fig, perl=T)
                ,".json")
)



# Import data -------------------------------------------------------------

data.survey.ini <- read.table(metadata_exe$data_input, header=TRUE, sep = ",", check.names = FALSE)

# Select the species of interest, and loc, type and date
data.survey <- data.survey.ini[c(
  metadata_input$col_name_site, 
  metadata_input$col_name_type, 
  metadata_input$col_name_day, 
  metadata_input$species_selected
)]

data.survey[, metadata_input$col_name_site] = as.factor(
  data.survey[[metadata_input$col_name_site]]
)
data.survey[, metadata_input$col_name_type] = factor(
  data.survey[[metadata_input$col_name_type]], 
  levels = metadata_input$order_types
)

# Sort the dataframe on types, and by location within types, and then by date
data.survey <- data.survey[order(
  data.survey[[metadata_input$col_name_type]], 
  data.survey[[metadata_input$col_name_site]], 
  data.survey[[metadata_input$col_name_day]]), 
]

print("Data of the survey used in this code")
str(data.survey)


# Fill in metadata_input with the indices of the columns used
metadata_input[["idx_col_loc"]] =  which(colnames(data.survey) == metadata_input$col_name_site)
metadata_input[["idx_col_type"]] = which(colnames(data.survey) == metadata_input$col_name_type)
metadata_input[["idx_col_day"]] =  which(colnames(data.survey) == metadata_input$col_name_day)
metadata_input[["idxs_cols_sp"]] = which(colnames(data.survey) %in% metadata_input$species_selected)


# Summarise the plot types (how many plots per type)
metadata_input[["nbr_per_types"]] = data.survey %>% 
  group_by(data.survey[, metadata_input$col_name_type]) %>% 
  summarise(nbr = n()) %>% 
  ungroup()
colnames(metadata_input$nbr_per_types)[1] = "type"
# Summarise the plot locs (how many plots per location)
metadata_input[["nbr_per_locs"]] = data.survey %>% 
  group_by(data.survey[, metadata_input$col_name_site]) %>% 
  summarise(nbr = n()) %>% 
  ungroup()
colnames(metadata_input$nbr_per_locs)[1] = "loc"





# Species richness per rounds ---------------------------------------------

species_per_round = data.survey.ini %>% 
  filter(round == 1) %>% 
  dplyr::select(c(6, 8:ncol(.))) %>%
  rename_with(~ gsub(metadata_input$col_name_type, "type", .x), everything()) %>%
  group_by(type) %>%
  summarise(across(everything(), ~sum(.)), .groups = "drop") %>%
  ungroup() %>%
  add_column(num_species = rowSums(.[, -1] > 0), .before = 2) %>% 
  dplyr::select(type, num_species) %>% 
  mutate(rounds = "1") %>% 
  
  rbind(
    data.survey.ini %>% 
      dplyr::select(c(6, 8:ncol(.))) %>%
      rename_with(~ gsub(metadata_input$col_name_type, "type", .x), everything()) %>%
      group_by(type) %>%
      summarise(across(everything(), ~sum(.)), .groups = "drop") %>%
      ungroup() %>%
      add_column(num_species = rowSums(.[, -1] > 0), .before = 2) %>% 
      dplyr::select(type, num_species) %>% 
      mutate(rounds = "all")
  ) %>% 
  pivot_wider(names_from = rounds, values_from = num_species) %>% 
  mutate(diff = all - `1`) %>% 
  pivot_longer(cols = c("1", "all", "diff"), names_to = "rounds", values_to = "num_species") %>% 
  
  mutate(type = factor(type, levels = metadata_input$order_types),
         rounds = factor(rounds, levels = c("all", "diff", "1"), 
                         labels = c("All", "1 & 2", "1")))

palette_S2 = c("1" = scales::pal_hue()(2)[1], "1 & 2" = scales::pal_hue()(2)[2])
  
p_S2 = species_per_round %>% filter(rounds != "All") %>% 
  ggplot(aes(x = type, y = num_species, fill = rounds)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  scale_fill_manual(values = palette_S2, guide = guide_legend(reverse = TRUE)) +
  theme_bw() +
  labs(# title = "Number of species per round and per type of plot",
       x = "Type of subplot", y = "Total number of species", fill = "Visit")

if (metadata_exe$do_we_save_plots){
  for (type_plot in metadata_exe$types_plots){
    ggsave(paste0(metadata_exe$main_folder_fig, type_plot, "/", "supp_mat_S02-species_per_round.", type_plot),
           plot = p_S2, width = 4, height = 4)
  }
}



# Increase in species richness for each round -----------------------------


species_per_plot_round = rbind(
  data.survey.ini %>% 
    filter(round == 1) %>% 
    dplyr::select(c(2, 3, 6, 8:ncol(.))) %>%
    rename_with(~ gsub(metadata_input$col_name_type, "type", .x), everything()) %>%
    rename_with(~ gsub(metadata_input$col_name_site, "loc", .x), everything()) %>%
    add_column(num_species = rowSums(.[, -c(1:3)] > 0), .before = 4) %>% 
    dplyr::select(loc, type, num_species) %>%
    mutate(rounds = "1"),
  data.survey.ini %>% 
    dplyr::select(c(2, 6, 8:ncol(.))) %>%
    rename_with(~ gsub(metadata_input$col_name_type, "type", .x), everything()) %>%
    rename_with(~ gsub(metadata_input$col_name_site, "loc", .x), everything()) %>%
    group_by(loc, type) %>%
    summarise(across(everything(), ~sum(.)), .groups = "drop") %>%
    ungroup() %>%
    add_column(num_species = rowSums(.[, -c(1:3)] > 0), .before = 3) %>% 
    dplyr::select(loc, type, num_species) %>%
    mutate(rounds = "all")
) %>% 
  pivot_wider(names_from = rounds, values_from = num_species) %>% 
  mutate(rate_increase = all / `1`)



mod = lm(rate_increase ~ all * type, data = species_per_plot_round)
sink(paste0(metadata_exe$main_folder_fig, "supp_mat_S03-species_per_plot_round_increase_summary.txt"))
summary(mod)
Anova(mod)
sink()



palette_S3 = palette_types_subplots

p_S3 = ggplot(species_per_plot_round, aes(x = all, y = rate_increase, color = type)) +
  geom_line(stat = "smooth",method = "lm", linetype ="dashed", linewidth = 0.7, alpha = 0.5) + 
  geom_smooth(aes(fill = type), method = "lm", alpha = 0.05, linetype = "blank",
              show.legend = FALSE) +
  geom_point(size = 2.5) + 
  scale_color_manual(values = palette_S3) +
  labs(x = "Total number of species (both visits for each subplot)", 
       y = "Rate of increase in number of species observed\nfrom the first to the second visit",
       color = "Type of subplot") +
  theme_bw() + 
  theme(legend.position = "top")

if (metadata_exe$do_we_save_plots){
  for (type_plot in metadata_exe$types_plots){
    ggsave(paste0(metadata_exe$main_folder_fig, type_plot, "/", "supp_mat_S03-species_per_plot_round_increase.", type_plot),
           plot = p_S3, width = 6, height = 5)
  }
}

  




# Compute matrices --------------------------------------------------------

print("* * * Matrices computation * * *")

# Compute different useful matrices: 
#   - matAb = matrix of abundances per visits)
#   - matPA = matrix of presence/absence per visits
#   - diversity_indices = dataframe containing species richness, Shannon index 
#                         the total abundance for each plots
#   - the matrices of distances between plots (dissimilarity)
#   - the long dataframes of distances between plots
#   - the long dataframes of distances between plots in the same location


# * * * Compute first matrices (abundance, P-A and species richness) * * * 

matAb <- data.survey %>% 
  dplyr::select(all_of(metadata_input$idxs_cols_sp)) %>%  # Select only the columns of species we consider
  as.matrix()
# Define the round of the visit depending on the date
round = data.survey %>% 
  group_by(data.survey[, c(metadata_input$idx_col_loc, metadata_input$idx_col_type)]) %>%
  mutate(round_num = rank(.data[[colnames(data.survey)[metadata_input$idx_col_day]]])) %>% 
  ungroup() %>% 
  pull(round_num)
rownames(matAb) <- paste(data.survey[[metadata_input$idx_col_loc]], 
                         data.survey[[metadata_input$idx_col_type]], 
                         round, 
                         sep="_") # [loc]_[type]_[round]

matPA <- (matAb > 0) * 1

diversity_indices = data.frame(
  sp.rich = rowSums(matPA),
  shannon.idx = diversity(matAb, index="shannon"),
  total.indiv.Ab = rowSums(matAb)
)



# * * * Compute dissimilarity matrices and convert it into long dataframe * * * 

dist_dissim = list()
df_dissim = list()

dist_dissim[["jaccardPA"]] = vegdist(matPA, method = "jaccard")


mat = as.matrix(vegdist(matPA, method = "jaccard"))
ind.strict.low <- which(lower.tri(mat, diag = FALSE), arr.ind = TRUE)
nn <- dimnames(mat)
long.df = data.frame(plot_i = nn[[1]][ind.strict.low[, 1]], 
                     plot_j = nn[[2]][ind.strict.low[, 2]],
                     val = mat[ind.strict.low])
colnames(long.df)[which(names(long.df) == "val")] <- "jaccardPA"
df_dissim[["jaccardPA"]] = long.df





# * * * Filter the dataframe of dissimilarities only within locations * * *
  
df_dissim_intra_loc = list()

sub_df_temp = df_dissim[["jaccardPA"]] %>% 
  filter(sub("_.*", "", plot_i) == sub("_.*", "", plot_j))
# We build a dataframe with all info for this purpose
sub_df_only_intra_loc = data.frame(
  loc = as.factor(sub("_.*", "", sub_df_temp$plot_i)),
  pair.types = as.factor(paste0(gsub(".*[_]([^_]+)[_].*", "\\1", 
                                     sub_df_temp$plot_i),
                                "-",
                                gsub(".*[_]([^_]+)[_].*", "\\1", 
                                     sub_df_temp$plot_j))),
  pair.rounds = as.factor(paste0(sub(".*_", "", sub_df_temp$plot_i),
                                 "-",
                                 sub(".*_", "", sub_df_temp$plot_j))),
  col_meth = sub_df_temp[, "jaccardPA"]
)
colnames(sub_df_only_intra_loc)[
  colnames(sub_df_only_intra_loc) == "col_meth"] = "jaccardPA"
# We ordered the rows of dataframe
df_dissim_intra_loc[["jaccardPA"]] = sub_df_only_intra_loc[
  order(sub_df_only_intra_loc[, "loc"], 
        sub_df_only_intra_loc[, "pair.types"], 
        sub_df_only_intra_loc[, "pair.rounds"]), ]
  
  
  
  
matrices = list("surveys" = list("matAb" = matAb, 
                                 "matPA" = matPA, 
                                 "diversity_indices" = diversity_indices),
                "dist_dissim" = dist_dissim,
                "df_dissim" = df_dissim,
                "df_dissim_intra_loc" = df_dissim_intra_loc)




# Table summary observations ----------------------------------------------

table_summary_observations = full_join(
  matrices$surveys$matAb %>% 
    as.data.frame() %>% 
    add_column(plot = rownames(.), .before = 1) %>% 
    add_column(type = gsub(".*[_]([^_]+)[_].*", "\\1", .[["plot"]]), .before = 2) %>% 
    dplyr::select(-plot) %>% 
    group_by(type) %>% 
    summarise(across(everything(), ~sum(.))) %>% 
    ungroup() %>% 
    t() %>%
    as.data.frame() %>%
    {
      colnames(.) <- .[1,]
      .[-1,]
    } %>% 
    mutate(across(everything(), as.numeric)) %>%
    rename_with(~paste0(., ".abund")) %>% 
    mutate(total.abund = rowSums(.)) %>% 
    add_column(species = rownames(.), .before = 1),
  
  matrices$surveys$matPA %>% 
    as.data.frame() %>% 
    rename_with(~colnames(matrices$surveys$matAb)) %>% 
    add_column(plot = rownames(.), .before = 1) %>% 
    add_column(type = gsub(".*[_]([^_]+)[_].*", "\\1", .[["plot"]]), .before = 2) %>% 
    dplyr::select(-plot) %>% 
    group_by(type) %>% 
    summarise(across(everything(), ~sum(.))) %>% 
    ungroup() %>% 
    t() %>%
    as.data.frame() %>%
    {
      colnames(.) <- .[1,]
      .[-1,]
    } %>% 
    mutate(across(everything(), as.numeric)) %>%
    rename_with(~paste0(., ".pres")) %>% 
    mutate(total.pres = rowSums(.)) %>% 
    add_column(species = gsub("\\.", " ", rownames(.)), .before = 1),
  
  by = "species"
) %>% 
  arrange(-total.abund) %>% 
  dplyr::select(c("species", 
                  paste0(metadata_input$order_types, ".abund"), "total.abund",
                  paste0(metadata_input$order_types, ".pres"), "total.pres"))


if (metadata_exe$do_we_save_plots){
  write.csv(table_summary_observations, row.names = FALSE,
            file = paste0(metadata_exe$main_folder_fig, "csv/supp_mat_S01-table_summary_observations.csv"))
}




# Tukey tests function ----------------------------------------------------

apply_tukey_tests = function(data, y_vect, y_labels, x_value, x_label, 
                             type_models, rd_eff = "loc",
                             sort_x_groups = c(FALSE), prefix_plots = c("S05", "F2"),
                             this_palette = palette_types_subplots, color_tukey_group = "black",
                             size_plot_comp = c(6, 4), size_plot_group = c(4, 5), 
                             rotate_xlab_group = FALSE, range_y_plot_comp = c(0, NA)) {
  for (i in 1:length(y_vect)) {
    
    cat(paste0("\n", y_labels[i], " ~ ", x_value, " | ",
               type_models[i], " model with random effect being ", rd_eff, "\n"))
    
    y_title = y_vect[i]
    y_label = y_labels[i]
    mod.formula = paste0(y_title, " ~ ", x_value, " + (1|", rd_eff, ")")
    
    linfct.base = eval(parse(text = paste0("mcp(", x_value, " = 'Tukey')")))
    
    
    
    if (type_models[i] == "anova") {
      mod = lmer(formula = mod.formula, data = data)
    } else if (type_models[i] == "gamma") {
      mod = glmer(formula = mod.formula, data = data, 
                  family = Gamma(link = "log"))
    } else if (type_models[i] == "poisson") {
      mod = glmer(formula = mod.formula, data = data, 
                  family = poisson(link = "log"))
    }
    # print(summary(mod))
    
    test.tukey = glht(mod, linfct = linfct.base)
    res.tukey = summary(test.tukey)
    letters.tukey = cld(test.tukey)
    CI.tukey <- confint(test.tukey)
    
    comparison.tukey <- data.frame(comparison = rownames(CI.tukey$confint),
                                   estimate = summary(test.tukey)$test$coefficients,
                                   pval = summary(test.tukey)$test$pvalues,
                                   std_error = summary(test.tukey)$test$sigma,
                                   lower = CI.tukey$confint[, "lwr"],
                                   upper = CI.tukey$confint[, "upr"])
    
    if (metadata_exe$do_we_save_plots) {
      write.csv(comparison.tukey, row.names = FALSE,
                file = paste0(metadata_exe$main_folder_fig, "csv/", prefix_plots[1], "_", i, "-test_",
                              type_models[i], "_comparaison-",
                              gsub(" ", "", y_label), "_", x_value, ".csv"))
    }
    
    # Plot the pairwise comparisons using ggplot
    p_comp = ggplot(comparison.tukey) +
      geom_pointrange(aes(x = comparison, y = estimate, ymin = lower, ymax = upper)) +
      geom_errorbar(aes(x = comparison, ymin = lower, ymax = upper), width = 0.1) +
      coord_flip() + 
      geom_hline(yintercept=0, linetype="dashed") +
      annotate("text", hjust = 0, size = 3, 
               x = comparison.tukey$comparison, 
               y = comparison.tukey$lower - 0.27*(max(comparison.tukey$upper) - min(comparison.tukey$lower)), 
               label = paste0("p-val = ", round(comparison.tukey$pval, 3))) +
      theme_bw() + 
      labs(# title = "Pairwise Comparisons with Confidence Intervals",
        # subtitle = paste0("Model ", type_models[i], " - ", y_label, "\n", mod.formula),
        x = paste0("Comparison on ", y_label)) +
      theme(plot.title = element_text(size=14), text = element_text(size = 14), 
            plot.margin = margin(0.3, 0.8, 0.3, 0.3, "cm") )
    if (type_models[i] == "anova") {
      p_comp = p_comp + labs(y = "Difference")
    } else if (type_models[i] == "gamma") {
      p_comp = p_comp + labs(y = "Difference (on estimate before link function gamma)")
    } else if (type_models[i] == "poisson") {
      p_comp = p_comp + labs(y = "Difference (on estimate before link function poisson)")
    }
    
    if (metadata_exe$do_we_save_plots) {
      for (type_plot in metadata_exe$types_plots){
        ggsave(paste0(metadata_exe$main_folder_fig, type_plot, "/", prefix_plots[1], "_", i, "-test_", 
                      type_models[i], "_comparaison-", 
                      gsub(" ", "", y_label), "_", x_value, ".", type_plot),
               plot = p_comp, width = size_plot_comp[1], height = size_plot_comp[2])
      }
    }
    
    # Prepare plot
    estimates.mod = as.data.frame(summary(mod)$coefficients)
    estimates.mod$Estimate[-1] = estimates.mod$Estimate[-1] + 
      estimates.mod$Estimate[1] # Intercept will be replaced the first level now
    rownames(estimates.mod) = levels(data[[x_value]])
    
    if (type_models[i] == "anova") {
      CI.mod = confint(mod)[
        c("(Intercept)", paste0(x_value, levels(data[[x_value]])[-1]) ) ,  ]
      CI.mod[-1, ] = CI.mod[-1, ] + estimates.mod$Estimate[1]
    } else if (type_models[i] == "gamma") {
      CI.mod = confint(mod, method = "Wald")[
        c("(Intercept)", paste0(x_value, levels(data[[x_value]])[-1]) ) ,  ]
      CI.mod[-1, ] = CI.mod[-1, ] + estimates.mod$Estimate[1]
      CI.mod = exp(CI.mod)
    } else if (type_models[i] == "poisson") {
      CI.mod = confint(mod)[
        c("(Intercept)", paste0(x_value, levels(data[[x_value]])[-1]) ) ,  ]
      CI.mod[-1, ] = CI.mod[-1, ] + estimates.mod$Estimate[1]
      CI.mod = exp(CI.mod)
    }
    
    estimates.mod = estimates.mod %>% 
      add_column(!!x_value := rownames(estimates.mod), .before = 1) %>% 
      mutate(letter = letters.tukey$mcletters$Letters,
             CI.low.2.5 = CI.mod[, 1], 
             CI.up.97.5 = CI.mod[, 2])
    if (type_models[i] %in% c("gamma", "poisson")) {
      estimates.mod = estimates.mod %>% 
        mutate(real_param = exp(Estimate))
    }
    
    df_plot = data
    if (sort_x_groups[i]) { 
      df_plot[[x_value]] = factor(df_plot[[x_value]],
                                  levels = estimates.mod[order(-estimates.mod$Estimate), x_value] ) 
    }
    
    # * * * Reorder the letters in the Tukey test for the plot (if less than 10 different letters) * * *
    estimates.mod$new_letter = estimates.mod$letter
    df_rename_letters = data.frame(
      init = unique(unlist(strsplit(
        paste0(estimates.mod$letter, collapse = ""), "" ) )))
    df_rename_letters$new_idx = NA
    df_rename_letters$new_letter = NA
    df_letters_estimates = estimates.mod[, c("letter", "Estimate")] %>% 
      arrange(-Estimate)
    # Find the way to rename letters
    idx_letter = 0
    for (k in 1:dim(df_letters_estimates)[1]) {
      these.letters = unlist(strsplit(df_letters_estimates$letter[k], "") )
      for (l in these.letters) {
        if ( is.na(df_rename_letters[df_rename_letters$init == l, "new_idx"]) ) {
          df_rename_letters[df_rename_letters$init == l, "new_idx"] = as.character(idx_letter)
          df_rename_letters[df_rename_letters$init == l, "new_letter"] = letters[idx_letter + 1]
          idx_letter = idx_letter + 1
        }
      }
    }
    # Rename letters in the dataframe
    replacement_map_idx <- setNames(df_rename_letters$new_idx, df_rename_letters$init)
    replacement_map_letter <- setNames(df_rename_letters$new_letter, df_rename_letters$new_idx)
    estimates.mod = estimates.mod %>% 
      mutate(new_letter = str_replace_all(letter, replacement_map_idx)) %>% # convert into digit
      mutate(new_letter = str_replace_all(new_letter, replacement_map_letter)) %>% # convert into letters
      mutate(new_letter = sapply(new_letter, 
                                 function(s) paste( sort(unlist( 
                                   strsplit(s, "") )), collapse = "") 
      )
      ) # sort alphabetically each str of letters
    rm(df_rename_letters, df_letters_estimates, these.letters, idx_letter, 
       replacement_map_idx, replacement_map_letter)
    
    if (metadata_exe$do_we_save_plots) {
      write.csv(estimates.mod, row.names = FALSE,
                file = paste0(metadata_exe$main_folder_fig, "csv/", prefix_plots[2], "_", i, "-test_",
                              type_models[i], "_estimates-",
                              gsub(" ", "", y_label), "_", x_value, ".csv"))
    }
    
    # Plot
    palette_group = this_palette
    # custom_labels_group <- setNames(paste0("<span style='color:", palette_group, ";'>", 
    #                                        levels(df_plot[[x_value]]), "</span>"), 
    #                                 levels(df_plot[[x_value]]))
    p_group = ggplot() +
      # scale_x_discrete(name = x_label, labels = custom_labels_group) +
      scale_x_discrete(name = x_label) +
      geom_boxplot(aes(y = !!sym(y_title), x = !!sym(x_value), color = !!sym(x_value)),
                   data = df_plot, width = 0.3, outlier.shape = NA, show.legend = F, lwd=1) + 
      scale_color_manual(values = palette_group) + 
      geom_errorbar(data = estimates.mod, aes(ymin = CI.low.2.5, ymax = CI.up.97.5, x = !!sym(x_value)), 
                    width = 0.05, color = color_tukey_group, position = position_nudge(x = 0.25) )
    
    if (!is.null(range_y_plot_comp)) {
      p_group = p_group +
        scale_y_continuous(name = y_label, limits = range_y_plot_comp,
                           expand = expansion(mult = c(0,0.1)) )
    } else {
      p_group = p_group +
        scale_y_continuous(name = y_label)
    }
    
    if (type_models[i] == "anova") {
      p_group = p_group + 
        geom_point(data = estimates.mod, aes(y = Estimate, x = !!sym(x_value)), 
                   size = 2, color = color_tukey_group, position = position_nudge(x = 0.25)) +
        geom_text(data = estimates.mod, aes(y = Estimate, x = !!sym(x_value), label = new_letter),
                  position = position_nudge(x = 0.35), hjust = 0, color = color_tukey_group )
    } else if (type_models[i] %in% c("gamma", "poisson")) {
      p_group = p_group + 
        geom_point(data = estimates.mod, aes(y = real_param, x = !!sym(x_value)), 
                   size = 2, color = color_tukey_group, position = position_nudge(x = 0.25)) +
        geom_text(data = estimates.mod, aes(y = real_param, x = !!sym(x_value), label = new_letter),
                  position = position_nudge(x = 0.35), hjust = 0, color = color_tukey_group )
    }
    
    p_group = p_group + 
      # labs(title = paste0("Test differences between groups - ", x_label),
      #      subtitle = paste0("Model ", type_models[i], " - ", y_label, "\n", mod.formula),
      #      caption = str_wrap("Black dots represent raw data. Red dots and error bars represent (estimated marginal) means ± 95% confidence interval per group. Means not sharing any letter are significantly different by the Tukey-test at the 5% level of significance.", 
      #                         width = 70) ) +
      theme_bw()
    if (rotate_xlab_group){
      p_group = p_group + 
        # theme(axis.text.x = element_markdown(face = "bold", size = 14, vjust = 0.5, hjust=1),
        #       text = element_text(size = 14))
        theme(axis.text.x = element_text(size = 14, angle = 90, vjust = 0.5, hjust=1),
              text = element_text(size = 14))
    } else {
      p_group = p_group + 
        # theme(axis.text.x = element_markdown(face = "bold", size = 14), 
        #       text = element_text(size = 14))
        theme(axis.text.x = element_text(size = 14), text = element_text(size = 14))
    }
    
    
    if (metadata_exe$do_we_save_plots) {
      for (type_plot in metadata_exe$types_plots){
        ggsave(paste0(metadata_exe$main_folder_fig, type_plot, "/", prefix_plots[2], "_", i, "-test_", 
                      type_models[i], "_groups-", 
                      gsub(" ", "", y_label), "_", x_value, ".", type_plot),
               plot = p_group, width = size_plot_group[1], height = size_plot_group[2])
      }
    }
  }
  
}



# Boxplots Tukey tests global indices -------------------------------------

df_diversity_plot = matrices$surveys$diversity_indices
df_diversity_plot$loc = as.factor(sub("_.*", "", rownames(df_diversity_plot)))
df_diversity_plot$type = factor(gsub(".*[_]([^_]+)[_].*", "\\1", 
                                     rownames(df_diversity_plot) ),
                                levels = metadata_input$order_types)
df_diversity_plot$round = as.factor(sub(".*_", "", rownames(df_diversity_plot) ))

vect_biodiv_index = c("sp.rich", "shannon.idx", "total.indiv.Ab")
vect_names = c("Species Richness", "Shannon Index", "Total Abundance")
type_models = c("poisson", "gamma", "poisson")

x_title = "type"
x_label = "Type"

sort_x_groups = c(FALSE, FALSE, FALSE)

apply_tukey_tests(df_diversity_plot, vect_biodiv_index, vect_names, 
                  x_title, x_label, type_models, rd_eff = "loc", 
                  sort_x_groups = sort_x_groups, 
                  prefix_plots = c("supp_mat_S04", "01-figure_2"))


# Boxplots Tukey tests dissimilarity indices ------------------------------

df_dissim_plot = matrices$df_dissim[["jaccardPA"]]
df_dissim_plot$pair.types = as.factor(
  paste0(gsub(".*[_]([^_]+)[_].*", "\\1", df_dissim_plot$plot_i),
         "-", gsub(".*[_]([^_]+)[_].*", "\\1", df_dissim_plot$plot_j)) )
df_dissim_plot$loc.i = as.numeric(gsub("_.*$", "", df_dissim_plot$plot_i))
df_dissim_plot$loc.j = as.numeric(gsub("_.*$", "", df_dissim_plot$plot_j))
df_dissim_plot$pair.loc.ij = as.factor(paste(pmin(df_dissim_plot$loc.i, df_dissim_plot$loc.j), 
                                             pmax(df_dissim_plot$loc.i, df_dissim_plot$loc.j), 
                                             sep = "_") )

apply_tukey_tests(df_dissim_plot, 
                  c("jaccardPA"), c("Jaccard dissimilarity"),
                  # rep("jaccardPA", 3), rep("Jaccard dissimilarity", 3), 
                  "pair.types", "Pair of types of subplots", c("anova"),#, "gamma", "poisson"),
                  rd_eff = "pair.loc.ij", 
                  sort_x_groups = c(TRUE),
                  # sort_x_groups = rep(TRUE, 3), 
                  prefix_plots = c("supp_mat_S06", "supp_mat_S06"),
                  this_palette = rep("#444444", length(unique(df_dissim_plot$pair.types))), 
                  color_tukey_group = "tomato", size_plot_comp = c(7, 14), 
                  size_plot_group = c(8, 6), rotate_xlab_group = TRUE)



# Tukey tests with functional traits --------------------------------------


load(metadata_exe$trait_data)
avonet_func_data = traits_data
rm(traits_data)

traits_name_test = c("Mass", "Wing.Length", "Hand.Wing.Index")
traits_name_test_labels = c("Body Mass (g)", "Wing Length (mm)", "Hand Wing Index (HWI)")

matAbProp = matrices$surveys$matAb / rowSums(matrices$surveys$matAb)

# sum(1 - colnames(matAbProp) %in% rownames(avonet_func_data)) # = 0 (all species are in the functional data)
df.traits.per.plot = matAbProp %*% 
  (avonet_func_data[colnames(matAbProp), traits_name_test] %>% as.matrix())

df.traits.per.plot = df.traits.per.plot %>% 
  as.data.frame() %>% 
  add_column(plot = rownames(.), .before = 1)
df.traits.per.plot$loc = as.factor(sub("_.*", "", rownames(df.traits.per.plot)))
df.traits.per.plot$type = factor(gsub(".*[_]([^_]+)[_].*", "\\1", 
                                     rownames(df.traits.per.plot) ),
                                levels = metadata_input$order_types)
df.traits.per.plot$round = as.factor(sub(".*_", "", rownames(df.traits.per.plot) ))

# Save the average functional traits per plot
if (metadata_exe$do_we_save_plots) {
  write.csv(
    df.traits.per.plot, 
    file = paste0(metadata_exe$main_folder_fig, '/csv', "/supp_mat_S14-weighted_average_functional_traits_per_plot.csv"), 
    row.names = FALSE
  )
}


apply_tukey_tests(df.traits.per.plot, traits_name_test, traits_name_test_labels, 
                  x_value = "type", x_label = "Type", 
                  type_models = c("gamma", "gamma", "gamma"), rd_eff = "loc",
                  sort_x_groups = c(FALSE, FALSE, FALSE), 
                  prefix_plots = c("supp_mat_S15", "04-figure_4"), range_y_plot_comp = NULL)




# NMDS --------------------------------------------------------------------


dissim_meth = "jaccard"

# mat = matrices$surveys$matPA

list_mat = list(matrices$surveys$matPA, matrices$surveys$matAb)
label_mat = c("PA", "Abund")

for (i in 1:length(list_mat)) {
  
  mat = list_mat[[i]]
  
  nmds = metaMDS(mat, distance = dissim_meth)
  data.scores.sites = as.data.frame(scores(nmds)$sites)
  data.scores.species = as.data.frame(scores(nmds)$species)
  types_in_mat = factor(gsub(".*[_]([^_]+)[_].*", "\\1", rownames(mat) ),
                        levels = metadata_input$order_types)
  data.scores.sites$type = types_in_mat
  data.scores.species$species = rownames(data.scores.species)
  
  
  # * * * Dot products sites - species * * *
  
  # For each species, compute the dot product with the sites (coordinates in the NMDS space)
  dot_products_sites_species = data.scores.sites
  for (this_species in data.scores.species$species) {
    vect_NDMS_this_species = as.numeric(data.scores.species[data.scores.species$species == this_species, c("NMDS1", "NMDS2")])
    dot_products_sites_species[[this_species]] = 
      dot_products_sites_species$NMDS1 * vect_NDMS_this_species[1] +
      dot_products_sites_species$NMDS2 * vect_NDMS_this_species[2]
  }
  dot_products_sites_species = dot_products_sites_species %>% 
    mutate(type = factor(type, levels = metadata_input$order_types))
  
  # Boxplot of the dot products per species and type of plot
  plot.box.dot.product.NMDS <- dot_products_sites_species %>% 
    pivot_longer(cols = -c(NMDS1, NMDS2, type), names_to = "species", values_to = "dot_product") %>% 
    ggplot(aes(x = type, y = dot_product, fill = type)) +
    facet_wrap(~species, scales = "free_y") +
    geom_boxplot() + 
    scale_fill_manual(values = palette_types_subplots) +
    labs(x = "Type of plot",
         y = "Dot product of NMDS coordinates between species and visits") + 
    theme_minimal() + 
    theme(legend.position = "none", 
          axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5, hjust = 0.5),
          strip.text = element_text(size = 7, face = "bold"))
          
  
  # Average dot products per species and type of plot
  means_dot_products_sites_species = dot_products_sites_species %>% 
    group_by(type) %>%
    summarise(across(!c(NMDS1, NMDS2), mean)) %>% 
    pivot_longer(cols = -type, names_to = "species", values_to = "dot_product")
  
  plot.avg.dot.product.NMDS <- means_dot_products_sites_species %>% 
    mutate(species = factor(
      species, 
      levels = means_dot_products_sites_species %>% 
        filter(type == 'forest') %>% arrange(dot_product) %>% pull(species)
    )) %>% 
    ggplot(aes(x = type, y = species, fill = dot_product)) +
    geom_tile(color = '#555555') +
    scale_fill_gradient2(low = "royalblue3", mid = "white", high = "forestgreen", midpoint = 0) +
    labs(x = "Type of plot", y = "Species", 
         fill = "Average dot product\nof NMDS coordinates\nbetween species and visits,\nper type of plots") +
    theme_minimal() + 
    # Adjust legend position below the graph but a bit to the left
    # theme(legend.position = c(0.2, -0.05), legend.direction = "horizontal")
    theme(legend.position = "bottom", 
          legend.text = element_text(size = 6, angle = 45, hjust = 1, vjust = 1.3),
          legend.title = element_text(size = 10), 
          legend.box = "horizontal", 
          legend.box.just = "left", 
          legend.margin = margin(t = 0, r = 0.4, b = 0, l = 0, unit = "cm"),
          legend.spacing.x = unit(0.1, "cm"),
          legend.key.size = unit(0.4, "cm"))
  
  # Save these plots
  if (metadata_exe$do_we_save_plots) {
    for (type_plot in metadata_exe$types_plots){
      ggsave(paste0(metadata_exe$main_folder_fig, type_plot, "/supp_mat_S12-nmds-dot_product_species_sites-box-", 
                    dissim_meth, "-", label_mat[i], ".", type_plot), 
             plot = plot.box.dot.product.NMDS, width = 13, height = 13)
      ggsave(paste0(metadata_exe$main_folder_fig, type_plot, "/supp_mat_S13-nmds-dot_product_species_sites-avg-", 
                    dissim_meth, "-", label_mat[i], ".", type_plot), 
             plot = plot.avg.dot.product.NMDS, width = 5, height = 8)
    }
  }
    
  
  # * * * Plot sites * * *
  
  # Extract the ellipses per type (store the data in df_ell)
  data.scores.sites.mean = aggregate(data.scores.sites[,1:2], 
                                     list(type = factor(data.scores.sites$type, 
                                                        levels = metadata_input$order_types)), 
                                     "mean")
  veganCovEllipse = function(cov, center = c(0, 0), scale = 1, npoints = 100) {
    theta = (0:npoints) * 2 * pi/npoints
    Circle = cbind(cos(theta), sin(theta))
    t(center + scale * t(Circle %*% chol(cov)))
  }
  p_ini = plot(nmds) # to prevent an error with the ordiellipse function
  ord = ordiellipse(nmds, data.scores.sites$type, display = "sites", 
                    kind = "se", conf = 0.95, label = T)
  df_ell <- data.frame()
  for(g in levels(data.scores.sites$type)){
    df_ell <- rbind(df_ell, 
                    cbind(as.data.frame(with(data.scores.sites[data.scores.sites$type == g, ],
                                             veganCovEllipse(ord[[g]]$cov, 
                                                             ord[[g]]$center,
                                                             ord[[g]]$scale) )),
                          type = g) )
  }
  df_ell$type = factor(df_ell$type, levels = metadata_input$order_types)
  # Plot of the sites
  plot.sites = ggplot(data.scores.sites, aes(x = NMDS1, y = NMDS2, colour = type)) + 
    geom_point() +
    # stat_ellipse(aes(x = NMDS1, y = NMDS2, color = type),
    #              geom="polygon",type="norm", level=0.75, alpha=0.1, show.legend=F) +
    geom_polygon(data = df_ell, aes(x = NMDS1, y = NMDS2, fill = type), alpha = 0.2) +
    geom_path(data = df_ell, aes(x = NMDS1, y = NMDS2, colour = type), linewidth=1) +
    # annotate("text", fontface = "bold", size = 5, 
    #          x = data.scores.sites.mean$NMDS1, y = data.scores.sites.mean$NMDS2, 
    #          label = data.scores.sites.mean$type, 
    #          colour = scales::hue_pal()(length(levels(data.scores.sites.mean$type))
    #          )[as.numeric(data.scores.sites.mean$type)] ) +
    theme_bw() + 
    scale_color_manual(values = palette_types_subplots) +
    scale_fill_manual(values = palette_types_subplots) +
    labs(# title = paste0("NMDS - sites (", dissim_meth, ")"),
      x = "NMDS1", y = "NMDS2", colour = "Type of plot", fill = "Type of plot")
  
  # And with sites labels
  plot.sites.labels = plot.sites +
    geom_text_repel(aes(label = rownames(data.scores.sites)), vjust = 0, size = 2, 
                    nudge_y = (max(data.scores.sites$NMDS2) - min(data.scores.sites$NMDS2))*0.013)
  # Save these plots
  if (metadata_exe$do_we_save_plots) {
    for (type_plot in metadata_exe$types_plots){
      ggsave(paste0(metadata_exe$main_folder_fig, type_plot, "/02-figure_3-nmds-sites-", 
                    dissim_meth, "-", label_mat[i], ".", type_plot), 
             plot = plot.sites, width = 7, height = 5)
      ggsave(paste0(metadata_exe$main_folder_fig, type_plot, "/", "supp_mat_S10-nmds-sites_with_labels-", 
                    dissim_meth, "-", label_mat[i], ".", type_plot), 
             plot = plot.sites.labels, width = 7, height = 5)
    }
  }
  
  
  # * * * Plot species * * *
  
  # Adjust the range limits x-y to let some space to labels
  mar_x = 0.1
  mar_y = 0.025
  x_lim = c(min(data.scores.species$NMDS1) - mar_x*(max(data.scores.species$NMDS1)-min(data.scores.species$NMDS1)), 
            max(data.scores.species$NMDS1) + mar_x*(max(data.scores.species$NMDS1)-min(data.scores.species$NMDS1)) ) 
  y_lim =  c(min(data.scores.species$NMDS2) - mar_y*(max(data.scores.species$NMDS2)-min(data.scores.species$NMDS2)), 
             max(data.scores.species$NMDS2) + mar_y*(max(data.scores.species$NMDS2)-min(data.scores.species$NMDS2)) ) 
  # Plot
  p_S10 = data.scores.species %>% 
    ggplot(aes(x = NMDS1, y = NMDS2)) + 
    geom_point() + 
    geom_text_repel(aes(label = species), size = 3, show.legend = FALSE, nudge_y = 0.06) + 
    scale_x_continuous(limit = x_lim) + scale_y_continuous(limit = y_lim) + 
    theme_bw() + 
    labs(# title = paste0("NMDS - species (", dissim_meth, ")"),
      x = "NMDS1", y = "NMDS2")
  # Save
  if (metadata_exe$do_we_save_plots) {
    for (type_plot in metadata_exe$types_plots){
      ggsave(paste0(metadata_exe$main_folder_fig, type_plot, "/", "supp_mat_S11-nmds-species-", 
                    dissim_meth, "-", label_mat[i], ".", type_plot), 
             plot = p_S10, width = 7, height = 5)
    }
  }
  
  
  
  # * * * Plot species and sites * * *
  p_S11 = ggplot() + 
    geom_point(data = data.scores.sites, aes(x = NMDS1, y = NMDS2, colour = type)) +
    geom_polygon(data = df_ell, aes(x = NMDS1, y = NMDS2, fill = type), alpha = 0.2) +
    geom_path(data = df_ell, aes(x = NMDS1, y = NMDS2, colour = type), linewidth=1) + 
    scale_color_manual(values = palette_types_subplots) +
    scale_fill_manual(values = palette_types_subplots) +
    
    geom_point(data = data.scores.species, aes(x = NMDS1, y = NMDS2), alpha = 0.5) + 
    geom_text_repel(data = data.scores.species, aes(x = NMDS1, y = NMDS2, label = species), 
                    size = 2, show.legend = FALSE, nudge_y = 0.06, alpha = 0.4) +
    theme_bw() +
    labs(# title = paste0("NMDS - species and sites (", dissim_meth, ")"),
      x = "NMDS1", y = "NMDS2", colour = "Type of plot", fill = "Type of plot")
  # Save
  if (metadata_exe$do_we_save_plots) {
    for (type_plot in metadata_exe$types_plots){
      ggsave(paste0(metadata_exe$main_folder_fig, type_plot, "/", "supp_mat_S07-nmds-species_and_sites-", 
                    dissim_meth, "-", label_mat[i], ".", type_plot),
             plot = p_S11, width = 7, height = 5)
    }
  }
  
  
  
  
  # ANOSIM ------------------------------------------------------------------
  
  nbr_rd_permutations = 999
  
  
  types_in_mat = factor(gsub(".*[_]([^_]+)[_].*", "\\1", rownames(mat) ),
                        levels = metadata_input$order_types)
  
  ano = anosim(mat, types_in_mat, distance = dissim_meth, 
               permutations = nbr_rd_permutations)
  # ano
  # summary(ano)
  # str(ano)
  # ano$statistic  # The test statistic R
  # ano$perm  # Values from permutations
  p_S12 = autoplot(ano) + 
    theme_bw() + 
    labs(title = paste0("Analysis of similarities (dissimilarity: ", dissim_meth, ")"),
         y = "Ranks of distances among sample units",
         x = "Types of plots (Between = any type of plots)")
  if (metadata_exe$do_we_save_plots) {
    for (type_plot in metadata_exe$types_plots){
      ggsave(paste0(metadata_exe$main_folder_fig, type_plot, "/", "supp_mat_S08-anosim-plot_result-", 
                    dissim_meth, "-", label_mat[i], ".", type_plot), 
             plot = p_S12, width = 7, height = 5)
    }
  }
  
  
  # # Plot of the distribution in the test
  # R.values <- with(ano, data.frame(R = c(statistic, perm) ) )
  # R.values$Type <- c("actual", rep("perm", length(R.values$R) - 1))
  # ggplot(data = R.values, aes(x = R)) +
  #   geom_density() + # permuted P-values
  #   geom_vline(data = R.values[R.values$Type == "actual" , ], aes(xintercept = R), colour = "red") +
  #   theme_bw()
  
  
  # * * * Indicator species * * *
  
  inv = multipatt(mat, types_in_mat, func = "r.g", 
                  control = how(nperm = nbr_rd_permutations) )
  # summary(inv)
  # summary(inv, alpha = 1)
  # inv$sign
  # inv$sign %>% filter(p.value < 0.05)
  # inv$sign[order(inv$sign$p.value), ]
  
  # Save the results of the tests
  if (metadata_exe$do_we_save_plots) {
    sink(paste0(metadata_exe$main_folder_fig, "csv/",
                "supp_mat_S09-indicator_species_analysis-summary_model-", 
                dissim_meth, "-", label_mat[i], ".txt"))
    summary(inv, alpha = 1)
    sink()
    write.csv(inv$sign[order(inv$sign$p.value), ], row.names = FALSE, 
              file = paste0(metadata_exe$main_folder_fig, "csv/",
                            "supp_mat_S09-indicator_species_analysis-all_species-", 
                            dissim_meth, "-", label_mat[i], ".csv") )
  }
  
  
  df_long <- inv$sign[order(inv$sign$p.value), ] %>% 
    arrange(across(1:4, desc), p.value) %>% 
    dplyr::select(-stat, -index) %>%
    mutate(species = factor(rownames(.), levels = rownames(.))) %>%
    pivot_longer(cols = -species, names_to = "variable", values_to = "value") %>%
    mutate(variable = factor(variable, 
                             levels = c(paste0("s.", metadata_input$order_types), "p.value"),
                             labels = c(metadata_input$order_types, "p-value")) ) %>% 
    
    mutate(fill_color = case_when(
      variable %in% metadata_input$order_types & value == 1 ~ "#31a6f7",
      variable %in% metadata_input$order_types & value == 0 ~ "grey",
      variable == "p-value" & value < 0.05 ~ "#5cc864",
      variable == "p-value" & value < 0.10 & value >= 0.05 ~ "#c7e6ca",
      TRUE ~ "white"
    ))
  
  # Create the plot
  ggplot(df_long, aes(x = variable, y = species, fill = fill_color)) +
    scale_y_discrete(limits = rev(levels(df_long$species))) +
    geom_tile(color = "black") + 
    geom_text(aes(label = ifelse(variable == "p-value", round(value, 3), "")), 
              size = 3, color = "#444444") +
    scale_fill_manual(values = c("#31a6f7" = "#31a6f7", "grey" = "grey", "white" = "white", 
                                 "#5cc864" = "#5cc864", "#c7e6ca" = "#c7e6ca"),
                      labels = c("Represented", "Not concerned"),
                      breaks = c("#31a6f7", "grey")) +
    theme_bw() +
    labs(fill = "Plot types of the\nindicator species") + 
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
          legend.position = "bottom")
  if (metadata_exe$do_we_save_plots) {
    for (type_plot in metadata_exe$types_plots){
      ggsave(paste0(metadata_exe$main_folder_fig, type_plot, "/", "supp_mat_S09-indicator_species_analysis-", 
                    dissim_meth, "-", label_mat[i], ".", type_plot), 
             plot = last_plot(), width = 5.5, height = 8)
    }
  }
  
  
  
  # Loop ANOSIM -------------------------------------------------------------
  
  
  nbr_rd_permutations = 999
  
  # mat = matrices$surveys$matPA
  types_in_mat = factor(gsub(".*[_]([^_]+)[_].*", "\\1", rownames(mat) ),
                        levels = metadata_input$order_types)
  
  
  types_included = list(
    c("forest", "path", "road", "edge"),
    c("forest", "edge"),
    c("path", "edge"),
    c("road", "edge"),
    c("forest", "path", "road"),
    c("forest", "path"),
    c("path", "road"),
    c("forest", "road")
  )
  
  # list_anosim = list()
  # df_anosim = data.frame()
  # for (types_incl in types_included) {
  #   ano = anosim(mat, 
  #                types_in_mat, 
  #                distance = dissim_meth, 
  #                permutations = nbr_rd_permutations, 
  #                strata = types_in_mat %in% types_incl)
  #   list_anosim[[paste(types_incl, collapse = "-")]] = ano
  #   df_anosim = rbind(df_anosim, 
  #                     data.frame(types_incl = paste(types_incl, collapse = "-"), 
  #                                R = ano$statistic, 
  #                                p.value = ano$signif) )
  # }
  
  list_anosim2 = list()
  df_anosim2 = data.frame()
  for (types_incl in types_included) {
    ano = anosim(mat[types_in_mat %in% types_incl, ], 
                 types_in_mat[types_in_mat %in% types_incl], 
                 distance = dissim_meth, 
                 permutations = nbr_rd_permutations)
    list_anosim2[[paste(types_incl, collapse = "-")]] = ano
    df_anosim2 = rbind(df_anosim2, 
                       data.frame(types_incl = paste(types_incl, collapse = "-"), 
                                  R = ano$statistic, 
                                  p.value = ano$signif) )
  }
  # df_anosim
  df_anosim2
  
  if (metadata_exe$do_we_save_plots) {
    # write.csv(df_anosim, row.names = FALSE,
    #           file = paste0(metadata_exe$main_folder_fig, 
    #                         "S14-anosim_results_selected_types(strata)-", dissim_meth, ".csv") )
    write.csv(df_anosim2, row.names = FALSE, 
              file = paste0(metadata_exe$main_folder_fig, 
                            "csv/03-table_2-anosim_results_selected_types-", dissim_meth, 
                            "-", label_mat[i], ".csv") )
  }
}
