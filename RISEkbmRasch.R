### RISE KBM Rasch analysis package
### Created by magnus.p.johansson@ri.se ORCID: 0000-0003-1669-592X
### The contents of this file is licensed according to 
### Creative Commons Attribution 4.0 International Public License
### https://creativecommons.org/licenses/by/4.0/ 

# library(devtools)
# library(roxygen2)

#' Imports:
#'   tidyverse,
#'   eRm,
#'   psych,
#'   kabelExtra,
#'   mirt
#' 
#' #' @export
#' #' 

##### Show items based on itemlabels file

Rlistitems <- function(dfin, pdf.out) {
  if(missing(pdf.out)) {
    itemlabels %>% 
      filter(itemnr %in% names(dfin)) %>% 
    formattable(align=c("c","l"), list(
      `itemnr` = formatter("span", style = ~ style(color = "grey",font.weight = "bold"))),
      table.attr = 'class=\"table table-striped\" style="font-size: 15px; font-family: Lato; width: 60%"')
  } else {
    itemlabels %>% 
      kbl(booktabs = T, escape = F,
          table.attr = "style='width:60%;'") %>%
      # options for HTML output
      kable_styling(bootstrap_options = c("striped", "hover"), 
                    position = "center",
                    full_width = T,
                    font_size = r.fontsize,
                    fixed_thead = T) %>% 
      column_spec(1, bold = T) %>% 
      kable_classic(html_font = "Lato") %>% 
      # latex_options are for PDF output
      kable_styling(latex_options = c("striped","scale_down"))
  }
}




Rpcm <- function(dfin, no.table) {
  if(missing(no.table)) {
  df.erm<-PCM(dfin) # run PCM model, replace with RSM (rating scale) or RM (dichotomous) for other models
  # get estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
  item.estimates <- eRm::thresholds(df.erm)
  item_difficulty <- item.estimates[["threshtable"]][["1"]]
  item_difficulty<-as.data.frame(item_difficulty)
  item.se <- item.estimates$se.thresh
  person.locations.estimate <- person.parameter(df.erm)
  item.fit <- eRm::itemfit(person.locations.estimate)
  std.resids <- item.fit$st.res
  # PCA of Rasch residuals
  pca <- pca(std.resids, nfactors = ncol(dfin), rotate = "oblimin")
  # create table with top 5 eigenvalues
  pca$values %>%
    round(2) %>%
    head(5) %>% 
    as_tibble() %>% 
    rename('Eigenvalues' = 'value') %>% 
    kbl(booktabs = T, escape = F, table.attr = "style='width:25%;'") %>%
    # options for HTML output
    kable_styling(bootstrap_options = c("striped", "hover"), 
                  position = "center",
                  full_width = T,
                  font_size = r.fontsize,
                  fixed_thead = F) %>% 
    column_spec(1, bold = T) %>% 
    kable_classic(html_font = "Lato") %>% 
    # latex_options are for PDF output
    kable_styling(latex_options = c("striped","scale_down"))
  } else {
    df.erm<-PCM(dfin) # run PCM model, replace with RSM (rating scale) or RM (dichotomous) for other models
    # get estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
    item.estimates <- eRm::thresholds(df.erm)
    item_difficulty <- item.estimates[["threshtable"]][["1"]]
    item_difficulty<-as.data.frame(item_difficulty)
    item.se <- item.estimates$se.thresh
    person.locations.estimate <- person.parameter(df.erm)
    item.fit <- eRm::itemfit(person.locations.estimate)
    std.resids <- item.fit$st.res
    # PCA of Rasch residuals
    pca <- pca(std.resids, nfactors = ncol(dfin), rotate = "oblimin")
  }
}

#####

Rtileplot <- function(dfin) {
  dfin %>% 
    pivot_longer(everything()) %>% 
    dplyr::count(name, value) %>% 
    ggplot(aes(x = value, y = name, fill = n)) +
    geom_tile() +
    scale_fill_viridis_c(expression(italic(n)), limits = c(0, NA)) +
    scale_x_continuous("Response", expand = c(0, 0), breaks = 0:7) + # change breaks to fit number of response categories
    ggtitle("Items") +
    ylab("") +
    theme(axis.text.x = element_text(size = 8)) +
    geom_text(aes(label=n), colour = "orange") 
}

#####

Rbarplot <- function(dfin) {
  for (i in 1:ncol(dfin)) {
    barplot(table(dfin[,i]), col="#8dc8c7",
            main=names(dfin[i]),
            ylab="Number of responses", 
            xlab=(itemlabels[i,2]))
  }
}


##### this doesn't make the plot?

Rrespcats <- function(dfin) {
  mirt.rasch <- mirt(dfin, model=1, itemtype='Rasch') # unidimensional Rasch model
  plot(mirt.rasch, type="trace")
}


Ritemcats <- function(dfin, items) {
  # individual plots for those two items:
  df.erm<-PCM(dfin) # run PCM model, replace with RSM (rating scale) or RM (dichotomous) for other models
  plotICC(df.erm, xlim = c(-6, 6), # change the theta interval to display
          legpos = FALSE, # change legpos to TRUE if you want the legend displayed 
          ylab = "Probability", xlab = "Person location/ability",
          item.subset = items)
}

#make this escape the "hit return to see next plot"
