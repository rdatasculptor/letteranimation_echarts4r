library(echarts4r)
library(manipulateWidget)
library(htmltools)
library(dplyr)
haalletter <- function(letter = data.frame(kolom = c("a","b")),groep = c(1:2),seed = 1){
  require(RColorBrewer)
  kleuren <- brewer.pal(12, "Paired")
  set.seed(seed)
  kleur <- kleuren[sample(1:length(kleuren),1)]
  lijst <- list()
  for (i in 1:nrow(letter)){
  df1a <- readxl::read_xlsx("pixelletters.xlsx", sheet = as.character(letter[i,1])) %>% arrange(desc(aa))
df1b <- df1a
df1b[df1b==1] <- 2
df1b[df1b==0] <- 1
df1b[df1b==2] <- 0
names(df1a)[2:length(df1a)] <- paste0(names(df1a)[2:length(df1a)],"aan")
names(df1b)[2:length(df1b)] <- paste0(names(df1b)[2:length(df1b)],"uit")
df1 <- inner_join(df1a,df1b)
df1 <- df1[names(df1)[order(names(df1))]]
df1$aa <- c("a","b","c","d","e","f")
df1$groep <- groep[i]
lijst[[i]] <- df1
  }
df <- bind_rows(lijst)
df %>% 
  group_by(groep) %>%
  e_charts(aa, timeline = TRUE, reorder = FALSE) %>% 
  e_timeline_opts(
    autoPlay = TRUE,
    show = FALSE,
    playInterval = 5000
  ) %>% 
  e_animation(duration.update = 4000) %>%
  e_legend(show = FALSE) %>%
  e_legend(show = TRUE, bottom = 0) %>%
  e_bar(k0aan, stack = "grp", barCategoryGap = "0%") %>% 
  e_bar(k0uit, stack = "grp", barCategoryGap = "0%") %>%
  e_bar(k1aan, stack = "grp", barCategoryGap = "0%") %>% 
  e_bar(k1uit, stack = "grp", barCategoryGap = "0%") %>%
  e_bar(k2aan, stack = "grp", barCategoryGap = "0%") %>% 
  e_bar(k2uit, stack = "grp", barCategoryGap = "0%") %>%
  e_bar(k3aan, stack = "grp", barCategoryGap = "0%") %>% 
  e_bar(k3uit, stack = "grp", barCategoryGap = "0%") %>%
  e_bar(k4aan, stack = "grp", barCategoryGap = "0%") %>% 
  e_bar(k4uit, stack = "grp", barCategoryGap = "0%") %>%
  e_bar(keindaan, stack = "grp", barCategoryGap = "0%") %>% 
  e_bar(keinduit, stack = "grp", barCategoryGap = "0%") %>%
  e_flip_coords() %>%
  e_color(c(kleur,"white")) %>%
  e_hide_grid_lines() %>%
  e_grid(left = 0, right = 0, top = 0, bottom = 0) %>%
  e_x_axis(axisLine=list(show = FALSE),axisTick = list(show = FALSE),axisLabel = list(show = FALSE)) %>%
  e_y_axis(axisLabel = list(show = FALSE),axisLine = list(show = FALSE),axisTick = list(show = FALSE)) 
}

text <- readxl::read_xlsx("pixelletters.xlsx", sheet = "text", col_names = FALSE)
lijst <- list()
for (i in 1:length(text)){
  lijst[[i]] <- haalletter(letter = text[,i],groep = 1:nrow(text),seed = i*1234)
}
combineWidgets(list = lijst, ncol = 10)

