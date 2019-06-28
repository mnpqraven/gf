library(shiny)

server <- function(input, output) {
  library(dplyr)
  library(kableExtra)
  source('readtable_offline.R')
  #source('readtable_online.R')
  source('dataframes.R')
  colorAR <-         "#97c9ae"
  colorSMG <-        "#ae97c9"
  colorHG <-         "#c997b1"
  colorRF <-         "#c5f29d"
  colorMG <-         "#9dc5f2"
  colorSG <-         "#ca9df2"
  colorFairyStrat <- "#6bc3ff"
  colorFairyBattl <- "#ff6866"
  colorTotal <-      "yellow"
  colorRarity5 <-    "#FFB600"
  colorRarity4 <-    "#D6E35A"
  colorRarity3 <-    "#6BDFCE"
  colorRarity1 <-    "#DEB6FF"
  output$doll_kable <- function() {
    req(input$Lv)
    req(input$SLv)
    req(input$Link)
    req(input$MOD)
    df.dolllist.pretty %>%
    mutate(
      Ringed = cell_spec(Ringed, "html", background = ifelse(Ringed == "Yes","#F7AEA5","NULL")),
      Name = cell_spec(Name, "html", background =
                         ifelse(df.dolllist$rarity == 5 | (df.dolllist$rarity == 4 & df.dolllist$mod > 0), colorRarity5,
                         ifelse(df.dolllist$rarity == 4 | ((df.dolllist$rarity == 3 | df.dolllist$rarity == 2) & df.dolllist$mod > 0), colorRarity4,
                         ifelse(df.dolllist$rarity == 3, colorRarity3,
                         ifelse(df.dolllist$rarity == 1, colorRarity1, "white")))),
                       background_as_tile=F, font_size = 18),
      Type = cell_spec(Type, "html", background = ifelse(Type == "AR", colorAR,
                                                  ifelse(Type =="SMG", colorSMG,
                                                  ifelse(Type == "HG", colorHG,
                                                  ifelse(Type == "RF", colorRF,
                                                  ifelse(Type == "MG", colorMG,
                                                  ifelse(Type == "SG", colorSG, "white"
                                                  ))))))),
      Lv = cell_spec(Lv,color = spec_color(Lv, begin = 0, end = 1, option = "A", direction = -1), background = spec_color(Lv, begin = 0.5, end = 0.9, option = "D")),
      Link = cell_spec(Link, color = ifelse(Link == 5, "green", "black")),
      SLv = cell_spec(SLv,color = spec_color(SLv, begin = 0, end = 1, option = "A", direction = -1), background = spec_color(SLv, begin = 0.5, end = 0.9, option = "D"))) %>%
      filter(df.dolllist.pretty$Lv <= input$Lv & df.dolllist.pretty$SLv <= input$SLv & df.dolllist.pretty$Link <= input$Link & (df.dolllist.pretty$MOD >= min(input$MOD) & df.dolllist.pretty$MOD <= max(input$MOD))) %>%
      kable(format = "html", escape = F, align = c("rcclclll")) %>%
      kable_styling("striped", full_width = T) %>%
      column_spec(1:8,extra_css = "vertical-align:middle;") %>%
      kable_styling(c("bordered", "striped", "condensed", "hover"), fixed_thead=T)
  }

  output$todo.slv_kable <- function() {
  filter.todo.slv %>%
    mutate(
      Type = cell_spec(Type, "html", background = ifelse(Type == "fairyBattl", colorFairyBattl,
                                                  ifelse(Type == "fairyStrat", colorFairyStrat,
                                                  ifelse(Type == "AR", colorAR,
                                                  ifelse(Type =="SMG", colorSMG,
                                                  ifelse(Type == "HG", colorHG,
                                                  ifelse(Type == "RF", colorRF,
                                                  ifelse(Type == "MG", colorMG,
                                                  ifelse(Type == "SG", colorSG, "NULL"
                                                  ))))))))),
      Name = cell_spec(Name, "html", background = ifelse(is.na(filter.todo.slv$Name), "NULL",
                                                  ifelse(fulltodo$rarity.y == 5 | (fulltodo$rarity.y == 4 & !is.na(fulltodo$mod) & fulltodo$mod > 0), colorRarity5,
                                                  ifelse(fulltodo$rarity.y == 4 | ((fulltodo$rarity.y == 3 | fulltodo$rarity.y == 2) & !is.na(fulltodo$mod) & fulltodo$mod > 0), colorRarity4,
                                                  ifelse(fulltodo$rarity.y == 3, colorRarity3,
                                                  ifelse(fulltodo$rarity.y == 1, colorRarity1, "white"))))),
                       background_as_tile=F, font_size = 18),
     ) %>%
    kable(format = "html", escape = F, align = c("cllllrrr")) %>%
    kable_styling(c("bordered", "striped", "hover"), fixed_thead=T) %>%
    column_spec(1:8,extra_css = "vertical-align:middle;") %>%
    column_spec(1, width = "8em") %>%
    column_spec(3, width = "5em") %>%
    column_spec(4, width = "6.5em") %>%
    column_spec(5, width = "6.5em") %>%
    column_spec(6, width = "8em") %>%
    column_spec(7, width = "8em") %>%
    column_spec(8, width = "8em") %>%
    row_spec(nrow(filter.todo.slv), background = colorTotal, bold=T)
  }

  output$todo.core_kable <- function() {
  filter.todo.core %>%
    mutate(
      Type = cell_spec(Type, "html", background = ifelse(Type == "fairyBattl", colorFairyBattl,
                                                  ifelse(Type == "fairyStrat", colorFairyStrat,
                                                  ifelse(Type == "AR", colorAR,
                                                  ifelse(Type =="SMG", colorSMG,
                                                  ifelse(Type == "HG", colorHG,
                                                  ifelse(Type == "RF", colorRF,
                                                  ifelse(Type == "MG", colorMG,
                                                  ifelse(Type == "SG", colorSG, "NULL"
                                                  ))))))))),
      Name = cell_spec(Name, "html", background = ifelse(is.na(filter.todo.core$Name), "NULL",
                                                  ifelse(filter.todo.core$rarity.y == 5, colorRarity5,
                                                  ifelse(filter.todo.core$rarity.y == 4, colorRarity4,
                                                  ifelse(filter.todo.core$rarity.y == 3, colorRarity3,
                                                  ifelse(filter.todo.core$rarity.y == 1, colorRarity1, "white"))))),
                       background_as_tile=F, font_size = 18),
    ) %>%
    select(-dID,-rarity.y) %>%
    #kable(format = "html", escape = F, align = c("clllr")) %>%
    kable(format = "html", escape = F, align = c("cllrr")) %>%
    kable_styling(c("bordered", "striped", "hover"), fixed_thead=T) %>%
    column_spec(1:5,extra_css = "vertical-align:middle;") %>%
    column_spec(1, width = "8em") %>%
    column_spec(3, width = "8em") %>%
    column_spec(4, width = "10em") %>%
    column_spec(5, width = "10em") %>%
    row_spec(nrow(filter.todo.core), background = colorTotal, bold=T)
  }

  output$todo.MOD_kable  <- function() {
filter.todo.MOD %>%
  mutate(
    Type = cell_spec(Type, "html", background = ifelse(Type == "fairyBattl", colorFairyBattl,
                                                ifelse(Type == "fairyStrat", colorFairyStrat,
                                                ifelse(Type == "AR", colorAR,
                                                ifelse(Type =="SMG", colorSMG,
                                                ifelse(Type == "HG", colorHG,
                                                ifelse(Type == "RF", colorRF,
                                                ifelse(Type == "MG", colorMG,
                                                ifelse(Type == "SG", colorSG, "NULL"
                                                ))))))))),
    Name = cell_spec(Name, "html", background = ifelse(is.na(filter.todo.MOD$Name), "NULL",
                                                ifelse(filter.todo.MOD$rarity.y == 4, colorRarity5,
                                                ifelse(filter.todo.MOD$rarity.y == 3, colorRarity4,
                                                ifelse(filter.todo.MOD$rarity.y == 2, colorRarity4,
                                                ifelse(filter.todo.MOD$rarity.y == 1, colorRarity1, "white"))))),
                     background_as_tile=F, font_size = 18),
  ) %>%
  select(-rarity.y, -dID) %>%
  kable(format = "html", escape = F, align = c("cllrr")) %>%
  kable_styling(c("bordered", "striped", "hover"), fixed_thead=T) %>%
  column_spec(1:5,extra_css = "vertical-align:middle;") %>%
  column_spec(1, width = "8em") %>%
  column_spec(3, width = "8em") %>%
  column_spec(4, width = "10em") %>%
  column_spec(5, width = "10em") %>%
  row_spec(nrow(filter.todo.MOD), background = colorTotal, bold=T)
  }

  output$fairylist_kable <- function() {
df.fairylist.pretty %>%
  mutate(
    Type = cell_spec(Type, "html", background = ifelse(Type == "fairyBattl", colorFairyBattl,
                                                ifelse(Type == "fairyStrat", colorFairyStrat, "NULL"))),
    Name = cell_spec(Name, "html", background = ifelse(is.na(df.fairylist.pretty$Name), "NULL",
                                                ifelse(df.fairylist.pretty$Rarity == 5, colorRarity5,
                                                ifelse(df.fairylist.pretty$Rarity == 4, colorRarity4,
                                                ifelse(df.fairylist.pretty$Rarity == 3, colorRarity3,
                                                ifelse(df.fairylist.pretty$Rarity == 1, colorRarity1, "white"))))),
                     background_as_tile=F, font_size = 18),
  ) %>%
  kable(format = "html", escape = F, align = c("cllrr")) %>%
  kable_styling(c("bordered", "striped", "hover"), fixed_thead=T) %>%
  column_spec(1:5,extra_css = "vertical-align:middle;") %>%
  column_spec(1, width = "8em") %>%
  column_spec(2, width = "8em") %>%
  column_spec(4, width = "12em") %>%
  column_spec(5, width = "12em")
  }
}
