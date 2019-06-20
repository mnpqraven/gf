library(shiny)
# UI
ui <- fluidPage(

  # Application title
#  titlePanel("Othi's Doll Index"),
#  tabsetPanel(
#    tabPanel("Plot",
#    ),
#    tabPanel("tab2",
#    ),
#
#  ),
#  sidebarLayout(
#    sidebarPanel(
#      sliderInput("Lv", "lv Limit",
#                  min = 1, max = 100, value = 100),
#      sliderInput("SLv", "slv Limit",
#                  min = 1, max = 10, value = 10),
#      sliderInput("Link", "link Limit",
#                  min = 1, max = 5, value = 5)
#    ),
#
#    mainPanel(
#      tableOutput("doll_kable")
#    )
#  )
  titlePanel("Othi's Doll Index"),
  tabsetPanel(
              tabPanel("Doll Index",
                       sidebarLayout(
                                     sidebarPanel(
                                                  sliderInput("Lv", "lv Limit",
                                                              min = 1, max = 100, step = 10, value = 100),
                                                  sliderInput("SLv", "slv Limit",
                                                              min = 1, max = 10, value = 10),
                                                  sliderInput("Link", "link Limit",
                                                              min = 1, max = 5, value = 5),
                                                  sliderInput("MOD", "MOD limit",
                                                              min = 0, max = 3, value = c(0,3))
                                                  ),
                                     mainPanel(
                                               tableOutput("doll_kable")
                                     )
                       )
              ),
              tabPanel("Fairy Index", tableOutput("fairylist_kable")),
              tabPanel("Skill Leveling", tableOutput("todo.slv_kable")),
              tabPanel("Core List", tableOutput("todo.core_kable")),
              tabPanel("Mod List", tableOutput("todo.MOD_kable"))
  )
)
