library(shiny)
library(shinydashboard)

ui <- dashboardPage (
                     dashboardHeader(title = "Othi's Doll Index"),
                     dashboardSidebar(
                                      sidebarMenu(
                                                  menuItem("Doll Index", tabName = "doll"),
                                                  menuItem("Fairy Index", tabName = "fairy"),
                                                  menuItem("SLv Planning", tabName = "slv"),
                                                  menuItem("Core", tabName = "core"),
                                                  menuItem("MOD", tabName = "mod"),
                                                  sliderInput("Lv", "lv Limit",min = 1, max = 100, step = 10, value = 100),
                                                  sliderInput("SLv", "slv Limit",min = 1, max = 10, value = 10),
                                                  sliderInput("Link", "link Limit",min = 1, max = 5, value = 5),
                                                  sliderInput("MOD", "MOD limit (error means no result)",min = 0, max = 3, value = c(0,3))
                                                  )
                                      ),
                     dashboardBody(
                                   tabItems(
                                            tabItem(tabName = "doll",
                                                              tableOutput("doll_kable")
                                                    ),
                                            tabItem(tabName = "fairy",
                                                    fluidPage(
                                                              tableOutput("fairylist_kable")
                                                    )
                                            ),
                                            tabItem(tabName = "slv",
                                                    fluidPage(
                                                              tableOutput("todo.slv_kable")
                                                    )
                                            ),
                                            tabItem(tabName = "core",
                                                    fluidPage(
                                                              tableOutput("todo.core_kable")
                                                    )
                                            ),
                                            tabItem(tabName = "mod",
                                                    fluidPage(
                                                              tableOutput("todo.MOD_kable")
                                                    )
                                            )
                                   )
                     )
)
