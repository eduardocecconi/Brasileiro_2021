library(lavaan)
library(tidyverse)
library(readxl)
library(stats)
library(psych)
library(semPlot)
library(stats)

Brasileiro_2021 <- read_excel("Brasileiro_Matchs_2021.xlsx")

Brasileiro_2021 <- Brasileiro_2021 %>% 
                      mutate(Local = rep(c("H", "A"), times = 379)) %>%
                      select(1:3, Local, everything()) 

Brasileiro_2021 <- Brasileiro_2021 %>% 
                      mutate(Goals_Conceded = case_when(
                              Brasileiro_2021$Local == "H" ~ lead(Brasileiro_2021$Goals),
                              Brasileiro_2021$Local == "A" ~ lag(Brasileiro_2021$Goals)),
                             Set_Pieces_Conceded = case_when(
                              Brasileiro_2021$Local == "H" ~ lead(Brasileiro_2021$`Attacks with shots 0 Set pieces attacks` ),
                              Brasileiro_2021$Local == "A" ~ lag(Brasileiro_2021$`Attacks with shots 0 Set pieces attacks`)),
                             Shots_Conceded = case_when(
                              Brasileiro_2021$Local == "H" ~ lead(Brasileiro_2021$`Shots on target`),
                              Brasileiro_2021$Local == "A" ~ lag(Brasileiro_2021$`Shots on target`))) %>%
                             select(1:5, Goals_Conceded, Shots_Conceded, Set_Pieces_Conceded, everything())

Brasileiro_2021 <- Brasileiro_2021 %>% 
                      mutate(Points = case_when(
                            Brasileiro_2021$Goals == Brasileiro_2021$Goals_Conceded ~ 1,
                            Brasileiro_2021$Goals > Brasileiro_2021$Goals_Conceded ~ 3,
                            Brasileiro_2021$Goals < Brasileiro_2021$Goals_Conceded ~ 0)) %>%
                      select(1:3, Points, everything())

Metrics_CFA <- Brasileiro_2021 %>% select(Teams, Points, Goals, Goals_Conceded, 
                                             `Accurate passes`, Fouls, Passes,
                                             `Key passes accurate`, `Ball recoveries`, 
                                             `Ball possession, %`, `Ball possessions, quantity`,
                                             `Ball recoveries in opponent's half`,
                                             `Team pressing successful`, `Opponent's xG`,
                                             `xG (Expected goals)`, `Dribbles successful`,
                                             `Defensive challenges won`,  Building0ups,
                                             `Ball interceptions`, `High pressing successful`,
                                             `Low pressing successful`, `Expected points`,
                                             `Opponent's passes per defensive action`,
                                             `Average duration of ball possession, min`,
                                             `Positional attacks with shots`, 
                                             `Counter0attacks with a shot`, `Yellow cards`,
                                             `Red cards`, `Crosses accurate`,
                                             `Attacking challenges won`,
                                             `Air challenges won`, `Tackles successful`,
                                             `Lost balls`, `Lost balls in own half`,
                                             `Entrances on opponent's half`,
                                             `Entrances on final third of opponent's half`,
                                             `Entrances to the opponent's box`,
                                             `Attacks with shots 0 Set pieces attacks`,
                                              Set_Pieces_Conceded, Shots_Conceded) %>%
                                       rename(Expected_Points = `Expected points`,
                                              xG = `xG (Expected goals)`,
                                              Positional_shot = `Positional attacks with shots`,
                                              Counter_shot = `Counter0attacks with a shot`, 
                                              Set_Piece_shot = `Attacks with shots 0 Set pieces attacks`,
                                              Key_Passes_acc = `Key passes accurate`,
                                              Crosses_acc = `Crosses accurate`, 
                                              Dribbles_acc = `Dribbles successful`,
                                              xG_Conceded = `Opponent's xG`,
                                              Interceptions = `Ball interceptions`,
                                              Team_Pressing = `Team pressing successful`,
                                              High_press_acc = `High pressing successful`,
                                              Low_press_acc = `Low pressing successful`,
                                              Recoveries = `Ball recoveries`,
                                              Recoveries_High = `Ball recoveries in opponent's half`,
                                              PPDA = `Opponent's passes per defensive action`,
                                              Duels_def = `Defensive challenges won`, 
                                              Duels_ata = `Attacking challenges won`,
                                              Duels_air = `Air challenges won`, 
                                              Tackles_acc = `Tackles successful`,
                                              Yellow_Card = `Yellow cards`,
                                              Red_Card = `Red cards`,
                                              Passes_acc = `Accurate passes`,
                                              Possession_Time = `Ball possession, %`,
                                              Possession_Quant = `Ball possessions, quantity`,
                                              Time_per_possession = `Average duration of ball possession, min`,
                                              Build_Total = Building0ups,
                                              Lost_Balls = `Lost balls`, 
                                              Lost_Balls_own = `Lost balls in own half`,
                                              Second_third = `Entrances on opponent's half`,
                                              Final_third = `Entrances on final third of opponent's half`,
                                              Box = `Entrances to the opponent's box`) %>%
                                       mutate(Conversion = Goals / xG,
                                              Conversion_Points = Points / Expected_Points,
                                              Attacks_Shot = Positional_shot + 
                                                             Counter_shot + Set_Piece_shot,
                                              Shot_Assist = (Key_Passes_acc + Crosses_acc) / 10,
                                              Conversion_Conceded = Goals_Conceded / 
                                                                    xG_Conceded,
                                              Imposition = (Duels_def + Duels_ata + Duels_air) - 
                                                           (Fouls + Red_Card + Yellow_Card),
                                              Contention = log(Tackles_acc + Recoveries + 
                                                           Interceptions + Low_press_acc),
                                              Pressing = Recoveries_High + High_press_acc + 
                                                         Team_Pressing,
                                              Ball_Care = log((Passes_acc + Dribbles_acc) - Lost_Balls),
                                              Passing_Speed = 10* ((Passes / Possession_Quant) / 
                                                              Time_per_possession),
                                              Build_Up = Build_Total - Lost_Balls_own,
                                              Progression = (Second_third - Final_third) +
                                                            (Final_third - Box)) %>%
                                       select(Teams, Points, Expected_Points, 
                                              Conversion_Points, Goals, xG, Attacks_Shot,
                                              Conversion, Shot_Assist, Goals_Conceded, 
                                              xG_Conceded, Conversion_Conceded, Shots_Conceded, 
                                              Contention, Pressing, Imposition, PPDA,
                                              Possession_Time, Passing_Speed, Ball_Care,
                                              Build_Up, Progression, Set_Piece_shot,
                                              Set_Pieces_Conceded)

correlation <- cor(Metrics_CFA[ , 2:24])

covariance <- cov(Metrics_CFA[ , 2:24])

variance <- var(Metrics_CFA[ , 2:24])

Metrics_CFA[, c(7, 13:17, 20, 22)] <- apply(Metrics_CFA[, c(7, 13:17, 20, 22)],  2, scale)

Metrics_CFA <- Metrics_CFA %>% mutate_if(is.numeric, round, 2)
                  
Attack_Model <- ' Attack =~ Goals + Conversion + Attacks_Shot + Shot_Assist '        

Fit_Attack_Model <- cfa(Attack_Model, data= Metrics_CFA)

summary(Fit_Attack_Model, fit.measures = TRUE, standardized = TRUE)

Defense_Model <- ' Defense =~ Goals_Conceded + Pressing + Conversion_Conceded + Contention + PPDA + Shots_Conceded '

Fit_Defense_Model <- cfa(Defense_Model, data= Metrics_CFA)

summary(Fit_Defense_Model, fit.measures = TRUE, standardized = TRUE)

Possession_Model <- ' Possession =~ Possession_Time + Passing_Speed + Ball_Care + Build_Up + Progression + Imposition '

Fit_Possession_Model <- cfa(Possession_Model, data= Metrics_CFA)

summary(Fit_Possession_Model, fit.measures = TRUE, standardized = TRUE)

Set_Pieces_Model <- ' Set_Pieces =~ a*Set_Piece_shot + a*Set_Pieces_Conceded '

Fit_Set_Pieces_Model <- cfa(Set_Pieces_Model, data= Metrics_CFA)

summary(Fit_Set_Pieces_Model, fit.measures = TRUE, standardized = TRUE)

Total_Model <- '
              Attack =~ Conversion + Goals + Attacks_Shot + Shot_Assist
              Defense =~ Conversion_Conceded + Goals_Conceded + Pressing + Contention + PPDA + Shots_Conceded 
              Possession =~ Possession_Time + Passing_Speed + Ball_Care + Build_Up + Progression + Imposition
              kpi =~ Possession + Attack + Defense
              '                                              

Fit_Total_Model <- cfa(Total_Model, data= Metrics_CFA, check.gradient = FALSE)
                                       
summary(Fit_Total_Model, fit.measures = TRUE, standardized = TRUE)

fit_indices <- c("Índice", "Resultado", "Parâmetro", "CFI", 0594, "> 0.9", "TLI", 0.518, "> 0.9", 
                 "RMSEA", 0.176, "< 0.05", "SRMR", 0.164, "< 0.08")

fit_indices_tabela <- matrix(fit_indices, nrow = 5, ncol = 3, byrow = TRUE)

semPaths(Fit_Total_Model, "par", weighted = FALSE, nCharNodes = 7, shapeMan = "rectangle",
         sizeMan = 8, sizeMan2 = 5)

semPlot::semPaths(Fit_Modelo, "std")

ranking <- Metrics_CFA %>% mutate(Attack = Conversion + Goals + Attacks_Shot + Shot_Assist,
                                     Defense = (Pressing + Contention + PPDA) - 
                                       (Conversion_Conceded + Goals_Conceded + Shots_Conceded),
                                     Possession = Possession_Time + Passing_Speed + 
                                       Ball_Care + Build_Up + Progression + Imposition,
                                     Set_Pieces = Set_Piece_shot - Set_Pieces_Conceded,
                                     KPI = Attack + Defense + Possession + Set_Pieces) %>%
                              select(Teams, KPI, Attack, Defense, Possession, Set_Pieces) 

ranking[, c(2:6)] <- apply(Metrics_CFA[, c(2:6)],  2, scale)

ranking <- ranking %>% mutate_if(is.numeric, round, 2)

ranking_total <- ranking %>% group_by(Teams) %>% 
                             summarise(Attack_Total=sum(Attack),
                                       Defense_Total=sum(Defense),
                                       Possession_Total=sum(Possession),
                                       Set_Pieces=sum(Set_Pieces),
                                       KPI_Total = sum(KPI)) %>%
                             select(Teams, KPI_Total, Attack_Total, Defense_Total, Possession_Total, Set_Pieces) %>%
                             arrange(desc(KPI_Total))