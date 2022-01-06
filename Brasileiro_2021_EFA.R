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

Metrics_EFA <- Brasileiro_2021 %>% select(Teams, Points, Goals, Goals_Conceded, 
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

correlation <- cor(Metrics_EFA[ , 2:24])

covariance <- cov(Metrics_EFA[ , 2:24])

variance <- var(Metrics_EFA[ , 2:24])

cortest.bartlett(correlation, n = 758)

KMO(Metrics_EFA[ , 2:24])

lowerCor(Metrics_EFA[ , 2:24])

fa.parallel(Metrics_EFA[ , 2:24],fm="pa", fa="fa", n.iter=500)

fit <- fa(Metrics_EFA[ , 2:24], nfactors = 4, rotate = "Promax", scores = TRUE, fm = "ml")

View(fit)

print(fit, cut = .30, sort = TRUE, digits = 3)

Pattern_Matrix <- fit$loadings[1:23, ]

fit$Structure[1:23, ]

fa.diagram(fit, digits = 2, main = "Factor Diagram", 
           cut = .30, 
           simple = T, 
           errors = T)

Scores <- factor.scores(Metrics_EFA[ , 2:24],fit, 
                         Phi = NULL, 
                         method = "tenBerge",
                         rho=NULL)

Scores <- as.data.frame(Scores$scores)

Scores_Bind <- bind_cols(Metrics_EFA, Scores)

Scores_Bind <- Scores_Bind %>% select(1, 25:28) %>%
                               mutate(Ranking = ML4 + ML2 + ML3 - ML1) %>%
                               select(Teams, Ranking, everything())

Scores_Final <- Scores_Bind %>% group_by(Teams) %>% 
                                summarise(Ranking_Total=sum(Ranking),
                                          ML4_Total=sum(ML4),
                                          ML2_Total=sum(ML2),
                                          ML3_Total=sum(ML3),
                                          ML1_Total=sum(ML1)) %>%
                                mutate(Expectancy = ML4_Total,
                                         Possession = ML2_Total,
                                         Efficiency_Off = ML2_Total,
                                         Efficiency_Def = ML1_Total) %>%
                                select(Teams, Ranking_Total, Expectancy, 
                                       Possession, Efficiency_Off, Efficiency_Def) %>%
                                mutate_if(is.numeric, round, 2) %>%
                                arrange(desc(Ranking_Total))
