# stat_definitions.R

stat_definitions <- list(
  offensive = list(
    B1 = list(group = "Cognition", plus = "player stats:B1 +", minus = "player stats:B1 -"),
    D2 = list(group = "Cognition", plus = "player stats:D2 +", minus = "player stats:D2 -"),
    EPA = list(group = "Cognition", plus = "player stats:EPA +", minus = "player stats:EPA -"),
    P5 = list(group = "Cognition", plus = "player stats:P5 +", minus = "player stats:P5 -"),
    Screener = list(group = "Cognition", plus = "player stats:Screener +", minus = "player stats:Screener -"),
    Handler = list(group = "Cognition", plus = "player stats:Handler +", minus = "player stats:Handler -"),
    
    Cut = list(group = "Spacing", plus = "player stats:Cut +", minus = "player stats:Cut -"),
    PullBehind = list(group = "Spacing", plus = "player stats:Pull Behind +", minus = "player stats:Pull Behind -"),
    EmptyOut = list(group = "Spacing", plus = "player stats:Empty Out +", minus = "player stats:Empty Out -"),
    Squeeze = list(group = "Spacing", plus = "player stats:Squeeze +", minus = "player stats:Squeeze -"),
    Exits = list(group = "Spacing", plus = "player stats:Exits +", minus = "player stats:Exits -"),
    
    OutsideFoot = list(group = "Process", plus = "player stats:Outside Foot +", minus = "player stats:Outside Foot -"),
    JJ = list(group = "Process", plus = "player stats:JJ +", minus = "player stats:JJ -"),
    Stampede = list(group = "Process", plus = "player stats:Stampede +", minus = "player stats:Stampede -"),
    Tipper = list(group = "Process", plus = "player stats:Tipper +", minus = "player stats:Tipper -"),
    
    InsideOut3 = list(group = "Shooting", plus = "shot chart:IO3 +", minus = "shot chart:IO3 -"),
    NonInsideOut3 = list(group = "Shooting", plus = "shot chart:NIO3 +", minus = "shot chart:NIO3 -"),
    RedZone = list(group = "Shooting", plus = "shot chart:RZ +", minus = "shot chart:RZ -")
  ),
  
  defensive = list(
    StickHand = list(group = "Overall Defense", plus = "player stats:StickHand +", minus = "player stats:StickHand -"),
    SprintToGap = list(group = "Overall Defense", plus = "player stats:SprintToGap +", minus = "player stats:SprintToGap -"),
    MIG = list(group = "Overall Defense", plus = "player stats:MIG +", minus = "player stats:MIG -"),
    XOut = list(group = "Overall Defense", plus = "player stats:X Out +", minus = "player stats:X Out -"),
    Crackdown = list(group = "Overall Defense", plus = "player stats:Crackdown +", minus = "player stats:Crackdown -"),
    AttackTheBack = list(group = "Overall Defense", plus = "player stats:AttackTheBack +", minus = "player stats:AttackTheBack -")
  )
)

# Helper functions to extract maps and groups
generate_stat_map <- function(stats) {
  stat_map <- list()
  for (key in names(stats)) {
    stat_map[[key]] <- c(plus = stats[[key]]$plus, minus = stats[[key]]$minus)
  }
  return(stat_map)
}

generate_stat_groups <- function(stats) {
  groups <- list()
  for (key in names(stats)) {
    group <- stats[[key]]$group
    label <- paste(key, "Success Rate")
    groups[[group]] <- c(groups[[group]], label)
  }
  return(groups)
}
