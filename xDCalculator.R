#' Calculate expected dribbling success (xD)
#' 
#' @param velocity Attacker velocity in km/h
#' @param angle Movement direction relative to goal (degrees)
#' @param freeze_frame DataFrame with player positions (teammate, actor, keeper, location)
#' @param minute Current match minute
#' @param attacker_minutes_played Minutes played by attacker
#' @param defender_minutes_played Minutes played by defender
#' @param goal_difference Current goal difference
#' @return List with xD value and component breakdowns
calculate_xd <- function(velocity=15, angle=15, freeze_frame,
                         minute = 45,
                         attacker_minutes_played = 45,
                         defender_minutes_played = 45,
                         goal_difference = 0) {
  
  # Process freeze frame to get distance variables and dribbler location
  distance_vars <- process_freeze_frame(freeze_frame)
  
  # Extract coordinates
  x <- distance_vars$dribbler_location[1]
  y <- distance_vars$dribbler_location[2]
  
  # Extract processed variables
  teammates_close <- distance_vars$teammates_close
  teammates_medium <- distance_vars$teammates_medium
  teammates_far <- distance_vars$teammates_far
  closest_defender_distance <- distance_vars$closest_defender_distance
  defenders_very_close <- distance_vars$defenders_very_close
  defenders_close <- distance_vars$defenders_close
  
  # Calculate Attack Component (A)
  
  # Z_campo: Field value
  if (x < 60) {
    z_campo <- 0.09 * exp(0.027 * x)
  } else {
    z_campo <- 0.09 * exp(0.021 * x) + 0.008 * (x - 60)
  }
  
  # V: Velocity factor
  v_norm <- velocity / 36
  V <- 0.5 + 0.3 * v_norm
  
  # C: Teammate support
  c_close <- 1 + 0.06 * teammates_close
  c_medium <- 1 + 0.03 * teammates_medium
  c_far <- 1 + 0.015 * teammates_far
  C <- c_close * c_medium * c_far
  
  # M_d: Movement direction
  angle_rad <- angle * pi / 180
  center_y <- 40
  y_factor <- 1 - (abs(y - center_y) / 40) * 0.3
  M_d <- 0.725 - 0.125 * cos(angle_rad) * y_factor
  
  # Total Attack
  A <- z_campo * V * C * M_d
  
  # Calculate Defense Component (D)
  
  # D_presion: Defensive pressure
  d_dist_norm <- closest_defender_distance / 10
  D_presion <- 0.3 + 0.4 * (1 - d_dist_norm)^2
  
  # D_cobertura: Defensive coverage
  D_cobertura <- 0.7 + 0.15 * defenders_very_close + 0.10 * defenders_close
  
  # M: Physical mismatch
  fatigue_att <- 1 - (attacker_minutes_played / 90) * 0.15
  fatigue_def <- 1 - (defender_minutes_played / 90) * 0.15
  M <- 0.33 + 0.67 * (fatigue_att / fatigue_def)
  
  # Total Defense
  D <- D_presion * D_cobertura * M
  
  # Calculate Context Component (X)
  
  # Time pressure
  if (minute <= 75) {
    time_factor <- 1.0
  } else {
    time_factor <- 1.0 + 0.02 * (minute - 75)
  }
  
  # Score pressure
  if (goal_difference == 0) {
    score_factor <- 1.0
  } else if (goal_difference > 0) {
    score_factor <- 0.95
  } else {
    score_factor <- 1.05
  }
  
  X <- 0.99 * time_factor * score_factor
  
  # Final xD calculation
  xD <- A * (1 - D) * X
  
  # Ensure xD is between 0 and 1
  xD <- max(0, min(1, xD))
  
  # Return results
  list(
    xD = xD,
    xD_percentage = paste0(round(xD * 100, 1), "%"),
    location = list(x = x, y = y),
    components = list(
      attack = A,
      defense = D,
      context = X
    ),
    factors = list(
      z_campo = z_campo,
      velocity = V,
      teammates = C,
      direction = M_d,
      pressure = D_presion,
      coverage = D_cobertura,
      physical = M
    ),
    distances = list(
      teammates_close = teammates_close,
      teammates_medium = teammates_medium,
      teammates_far = teammates_far,
      closest_defender_distance = closest_defender_distance,
      defenders_very_close = defenders_very_close,
      defenders_close = defenders_close
    )
  )
}

#' Process freeze frame to extract distance-based variables
#' 
#' @param freeze_frame DataFrame with columns teammate, actor, keeper, location
#' @return List with all calculated distances and counts, including dribbler location
process_freeze_frame <- function(freeze_frame) {
  
  # Find the actor (dribbler)
  actor_row <- freeze_frame[freeze_frame$actor == TRUE, ]
  if (nrow(actor_row) == 0) {
    stop("No actor found in freeze frame")
  }
  
  dribbler_x <- actor_row$location.x[1]
  dribbler_y <- actor_row$location.y[1]
  dribbler_location <- c(dribbler_x, dribbler_y)
  
  # Filter out only the dribbler
  players <- freeze_frame[!freeze_frame$actor, ]
  
  # Calculate distances
  players$distance <- sqrt(
    (players$location.x - dribbler_x)^2 + 
      (players$location.y - dribbler_y)^2
  )
  
  # Separate teammates and opponents
  teammates <- players[players$teammate == TRUE, ]
  opponents <- players[players$teammate == FALSE, ]
  
  # Calculate teammate counts
  teammates_close <- sum(teammates$distance < 3)
  teammates_medium <- sum(teammates$distance >= 3 & teammates$distance < 5)
  teammates_far <- sum(teammates$distance >= 5 & teammates$distance < 10)
  
  # Calculate defender counts
  defenders_very_close <- sum(opponents$distance < 2)
  defenders_close <- sum(opponents$distance >= 2 & opponents$distance < 5)
  
  # Find closest defender distance
  if (nrow(opponents) > 0) {
    closest_defender_distance <- min(opponents$distance)
  } else {
    closest_defender_distance <- 10
  }
  
  list(
    dribbler_location = dribbler_location,
    teammates_close = teammates_close,
    teammates_medium = teammates_medium,
    teammates_far = teammates_far,
    closest_defender_distance = closest_defender_distance,
    defenders_very_close = defenders_very_close,
    defenders_close = defenders_close
  )
}