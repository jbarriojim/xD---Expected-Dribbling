import numpy as np
import pandas as pd

# Definir la clase xDCalculator
class xDCalculator:
  def __init__(self):
    pass

  def _process_freeze_frame(self, freeze_frame):
    # Separate coordinates if location is a list/array
    if 'location' in freeze_frame.columns:
      freeze_frame = freeze_frame.copy()
      freeze_frame['x'] = freeze_frame['location'].apply(
        lambda loc: loc[0] if isinstance(loc, (list, np.ndarray)) else loc
      )
      freeze_frame['y'] = freeze_frame['location'].apply(
        lambda loc: loc[1] if isinstance(loc, (list, np.ndarray)) else loc
      )

    # Find the actor (dribbler)
    actor_row = freeze_frame[freeze_frame['actor'] == True]
    if len(actor_row) == 0:
      raise ValueError("No actor found in freeze frame")

    dribbler_x = actor_row.iloc[0]['x']
    dribbler_y = actor_row.iloc[0]['y']
    dribbler_location = (dribbler_x, dribbler_y)

    # Filter out only the dribbler himself
    players = freeze_frame[~freeze_frame['actor']].copy()

    # Calculate distances
    players['distance'] = players.apply(
      lambda row: np.sqrt(
        (row['x'] - dribbler_x)**2 +
        (row['y'] - dribbler_y)**2
      ),
      axis=1
    )

    # Separate teammates and opponents
    teammates = players[players['teammate'] == True]
    opponents = players[players['teammate'] == False]

    # Calculate teammate counts by distance
    teammates_close = len(teammates[teammates['distance'] < 3])
    teammates_medium = len(teammates[(teammates['distance'] >= 3) &
                                     (teammates['distance'] < 5)])
    teammates_far = len(teammates[(teammates['distance'] >= 5) &
                                  (teammates['distance'] < 10)])

    # Calculate defender counts by distance
    defenders_very_close = len(opponents[opponents['distance'] < 2])
    defenders_close = len(opponents[(opponents['distance'] >= 2) &
                                    (opponents['distance'] < 5)])

    # Find closest defender
    if len(opponents) > 0:
      closest_defender_distance = opponents['distance'].min()
    else:
      closest_defender_distance = 10  # Default if no defenders

    return {
      'dribbler_location': dribbler_location,
      'teammates_close': teammates_close,
      'teammates_medium': teammates_medium,
      'teammates_far': teammates_far,
      'closest_defender_distance': closest_defender_distance,
      'defenders_very_close': defenders_very_close,
      'defenders_close': defenders_close
    }

  def calculate_xd(self, velocity, angle, freeze_frame, minute=45,
                   attacker_minutes_played=45, defender_minutes_played=45,
                   goal_difference=0):
    # Process freeze frame to get distance variables and dribbler location
    distance_vars = self._process_freeze_frame(freeze_frame)
    x, y = distance_vars['dribbler_location']

    # Calculate Attack Component (A)

    # Z_campo: Field value
    if x < 60:  # Own half
      z_campo = 0.09 * np.exp(0.027 * x)
    else:  # Opponent half
      z_campo = 0.09 * np.exp(0.021 * x) + 0.008 * (x - 60)

    # V: Velocity factor
    v_norm = velocity / 36  # Max speed ~36 km/h
    V = 0.5 + 0.3 * v_norm

    # C: Teammate support
    c_close = 1 + 0.06 * distance_vars['teammates_close']
    c_medium = 1 + 0.03 * distance_vars['teammates_medium']
    c_far = 1 + 0.015 * distance_vars['teammates_far']
    C = c_close * c_medium * c_far

    # M_d: Movement direction
    angle_rad = np.radians(angle)
    center_y = 40  # Field center
    y_factor = 1 - (abs(y - center_y) / 40) * 0.3
    M_d = 0.725 - 0.125 * np.cos(angle_rad) * y_factor

    # Total Attack
    A = z_campo * V * C * M_d

    # Calculate Defense Component (D)

    # D_presion: Defensive pressure
    d_dist_norm = distance_vars['closest_defender_distance'] / 10
    D_presion = 0.3 + 0.4 * (1 - d_dist_norm)**2

    # D_cobertura: Defensive coverage
    D_cobertura = (0.7 +
                   0.15 * distance_vars['defenders_very_close'] +
                   0.10 * distance_vars['defenders_close'])

    # M: Physical mismatch
    fatigue_att = 1 - (attacker_minutes_played / 90) * 0.15
    fatigue_def = 1 - (defender_minutes_played / 90) * 0.15
    M = 0.33 + 0.67 * (fatigue_att / fatigue_def)

    # Total Defense
    D = D_presion * D_cobertura * M

    # Calculate Context Component (X)

    # Time pressure
    if minute <= 75:
      time_factor = 1.0
    else:
      time_factor = 1.0 + 0.02 * (minute - 75)

    # Score pressure
    if goal_difference == 0:
      score_factor = 1.0
    elif goal_difference > 0:
      score_factor = 0.95
    else:
      score_factor = 1.05

    X = 0.99 * time_factor * score_factor

    # Final xD calculation
    xD = A * (1 - D) * X

    # Ensure xD is between 0 and 1
    xD = max(0, min(1, xD))

    return {
      'xD': xD,
      'xD_percentage': f"{xD * 100:.1f}%",
      'location': {'x': x, 'y': y},
      'components': {
        'attack': A,
        'defense': D,
        'context': X
      },
      'factors': {
        'z_campo': z_campo,
        'velocity': V,
        'teammates': C,
        'direction': M_d,
        'pressure': D_presion,
        'coverage': D_cobertura,
        'physical': M
      },
      'distances': {k: v for k, v in distance_vars.items() if k != 'dribbler_location'}
    }