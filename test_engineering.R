
df <- nflreadr::load_participation(2022, include_pbp = TRUE)


df |> count(old_game_id)



tbl(con, "tracking") |> 
  #filter(event == 'ball_snap')
  filter(game_id == 2022090800, play_id == 101) |> 
  collect() -> play_

df |> 
  filter(old_game_id == 2022090800, play_id == 101) |> 
  mutate(game_id = as.numeric(old_game_id)) -> pbp_

tbl(con, "plays") |> 
  filter(game_id == 2022090800, play_id == 101) |> 
  collect() -> pbp_


tbl(con, "tackles") |> 
  filter(game_id == 2022090800, play_id == 101) |> 
  collect() -> tackle_



template_ <- play_ |> 
  left_join(pbp_) |> 
  left_join(tackle_) |> 
  mut


euc.dist <- function(x1, x2, y1, y2) {
  sqrt( ((x2 - x1) ** 2) + ((y2 - y1) ** 2) )
} 



template_ |> 
  mutate(
    o_std = ifelse(play_direction == 'left', abs(o - 180), o), 
    dir_std = ifelse(play_direction == 'left', abs(dir - 180), dir)
  ) |> 
  group_by(play_id, game_id, frame_id) |> 
  mutate(
    ball_x_std = max(ifelse(ball_carrier_id == nfl_id, x_std, NA), na.rm = TRUE),
    ball_y_std = max(ifelse(ball_carrier_id == nfl_id, y_std, NA), na.rm = TRUE)
    ) |> 
  ungroup() |>
  mutate(
    x_std_end = s*cos((90 - dir_std)*pi / 180) + x_std,
    y_std_end = s*sin((90 - dir_std)*pi / 180) + y_std,
  ) |> 
  #filter(ball_carrier_id == nfl_id | defensive_team == club ) |> 
  #filter(club == 'BUF', frame_id == 1) |> #select(x_std, y_std) -> tmp
  filter(frame_id == 10) |> 
  mutate(bc_dist = pmap_dbl(list(x_std, ball_x_std, y_std, ball_y_std), euc.dist)) |> 
  ggplot(aes(x_std, y_std, color = club, shape = club)) + 
  geom_point(size = 2) +
  geom_segment(aes(x = x_std, y = y_std, xend = x_std_end, yend = y_std_end)) +
  #scale_shape_manual(values = c(1, 16, 4)) +
  coord_fixed(xlim = c(30, 70), ylim = c(0, 50))
  #scale_color_gradient2(midpoint = 8, high = 'blue', mid = 'cyan', low = 'red')


tbl(con, "tracking") |> 
  mutate(
    ball_x_std = sql("MIN(CASE WHEN club = 'football' THEN x_std ELSE NULL END) OVER (PARTITION BY game_id, play_id, frame_id)"),
    ball_y_std = sql("MIN(CASE WHEN club = 'football' THEN y_std ELSE NULL END) OVER (PARTITION BY game_id, play_id, frame_id)"),
    ball_dist = sql("SQRT( POW(x_std - ball_x_std, 2) + POW(y_std - ball_y_std, 2) )")
  ) |> 
  select(x_std, y_std,  ball_x_std, ball_y_std, club, ball_dist) |> 
  filter(club != 'football')


template_ |> 
  mutate(
    o_std = ifelse(play_direction == 'left', abs(o - 180), o), 
    d_std = ifelse(play_direction == 'left', abs(dir - 180), dir)
    )








