
df <- nflreadr::load_participation(2022, include_pbp = TRUE)


df |> count(old_game_id)


tbl(con, "tracking") |> 
  filter(club != 'football') |>
  left_join(
    tbl(con, "tracking") |> 
      filter(club == 'football') |> 
      select(game_id, play_id, frame_id, x = x_std, y = y_std, s, a, dis, o, dir),
    by = c('game_id', 'play_id', 'frame_id'),
    suffix = c('', '_ball')
  ) |> 
  select(-x, -y) |> 
  rename(x = x_std, y = y_std) |> 
  mutate(
    o = ifelse(play_direction == 'left', abs(o - 180), o), 
    o_ball = ifelse(play_direction == 'left', abs(o_ball - 180), o_ball), 
    dir = ifelse(play_direction == 'left', abs(dir - 180), dir),
    dir_ball = ifelse(play_direction == 'left', abs(dir_ball - 180), dir_ball),
    x_end = s*cos((90 - dir)*pi / 180) + x,
    y_end = s*sin((90 - dir)*pi / 180) + y,
    x_ball_end = s_ball*cos((90 - dir_ball) * pi/180) + x_ball,
    y_ball_end = s_ball*sin((90 - dir_ball)*pi / 180) + y_ball,
    angle_to_ball = abs((atan2( (y_ball - y), (x_ball - x)) * (180/pi))),
    dist_from_ball = sqrt( ((x - x_ball) ** 2) + ((y - y_ball) ** 2) )
  ) |> 
  filter(game_id == 2022090800, play_id == 101) |> 
  collect() -> play_


tbl(con, "tracking") |> 
  mutate(
    ball_x_std = sql("MIN(CASE WHEN club = 'football' THEN x_std ELSE NULL END) OVER (PARTITION BY game_id, play_id, frame_id)"),
    ball_y_std = sql("MIN(CASE WHEN club = 'football' THEN y_std ELSE NULL END) OVER (PARTITION BY game_id, play_id, frame_id)"),
    ball_s = sql("MIN(CASE WHEN club = 'football' THEN s ELSE NULL END OVER (PARTITION BY game_id, play_id, frame_id)"),
    ball_dir = sql("MIN(CASE WHEN club = 'football' THEN s ELSE NULL END OVER (PARTITION BY game_id, play_id, frame_id)"),
    o_std = ifelse(play_direction == 'left', abs(o - 180), o), 
    dir_std = ifelse(play_direction == 'left', abs(dir - 180), dir),
    x_std_end = s*cos((90 - dir_std)*pi / 180) + x_std,
    y_std_end = s*sin((90 - dir_std)*pi / 180) + y_std,
    
    angle_to_ball = abs((atan2( (ball_y_std - y_std), (ball_x_std - x_std)) * (180/pi))),
    ball_distance = sqrt( ((x_std - ball_x_std) ** 2) + ((y_std - ball_y_std) ** 2) ),
  ) |> 
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
  left_join(pbp_, by = c('game_id', 'play_id')) |> 
  left_join(tackle_, by = c('game_id', 'play_id', 'nfl_id')) 


library(ggvor)

template_ |> 
  #filter(frame_id  == 1 | frame_id == 30) |> #select(face_ball)
  ggplot(aes(x, y, color = club, shape = club)) + 
  geom_point(size = 2, alpha = 0.6) +
  #geom_text(aes(label = round(angle_to_ball))) +
  # geom_line(aes(x = max(ifelse(nfl_id == ball_carrier_id & frame_id == 1, x_std, NA)),
  #               y = max(ifelse(nfl_id == ball_carrier_id & frame_id == 1, y_std, NA)))) +
  geom_segment(aes(x = x, y = y, xend = x_end, yend = y_end)) +
  #scale_shape_manual(values = c(1, 16, 4)) +
  coord_fixed(xlim = c(30, 70), ylim = c(0, 50)) # + facet_wrap(~frame_id)
#scale_color_gradient2(midpoint = 8, high = 'blue', mid = 'cyan', low = 'red')



example_nfl_play <- 
  template_ |> 
  group_by(play_id, game_id, frame_id) |> 
  mutate(
    ball_x_std = max(ifelse(ball_carrier_id == nfl_id, x_std, NA), na.rm = TRUE),
    ball_y_std = max(ifelse(ball_carrier_id == nfl_id, y_std, NA), na.rm = TRUE)
  ) |> 
  ungroup() |>
  mutate(
    x_std_end = s*cos((90 - dir_std)*pi / 180) + x_std,
    y_std_end = s*sin((90 - dir_std)*pi / 180) + y_std,
  ) 

library(gganimate)
p <- ggplot(example_nfl_play, aes(x_std, y_std, color = club)) +
  geom_point(aes(shape = club), size = 2)  +
  geom_segment(aes(x = x_std, y = y_std, xend = x_std_end, yend = y_std_end)) +
  coord_flip(xlim = c(0, 120), ylim = c(0, 50)) + 
  #coord_fixed(xlim = c(0, 120), ylim = c(0, 50)) +
  #xlim(c(0, 53.3)) +
  transition_time(frame_id) +
  ease_aes('linear') +
  theme(
    legend.position = c(.9, .3)
  ) +
  NULL


p <- ggplot(template_, aes(x, y, color = club)) +
  geom_point(aes(shape = club), size = 2)  +
  geom_segment(aes(x = x, y = y, xend = x_end, yend = y_end)) +
  coord_flip(xlim = c(0, 120), ylim = c(0, 50)) + 
  #coord_fixed(xlim = c(0, 120), ylim = c(0, 50)) +
  #xlim(c(0, 53.3)) +
  transition_time(frame_id) +
  ease_aes('linear') +
  theme(
    legend.position = c(.9, .3)
  ) +
  NULL


a <- animate(p, duration = 5, fps = 10, nframe = length(unique(example_nfl_play$frame_id)))
anim_save('~/Desktop/test.gif', a)




