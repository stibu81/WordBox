tmpfile <- tempfile("WordBox_logo", fileext = ".png")

hexSticker::sticker(
  devtools::package_file("man", "figures", "wordbox.png"),
  package = NULL, #"WordBox",
  p_size = 17, p_color = "#5b4e42",
  p_family = "sans",
  p_x = 1, p_y = 0.47,
  s_x = 1, s_y = 1.05, s_width = 0.75,
  h_fill = "#faf2e2",
  h_color = "#51463c",
  h_size = 1.5,
  filename = tmpfile
)

usethis::use_logo(tmpfile, "360x417")
