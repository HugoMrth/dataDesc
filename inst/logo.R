library(hexSticker)
        
        
#### Pour enlever le fond d'une image
# https://www.remove.bg/fr


# Create the sticker
sticker(
  subplot = "C:/Users/MARTHINETEX/Downloads/epiplus-main/360_F_182222479_ltyLPNCmQcAuSTnXHNJONIdg8FvA97Bk-removebg-preview.png",
  filename = "epiplus_sticker.png",  # Output file name
  #img = "C:/Users/MARTHINETEX/Documents/02 - Code/logo.png",     # Path to your background image
  package = "epiplus",                 # Package name
  p_size = 16,
  p_y = 1.23,
  p_x = 1.4,# Text size
  s_x = 0.85,                            # X position of the image
  s_y = 1,                         # Y position of the image
  s_width = 0.75,                      # Width of the image
  h_fill = "#FFFFFF",                 # Background color
  h_color = "#1F77B4",                # Hex border color
  p_color = "#1F77B4",                # Package name color
  layout = "straight",                # Text layout
  text_y = 1.2                       # Y position of the text
)
